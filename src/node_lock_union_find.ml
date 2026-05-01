type t =
  {
    size : int;
    parent : int Atomic.t array;
    rank : int Atomic.t array;
    locks : Mutex.t array;
    counters : Union_find_stats.counters;
  }

let create size =
  Union_find_common.check_size size;
  {
    size;
    parent = Array.init size (fun i -> Atomic.make i);
    rank = Array.init size (fun _ -> Atomic.make 0);
    locks = Array.init size (fun _ -> Mutex.create ());
    counters = Union_find_stats.create ();
  }

 let rec find_root_no_compress t x =
  let parent = Atomic.get t.parent.(x) in
  if parent = x then x else find_root_no_compress t parent
(*
let find t x =
  Union_find_common.check_index t.size x;
  Union_find_stats.bump t.counters.find_calls;
  let rec collect node path =
    let parent = Atomic.get t.parent.(node) in
    if parent = node then (node, path) else collect parent (node :: path)
  in
  let root, path = collect x [] in
  List.iter
    (fun node ->
      if node <> root then (
        Mutex.lock t.locks.(node);
        Fun.protect
          ~finally:(fun () -> Mutex.unlock t.locks.(node))
          (fun () ->
            let current = Atomic.get t.parent.(node) in
            if current <> root && current <> node then (
              Atomic.set t.parent.(node) root;
              Union_find_stats.bump t.counters.path_compactions))))
    path;
  root *)

let rec find_root_halving t x =
  Mutex.lock t.locks.(x);
  let parent = Atomic.get t.parent.(x) in
  if parent = x then begin
    Mutex.unlock t.locks.(x);
    x
  end else begin
    let grandparent = Atomic.get t.parent.(parent) in
    if grandparent <> parent then
      Atomic.set t.parent.(x) grandparent;
    Mutex.unlock t.locks.(x);
    find_root_halving t parent
  end

let find t x =
  Union_find_common.check_index t.size x;
  Union_find_stats.bump t.counters.find_calls;
  find_root_halving t x

let union t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  Union_find_stats.bump t.counters.union_calls;
  let rec attempt () =
    let root_x = find_root_halving t x in
    let root_y = find_root_halving t y in
    if root_x = root_y then false
    else
      let first, second =
        if root_x < root_y then (root_x, root_y) else (root_y, root_x)
      in
      Mutex.lock t.locks.(first);
      Mutex.lock t.locks.(second);
      (* first, second are used for dead lock prevention. 
      Always lock the one with the smaller index first *)
      let result =
        Fun.protect
          ~finally:(fun () ->
            Mutex.unlock t.locks.(second);
            Mutex.unlock t.locks.(first))
          (fun () ->
            (* if the roots have changed after find, before lock acquisition *)
            if Atomic.get t.parent.(root_x) <> root_x
               || Atomic.get t.parent.(root_y) <> root_y
            then `Retry
            else
              (* if both roots are still valid merger them, update rank if necessary *)
              let rank_x = Atomic.get t.rank.(root_x) in
              let rank_y = Atomic.get t.rank.(root_y) in
              let winner, loser, same_rank =
                Union_find_common.choose_link root_x rank_x root_y rank_y
              in
              Atomic.set t.parent.(loser) winner;
              if same_rank then (
                Atomic.set t.rank.(winner) (Atomic.get t.rank.(winner) + 1);
                Union_find_stats.bump t.counters.rank_updates);
              `Merged)
      in
      match result with
      | `Retry -> attempt ()
      | `Merged -> true
  in
  attempt ()

let same_set t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  if x = y then true
  else
    let rec attempt () =
      let root_x = find_root_halving t x in
      let root_y = find_root_halving t y in
      if root_x = root_y then true
      else if Atomic.get t.parent.(root_x) <> root_x then attempt ()
      else if Atomic.get t.parent.(root_y) <> root_y then attempt ()
      else false
    in
    attempt ()

let component_count t =
  Union_find_common.component_count ~size:t.size ~find_root:(find_root_no_compress t)

let stats t = Union_find_stats.snapshot t.counters

let reset_stats t = Union_find_stats.reset t.counters