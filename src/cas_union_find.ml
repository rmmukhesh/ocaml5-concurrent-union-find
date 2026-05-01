(* 
  Implements Disjoint Set Union (Union-Find) using Compare-and-Set (CAS) operations
  for concurrent union and find operations. This implementation uses path compression 
  with halving and union by rank to optimize the structure. It also tracks various 
  stats like cas_attempts, cas_failures, find_calls, union_calls,  for performance analysis.
*)

type t =
  {
    size : int;
    parent : int Atomic.t array;
    rank : int Atomic.t array;
    counters : Union_find_stats.counters;
  }

let create size =
  Union_find_common.check_size size;
  {
    size;
    parent = Array.init size (fun i -> Atomic.make i);
    rank = Array.init size (fun _ -> Atomic.make 0);
    counters = Union_find_stats.create ();
  }

let rec find_root t x =
  let parent = Atomic.get t.parent.(x) in
  (* if parent of x is x, then x is a root *)
  if parent = x then x
  else begin
    let grandparent = Atomic.get t.parent.(parent) in
    (* if grandparent is same as parent, then parent is the root
    else we perform path compression by halving the path 
    i.e., we assign the grandparent of x as parent of x and find the root of parent *)
    if grandparent = parent then parent
    else begin
      Union_find_stats.bump t.counters.cas_attempts;
      
      if Atomic.compare_and_set t.parent.(x) parent grandparent then
        Union_find_stats.bump t.counters.path_compactions
      else Union_find_stats.bump t.counters.cas_failures;
      
      find_root t parent 
    end
  end

let find t x =
  Union_find_common.check_index t.size x;
  Union_find_stats.bump t.counters.find_calls;
  find_root t x

let union t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  Union_find_stats.bump t.counters.union_calls;
  let rec attempt () =
    let root_x = find_root t x in
    let root_y = find_root t y in
    (* if roots are the same, sets are already in the same component *)
    if root_x = root_y then false
    else
      let rank_x = Atomic.get t.rank.(root_x) in
      let rank_y = Atomic.get t.rank.(root_y) in
      let winner, loser, same_rank =
        Union_find_common.choose_link root_x rank_x root_y rank_y
      in
      Union_find_stats.bump t.counters.cas_attempts;
      (* if cas succeeds that means no other thread modified the parent of loser 
        i.e., the loser was still the root of its component *)
      if Atomic.compare_and_set t.parent.(loser) loser winner then (
        (* if the winner and loser had same rank, we need to increment the rank of the winner *)
        if same_rank && Atomic.get t.parent.(winner) = winner then (
          let current_rank = Atomic.get t.rank.(winner) in
          Union_find_stats.bump t.counters.cas_attempts;
          if Atomic.compare_and_set t.rank.(winner) current_rank (current_rank + 1)
          then Union_find_stats.bump t.counters.rank_updates
          else Union_find_stats.bump t.counters.cas_failures);
        true)
      (* if cas fails, it means another thread modified the parent of loser 
         i.e., the loser is no longer the root of its component *)
      else (
        Union_find_stats.bump t.counters.cas_failures;
        attempt ())
  in
  attempt ()

let same_set t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  if x = y then true
  else
    let rec attempt () =
      let root_x = find_root t x in
      let root_y = find_root t y in
      if root_x = root_y then true
      else if Atomic.get t.parent.(root_x) <> root_x then attempt ()
      else if Atomic.get t.parent.(root_y) <> root_y then attempt ()
      else false
    in
    attempt ()

let component_count t =
  Union_find_common.component_count ~size:t.size ~find_root:(find_root t)

let stats t = Union_find_stats.snapshot t.counters

let reset_stats t = Union_find_stats.reset t.counters
