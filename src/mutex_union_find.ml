(* 
  Implements Disjoint Set Union (Union-Find) using Mutex locking for concurrent union and find operations.
  This implementation uses full path compression, and union by rank to optimize the structure.
*)

type t =
  {
    size : int;
    parent : int array;
    rank : int array;
    lock : Mutex.t;
    counters : Union_find_stats.counters;
  }

let create size =
  Union_find_common.check_size size;
  {
    size;
    parent = Array.init size Fun.id;
    rank = Array.make size 0;
    lock = Mutex.create ();
    counters = Union_find_stats.create ();
  }

(* Path compression via halving: instead of fully compressing to root,
   we compress by halving the path (setting parent to grandparent).
   We call find_root after obtaining the lock, hence recursing is fine. *)
let rec find_root t x =
  let parent = t.parent.(x) in
  if parent = x then x
  else begin
    let grandparent = t.parent.(parent) in
    if grandparent = parent then parent
    else begin
      t.parent.(x) <- grandparent;
      Union_find_stats.bump t.counters.path_compactions;
      find_root t parent
    end
  end

(* equivalent to 
Mutex.lock t.lock; 
let result = f () in 
Mutex.unlock t.lock;
result 
*)
let with_lock t f =
  Mutex.lock t.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) f

let find t x =
  Union_find_common.check_index t.size x;
  Union_find_stats.bump t.counters.find_calls;
  with_lock t (fun () -> find_root t x)


(* 
  We acquire the lock before performing union to ensure that the union operation is atomic 
  i.e., we won't have any other thread modifying the structure while we are performing union.
  Union x y :- if x and y are of same rank, we take the smaller root as the new root and increment its rank by 1.
  else we take the root with higher rank as the new root. 
*)
let union t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  Union_find_stats.bump t.counters.union_calls;
  with_lock t (fun () ->
      let root_x = find_root t x in
      let root_y = find_root t y in
      if root_x = root_y then false
      else
        let rank_x = t.rank.(root_x) in
        let rank_y = t.rank.(root_y) in
        let winner, loser, same_rank =
          Union_find_common.choose_link root_x rank_x root_y rank_y
        in
        t.parent.(loser) <- winner;
        if same_rank then (
          t.rank.(winner) <- t.rank.(winner) + 1;
          Union_find_stats.bump t.counters.rank_updates);
        true)

(* 
  Acquire the lock, find the roots of x and y, and check if they are the same.
*)
let same_set t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  with_lock t (fun () -> find_root t x = find_root t y)

let component_count t =
  with_lock t (fun () ->
      Union_find_common.component_count ~size:t.size ~find_root:(find_root t))

let stats t = Union_find_stats.snapshot t.counters

let reset_stats t = Union_find_stats.reset t.counters
