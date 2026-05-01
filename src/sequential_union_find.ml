type t =
  {
    size : int;
    parent : int array;
    rank : int array;
    counters : Union_find_stats.counters;
  }

let create size =
  Union_find_common.check_size size;
  {
    size;
    parent = Array.init size Fun.id;
    rank = Array.make size 0;
    counters = Union_find_stats.create ();
  }

let rec find_root t x =
  let parent = t.parent.(x) in
  if parent = x then x
  else
    let root = find_root t parent in
    if t.parent.(x) <> root then (
      t.parent.(x) <- root;
      Union_find_stats.bump t.counters.path_compactions);
    root

let find t x =
  Union_find_common.check_index t.size x;
  Union_find_stats.bump t.counters.find_calls;
  find_root t x

let union t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  Union_find_stats.bump t.counters.union_calls;
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
    true

let same_set t x y =
  Union_find_common.check_index t.size x;
  Union_find_common.check_index t.size y;
  find_root t x = find_root t y

let component_count t =
  Union_find_common.component_count ~size:t.size ~find_root:(find_root t)

let stats t = Union_find_stats.snapshot t.counters

let reset_stats t = Union_find_stats.reset t.counters