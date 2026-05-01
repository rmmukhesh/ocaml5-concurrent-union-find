let check_size size =
  if size < 0 then invalid_arg "Union-find size must be non-negative"

let normalize_domains domains =
  if domains <= 0 then invalid_arg "domains must be positive" else domains

let check_index size x =
  if x < 0 || x >= size then invalid_arg "Union-find index out of bounds"

let choose_link x rank_x y rank_y =
  if rank_x > rank_y then (x, y, false)
  else if rank_y > rank_x then (y, x, false)
  else if x <= y then (x, y, true)
  else (y, x, true)

let component_count ~size ~find_root =
  let seen = Array.make size false in
  let count = ref 0 in
  for i = 0 to size - 1 do
    let root = find_root i in
    if not seen.(root) then (
      seen.(root) <- true;
      incr count)
  done;
  !count
