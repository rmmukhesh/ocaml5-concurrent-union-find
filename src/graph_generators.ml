type edge = Graph_connectivity.edge

let shuffle_in_place state arr =
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.State.int state (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done

let random_non_self_edge ~nodes state =
  if nodes <= 1 then (0, 0)
  else
    let u = Random.State.int state nodes in
    let rec pick_v () =
      let v = Random.State.int state nodes in
      if v = u then pick_v () else v
    in
    (u, pick_v ())

(* Generates a random graph following the simplified Erdős–Rényi model
    1. Fixes the number of edges to be generated
    2. An edge can repeat *)
let erdos_renyi ~nodes ~avg_degree ~state =
  if nodes <= 1 || avg_degree <= 0 then [||]
  else
    let edge_count = max 0 ((nodes * avg_degree) / 2) in
    Array.init edge_count (fun _ -> random_non_self_edge ~nodes state)

let star ~nodes =
  if nodes <= 1 then [||]
  else Array.init (nodes - 1) (fun i -> (0, i + 1))

let disconnected_components sizes =
  let rec build offset edges = function
    | [] -> List.rev edges
    | size :: rest ->
      let component_edges =
        if size <= 1 then []
        else List.init (size - 1) (fun i -> (offset + i, offset + i + 1))
      in
      build (offset + max size 0) (List.rev_append component_edges edges) rest
  in
  build 0 [] sizes |> Array.of_list
