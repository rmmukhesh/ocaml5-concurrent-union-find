open Union_find

type packed_impl = Impl : string * (module DSU) -> packed_impl

let format_int_array array =
  array
  |> Array.to_list
  |> List.map string_of_int
  |> String.concat "; "
  |> Printf.sprintf "[|%s|]"

let canonicalize_labels labels =
  let mapping = Hashtbl.create (Array.length labels) in
  let next_label = ref 0 in
  Array.map
    (fun label ->
      match Hashtbl.find_opt mapping label with
      | Some canonical -> canonical
      | None ->
        let canonical = !next_label in
        incr next_label;
        Hashtbl.add mapping label canonical;
        canonical)
    labels

let partition_of_uf
    (type a)
    (module Uf : DSU with type t = a)
    ~nodes
    uf =
  Graph_connectivity.labels (module Uf) ~nodes uf |> canonicalize_labels

let partition_after_edges
    (module Uf : DSU)
    ~nodes
    edges =
  let uf = Graph_connectivity.run_edges (module Uf) ~nodes edges in
  partition_of_uf (module Uf) ~nodes uf

let partition_after_parallel_edges
    (module Uf : DSU)
    ~nodes
    ~domains
    edges =
  let uf = Graph_connectivity.run_parallel (module Uf) ~nodes ~domains edges in
  partition_of_uf (module Uf) ~nodes uf

let equal_partition left right =
  Array.length left = Array.length right
  && Array.for_all2 Int.equal left right

let failf fmt = Printf.ksprintf failwith fmt

let assert_true condition message =
  if not condition then failwith message

let assert_partition_equal ~context expected actual =
  if not (equal_partition expected actual) then
    failf "%s\nexpected: %s\nactual:   %s" context
      (format_int_array expected)
      (format_int_array actual)
