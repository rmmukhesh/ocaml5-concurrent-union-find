open Union_find

type edge = int * int

let run_edges
    (type a)
    (module Uf : DSU with type t = a)
    ~nodes
    (edges : edge array) =
  let uf = Uf.create nodes in
  Array.iter (fun (u, v) -> ignore (Uf.union uf u v)) edges;
  uf

let run_parallel
    (type a)
    (module Uf : DSU with type t = a)
    ~nodes
    ~domains
    (edges : edge array) =
  let domains = Union_find_common.normalize_domains domains in
  let uf = Uf.create nodes in
  let edge_count = Array.length edges in
  let worker domain_index =
    let start = (domain_index * edge_count) / domains in
    let stop = ((domain_index + 1) * edge_count) / domains in
    for i = start to stop - 1 do
      let u, v = edges.(i) in
      ignore (Uf.union uf u v)
    done
  in
  if domains = 1 then worker 0
  else (
    let spawned =
      Array.init (domains - 1) (fun domain_index ->
          Domain.spawn (fun () -> worker domain_index))
    in
    worker (domains - 1);
    Array.iter Domain.join spawned);
  uf

let labels
    (type a)
    (module Uf : DSU with type t = a)
    ~nodes
    uf =
  Array.init nodes (fun i -> Uf.find uf i)
