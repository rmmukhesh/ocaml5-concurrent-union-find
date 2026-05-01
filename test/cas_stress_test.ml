open Test_support

let compare_partitions ~context expected actual =
  assert_partition_equal ~context expected actual

let graph_stress_trials () =
  let state = Random.State.make [| 99; 123; 5 |] in
  for trial = 1 to 30 do
    let nodes = 12 + Random.State.int state 40 in
    let random_edges = Graph_generators.erdos_renyi ~nodes ~avg_degree:12 ~state in
    let star_edges = Graph_generators.star ~nodes in
    let edges = Array.append random_edges star_edges in
    let expected = partition_after_edges (module Sequential_union_find) ~nodes edges in
    let actual =
      partition_after_parallel_edges (module Cas_union_find) ~nodes ~domains:4 edges
    in
    compare_partitions
      ~context:(Printf.sprintf "cas graph stress trial %d" trial)
      expected actual
  done

let mixed_operation_trials () =
  let workers = 4 in
  let operations_per_worker = 400 in
  for trial = 1 to 20 do
    let nodes = 24 in
    let uf = Cas_union_find.create nodes in
    let tasks =
      Array.init workers (fun worker_id ->
          Domain.spawn (fun () ->
              let state = Random.State.make [| trial; worker_id; 2026 |] in
              let unions = ref [] in
              for _ = 1 to operations_per_worker do
                match Random.State.int state 10 with
                | 0 | 1 | 2 | 3 | 4 | 5 ->
                  let u =
                    if Random.State.bool state then 0
                    else Random.State.int state nodes
                  in
                  let v = Random.State.int state nodes in
                  ignore (Cas_union_find.union uf u v);
                  unions := (u, v) :: !unions
                | 6 | 7 ->
                  ignore (Cas_union_find.find uf (Random.State.int state nodes))
                | _ ->
                  let x = Random.State.int state nodes in
                  let y = Random.State.int state nodes in
                  ignore (Cas_union_find.same_set uf x y)
              done;
              List.rev !unions))
    in
    let logged_unions = Array.map Domain.join tasks in
    let expected = Sequential_union_find.create nodes in
    Array.iter
      (List.iter (fun (u, v) -> ignore (Sequential_union_find.union expected u v)))
      logged_unions;
    let expected_partition =
      partition_of_uf (module Sequential_union_find) ~nodes expected
    in
    let actual_partition = partition_of_uf (module Cas_union_find) ~nodes uf in
    compare_partitions
      ~context:(Printf.sprintf "cas mixed-operation stress trial %d" trial)
      expected_partition actual_partition;
    let stats = Cas_union_find.stats uf in
    assert_true (stats.Union_find.union_calls > 0)
      "cas stress should record union calls";
    assert_true (stats.find_calls > 0)
      "cas stress should record find calls"
  done

let () =
  Printf.printf "running CAS graph stress trials\n%!";
  graph_stress_trials ();
  Printf.printf "running CAS mixed-operation stress trials\n%!";
  mixed_operation_trials ();
  Printf.printf "CAS stress tests passed\n%!"
