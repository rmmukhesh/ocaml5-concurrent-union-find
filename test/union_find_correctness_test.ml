open Test_support

let all_impls : packed_impl list =
  [
    Impl ("sequential", (module Sequential_union_find : Union_find.DSU));
    Impl ("mutex", (module Mutex_union_find : Union_find.DSU));
    Impl ("node-lock", (module Node_lock_union_find : Union_find.DSU));
    Impl ("cas", (module Cas_union_find : Union_find.DSU));
  ]

let concurrent_impls : packed_impl list =
  [
    Impl ("mutex", (module Mutex_union_find : Union_find.DSU));
    Impl ("node-lock", (module Node_lock_union_find : Union_find.DSU));
    Impl ("cas", (module Cas_union_find : Union_find.DSU));
  ]

let zero_stats =
  {
    Union_find.find_calls = 0;
    union_calls = 0;
    cas_attempts = 0;
    cas_failures = 0;
    path_compactions = 0;
    rank_updates = 0;
  }

let component_count_of_partition partition =
  if Array.length partition = 0 then 0
  else 1 + Array.fold_left max (-1) partition

let check_stats_change name (module Uf : Union_find.DSU) =
  let uf = Uf.create 4 in
  ignore (Uf.union uf 0 1);
  ignore (Uf.find uf 0);
  assert_true (Uf.stats uf <> zero_stats)
    (Printf.sprintf "%s: stats should change after operations" name)

let check_basic_operations name (module Uf : Union_find.DSU) =
  let uf = Uf.create 6 in
  assert_true (Uf.component_count uf = 6)
    (Printf.sprintf "%s: initial component count should match node count" name);
  assert_true (Uf.union uf 0 1)
    (Printf.sprintf "%s: first union should merge two singleton sets" name);
  assert_true (not (Uf.union uf 0 1))
    (Printf.sprintf "%s: repeated union should report no merge" name);
  assert_true (not (Uf.union uf 1 0))
    (Printf.sprintf "%s: symmetric repeated union should report no merge" name);
  assert_true (not (Uf.union uf 2 2))
    (Printf.sprintf "%s: self-union should report no merge" name);
  ignore (Uf.union uf 1 2);
  ignore (Uf.union uf 3 4);
  assert_true (Uf.same_set uf 0 2)
    (Printf.sprintf "%s: 0 and 2 should be connected" name);
  assert_true (not (Uf.same_set uf 0 3))
    (Printf.sprintf "%s: 0 and 3 should remain disconnected" name);
  assert_true (Uf.component_count uf = 3)
    (Printf.sprintf "%s: expected 3 components after merges" name);
  let partition = partition_of_uf (module Uf) ~nodes:6 uf in
  assert_partition_equal ~context:(Printf.sprintf "%s: fixed partition mismatch" name)
    [| 0; 0; 0; 1; 1; 2 |]
    partition

let check_empty_and_singleton name (module Uf : Union_find.DSU) =
  let empty = Uf.create 0 in
  assert_true (Uf.component_count empty = 0)
    (Printf.sprintf "%s: empty structure should have zero components" name);
  let singleton = Uf.create 1 in
  assert_true (Uf.find singleton 0 = 0)
    (Printf.sprintf "%s: singleton root should be itself" name);
  assert_true (Uf.same_set singleton 0 0)
    (Printf.sprintf "%s: singleton should be connected to itself" name);
  assert_true (Uf.component_count singleton = 1)
    (Printf.sprintf "%s: singleton should have one component" name)

let check_fixed_graphs name (module Uf : Union_find.DSU) =
  let edges = [| (2, 3); (0, 1); (1, 2); (4, 5); (5, 6) |] in
  let expected = [| 0; 0; 0; 0; 1; 1; 1 |] in
  let partition = partition_after_edges (module Uf) ~nodes:7 edges in
  assert_partition_equal ~context:(Printf.sprintf "%s: permuted union order mismatch" name)
    expected partition;
  (* generates graph with disconnected components whose sizes are 3, 1, and 2 *)
  let disconnected = Graph_generators.disconnected_components [ 3; 1; 2 ] in
  (* runs edges i.e., unions them and gets the final partition i.e., the roots of each index in canonical form *)
  let partition = partition_after_edges (module Uf) ~nodes:6 disconnected in
  assert_partition_equal ~context:(Printf.sprintf "%s: disconnected component mismatch" name)
    [| 0; 0; 0; 1; 2; 2 |]
    partition

let compare_against_reference ~label ~nodes edges =
  let expected = partition_after_edges (module Sequential_union_find) ~nodes edges in
  List.iter
    (fun (Impl (name, implementation)) ->
      let actual = partition_after_edges implementation ~nodes edges in
      assert_partition_equal
        ~context:(Printf.sprintf "%s: %s sequential replay mismatch" label name)
        expected actual;
      let expected_components = component_count_of_partition expected in
      let actual_components =
        let module Uf = (val implementation : Union_find.DSU) in
        let uf = Graph_connectivity.run_edges (module Uf) ~nodes edges in
        Uf.component_count uf
      in
      assert_true (actual_components = expected_components)
        (Printf.sprintf "%s: %s component_count mismatch" label name))
    all_impls;
  List.iter
    (fun (Impl (name, implementation)) ->
      let actual =
        partition_after_parallel_edges implementation ~nodes ~domains:4 edges
      in
      assert_partition_equal
        ~context:(Printf.sprintf "%s: %s parallel replay mismatch" label name)
        expected actual)
    concurrent_impls

let run_random_cross_checks () =
  let state = Random.State.make [| 2026; 4; 22 |] in
  for trial = 1 to 30 do
    let nodes = 1 + Random.State.int state 24 in
    let avg_degree = 1 + Random.State.int state 8 in
    let er = Graph_generators.erdos_renyi ~nodes ~avg_degree ~state in
    compare_against_reference
      ~label:(Printf.sprintf "random-er-%d" trial)
      ~nodes er;
    compare_against_reference
      ~label:(Printf.sprintf "random-star-%d" trial)
      ~nodes
      (Graph_generators.star ~nodes);
    let sizes =
      if nodes < 4 then [ nodes ] else [ nodes / 2; 1; nodes - (nodes / 2) - 1 ]
    in
    compare_against_reference
      ~label:(Printf.sprintf "random-disconnected-%d" trial)
      ~nodes
      (Graph_generators.disconnected_components sizes)
  done

let run_parallel_stress () =
  let state = Random.State.make [| 7; 11; 13 |] in
  for trial = 1 to 20 do
    let nodes = 8 + Random.State.int state 32 in
    let er = Graph_generators.erdos_renyi ~nodes ~avg_degree:10 ~state in
    let hub = Graph_generators.star ~nodes in
    let edges = Array.append er hub in
    compare_against_reference
      ~label:(Printf.sprintf "parallel-stress-%d" trial)
      ~nodes edges
  done

let named_checks =
  [
    ("empty and singleton", check_empty_and_singleton);
    ("basic operations", check_basic_operations);
    ("fixed graphs", check_fixed_graphs);
    ("stats change", check_stats_change);
  ]

let () =
  List.iter
    (fun (Impl (name, implementation)) ->
      List.iter
        (fun (label, check) ->
          Printf.printf "running %s for %s\n%!" label name;
          check name implementation)
        named_checks)
    all_impls;
  Printf.printf "running randomized cross-checks\n%!";
  run_random_cross_checks ();
  Printf.printf "running parallel stress cross-checks\n%!";
  run_parallel_stress ();
  Printf.printf "all deterministic and randomized correctness tests passed\n%!"
