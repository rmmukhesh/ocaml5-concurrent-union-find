open Printf

type graph_profile =
  | Sparse_random
  | Dense_random
  | Star

type config =
  {
    nodes : int;
    operations : int;
    union_percent : int;
    repeats : int;
    warmup : int;
    domains : int list;
  }

type op =
  | Union of int * int
  | Find of int

type sample =
  {
    seconds : float;
    components : int;
    stats : Union_find.stats;
  }

type measurement =
  {
    median_seconds : float;
    min_seconds : float;
    max_seconds : float;
    components : int;
    stats : Union_find.stats;
  }

type workload_summary =
  {
    partition_count : int;
    min_partition_size : int;
    max_partition_size : int;
    operations : int;
    union_ops : int;
    find_ops : int;
  }

let default_config =
  {
    nodes = 200_000;
    operations = 1_000_000;
    union_percent = 20;
    repeats = 3;
    warmup = 1;
    domains = [ 1; 2; 4; 8; 16 ];
  }

let graph_profiles = [ Sparse_random; Dense_random; Star ]

let string_of_graph_profile = function
  | Sparse_random -> "sparse_random"
  | Dense_random -> "dense_random"
  | Star -> "star"

let parse_domains value =
  value |> String.split_on_char ','
  |> List.filter_map (fun part ->
         let part = String.trim part in
         if part = "" then None else Some (int_of_string part))
  |> List.map Union_find_common.normalize_domains

let make_sparse_random_edge ~nodes ~state =
  let u = Random.State.int state nodes in
  let v = Random.State.int state nodes in
  let v = if v = u then (v + 1) mod nodes else v in
  (u, v)

(* Dense random: all edges drawn from a 25%-of-nodes hot zone.
   nodes/20 (5%) was too small — the subgraph converged after ~10k union ops,
   leaving 90% of the run as no-op unions with zero CAS pressure.
   nodes/4 (25%) stays meaningfully denser than sparse_random (which uses all
   nodes) while taking long enough to converge that contention is sustained
   throughout the full run. *)
let make_dense_random_edge ~nodes ~state =
  let active_nodes = max 2 (nodes / 4) in
  let u = Random.State.int state active_nodes in
  let v = Random.State.int state active_nodes in
  let v = if v = u then (v + 1) mod active_nodes else v in
  (u, v)

let make_star_edge ~nodes ~state =
  let leaf = 1 + Random.State.int state (nodes - 1) in
  (0, leaf)

let make_union_edge ~profile ~nodes ~state =
  match profile with
  | Sparse_random -> make_sparse_random_edge ~nodes ~state
  | Dense_random -> make_dense_random_edge ~nodes ~state
  | Star -> make_star_edge ~nodes ~state

let make_edge_pool ~profile ~nodes =
  match profile with
  | Star ->
    Array.init nodes (fun index ->
        let leaf = if index = 0 then 1 else index in
        (0, leaf))
  | Sparse_random | Dense_random ->
    let state =
      Random.State.make
        [| 2026; 4; 29; nodes;
           (match profile with
            | Sparse_random -> 1
            | Dense_random -> 2
            | Star -> 3)
        |]
    in
    Array.init nodes (fun _ -> make_union_edge ~profile ~nodes ~state)

let make_ops ~profile ~nodes ~operations ~union_percent =
  let union_ops = operations * union_percent / 100 in
  let find_ops = operations - union_ops in
  let edge_pool = make_edge_pool ~profile ~nodes in
  let state =
    Random.State.make
      [| 2026; 4; 29; nodes; operations; union_percent;
         (match profile with
          | Sparse_random -> 1
          | Dense_random -> 2
          | Star -> 3)
      |]
  in
  let is_union = Array.init operations (fun index -> index < union_ops) in
  Graph_generators.shuffle_in_place state is_union;
  let ops =
    Array.map
      (fun use_union ->
        if use_union then
          let u, v = edge_pool.(Random.State.int state nodes) in
          Union (u, v)
        else
          Find (Random.State.int state nodes))
      is_union
  in
  let summary =
    {
      partition_count = 1;
      min_partition_size = nodes;
      max_partition_size = nodes;
      operations;
      union_ops;
      find_ops;
    }
  in
  (ops, summary)

let run_ops ~nodes ~domains ops =
  let domains = Union_find_common.normalize_domains domains in
  let uf = Cas_union_find.create nodes in
  Cas_union_find.reset_stats uf;
  Gc.full_major ();
  let total_ops = Array.length ops in
  let worker worker_id =
    let index = ref worker_id in
    while !index < total_ops do
      (match ops.(!index) with
       | Union (u, v) -> ignore (Cas_union_find.union uf u v)
       | Find x -> ignore (Cas_union_find.find uf x));
      index := !index + domains
    done
  in
  let start_time = Unix.gettimeofday () in
  if domains = 1 then worker 0
  else (
    let spawned =
      Array.init (domains - 1) (fun worker_id ->
          Domain.spawn (fun () -> worker worker_id))
    in
    worker (domains - 1);
    Array.iter Domain.join spawned);
  let seconds = Unix.gettimeofday () -. start_time in
  let stats = Cas_union_find.stats uf in
  let components = Cas_union_find.component_count uf in
  { seconds; components; stats }

let compare_float a b = Stdlib.compare a b

let summarize_samples samples =
  let by_seconds left right = compare_float left.seconds right.seconds in
  let ordered = List.sort by_seconds samples in
  let rec nth list index =
    match (list, index) with
    | x :: _, 0 -> x
    | _ :: rest, _ -> nth rest (index - 1)
    | [], _ -> invalid_arg "nth"
  in
  let count = List.length ordered in
  let median_sample = nth ordered (count / 2) in
  {
    median_seconds = median_sample.seconds;
    min_seconds = (List.hd ordered).seconds;
    max_seconds = (List.hd (List.rev ordered)).seconds;
    components = median_sample.components;
    stats = median_sample.stats;
  }

let measure ~config ~domains ops =
  for _ = 1 to config.warmup do
    ignore (run_ops ~nodes:config.nodes ~domains ops)
  done;
  let rec loop remaining acc =
    if remaining = 0 then summarize_samples (List.rev acc)
    else
      let sample = run_ops ~nodes:config.nodes ~domains ops in
      loop (remaining - 1) (sample :: acc)
  in
  loop config.repeats []

let cas_failure_rate stats =
  if stats.Union_find.cas_attempts = 0 then nan
  else float stats.cas_failures /. float stats.cas_attempts

let format_rate rate =
  if Float.is_nan rate then "n/a" else sprintf "%.6f" rate

let open_csv_out () =
  let root =
    match Sys.getenv_opt "BENCH_ROOT" with
    | Some value when value <> "" -> value
    | _ -> "."
  in
  let path = Filename.concat root "bench_graph_structure_results.csv" in
  let oc = open_out path in
  eprintf "writing CSV results to: %s\n%!" path;
  oc

let tee_line oc line =
  print_string line;
  print_char '\n';
  flush stdout;
  output_string oc line;
  output_char oc '\n';
  flush oc

let print_csv_header oc =
  tee_line oc
    "implementation,domains,mode,graph_profile,union_percent,partitions,nodes,operations,union_ops,find_ops,min_partition_size,max_partition_size,median_seconds,min_seconds,max_seconds,speedup,operations_per_second,components,find_calls,union_calls,cas_attempts,cas_failures,cas_failure_rate,path_compactions,rank_updates"

let print_csv_row oc ~profile ~domains ~(config : config)
    ~(summary : workload_summary) ~baseline measurement =
  let speedup = baseline /. measurement.median_seconds in
  let operations_per_second =
    float summary.operations /. measurement.median_seconds
  in
  let stats = measurement.stats in
  tee_line oc
    (sprintf
       "%s,%d,%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%.6f,%.6f,%.6f,%.3f,%.0f,%d,%d,%d,%d,%d,%s,%d,%d"
       "cas_union_find" domains "shared" (string_of_graph_profile profile)
       config.union_percent summary.partition_count config.nodes summary.operations
       summary.union_ops summary.find_ops summary.min_partition_size
       summary.max_partition_size measurement.median_seconds
       measurement.min_seconds measurement.max_seconds speedup operations_per_second
       measurement.components stats.find_calls stats.union_calls stats.cas_attempts
       stats.cas_failures (format_rate (cas_failure_rate stats))
       stats.path_compactions stats.rank_updates)

let run config =
  if config.nodes < 2 then invalid_arg "--nodes must be at least 2";
  if config.operations < 1 then invalid_arg "--operations must be at least 1";
  if config.union_percent < 0 || config.union_percent > 100 then
    invalid_arg "--union-percent must be between 0 and 100";
  if config.repeats < 1 then invalid_arg "--repeats must be at least 1";
  if config.warmup < 0 then invalid_arg "--warmup must be non-negative";
  let domains = List.sort_uniq Int.compare config.domains in
  eprintf
    "graph-structure CAS workload: profiles=sparse_random,dense_random,star nodes=%d edges=%d operations=%d union_percent=%d domains=%s repeats=%d warmup=%d\n%!"
    config.nodes config.nodes config.operations config.union_percent
    (String.concat "," (List.map string_of_int domains))
    config.repeats config.warmup;
  let csv_oc = open_csv_out () in
  Fun.protect
    ~finally:(fun () -> close_out csv_oc)
    (fun () ->
      print_csv_header csv_oc;
      List.iter
        (fun profile ->
          eprintf "\n--- starting graph_profile=%s ---\n%!"
            (string_of_graph_profile profile);
          let ops, summary =
            make_ops ~profile ~nodes:config.nodes ~operations:config.operations
              ~union_percent:config.union_percent
          in
          let baseline_measurement = measure ~config ~domains:1 ops in
          if List.mem 1 domains then
            print_csv_row csv_oc ~profile ~domains:1 ~config ~summary
              ~baseline:baseline_measurement.median_seconds baseline_measurement;
          let baseline = baseline_measurement.median_seconds in
          List.iter
            (fun domain_count ->
              if domain_count <> 1 then
                let measurement = measure ~config ~domains:domain_count ops in
                print_csv_row csv_oc ~profile ~domains:domain_count ~config
                  ~summary ~baseline measurement)
            domains)
        graph_profiles)

let () =
  let config = ref default_config in
  let set_nodes nodes = config := { !config with nodes } in
  let set_operations operations = config := { !config with operations } in
  let set_union_percent union_percent =
    config := { !config with union_percent }
  in
  let set_repeats repeats = config := { !config with repeats } in
  let set_warmup warmup = config := { !config with warmup } in
  let set_domains value =
    config := { !config with domains = parse_domains value }
  in
  let specs =
    [
      ("--nodes", Arg.Int set_nodes, "number of graph nodes, default 200000");
      ( "--operations",
        Arg.Int set_operations,
        "timed operations, default 1000000" );
      ( "--queries",
        Arg.Int set_operations,
        "alias for --operations" );
      ( "--union-percent",
        Arg.Int set_union_percent,
        "percent of operations that are unions, default 20" );
      ( "--repeats",
        Arg.Int set_repeats,
        "measured runs per graph/domain, median is reported" );
      ( "--warmup",
        Arg.Int set_warmup,
        "untimed warmup runs per graph/domain, default 1" );
      ( "--domains",
        Arg.String set_domains,
        "comma-separated domain counts, default 1,2,4,8,16" );
    ]
  in
  Arg.parse specs
    (fun arg -> raise (Arg.Bad ("unexpected positional argument: " ^ arg)))
    "Benchmark CAS union-find contention across sparse, dense, and star graph structures.";
  run !config
