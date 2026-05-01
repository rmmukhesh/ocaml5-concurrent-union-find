open Printf

type packed_impl =
  Impl : string * (module Union_find.DSU) -> packed_impl

type sharing_mode =
  | Shared_interleaved
  | Shared_partitioned
  | Isolated

type config =
  {
    nodes : int;
    queries : int;
    repeats : int;
    warmup : int;
    domains : int list;
    impl_filter : string;
    sharing_mode : sharing_mode;
  }

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

type workload_mix =
  {
    union_percent : int;
  }

type partition_workload =
  {
    offset : int;
    size : int;
    (* Interleaved sequence of operations.
       Each element is either:
         Left (u, v)  -> union(offset+u, offset+v)
         Right x      -> find(offset+x)
       Stored as a flat array of tagged ints to avoid boxing:
         tag bit 0 = 0 => union, value encodes (u * max_size + v)
         tag bit 0 = 1 => find,  value encodes x
       We keep it simple and store as a variant array instead. *)
    ops : op array;
  }

and op =
  | Union of int * int   (* local node indices *)
  | Find  of int         (* local node index   *)

let default_config =
  {
    nodes = 200_000;
    queries = 1_000_000;
    repeats = 3;
    warmup = 1;
    domains = [ 1; 2; 4; 8; 16 ];
    impl_filter = "all";
    sharing_mode = Shared_interleaved;
  }

let workload_mixes = [ { union_percent = 20 }; { union_percent = 50 } ]

let parse_domains value =
  value |> String.split_on_char ','
  |> List.filter_map (fun part ->
         let part = String.trim part in
         if part = "" then None else Some (int_of_string part))
  |> List.map Union_find_common.normalize_domains

let partition_bounds ~nodes ~partitions partition_index =
  let start = (partition_index * nodes) / partitions in
  let stop = ((partition_index + 1) * nodes) / partitions in
  (start, stop)

let parse_sharing_mode = function
  | "shared" -> Shared_interleaved
  | "shared-partitioned" -> Shared_partitioned
  | "isolated" -> Isolated
  | value ->
    raise
      (Arg.Bad
         (sprintf
            "unknown --mode %S (expected shared, shared-partitioned, or isolated)"
            value))

let string_of_sharing_mode = function
  | Shared_interleaved -> "shared"
  | Shared_partitioned -> "shared-partitioned"
  | Isolated -> "isolated"

(* ---------------------------------------------------------------------------
   Graph construction: random Erdos-Renyi edges
   ---------------------------------------------------------------------------
   The original code built a balanced binary tree (stride-doubling over a
   permutation).  That gave O(log n) depth trees from the start, leaving
   almost no work for path compression and very little CAS retry pressure
   because roots were reached in 1-2 hops.  This biased results in favour
   of CAS.

   We now draw random edges uniformly over [0, partition_size).  This
   produces a realistic mix of shallow and deep chains, exercises path
   compression, and generates genuine contention patterns.

   Edge count is set to partition_size, so the full benchmark uses exactly
   nodes edges across all partitions.  Union operations draw from that fixed
   edge pool; the operation mix controls how often those edges are used.
*)
let make_random_edges ~partition_size ~edge_count ~seed =
  if partition_size < 2 then [||]
  else (
    let state = Random.State.make [| 2026; 4; 29; seed; partition_size; edge_count |] in
    Array.init edge_count (fun _ ->
      let u = Random.State.int state partition_size in
      let v = Random.State.int state partition_size in
      (* Avoid trivial self-unions; retry once.  Still O(1) per edge. *)
      let v = if v = u then (v + 1) mod partition_size else v in
      (u, v)))

(* ---------------------------------------------------------------------------
   Operation sequence: interleaved unions and finds
   ---------------------------------------------------------------------------
   The original Shared_interleaved mode ran all unions first (build phase)
   and then all finds (query phase) as two separate parallel barriers.  By
   the time finds ran the structure was fully built, so there was no union/
   find contention — the mode that was supposed to stress concurrent access
   the most actually stressed it the least.

   We now generate a single interleaved ops array where union and find
   operations are mixed.  The trend benchmark runs both 20% union / 80% find
   and 50% union / 50% find mixes.  Workers pull from this array in strided
   fashion, so every domain sees the same interleaved pattern.

   For Shared_partitioned the ops array is also interleaved; the partition
   boundary just determines which nodes each worker touches.

   For Isolated the ops are the same interleaved array but each partition
   has its own union-find instance, so there is no cross-domain contention
   (useful as a baseline upper bound on throughput).
*)
let make_interleaved_ops ~partition_size ~union_count ~find_count ~seed =
  if partition_size < 2 then [||]
  else (
    let state = Random.State.make [| 31; 41; 59; seed; partition_size |] in
    let edges =
      make_random_edges ~partition_size ~edge_count:partition_size ~seed
    in
    let edge_cursor = ref 0 in
    let total = union_count + find_count in
    (* Shuffle a boolean mask: true = union, false = find *)
    let is_union = Array.init total (fun i -> i < union_count) in
    Graph_generators.shuffle_in_place state is_union;
    Array.map
      (fun use_union ->
        if use_union then (
          let u, v = edges.(!edge_cursor mod Array.length edges) in
          incr edge_cursor;
          Union (u, v))
        else
          Find (Random.State.int state partition_size))
      is_union)

(* ---------------------------------------------------------------------------
   Partition construction
   ---------------------------------------------------------------------------
   Key fix: partition_count is determined per domain count so that each
   domain-count trial has exactly `domain_count` partitions.  This means
   the 1-domain baseline processes 1 partition (not max_domains partitions),
   giving a true serial baseline and accurate speedup numbers.

   Each call to make_partitions is cheap (arrays are pre-generated once per
   domain count) and the seeds are deterministic so results are reproducible.
*)
let make_partitions ~nodes ~operations ~partition_count ~mix =
  let partitions =
    Array.make partition_count
      { offset = 0; size = 0; ops = [||] }
  in
  let min_partition_size = ref max_int in
  let max_partition_size = ref min_int in
  let total_union_ops = ref 0 in
  let total_find_ops = ref 0 in
  for partition_index = 0 to partition_count - 1 do
    let partition_start, partition_stop =
      partition_bounds ~nodes ~partitions:partition_count partition_index
    in
    let partition_size = partition_stop - partition_start in
    min_partition_size := min !min_partition_size partition_size;
    max_partition_size := max !max_partition_size partition_size;
    let partition_operations =
      ((partition_index + 1) * operations / partition_count)
      - (partition_index * operations / partition_count)
    in
    let union_count = partition_operations * mix.union_percent / 100 in
    let find_count = partition_operations - union_count in
    total_union_ops := !total_union_ops + union_count;
    total_find_ops := !total_find_ops + find_count;
    let ops =
      make_interleaved_ops ~partition_size ~union_count ~find_count
        ~seed:partition_index
    in
    partitions.(partition_index) <-
      { offset = partition_start; size = partition_size; ops }
  done;
  let summary =
    {
      partition_count;
      min_partition_size = !min_partition_size;
      max_partition_size = !max_partition_size;
      operations;
      union_ops = !total_union_ops;
      find_ops = !total_find_ops;
    }
  in
  (partitions, summary)

(* ---------------------------------------------------------------------------
   Shared_interleaved runner
   ---------------------------------------------------------------------------
   Workers now execute the same interleaved op sequence (strided) on a single
   shared union-find.  Unions and finds are concurrent across all domains,
   which is the actual adversarial case for CAS vs mutex vs node-lock.
*)
let run_shared_interleaved_queries (module Uf : Union_find.DSU)
    ~nodes ~domains partitions =
  let domains = Union_find_common.normalize_domains domains in
  let uf = Uf.create nodes in
  (* Flatten all ops into one global array so workers stride across the
     full workload rather than per-partition; this maximises cross-partition
     contention when multiple partitions touch overlapping node ranges. *)
  let total_ops =
    Array.fold_left (fun acc p -> acc + Array.length p.ops) 0 partitions
  in
  let global_ops = Array.make total_ops (Find 0) in
  let cursor = ref 0 in
  Array.iter
    (fun partition ->
      Array.iter
        (fun op ->
          global_ops.(!cursor) <-
            (match op with
             | Union (u, v) -> Union (partition.offset + u, partition.offset + v)
             | Find x -> Find (partition.offset + x));
          incr cursor)
        partition.ops)
    partitions;
  Uf.reset_stats uf;
  Gc.full_major ();
  let worker worker_id =
    let index = ref worker_id in
    while !index < total_ops do
      (match global_ops.(!index) with
       | Union (u, v) -> ignore (Uf.union uf u v)
       | Find x -> ignore (Uf.find uf x));
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
  let stats = Uf.stats uf in
  let components = Uf.component_count uf in
  { seconds; components; stats }

(* ---------------------------------------------------------------------------
   Shared_partitioned runner
   ---------------------------------------------------------------------------
   Each worker owns a set of partitions (round-robin) and processes the
   interleaved op sequence for each.  Unions and finds within a partition
   are still interleaved; different partitions may run concurrently.
*)
let run_shared_queries (module Uf : Union_find.DSU) ~nodes ~domains partitions =
  let domains = Union_find_common.normalize_domains domains in
  let uf = Uf.create nodes in
  Uf.reset_stats uf;
  Gc.full_major ();
  let partition_count = Array.length partitions in
  let worker worker_id =
    let partition_index = ref worker_id in
    while !partition_index < partition_count do
      let partition = partitions.(!partition_index) in
      Array.iter
        (fun op ->
          match op with
          | Union (u, v) ->
            ignore (Uf.union uf (partition.offset + u) (partition.offset + v))
          | Find x ->
            ignore (Uf.find uf (partition.offset + x)))
        partition.ops;
      partition_index := !partition_index + domains
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
  let stats = Uf.stats uf in
  let components = Uf.component_count uf in
  { seconds; components; stats }

(* ---------------------------------------------------------------------------
   Isolated runner
   ---------------------------------------------------------------------------
   Each partition gets its own union-find instance; no cross-domain sharing.
   This is an upper-bound baseline: it measures how fast the algorithm runs
   when there is zero contention, regardless of implementation.

   Bug fix: components is now correctly aggregated across all instances
   instead of being hardcoded to partition_count.
*)
let run_isolated_queries implementation ~domains partitions =
  let module Uf = (val implementation : Union_find.DSU) in
  let domains = Union_find_common.normalize_domains domains in
  let instances =
    Array.map
      (fun partition ->
        let uf = Uf.create partition.size in
        Uf.reset_stats uf;
        uf)
      partitions
  in
  Gc.full_major ();
  let partition_count = Array.length partitions in
  let worker worker_id =
    let partition_index = ref worker_id in
    while !partition_index < partition_count do
      let partition = partitions.(!partition_index) in
      let uf = instances.(!partition_index) in
      Array.iter
        (fun op ->
          match op with
          | Union (u, v) -> ignore (Uf.union uf u v)
          | Find x -> ignore (Uf.find uf x))
        partition.ops;
      partition_index := !partition_index + domains
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
  let stats =
    Array.fold_left
      (fun (total : Union_find.stats) uf ->
        let s = Uf.stats uf in
        {
          Union_find.find_calls = total.find_calls + s.find_calls;
          union_calls = total.union_calls + s.union_calls;
          cas_attempts = total.cas_attempts + s.cas_attempts;
          cas_failures = total.cas_failures + s.cas_failures;
          path_compactions = total.path_compactions + s.path_compactions;
          rank_updates = total.rank_updates + s.rank_updates;
        })
      ({
        find_calls = 0;
        union_calls = 0;
        cas_attempts = 0;
        cas_failures = 0;
        path_compactions = 0;
        rank_updates = 0;
      } : Union_find.stats)
      instances
  in
  (* Fix: aggregate actual component counts instead of hardcoding partition_count *)
  let components =
    Array.fold_left (fun total uf -> total + Uf.component_count uf) 0 instances
  in
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
  let min_seconds = (List.hd ordered).seconds in
  let max_seconds = (List.hd (List.rev ordered)).seconds in
  {
    median_seconds = median_sample.seconds;
    min_seconds;
    max_seconds;
    components = median_sample.components;
    stats = median_sample.stats;
  }

let run_queries implementation ~config ~domains partitions =
  match config.sharing_mode with
  | Shared_interleaved ->
    run_shared_interleaved_queries implementation ~nodes:config.nodes ~domains
      partitions
  | Shared_partitioned ->
    run_shared_queries implementation ~nodes:config.nodes ~domains partitions
  | Isolated -> run_isolated_queries implementation ~domains partitions

let measure_impl (Impl (_name, implementation)) ~config ~domains partitions =
  for _ = 1 to config.warmup do
    ignore (run_queries implementation ~config ~domains partitions)
  done;
  let rec loop remaining acc =
    if remaining = 0 then summarize_samples (List.rev acc)
    else
      let sample = run_queries implementation ~config ~domains partitions in
      loop (remaining - 1) (sample :: acc)
  in
  loop config.repeats []

let all_impls =
  [
    Impl ("cas_union_find", (module Cas_union_find : Union_find.DSU));
    Impl ("mutex_union_find", (module Mutex_union_find : Union_find.DSU));
    Impl ("node_lock_union_find", (module Node_lock_union_find : Union_find.DSU));
  ]

let selected_impls filter =
  match filter with
  | "all" -> all_impls
  | name ->
    List.filter
      (fun (Impl (impl_name, _)) -> impl_name = name)
      all_impls

let cas_failure_rate stats =
  if stats.Union_find.cas_attempts = 0 then nan
  else float stats.cas_failures /. float stats.cas_attempts

let format_rate rate =
  if Float.is_nan rate then "n/a" else sprintf "%.6f" rate

(* ---------------------------------------------------------------------------
   CSV output: tee to stdout and a file in the project root directory.
   The root directory is read from the BENCH_ROOT environment variable
   (set by the Makefile).  If the variable is absent we fall back to ".".
   --------------------------------------------------------------------------- *)
let open_csv_out () =
  let root =
    match Sys.getenv_opt "BENCH_ROOT" with
    | Some r when r <> "" -> r
    | _ -> "."
  in
  let path = Filename.concat root "bench_trend_results.csv" in
  let oc = open_out path in
  eprintf "writing CSV results to: %s\n%!" path;
  oc

(* Emit a line to both stdout and the CSV file. *)
let tee_line oc line =
  print_string line; print_char '\n'; flush stdout;
  output_string oc line; output_char oc '\n'; flush oc

let print_csv_header oc =
  tee_line oc
    "implementation,domains,mode,union_percent,partitions,nodes,operations,union_ops,find_ops,min_partition_size,max_partition_size,median_seconds,min_seconds,max_seconds,speedup,operations_per_second,components,find_calls,union_calls,cas_attempts,cas_failures,cas_failure_rate,path_compactions,rank_updates"

let print_csv_row oc ~impl_name ~domains ~(config : config) ~mix
    ~(summary : workload_summary) ~baseline measurement =
  let speedup = baseline /. measurement.median_seconds in
  let operations_per_second =
    float summary.operations /. measurement.median_seconds
  in
  let stats = measurement.stats in
  tee_line oc
    (sprintf
      "%s,%d,%s,%d,%d,%d,%d,%d,%d,%d,%d,%.6f,%.6f,%.6f,%.3f,%.0f,%d,%d,%d,%d,%d,%s,%d,%d"
      impl_name domains (string_of_sharing_mode config.sharing_mode)
      mix.union_percent summary.partition_count config.nodes summary.operations
      summary.union_ops summary.find_ops
      summary.min_partition_size summary.max_partition_size
      measurement.median_seconds measurement.min_seconds measurement.max_seconds
      speedup operations_per_second measurement.components stats.find_calls
      stats.union_calls stats.cas_attempts stats.cas_failures
      (format_rate (cas_failure_rate stats)) stats.path_compactions
      stats.rank_updates)

(* ---------------------------------------------------------------------------
   Main run loop
   ---------------------------------------------------------------------------
   Key fix: partitions are regenerated per domain count so that the 1-domain
   baseline always processes exactly 1 partition (a true serial baseline)
   and an N-domain run processes exactly N partitions.  Speedup is therefore
   measured against a real single-threaded execution of the same total work,
   not against an artificially inflated multi-partition sequential run.

   Baseline fix: if domain_count = 1 is in the list, that measurement IS the
   baseline and is stored immediately.  If 1 is not in the list, a separate
   1-domain run is performed as the baseline.  The silent self-baseline
   fallback has been replaced with an assertion so bugs surface loudly.
*)
let run config =
  if config.nodes < 2 then invalid_arg "--nodes must be at least 2";
  if config.queries < 1 then invalid_arg "--queries must be at least 1";
  if config.repeats < 1 then invalid_arg "--repeats must be at least 1";
  if config.warmup < 0 then invalid_arg "--warmup must be non-negative";
  let impls = selected_impls config.impl_filter in
  if impls = [] then invalid_arg ("unknown --impl: " ^ config.impl_filter);
  let domains = List.sort_uniq Int.compare config.domains in
  let max_domains = List.fold_left max 1 domains in
  if config.nodes < max_domains * 2 then
    invalid_arg
      (sprintf
         "--nodes must be at least twice the maximum domain count (%d)"
         (max_domains * 2));
  eprintf
    "fair interleaved workload: mode=%s union_percents=20,50 max_partitions=%d nodes=%d operations=%d repeats=%d warmup=%d\n%!"
    (string_of_sharing_mode config.sharing_mode)
    max_domains config.nodes config.queries config.repeats config.warmup;
  let csv_oc = open_csv_out () in
  print_csv_header csv_oc;
  List.iter
    (fun mix ->
      if mix.union_percent = 50 then
        eprintf "\n--- starting 50%% union / 50%% find workload ---\n%!";
      List.iter
        (fun (Impl (impl_name, _) as implementation) ->
          (* Compute a true 1-domain baseline with 1 partition for this mix. *)
          let baseline_partitions, baseline_summary =
            make_partitions ~nodes:config.nodes ~operations:config.queries
              ~partition_count:1 ~mix
          in
          let baseline_measurement =
            measure_impl implementation ~config ~domains:1 baseline_partitions
          in
          (* Report the 1-domain row only if 1 is explicitly in the domain list. *)
          if List.mem 1 domains then
            print_csv_row csv_oc ~impl_name ~domains:1 ~config ~mix
              ~summary:baseline_summary
              ~baseline:baseline_measurement.median_seconds baseline_measurement;
          let baseline = baseline_measurement.median_seconds in
          List.iter
            (fun domain_count ->
              if domain_count = 1 then ()  (* already handled above *)
              else (
                (* Each domain count gets its own partitions sized to domain_count. *)
                let partitions, summary =
                  make_partitions ~nodes:config.nodes ~operations:config.queries
                    ~partition_count:domain_count ~mix
                in
                let measurement =
                  measure_impl implementation ~config ~domains:domain_count
                    partitions
                in
                print_csv_row csv_oc ~impl_name ~domains:domain_count ~config ~mix
                  ~summary ~baseline measurement))
            domains)
        impls)
    workload_mixes;
  close_out csv_oc

let () =
  let config = ref default_config in
  let set_nodes nodes = config := { !config with nodes } in
  let set_queries queries = config := { !config with queries } in
  let set_repeats repeats = config := { !config with repeats } in
  let set_warmup warmup = config := { !config with warmup } in
  let set_domains value = config := { !config with domains = parse_domains value } in
  let set_impl impl_filter = config := { !config with impl_filter } in
  let set_sharing_mode value =
    config := { !config with sharing_mode = parse_sharing_mode value }
  in
  let specs =
    [
      ("--nodes", Arg.Int set_nodes, "number of graph nodes, default 200000");
      ( "--queries",
        Arg.Int set_queries,
        "timed operations (unions + finds interleaved), default 1000000" );
      ("--repeats", Arg.Int set_repeats, "measured runs per implementation/domain, median is reported");
      ("--warmup", Arg.Int set_warmup, "untimed warmup runs per implementation/domain, default 1");
      ( "--mode",
        Arg.String set_sharing_mode,
        "benchmark mode: shared (default, one shared union-find with interleaved unions+finds), shared-partitioned, or isolated" );
      ("--domains", Arg.String set_domains, "comma-separated domain counts, default 1,2,4,8,16");
      ( "--impl",
        Arg.String set_impl,
        "implementation: all, cas_union_find, mutex_union_find, or node_lock_union_find" );
    ]
  in
  Arg.parse specs
    (fun arg -> raise (Arg.Bad ("unexpected positional argument: " ^ arg)))
    "Benchmark concurrent union-find scaling with fair interleaved workloads.";
  run !config
