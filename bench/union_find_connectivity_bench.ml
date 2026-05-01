(* Benchmark: concurrent connected-component labelling via union-only workloads.
   Each trial builds a fresh union-find structure, then spawns [domains] OCaml
   domains that concurrently call [union] on every edge of a graph.  The result
   is the number of connected components – the "correct" figure is verified
   against a sequential reference run at startup.

   Graph shapes
   ────────────
   • sparse_random  – Erdős–Rényi G(n, m) with avg_degree ≈ 4; many small
                      components expected; light contention.
   • dense_random   – same model but avg_degree ≈ 40; graph converges to a
                      single giant component; heavy CAS contention.
   • star           – one centre node connected to every leaf; all unions race
                      to set the same root; maximum contention on one node.
   • grid           – 2-D grid (√n × √n); structured local connectivity,
                      moderate contention, representative of image-processing
                      workloads.
   • chain          – n-1 edges forming a single path; sequential by nature,
                      near-zero contention even with many domains.

   CLI flags
   ─────────
   --nodes N             total node count (default 200 000)
   --repeats N           timed runs per (graph × domain) cell (default 3)
   --warmup N            untimed warm-up runs (default 1)
   --domains a,b,...     comma-separated domain counts (default 1,2,4,8,16)
   --avg-degree N        avg degree for random graphs (default 4)
   --no-verify           skip sequential correctness check
*)

open Printf

(* ── graph profiles ────────────────────────────────────────────────────── *)

type graph_profile =
  | Sparse_random
  | Dense_random
  | Star
  | Grid
  | Chain

let all_profiles = [ Sparse_random; Dense_random; Star; Grid; Chain ]

let string_of_profile = function
  | Sparse_random -> "sparse_random"
  | Dense_random  -> "dense_random"
  | Star          -> "star"
  | Grid          -> "grid"
  | Chain         -> "chain"

(* ── configuration ──────────────────────────────────────────────────────── *)

type config =
  { nodes      : int
  ; repeats    : int
  ; warmup     : int
  ; domains    : int list
  ; avg_degree : int   (* used only for random profiles *)
  ; verify     : bool
  }

let default_config =
  { nodes      = 200_000
  ; repeats    = 3
  ; warmup     = 1
  ; domains    = [ 1; 2; 4; 8; 16 ]
  ; avg_degree = 4
  ; verify     = true
  }

(* ── edge-list generation ───────────────────────────────────────────────── *)

(** Deterministic RNG seed so every run on the same config is comparable. *)
let make_state profile nodes avg_degree =
  Random.State.make
    [| 2026; 5; 1; nodes; avg_degree;
       (match profile with
        | Sparse_random -> 1 | Dense_random -> 2
        | Star -> 3 | Grid -> 4 | Chain -> 5) |]

let gen_sparse_random ~nodes ~avg_degree =
  let state = make_state Sparse_random nodes avg_degree in
  let edge_count = (nodes * avg_degree) / 2 in
  Array.init edge_count (fun _ ->
      Graph_generators.random_non_self_edge ~nodes state)

let gen_dense_random ~nodes ~avg_degree =
  (* Dense: use a higher degree – multiply avg_degree by 10, floored at 40. *)
  let dense_degree = max 40 (avg_degree * 10) in
  let state = make_state Dense_random nodes dense_degree in
  let edge_count = (nodes * dense_degree) / 2 in
  Array.init edge_count (fun _ ->
      Graph_generators.random_non_self_edge ~nodes state)

let gen_star ~nodes =
  (* nodes-1 edges: centre=0 connected to every leaf 1..n-1 *)
  Graph_generators.star ~nodes

let gen_grid ~nodes =
  (* Build a floor(√nodes) × ceil(nodes/floor(√nodes)) grid.
     Horizontal + vertical neighbours only; no diagonals. *)
  let cols = int_of_float (sqrt (float nodes)) in
  let cols = max 1 cols in
  let rows = (nodes + cols - 1) / cols in
  let node r c = r * cols + c in
  let acc = Array.make (rows * cols * 2) (0, 0) in
  let count = ref 0 in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let u = node r c in
      if u < nodes then begin
        (* right neighbour *)
        if c + 1 < cols then begin
          let v = node r (c + 1) in
          if v < nodes then begin
            acc.(!count) <- (u, v);
            incr count
          end
        end;
        (* down neighbour *)
        if r + 1 < rows then begin
          let v = node (r + 1) c in
          if v < nodes then begin
            acc.(!count) <- (u, v);
            incr count
          end
        end
      end
    done
  done;
  Array.sub acc 0 !count

let gen_chain ~nodes =
  (* n-1 edges: 0-1, 1-2, …, (n-2)-(n-1) *)
  if nodes <= 1 then [||]
  else Array.init (nodes - 1) (fun i -> (i, i + 1))

let generate_edges profile ~nodes ~avg_degree =
  match profile with
  | Sparse_random -> gen_sparse_random ~nodes ~avg_degree
  | Dense_random  -> gen_dense_random  ~nodes ~avg_degree
  | Star          -> gen_star          ~nodes
  | Grid          -> gen_grid          ~nodes
  | Chain         -> gen_chain         ~nodes

(* ── sequential reference (correctness baseline) ────────────────────────── *)

let sequential_components ~nodes (edges : (int * int) array) =
  let uf = Sequential_union_find.create nodes in
  Array.iter (fun (u, v) -> ignore (Sequential_union_find.union uf u v)) edges;
  Sequential_union_find.component_count uf

(* ── parallel trial ─────────────────────────────────────────────────────── *)

type sample =
  { seconds    : float
  ; components : int
  ; stats      : Union_find.stats
  }

type measurement =
  { median_seconds : float
  ; min_seconds    : float
  ; max_seconds    : float
  ; components     : int
  ; stats          : Union_find.stats
  }

(** Run [domains] domains concurrently, each processing a contiguous slice of
    [edges] – every domain calls [union] on its slice, so all edges are covered
    exactly once.  This is the "partitioned" strategy: domain k owns edges in
    [k*chunk … (k+1)*chunk).  Contention arises naturally whenever edges in
    different partitions share endpoints. *)
let run_trial ~nodes ~domains (edges : (int * int) array) =
  let domains = Union_find_common.normalize_domains domains in
  let uf = Cas_union_find.create nodes in
  Cas_union_find.reset_stats uf;
  Gc.full_major ();
  let edge_count = Array.length edges in
  let worker domain_index =
    let start = (domain_index * edge_count) / domains in
    let stop  = ((domain_index + 1) * edge_count) / domains in
    for i = start to stop - 1 do
      let u, v = edges.(i) in
      ignore (Cas_union_find.union uf u v)
    done
  in
  let t0 = Unix.gettimeofday () in
  if domains = 1 then
    worker 0
  else begin
    (* Spawn domains-1 workers; main domain takes the last slice. *)
    let spawned =
      Array.init (domains - 1) (fun id -> Domain.spawn (fun () -> worker id))
    in
    worker (domains - 1);
    Array.iter Domain.join spawned
  end;
  let seconds    = Unix.gettimeofday () -. t0 in
  let stats      = Cas_union_find.stats uf in
  let components = Cas_union_find.component_count uf in
  { seconds; components; stats }

let summarize samples =
  let ordered = List.sort (fun a b -> Float.compare a.seconds b.seconds) samples in
  let n       = List.length ordered in
  let median  = List.nth ordered (n / 2) in
  { median_seconds = median.seconds
  ; min_seconds    = (List.hd ordered).seconds
  ; max_seconds    = (List.nth ordered (n - 1)).seconds
  ; components     = median.components
  ; stats          = median.stats
  }

let measure ~config ~domains edges =
  for _ = 1 to config.warmup do
    ignore (run_trial ~nodes:config.nodes ~domains edges)
  done;
  let rec loop rem acc =
    if rem = 0 then summarize (List.rev acc)
    else loop (rem - 1) (run_trial ~nodes:config.nodes ~domains edges :: acc)
  in
  loop config.repeats []

(* ── CSV output ─────────────────────────────────────────────────────────── *)

let open_csv_out () =
  let root =
    match Sys.getenv_opt "BENCH_ROOT" with
    | Some v when v <> "" -> v
    | _ -> "."
  in
  let path = Filename.concat root "bench_connectivity_results.csv" in
  let oc   = open_out path in
  eprintf "writing CSV results to: %s\n%!" path;
  oc

let tee_line oc line =
  print_string line; print_char '\n'; flush stdout;
  output_string oc line; output_char oc '\n'; flush oc

let csv_header =
  "implementation,domains,graph_profile,nodes,edges,avg_degree,\
   median_seconds,min_seconds,max_seconds,speedup,edges_per_second,\
   components,expected_components,correct,\
   find_calls,union_calls,cas_attempts,cas_failures,cas_failure_rate,\
   path_compactions,rank_updates"

let cas_failure_rate (s : Union_find.stats) =
  if s.cas_attempts = 0 then nan
  else float s.cas_failures /. float s.cas_attempts

let format_rate r = if Float.is_nan r then "n/a" else sprintf "%.6f" r

let print_row oc ~profile ~domains ~nodes ~edge_count ~avg_degree
      ~expected ~baseline (m : measurement) =
  let speedup           = baseline /. m.median_seconds in
  let edges_per_second  = float edge_count /. m.median_seconds in
  let correct           = if m.components = expected then "true" else "false" in
  let s                 = m.stats in
  tee_line oc
    (sprintf
       "cas_union_find,%d,%s,%d,%d,%d,\
        %.6f,%.6f,%.6f,%.3f,%.0f,\
        %d,%d,%s,\
        %d,%d,%d,%d,%s,%d,%d"
       domains (string_of_profile profile) nodes edge_count avg_degree
       m.median_seconds m.min_seconds m.max_seconds speedup edges_per_second
       m.components expected correct
       s.find_calls s.union_calls s.cas_attempts s.cas_failures
       (format_rate (cas_failure_rate s))
       s.path_compactions s.rank_updates)

(* ── driver ─────────────────────────────────────────────────────────────── *)

let parse_domains str =
  str |> String.split_on_char ','
  |> List.filter_map (fun p ->
         let p = String.trim p in
         if p = "" then None else Some (int_of_string p))
  |> List.map Union_find_common.normalize_domains

let run (config : config) =
  if config.nodes < 2 then invalid_arg "--nodes must be at least 2";
  if config.repeats < 1 then invalid_arg "--repeats must be at least 1";
  if config.warmup  < 0 then invalid_arg "--warmup must be non-negative";
  if config.avg_degree < 1 then invalid_arg "--avg-degree must be at least 1";
  let domains = List.sort_uniq Int.compare config.domains in
  eprintf
    "connectivity bench: nodes=%d avg_degree=%d repeats=%d warmup=%d domains=%s\n%!"
    config.nodes config.avg_degree config.repeats config.warmup
    (String.concat "," (List.map string_of_int domains));
  let csv_oc = open_csv_out () in
  Fun.protect ~finally:(fun () -> close_out csv_oc) (fun () ->
    tee_line csv_oc csv_header;
    List.iter (fun profile ->
        eprintf "\n--- graph_profile=%s ---\n%!" (string_of_profile profile);
        let edges      = generate_edges profile ~nodes:config.nodes ~avg_degree:config.avg_degree in
        let edge_count = Array.length edges in
        eprintf "  edges generated: %d\n%!" edge_count;
        (* Sequential reference for correctness and baseline timing. *)
        let expected =
          if config.verify then begin
            let v = sequential_components ~nodes:config.nodes edges in
            eprintf "  sequential components: %d\n%!" v;
            v
          end else -1
        in
        (* Baseline: single-domain run used as the speedup denominator. *)
        let baseline_m = measure ~config ~domains:1 edges in
        eprintf "  baseline (1 domain) %.4fs  components=%d\n%!"
          baseline_m.median_seconds baseline_m.components;
        if List.mem 1 domains then
          print_row csv_oc ~profile ~domains:1 ~nodes:config.nodes
            ~edge_count ~avg_degree:config.avg_degree
            ~expected ~baseline:baseline_m.median_seconds baseline_m;
        List.iter (fun d ->
            if d <> 1 then begin
              let m = measure ~config ~domains:d edges in
              eprintf "  %d domains %.4fs  components=%d  speedup=%.2fx\n%!"
                d m.median_seconds m.components
                (baseline_m.median_seconds /. m.median_seconds);
              print_row csv_oc ~profile ~domains:d ~nodes:config.nodes
                ~edge_count ~avg_degree:config.avg_degree
                ~expected ~baseline:baseline_m.median_seconds m
            end)
          domains)
      all_profiles)

let () =
  let cfg = ref default_config in
  let specs =
    [ "--nodes",      Arg.Int  (fun v -> cfg := { !cfg with nodes      = v }),
                      " total node count (default 200000)"
    ; "--repeats",    Arg.Int  (fun v -> cfg := { !cfg with repeats    = v }),
                      " measured runs per cell (default 3)"
    ; "--warmup",     Arg.Int  (fun v -> cfg := { !cfg with warmup     = v }),
                      " untimed warm-up runs (default 1)"
    ; "--avg-degree", Arg.Int  (fun v -> cfg := { !cfg with avg_degree = v }),
                      " average degree for random graphs (default 4)"
    ; "--domains",    Arg.String (fun v -> cfg := { !cfg with domains  = parse_domains v }),
                      " comma-separated domain counts (default 1,2,4,8,16)"
    ; "--no-verify",  Arg.Unit (fun () -> cfg := { !cfg with verify    = false }),
                      " skip sequential correctness verification"
    ]
  in
  Arg.parse specs
    (fun a -> raise (Arg.Bad ("unexpected argument: " ^ a)))
    "Benchmark concurrent connected-component labelling via parallel union-only workloads.";
  run !cfg
