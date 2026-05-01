# CS6868 Research Mini-Project - Concurrent Union-Find in OCaml 5

**Team:** Yaswanth Srinivas, Prince, Mukhesh  

### Recording

Watch the full research walkthrough here:  
[![Watch the demo](https://img.youtube.com/vi/3dFy-hIGe_Y/0.jpg)](https://www.youtube.com/watch?v=3dFy-hIGe_Y)

> 💡 **Tip:** Watch at 1.5x speed for a faster overview!

### Overview

With the introduction of shared-memory parallelism in OCaml 5, optimizing fundamental data structures for multicore execution has become a critical challenge. This project implements, verifies, and benchmarks Concurrent Disjoint Set Union (DSU / Union-Find) designs. Because traditional sequential DSU relies on "self-modifying" optimizations like path compression, translating it to a concurrent environment risks race conditions and cyclic dependencies.

We evaluate four distinct variants: a sequential baseline, a coarse-grained global mutex lock, a fine-grained per-node lock, and a high-performance lock-free CAS (Compare-and-Set) implementation. 

*Note on Nomenclature: In our written report, we refer to our CAS traversal technique as 'path halving'. However, upon strict algorithmic review of our final OCaml code, we realized we actually implemented **'path splitting'**. Because our loop advances to the immediate parent rather than the grandparent after the CAS, it updates every node on the path rather than skipping every other node. Both techniques are single-pass and avoid the cache-miss penalties of full path compression, so our performance conclusions remain completely valid.*

### Repository Layout

```text
.
├── src/                  # Core algorithms and graph generation
│   ├── union_find.mli    # Shared DSU signature
│   ├── union_find.ml     # Shared types and stats definition
│   ├── sequential_union_find.ml # Sequential baseline
│   ├── mutex_union_find.ml      # Coarse-grained global mutex
│   ├── node_lock_union_find.ml  # Fine-grained per-node locks
│   ├── cas_union_find.ml        # Lock-free CAS with path splitting
│   ├── union_find_common.ml     # Shared utility functions
│   ├── union_find_stats.ml      # CAS failure/retry metrics tracker
│   ├── graph_connectivity.ml    # Parallel edge processing
│   └── graph_generators.ml      # Erdős–Rényi, Star, and Disconnected topologies
├── test/                 # Test suites
│   ├── union_find_correctness_test.ml # Unit and partitioning tests
│   ├── cas_stress_test.ml    # High-contention concurrency stress tests
│   ├── Test_support.ml       # Test assertion utilities
│   ├── Qcheck_lin.ml         # Linearizability tests (QCheck-Lin)
│   └── Qcheck_stm.ml         # Model-based parallel agreement tests (QCheck-STM)
├── bench/                # Benchmarking infrastructure
│   ├── union_find_bench.ml   # Throughput vs. Domain scaling benchmarks
│   ├── union_find_graph_bench.ml # Graph structure sensitivity benchmarks
│   ├── union_find_connectivity_bench.ml # Real-world connectivity timing
│   └── union_find_graph_structure_bench.ml 
├── scripts/              
│   └── plot_time.py      # Python script for plotting results
├── results/              # Output data and figures
│   ├── _bench.csv        # Raw benchmark outputs
│   └── _plot.png         # Generated throughput and scaling plots
├── report/               # Academic report and LaTeX source
│   ├── main.tex
│   ├── report.pdf
│   └── references.bib
├── Makefile
├── dune-project
├── dune
├── presentation.pptx
└── README.md
```

### Build and Run

**Prerequisites:**
* OCaml >= 5.0
* dune >= 3.0
* Python 3 (for plotting) with `pandas` and `matplotlib`
* opam packages: `qcheck-core`, `qcheck-stm`, `qcheck-lin`

**Install dependencies:**
```bash
opam install qcheck-core qcheck-stm qcheck-lin
```

**Build and test:**
```bash
make build
make test
```
*(This runs the correctness tests, QCheck-STM, QCheck-Lin, and CAS stress tests).*

**Run benchmarks:**
```bash
make bench
```

**Generate plots:**
```bash
python scripts/plot_time.py --trend results/trend.csv --graph results/graph.csv --connectivity results/connectivity.csv --outdir results/
```
*(Adjust csv filenames based on actual benchmark outputs).*

**Or use dune directly:**
```bash
dune build
dune test
dune exec bench/union_find_bench.exe
dune exec bench/union_find_graph_bench.exe
```

### Union-Find Interface

All Union-Find implementations conform to a standard signature defined in `src/union_find.mli` (approximate representation):
```ocaml
module type DSU = sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> bool
  val same_set : t -> int -> int -> bool
end
```

### Testing

* **Unit & Partitioning tests:** Validates that parallel edge insertions generate the exact same component sets as sequential runs (`test/union_find_correctness_test.ml`, `test/Test_support.ml`).
* **Stress Tests:** Forces heavy contention on the CAS implementation to ensure zero component loss (`test/cas_stress_test.ml`).
* **QCheck-STM:** Model-based sequential and parallel agreement tests to ensure thread-safe state transitions (`test/Qcheck_stm.ml`).
* **QCheck-Lin:** Linearizability tests for concurrent histories (`test/Qcheck_lin.ml`).

Run all tests:
```bash
make test
```

### Benchmarks

The `bench/` executables measure execution time and speedup for:
* **Thread counts:** 1 to 16 domains (scaling beyond the 8 physical cores to observe oversubscription and scheduler degradation).
* **Workloads:** Balanced (50% unions / 50% finds), Read-heavy (20% unions / 80% finds).
* **Graph Profiles:** Uniform random (Erdős–Rényi), Star graphs, and Disconnected Components (generated via `src/graph_generators.ml`).
* **Scale:** Up to 200,000 nodes and 1,000,000 interleaved operations.

Results are saved as `.csv` files and plotted using `scripts/plot_time.py`.

### Demo: Graph Connectivity

`bench/union_find_connectivity_bench.ml` and `bench/union_find_graph_bench.ml` demonstrate evaluating the concurrent Union-Find algorithms over generated graph topologies using parallel domains. It splits edge processing among spawned workers to calculate full graph components efficiently.

### Research Question

**Does a lock-free Compare-And-Set (CAS) approach provide a scalable throughput advantage over coarse and fine-grained locking strategies for Union-Find in OCaml 5?**

Yes. Our Lock-Free CAS approach achieved up to a **1.76x speedup** over the sequential baseline on balanced workloads (50% union mix), peaking at 4 OCaml domains. 

By tracking internal metrics (`src/union_find_stats.ml`), we observed that CAS failure rates remained below 0.1%. This proves that the algorithm successfully avoids retry storms. The scaling plateau beyond physical core limits is driven entirely by hardware atomic memory traffic constraints and OCaml domain scheduling overhead, rather than algorithmic contention. In contrast, global mutex completely eliminated parallelism, and node-level locking suffered from immense constant overhead that nullified its concurrent advantages.
```
