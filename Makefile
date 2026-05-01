.PHONY: all build clean test bench correctness stm lin cas-stress connectivity-bench

all: build

build:
	dune build

clean:
	dune clean

# Run all tests
test: build
	@echo "Running correctness tests..."
	dune exec ./Union_find_correctness_test.exe
	@echo ""
	@echo "Running QCheck-STM tests..."
	dune exec ./QCheck_stm.exe
	@echo ""
	@echo "Running QCheck-Lin tests..."
	dune exec ./QCheck_lin.exe
	@echo ""
	@echo "Running CAS stress tests..."
	dune exec ./cas_stress_test.exe

bench: build
	@echo "Running benchmarks..."
	dune exec ./union_find_bench.exe
	@echo ""
	dune exec ./union_find_graph_bench.exe
	@echo ""
	dune exec ./union_find_connectivity_bench.exe

connectivity-bench: build
	dune exec ./union_find_connectivity_bench.exe

# Individual targets
correctness: build
	dune exec ./Union_find_correctness_test.exe

stm: build
	dune exec ./QCheck_stm.exe

lin: build
	dune exec ./QCheck_lin.exe

cas-stress: build
	dune exec ./cas_stress_test.exe
