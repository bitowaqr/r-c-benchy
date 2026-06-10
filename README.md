# R vs C++ Benchmark

This directory contains a small, reproducible benchmark comparing R and C++ across five tasks:

- Matrix multiplication
- Element-wise numeric transform
- Numeric sorting
- Group-by summation
- Text field parsing

Run everything with:

```sh
./run_benchmarks.sh
```

The runner compiles the C++ benchmark, executes the R and C++ programs, writes raw timings to `results/timings.csv`, and generates a shareable `report.html`.

To collect more repetitions:

```sh
BENCH_ITERATIONS=12 ./run_benchmarks.sh
```
