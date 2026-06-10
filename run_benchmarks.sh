#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

mkdir -p build results

CXX_BIN="${CXX:-g++}"
CXXFLAGS_VALUE="${CXXFLAGS:--O3 -std=c++17 -DNDEBUG}"
LDFLAGS_VALUE="${LDFLAGS:-}"

if [[ "$(uname -s)" == "Darwin" ]]; then
  LDFLAGS_VALUE="${LDFLAGS_VALUE} -framework Accelerate"
fi

echo "Compiling C++ benchmark..."
# shellcheck disable=SC2086
"$CXX_BIN" $CXXFLAGS_VALUE src/bench_cpp.cpp -o build/bench_cpp $LDFLAGS_VALUE

echo "Running R benchmark..."
Rscript scripts/bench_r.R > results/r_timings.csv

echo "Running C++ benchmark..."
./build/bench_cpp > results/cpp_timings.csv

head -n 1 results/r_timings.csv > results/timings.csv
tail -n +2 results/r_timings.csv >> results/timings.csv
tail -n +2 results/cpp_timings.csv >> results/timings.csv

echo "Rendering HTML report..."
Rscript scripts/render_report.R results/timings.csv report.html
cp report.html index.html

echo "Done."
echo "Report: $ROOT_DIR/report.html"
echo "GitHub Pages entry: $ROOT_DIR/index.html"
echo "Raw timings: $ROOT_DIR/results/timings.csv"
