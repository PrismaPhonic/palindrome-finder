#!/usr/bin/env bash
set -euo pipefail

# Go PGO pipeline for palprod-go-{smallest,largest}
# - Builds baseline binaries
# - Runs --server workload to collect CPU profiles (pprof)
# - Builds PGO-optimized binaries using the collected profiles
#
# Docs: https://go.dev/doc/pgo

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

ITERS=${ITERS:-15000000}
WARMUP=${WARMUP:-250000}

mkdir -p ../target-bin target/pgo-profiles || true

build_one() {
  local cmd="$1"   # palprod-go-smallest | palprod-go-largest
  echo "[Go] Building baseline $cmd"
  go build -o "$cmd" "./cmd/$cmd"
}

profile_one() {
  local cmd="$1"
  local prof_path="target/pgo-profiles/${cmd}.pprof"
  echo "[Go] Collecting CPU profile for $cmd -> $prof_path"
  # Start server with CPU profile path. Feed the same workload as Rust PGO.
  "./$cmd" --server --cpu-profile "$prof_path" <<EOF
INIT 2 999
WARMUP $WARMUP
RUN $ITERS
QUIT
EOF
}

build_with_pgo_one() {
  local cmd="$1"
  local prof_path="target/pgo-profiles/${cmd}.pprof"
  local out_path="../target-bin/${cmd}-pgo"
  echo "[Go] Building $cmd with PGO -> $out_path"
  GOFLAGS="-pgo=$prof_path" go build -o "$out_path" "./cmd/$cmd"
}

# Smallest
build_one palprod-go-smallest
profile_one palprod-go-smallest
build_with_pgo_one palprod-go-smallest

# Largest
build_one palprod-go-largest
profile_one palprod-go-largest
build_with_pgo_one palprod-go-largest

echo "[DONE] Go PGO outputs in ../target-bin/:"
echo " - palprod-go-smallest-pgo"
echo " - palprod-go-largest-pgo"


