#!/usr/bin/env bash
set -euo pipefail

# Combined PGO -> BOLT pipeline using cargo-pgo
# - Generates PGO profiles and builds PGO-optimized binaries
# - Then uses BOLT with --with-pgo to instrument and optimize the PGO build
# Docs: https://github.com/Kobzol/cargo-pgo

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

ITERS=${ITERS:-15000000}
WARMUP=${WARMUP:-250000}

mkdir -p ../target-bin

find_artifact() {
  local name="$1"
  local p
  p=$(ls -1 target/*/release/"$name" 2>/dev/null | head -n1 || true)
  if [ -z "${p:-}" ]; then
    p=$(find target -type f -name "$name" | head -n1 || true)
  fi
  echo "$p"
}

pgo_one() {
  local bin="$1"
  local out_prefix="$2"
  echo "[PGO] Instrument/build for $bin"
  cargo pgo instrument build -- --bin "$bin"
  # Save instrumented binary snapshot (so baseline names never get overwritten)
  INSTR_PATH="$(find_artifact "$bin")"
  if [ -n "${INSTR_PATH:-}" ]; then
    cp "$INSTR_PATH" "../target-bin/${out_prefix}-pgo-instrumented"
  fi
  echo "[PGO] Run workload for $bin"
  cargo pgo run -- --bin "$bin" -- --server <<EOF
INIT 2 999
WARMUP $WARMUP
RUN $ITERS
QUIT
EOF
}

bolt_one_with_pgo() {
  local bin="$1"
  local inst_name="${bin}-bolt-instrumented"
  local opt_name="${bin}-bolt-optimized"
  local out_prefix="$2"

  echo "[BOLT] Build instrumented (with PGO) for $bin"
  cargo pgo bolt build --with-pgo -- --bin "$bin"

  # Ensure per-binary BOLT profile dir exists
  mkdir -p "target/bolt-profiles/${bin}"

  echo "[BOLT] Run instrumented workload for $bin"
  INST_PATH="$(find_artifact "$inst_name")"
  if [ -z "${INST_PATH:-}" ]; then echo "ERR: $inst_name not found"; exit 1; fi
  "$INST_PATH" --server <<EOF
INIT 2 999
WARMUP $WARMUP
RUN $ITERS
QUIT
EOF

  echo "[BOLT] Optimize (with PGO) for $bin"
  cargo pgo bolt optimize --with-pgo -- --bin "$bin"
  OPT_PATH="$(find_artifact "$opt_name")"
  if [ -z "${OPT_PATH:-}" ]; then echo "ERR: $opt_name not found"; exit 1; fi
  cp "$OPT_PATH" "../target-bin/${out_prefix}-bolt-optimized"
}

# --- Smallest ---
pgo_one palprod-rust-simd-smallest palprod-rust-simd-smallest
bolt_one_with_pgo palprod-rust-simd-smallest palprod-rust-simd-smallest

# Clear PGO profiles to avoid cross-contamination
rm -rf target/pgo-profiles || true

# --- Largest ---
pgo_one palprod-rust-simd-largest  palprod-rust-simd-largest
bolt_one_with_pgo palprod-rust-simd-largest  palprod-rust-simd-largest

echo "[DONE] Outputs in ../target-bin:"
echo " - palprod-rust-simd-smallest-pgo-instrumented"
echo " - palprod-rust-simd-smallest-bolt-optimized"
echo " - palprod-rust-simd-largest-pgo-instrumented"
echo " - palprod-rust-simd-largest-bolt-optimized"
