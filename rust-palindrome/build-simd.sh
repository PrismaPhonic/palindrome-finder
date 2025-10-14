#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

mkdir -p ../target-bin

echo "[Rust Simd] Building release binaries"
RUSTFLAGS="${RUSTFLAGS:-} -C target-cpu=native" cargo build --release

cp target/release/palprod-rust-simd-smallest ../target-bin/
cp target/release/palprod-rust-simd-largest  ../target-bin/

echo "[Rust Simd] Running PGO+BOLT pipeline (cargo-pgo required)"
./pgo_bolt_run_simd.sh

echo "[DONE] Rust simd binaries in ../target-bin:"
ls -1 ../target-bin | grep '^palprod-rust-simd' || true
