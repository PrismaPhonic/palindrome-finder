#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

mkdir -p ../target-bin

echo "[Rust] Building release binaries"
RUSTFLAGS="${RUSTFLAGS:-} -C target-cpu=native" cargo build --release

cp target/release/palprod-rust-smallest ../target-bin/
cp target/release/palprod-rust-largest  ../target-bin/

echo "[Rust] Running PGO+BOLT pipeline (cargo-pgo required)"
./pgo_bolt_run.sh

echo "[DONE] Rust binaries in ../target-bin:"
ls -1 ../target-bin | grep '^palprod-rust' || true


