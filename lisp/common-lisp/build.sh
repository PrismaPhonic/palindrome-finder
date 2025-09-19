#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

mkdir -p ../../target-bin

echo "[SBCL] Building smallest (fast)"
sbcl --dynamic-space-size 4096 --noinform --disable-debugger --non-interactive \
     --load build-fast-smallest-inner.lisp

echo "[SBCL] Building largest (fast)"
sbcl --dynamic-space-size 4096 --noinform --disable-debugger --non-interactive \
     --load build-fast-largest-inner.lisp

echo "[DONE] SBCL binaries in ../../target-bin:"
echo " - palprod-fast-smallest-inner"
echo " - palprod-fast-largest-inner"


