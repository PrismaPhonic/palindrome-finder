#!/usr/bin/env bash
set -euo pipefail

# Resolve paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

SBCL_BIN="${SBCL_BIN:-sbcl}"

if ! command -v "$SBCL_BIN" >/dev/null 2>&1; then
  echo "Error: SBCL not found (looked for '$SBCL_BIN'). Install SBCL or set SBCL_BIN."
  exit 1
fi

"$SBCL_BIN" --noinform --disable-debugger \
  --eval "(load \"~/.quicklisp/setup.lisp\")" \
  --eval "(declaim (optimize (speed 3) (safety 0) (debug 0)))" \
  --eval "(setf (symbol-plist ':coalton-config) nil)" \
  --eval "(dolist (kv '((:compiler-mode \"release\") (:perform-specialization t) (:perform-inlining t) (:emit-type-annotations t))) (setf (get ':coalton-config (first kv)) (second kv)))" \
  --eval "(asdf:load-asd \"$SCRIPT_DIR/coalton-palindrome.asd\")" \
  --eval "(asdf:load-system \"coalton-palindrome\")" \
  --eval "(load \"$SCRIPT_DIR/coalton-palindrome-test.lisp\")" \
  --eval "(in-package :coalton-palindrome-test)" \
  --eval "(run-tests)" \
  --quit


