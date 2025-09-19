#!/usr/bin/env bash
set -euo pipefail

echo "Running Common Lisp palindrome tests..."

# Resolve paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SBCL_BIN="${SBCL_BIN:-sbcl}"

if ! command -v "$SBCL_BIN" >/dev/null 2>&1; then
  echo "Error: SBCL not found (looked for '$SBCL_BIN'). Install SBCL or set SBCL_BIN."
  exit 1
fi

# Check if Quicklisp is available
if [ ! -f ~/.quicklisp/setup.lisp ]; then
  echo "Error: Quicklisp not found. Please install Quicklisp first."
  echo "Visit: https://www.quicklisp.org/beta/"
  exit 1
fi

cd "$SCRIPT_DIR"

"$SBCL_BIN" --noinform --disable-debugger \
  --eval "(load \"~/.quicklisp/setup.lisp\")" \
  --eval "(asdf:load-asd \"$SCRIPT_DIR/pp-fast.asd\")" \
  --eval "(asdf:load-system \"pp-fast\")" \
  --eval "(load \"$SCRIPT_DIR/palindrome-products-test.lisp\")" \
  --eval "(in-package :palindrome-products-test)" \
  --eval "(run-tests)" \
  --quit

echo ""
echo "Common Lisp tests completed!"
