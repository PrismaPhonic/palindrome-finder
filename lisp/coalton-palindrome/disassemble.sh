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

# Create output directory
mkdir -p "$SCRIPT_DIR/asm"

"$SBCL_BIN" --noinform --disable-debugger \
  --eval "(load \"~/.quicklisp/setup.lisp\")" \
  --eval "(declaim (optimize (speed 3) (safety 0) (debug 0)))" \
  --eval "(setf (symbol-plist ':coalton-config) nil)" \
  --eval "(dolist (kv '((:compiler-mode \"release\") (:perform-specialization t) (:perform-inlining t) (:emit-type-annotations t))) (setf (get ':coalton-config (first kv)) (second kv)))" \
  --eval "(asdf:load-asd \"$SCRIPT_DIR/coalton-palindrome.asd\")" \
  --eval "(asdf:load-system \"coalton-palindrome\")" \
  --eval "(asdf:load-asd \"$REPO_DIR/lisp/common-lisp/pp-fast.asd\")" \
  --eval "(asdf:load-system \"pp-fast\")" \
  --eval "(progn
    (format t \"Disassembling Coalton functions...~%\")
    (with-open-file (s \"$SCRIPT_DIR/asm/coalton-smallest.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"SMALLEST\" \"COALTON-PALINDROME\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/coalton-largest.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"LARGEST\" \"COALTON-PALINDROME\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/coalton-ispal.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"IS-PAL\" \"COALTON-PALINDROME\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/coalton-search-smallest-inlined.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"SEARCH-SMALLEST-INLINED\" \"COALTON-PALINDROME\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/coalton-search-largest-inlined.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"SEARCH-LARGEST-INLINED\" \"COALTON-PALINDROME\"))))
    (format t \"Disassembling CL functions...~%\")
    (with-open-file (s \"$SCRIPT_DIR/asm/cl-smallest-inner.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"SMALLEST-INNER\" \"PP-FAST\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/cl-largest-inner.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"LARGEST-INNER\" \"PP-FAST\"))))
    (with-open-file (s \"$SCRIPT_DIR/asm/cl-palindromep.asm\" :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (disassemble (find-symbol \"PALINDROMEP\" \"PP-FAST\"))))
    (format t \"Disassembly complete!~%\"))" \
  --quit
