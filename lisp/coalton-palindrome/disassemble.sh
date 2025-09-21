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
    (flet ((safe-disassemble (sym pkg path)
             (multiple-value-bind (s status) (find-symbol sym pkg)
               (when (and s (fboundp s))
                 (with-open-file (out path :direction :output :if-exists :supersede)
                   (let ((*standard-output* out)) (disassemble s)))))))
      (format t \"Disassembling Coalton functions...~%\")
      (safe-disassemble \"SMALLEST\" \"COALTON-PALINDROME\" \"$SCRIPT_DIR/asm/coalton-smallest.asm\")
      (safe-disassemble \"LARGEST\" \"COALTON-PALINDROME\" \"$SCRIPT_DIR/asm/coalton-largest.asm\")
      (safe-disassemble \"IS-PAL\" \"COALTON-PALINDROME\" \"$SCRIPT_DIR/asm/coalton-ispal.asm\")
      (safe-disassemble \"SEARCH-SMALLEST-INLINED\" \"COALTON-PALINDROME\" \"$SCRIPT_DIR/asm/coalton-search-smallest-inlined.asm\")
      (safe-disassemble \"SEARCH-LARGEST-INLINED\" \"COALTON-PALINDROME\" \"$SCRIPT_DIR/asm/coalton-search-largest-inlined.asm\")
      (format t \"Disassembling CL functions...~%\")
      (safe-disassemble \"SMALLEST-INNER\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-smallest-inner.asm\")
      (safe-disassemble \"LARGEST-INNER\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-largest-inner.asm\")
      (safe-disassemble \"PALINDROMEP\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-palindromep.asm\")
      (format t \"Disassembly complete!~%\")))" \
  --quit
