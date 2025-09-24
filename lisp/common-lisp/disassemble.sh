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
  --eval "(asdf:load-asd \"$REPO_DIR/lisp/common-lisp/pp-fast.asd\")" \
  --eval "(asdf:load-system \"pp-fast\")" \
  --eval "(let ((*default-pathname-defaults* (pathname \"$REPO_DIR/lisp/common-lisp/\"))) (load \"runner-fast-smallest-inner.lisp\") (load \"runner-fast-largest-inner.lisp\"))" \
  --eval "(progn
    (flet ((safe-disassemble (sym pkg path)
             (multiple-value-bind (s status) (find-symbol sym pkg)
               (when (and s (fboundp s))
                 (with-open-file (out path :direction :output :if-exists :supersede)
                   (let ((*standard-output* out)) (disassemble s)))))))
      (format t \"Disassembling CL functions...~%\")
      ;; Core library functions
      (safe-disassemble \"SMALLEST-INNER\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-smallest-inner.asm\")
      (safe-disassemble \"LARGEST-INNER\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-largest-inner.asm\")
      (safe-disassemble \"PALINDROMEP\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-palindromep.asm\")
      (safe-disassemble \"SUM-UB32-VECTOR\" \"PP-FAST\" \"$SCRIPT_DIR/asm/cl-sum-ub32-vector.asm\")
      (safe-disassemble \"%DO-ITERS\" \"PP-RUNNER-FAST-SMALLEST-INNER\" \"$SCRIPT_DIR/asm/cl-smallest-runner-doiters.asm\")
      (safe-disassemble \"%DO-ITERS\" \"PP-RUNNER-FAST-LARGEST-INNER\"  \"$SCRIPT_DIR/asm/cl-largest-runner-doiters.asm\")
      (format t \"Disassembly complete!~%\")))" \
  --quit
