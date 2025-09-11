(defpackage :pp-gc
  (:use :cl)
  (:export :prepare-gc-for-bench))
(in-package :pp-gc)

(defun %maybe-set-bytes-between-gcs (bytes)
  "Best-effort: set SBCL's GC trigger distance if the current runtime exposes it.
   Silently no-ops on SBCLs that don't have this knob."
  (declare (type (integer 0) bytes))
  (handler-case
      (progn
        ;; Try legacy special: SB-EXT:*BYTES-CONSED-BETWEEN-GCS*
        (let ((sym (find-symbol "*BYTES-CONSED-BETWEEN-GCS*" "SB-EXT")))
          (when (and sym (boundp sym))
            (setf (symbol-value sym) bytes)))
        ;; Try function forms some builds ship (guarded)
        (let ((setter (find-symbol "SET-BYTES-CONSED-BETWEEN-GCS" "SB-EXT")))
          (when (and setter (fboundp setter))
            (funcall setter bytes)))
        (let ((fn (find-symbol "BYTES-CONSED-BETWEEN-GCS" "SB-EXT")))
          ;; Some builds expose a callable 'bytes-consed-between-gcs'
          (when (and fn (fboundp fn))
            (ignore-errors (funcall fn :set bytes))))
        t)
    (error () nil)))

(defun prepare-gc-for-bench (&key (bytes-between-gcs (* 256 1024 1024)))
  "One-time GC tuning prior to the hot loop.
   - Force a full GC to start from a clean heap.
   - Increase the distance between GCs if supported by this SBCL."
  (declare (type (integer 0) bytes-between-gcs))
  (ignore-errors (funcall (find-symbol "GC" "SB-EXT") :full t))
  (%maybe-set-bytes-between-gcs bytes-between-gcs))
