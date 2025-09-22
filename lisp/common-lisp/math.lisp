(in-package :pp-fast)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; Canonical 32-bit unsigned type alias
(deftype word32 () '(unsigned-byte 32))

;; Ceil division for 32-bit unsigned inputs: ceil(numer/denom)
(declaim (inline u32-ceil-div)
         (ftype (function (word32 word32) word32) u32-ceil-div))

(defun u32-ceil-div (numer denom)
  (declare (type word32 numer denom))
  (the word32 (truncate (+ numer (the word32 (- denom 1))) denom)))

;; Integer square root for 32-bit unsigned inputs.
(declaim (inline u32-isqrt)
         (ftype (function (word32) word32) u32-isqrt))     
(defun u32-isqrt (n)
  (declare (type word32 n))
  (the word32 (isqrt n)))
