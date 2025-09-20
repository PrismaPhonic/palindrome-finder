;;;; Palindromic Products
;;;; ------------------------------------------------------------------
;;;; Design and performance notes
;;;; - Problem: Given MIN..MAX, find the smallest/largest palindromic product
;;;;   of two integers in that range, and return the product plus factor pairs.
;;;; - Approach:
;;;;   1) Two tight prunes keep the nested loops small:
;;;;        - largest:
;;;;            outer prune:  x*MAX <= best   -> stop remaining rows
;;;;            inner prune:  x*y   <= best   -> stop current row
;;;;        - smallest:
;;;;            outer prune:  x*x   >= best   -> stop remaining rows
;;;;            inner prune:  x*y   >= best   -> stop current row
;;;;   2) Numeric, half-reverse palindrome test (no strings, no consing).
;;;;   3) Inner search returns the product plus a flat simple-vector [x0 y0 x1 y1 ...]
;;;;      for benchmarks. Public wrappers convert that to ((x y) ...) only when needed.
;;;; - Types and codegen (SBCL-friendly):
;;;;   - fixnum declarations and (the fixnum ...) in hot code so SBCL emits
;;;;     inline fixnum arithmetic (no generic ops).
;;;;   - factor vectors use '(signed-byte 64) (unboxed) for better locality.
;;;; - No SIMD or stack allocation of returned data:
;;;;   - the core is div/mod plus branching; SIMD will not help
;;;;   - returned vectors escape, so they must live on the heap; dynamic-extent is
;;;;     only for non-escaping temporaries.
;;;; Highlights
;;;; - half-reversal palindrome check (divides by 10 only; minimal work)
;;;; - unboxed factor vectors '(signed-byte 64) for tighter memory
;;;; - guards that avoid expensive palindrome calls:
;;;;     - skip positive products ending in 0 (cannot be palindromes)
;;;;     - for even-digit products, if not divisible by 11 then skip
;;;; - strong inner/outer prunes; SBCL-friendly types

(defpackage :pp-fast
  (:use :cl)
  (:export
   :smallest-inner :largest-inner
   :smallest :largest :palindromep))
(in-package :pp-fast)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; ftypes so SBCL propagates types
(declaim
 (ftype (function (fixnum) boolean) palindromep)
 (ftype (function (fixnum) boolean) even-digit-count-p)
 (ftype (function ((simple-array (signed-byte 64) (*)) fixnum)
                  (simple-array (signed-byte 64) (*)))
        finalize-factor-buffer)
 (ftype (function (fixnum fixnum fixnum)
                  (simple-array (signed-byte 64) (*)))
        collect-positive-factor-pairs)
 (ftype (function (fixnum fixnum)
                  (values (or null fixnum)
                          (or null (simple-array (signed-byte 64) (*)))))
        smallest-inner largest-inner)
 (ftype (function (fixnum fixnum) (values (or null fixnum) list))
        smallest largest)
 (ftype (function ((or null (simple-array (signed-byte 64) (*)))) list)
        pairs-vector->list))

(declaim (inline palindromep even-digit-count-p
                 finalize-factor-buffer
                 collect-positive-factor-pairs
                 pairs-vector->list
                 smallest-inner largest-inner))

;; ------------------------------------------------------------------

(declaim (inline even-digit-count-p))
(defun even-digit-count-p (n)
  "Return T if N has an even number of decimal digits.
   Assumes N >= 11 (callers already filtered < 10 and trailing-zero)."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (cond
    ((< n (the fixnum 100))                      t)   ; 2 digits
    ((< n (the fixnum 1000))                     nil) ; 3
    ((< n (the fixnum 10000))                    t)   ; 4
    ((< n (the fixnum 100000))                   nil) ; 5
    ((< n (the fixnum 1000000))                  t)   ; 6
    ((< n (the fixnum 10000000))                 nil) ; 7
    ((< n (the fixnum 100000000))                t)   ; 8
    ((< n (the fixnum 1000000000))               nil) ; 9
    ((< n (the fixnum 10000000000))              t)   ; 10
    ((< n (the fixnum 100000000000))             nil) ; 11
    ((< n (the fixnum 1000000000000))            t)   ; 12
    ((< n (the fixnum 10000000000000))           nil) ; 13
    ((< n (the fixnum 100000000000000))          t)   ; 14
    ((< n (the fixnum 1000000000000000))         nil) ; 15
    ((< n (the fixnum 10000000000000000))        t)   ; 16
    ((< n (the fixnum 100000000000000000))       nil) ; 17
    ((< n (the fixnum 1000000000000000000))      t)   ; 18
    (t nil))) ; 19 digits (fixnum doesn't go past 19)

(defun palindromep (n)
  "Half-reverse: build REV from rightmost digits until REV >= M, then compare.
   Early rejects:
     - single-digit numbers are palindromes
     - positive numbers ending in 0 are not palindromes
     - even-length palindromes must be divisible by 11 (cheap gate)"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))

  ;; single-digit numbers are palindromes
  (when (< n 10)
    (return-from palindromep t))

  ;; trailing zero (e.g., 10, 40, 100) -> not a palindrome (aside from 0)
  (when (zerop (rem n 10))
    (return-from palindromep nil))

  ;; even-length palindromes must be divisible by 11
  ;; safe to call even-digit-count-p here because n >= 11
  (when (and (even-digit-count-p n)
             (not (zerop (mod n 11))))
    (return-from palindromep nil))

  ;; build only half the reverse
  (let ((m n) (rev 0))
    (declare (type fixnum m rev))
    (loop while (> m rev) do
      (let ((digit (the (integer 0 9) (rem m 10))))
        (declare (type (integer 0 9) digit))
        (setf rev (the fixnum (+ (the fixnum (* rev 10)) digit))
              m   (the fixnum (truncate m 10)))))

    ;; even length: m == rev; odd length: m == rev/10
    (or (eql m rev)
        (eql m (the fixnum (truncate rev 10))))))

;; ------------------------------------------------------------------
;; Allocation-tight builders for the flat factor-pair vector (unboxed)

(defun finalize-factor-buffer (buffer count)
  "Copy the first count entries from buffer (sb32) to a right-sized sb32 result.

   This is used so we can create factor pairs with stack allocated arrays, and
   avoid   pointer chasing. We finalize into a correctly sized output array with
   this helper"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (signed-byte 64) (*)) buffer)
           (type fixnum count)
           (dynamic-extent buffer))
  (let ((out (make-array count :element-type '(signed-byte 64))))
    (declare (type (simple-array (signed-byte 64) (*)) out))
    (loop for i of-type fixnum from 0 below count do
      (setf (aref out i) (aref buffer i)))
    out))

(defun collect-positive-factor-pairs (product min max)
  "Return flat vector [x0 y0 x1 y1 ...] for product > 0; empty if none."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum product min max))
  (let* ((sqrtp (the fixnum (isqrt product)))
         (low   (max min (truncate (+ product max -1) max)))
         (high  (min max sqrtp))
         ;; small fixed buffer; I've never see more than 3 pairs other than the
         ;; case of min == zero, which we already special case, so this should
         ;; be more than enough.
         (buff (make-array 6 :element-type '(signed-byte 64)))
         (count 0))
    (declare (type fixnum sqrtp low high count)
             (type (simple-array (signed-byte 64) (*)) buff)
             (dynamic-extent buff))
    (loop for x of-type fixnum from low to high do
      (when (zerop (mod product x))
        (let ((y (truncate product x)))
          (setf (aref buff count) x
                (aref buff (1+ count)) y)
          (incf count 2))))
    (finalize-factor-buffer buff count)))

(defun pairs-vector->list (vec)
  "Convert flat vector [x0 y0 x1 y1 ...] to ((x y) ...)."
  (declare (type (or null (simple-array (signed-byte 64) (*))) vec))
  (if (or (null vec) (zerop (length vec)))
      nil
      (let ((out '()))
        (declare (type list out))
        (loop for i of-type fixnum from 0 below (length vec) by 2 do
          (push (list (the fixnum (aref vec i))
                      (the fixnum (aref vec (1+ i))))
                out))
        (nreverse out))))

;; ------------------------------------------------------------------
;; Inner search functions (return product and flat vector; no lists)

;; ------------------------------
;; Smallest Search
;; ------------------------------

(defun smallest-inner (min max)
  "Return two values:
   1) the smallest palindromic product of two factors in [min, max], or NIL
   2) a flat vector [x0 y0 x1 y1 ...] of factor pairs (unboxed 64-bit ints)

Algorithm (ascending x, ascending y with a tight y upper bound):
- We maintain best, the smallest palindrome found so far (starts at +inf).
- For each row with fixed x (min -> max):
  - outer prune: if x*x >= best, then for any y >= x we have x*y >= x*x >= best,
    so no later rows can improve best -> stop the whole search
  - compute an inner upper bound for y: y_max = min(max, floor((best-1)/x)).
    we only care about products < best. for fixed x, those are exactly
    the y with y <= floor((best-1)/x). if that bound is < x, this row has no work.
  - iterate y from x to y_max. products grow in this row, but because we already
    capped y at y_max we do not need a per-iteration product check.
  - on the first palindrome hit, update best and stop the row (smaller y cannot
    produce a smaller product in this row).

This reduces calls to palindromep by shrinking each row as best improves."
  (declare (type fixnum min max))
  (let ((best most-positive-fixnum))
     (declare (type fixnum best))
     (block search
       ;; x ascends
       (loop for x of-type fixnum from min to max do
         ;; outer prune: after this, x*x only increases
         (when (>= (the fixnum (* x x)) best)
           (return-from search))
         ;; compute tight y upper bound from current best
         (let* ((y-upper
                  (min max (truncate (1- best) x)))) ; floor((best-1)/x)
           (when (< y-upper x)
             (return))                  ; no possible y in this row
           (block row
             ;; y ascends only until y-upper
             (loop for y of-type fixnum from x to y-upper
                   for product of-type fixnum
                     = #+sbcl (sb-ext:truly-the fixnum (* x y))
                   #-sbcl (* x y) do
                     (when (palindromep product)
                       (setf best product)
                       (return-from row)))))))
     ;; final answer
     (if (< best most-positive-fixnum)
         (values best (collect-positive-factor-pairs best min max))
         (values nil nil))))

;; ------------------------------
;; Largest Search
;; ------------------------------

(defun largest-inner (min max)
  "Return two values:
   1) the largest palindromic product of two factors in [min, max], or NIL
   2) a flat vector [x0 y0 x1 y1 ...] of factor pairs (unboxed 64-bit ints)

Algorithm (descending x, descending y with a tight y lower bound):
- We maintain best, the largest palindrome found so far (starts at -1).
- For each row with fixed x (max -> min):
  - outer prune: if x*max <= best, then for any y <= max we have x*y <= x*max <= best,
    so no later rows can improve best -> stop the whole search
  - compute an inner lower bound for y: y_min = max(x, floor(best/x)+1).
    we only care about products > best. for fixed x, those are exactly
    the y with y >= floor(best/x)+1. if max < y_min, this row has no work.
  - iterate y from max down to y_min. products shrink in this row,
    but starting at max and stopping at y_min avoids per-iteration product checks.
  - on the first palindrome hit, update best and stop the row (smaller y will only
    make products smaller in this row).

This mirrors the smallest optimization and further reduces palindromep calls."
  (declare (type fixnum min max))
  (let ((best -1))
    (declare (type fixnum best))
    (block search
      ;; x descends
      (loop for x of-type fixnum from max downto min do
        ;; outer prune: after this, x*max only decreases
        (when (<= (the fixnum (* x max)) best)
          (return-from search))
        ;; compute tight y lower bound from current best
        (let* ((y-lower
                 (max x (1+ (truncate (max best 0) x)))))
          (when (> y-lower max)
            (return)) ; no possible y in this row
          (block row
            ;; y descends only down to y-lower
            (loop for y of-type fixnum from max downto y-lower
                  for product of-type fixnum
                    = #+sbcl (sb-ext:truly-the fixnum (* x y))
                  #-sbcl (* x y) do
                    (when (palindromep product)
                      (setf best product)
                      (return-from row)))))))
    ;; final answer
    (if (>= best 0)
        (values best (collect-positive-factor-pairs best min max))
        (values nil nil))))

;; ------------------------------------------------------------------
;; Public wrappers (Exercism shape): convert vector -> list of lists

(defun smallest (min max)
  "Return two values:
   1) the smallest palindromic product in [min, max], or NIL if none
   2) a list of factor pairs ((x y) ...) that produce it, or NIL."
  (declare (type fixnum min max))
  (multiple-value-bind (prod vec) (smallest-inner min max)
    (if prod
        (values prod (pairs-vector->list vec))
        (values nil nil))))

(defun largest (min max)
  "Return two values:
   1) the largest palindromic product in [min, max], or NIL if none
   2) a list of factor pairs ((x y) ...) that produce it, or NIL."
  (declare (type fixnum min max))
  (multiple-value-bind (prod vec) (largest-inner min max)
    (if prod
        (values prod (pairs-vector->list vec))
        (values nil nil))))



