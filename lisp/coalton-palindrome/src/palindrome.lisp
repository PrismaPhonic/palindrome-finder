(in-package #:coalton-palindrome)
(named-readtables:in-readtable coalton:coalton)

;; Override Coalton's library optimization settings to disable safety checks
(cl:declaim (cl:optimize cl:speed (cl:safety 0) (cl:debug 0)))

(coalton-toplevel

 (declare u32-max U32)
 (define u32-max 4294967295)

 (inline)
 (declare u32-quot (U32 -> U32 -> U32))
 (define (u32-quot a b)
     (lisp U32 (a b)
           (cl:locally (cl:declare (cl:optimize (cl:speed 3) (cl:safety 0))
                                   (cl:type (cl:unsigned-byte 32) a b))
             (cl:the (cl:unsigned-byte 32) (cl:truncate a b)))))

 (inline)
 (declare u32-rem (U32 -> U32 -> U32))
 (define (u32-rem a b)
     (lisp U32 (a b)
           (cl:locally (cl:declare (cl:optimize (cl:speed 3) (cl:safety 0))
                                   (cl:type (cl:unsigned-byte 32) a b))
             (cl:the (cl:unsigned-byte 32) (cl:rem a b)))))

 (inline)
 (declare u32-isqrt (U32 -> U32))
 (define (u32-isqrt n)
     (lisp U32 (n)
           (cl:locally (cl:declare (cl:optimize (cl:speed 3) (cl:safety 0))
                                   (cl:type (cl:unsigned-byte 32) n))
             (cl:the (cl:unsigned-byte 32) (cl:isqrt n)))))

 (inline)
 (declare nonzero-quot-if-divisible (U32 -> U32 -> U32))
 ;; Rationale: single division for factor pairs.
 ;; Checking divisibility and then computing the quotient separately would perform two divisions.
 ;; Coalton also cannot cheaply return (q,r) without callback or tuple allocation.
 ;; Using CL's truncate once gives both q and r; we return q if r==0 else 0.
 ;; This compiles to a single idiv plus a remainder test in SBCL, avoiding double work.
 (define (nonzero-quot-if-divisible dividend divisor)
     (lisp U32 (dividend divisor)
           (cl:locally (cl:declare (cl:optimize (cl:speed 3) (cl:safety 0))
                                   (cl:type (cl:unsigned-byte 32) dividend divisor))
             (cl:multiple-value-bind (q r) (cl:truncate dividend divisor)
               (cl:declare (cl:type (cl:unsigned-byte 32) q r))
               (cl:if (cl:zerop r)
                      (cl:the (cl:unsigned-byte 32) q)
                      (cl:the (cl:unsigned-byte 32) 0))))))

 (inline)
 (declare has-even-digits (U32 -> Boolean))
 (define (has-even-digits n)
     (cond
       ((< n 100) True)
       ((< n 1000) False)
       ((< n 10000) True)
       ((< n 100000) False)
       ((< n 1000000) True)
       ((< n 10000000) False)
       ((< n 100000000) True)
       ((< n 1000000000) False)
       (True True)))

 (inline)
 (declare is-pal (U32 -> Boolean))
 ;; Rationale: quotient-only in the half-reverse loop.
 ;; Coalton cannot directly consume Common Lisp multiple values inside Coalton code.
 ;; Using CL's truncate to obtain (quotient, remainder) would force one of:
 ;;   - A CPS callback via coalton:call-coalton-function (adds per-iteration call indirection), or
 ;;   - Returning a (Tuple U32 U32) (heap allocation + pointer indirection per iteration).
 ;; Both approaches caused large slowdowns in this hot loop.
 ;; Instead we compute the quotient once with u32-quot (SBCL strength-reduces /10 to a magic-multiply),
 ;; then derive the remainder via r = n - q*10. This avoids extra calls/allocations and a second division.
 (define (is-pal n)
     (cond
       ((< n 10) True)
       ((== (mod n 10) 0) False)
       ((and (has-even-digits n) (/= (mod n 11) 0)) False)
       (True
        (rec half-reverse ((m n) (rev 0))
             (if (<= m rev)
                 (or (== m rev)
                     (== m (u32-quot rev 10)))
                 (let ((q (u32-quot m 10))
                       (r (- m (* q 10))))
                   (half-reverse q (+ (* rev 10) r))))))))

 (inline)
 (declare collect-factor-pairs (U32 -> U32 -> U32 -> (array:LispArray U32)))
 ;; Factor collection uses nonzero-quot-if-divisible to fuse divisibility check
 ;; and quotient computation into a single division. Returning 0 when not
 ;; divisible lets us branch once and reuse the quotient directly, avoiding a
 ;; second division and avoiding tuple/CPS overhead.
 (define (collect-factor-pairs prod lo hi)
     (let ((x-min (max lo (u32-quot (+ prod (- hi 1)) hi)))
           (x-max (min hi (u32-isqrt prod))))
       (let ((buff (array:make 4 0)))
         (rec fill ((x x-min) (i 0))
              (if (> x x-max)
                  (let ((out (array:make i 0)))
                    (rec copy ((j 0))
                         (when (< j i)
                           (let ((v (array:aref buff j)))
                             (array:set! out j v)
                             (copy (+ j 1)))))
                    out)
                  (let ((q (nonzero-quot-if-divisible prod x)))
                    (if (== q 0)
                        (fill (+ x 1) i)
                        (let ((j i))
                          (array:set! buff j x)
                          (array:set! buff (+ j 1) q)
                          (fill (+ x 1) (+ i 2))))))))))

 (inline)
 (declare smallest-search (U32 -> U32 -> U32))
 (define (smallest-search lo hi)
     (rec search-rows ((x lo) (best u32-max))
          (if (> x hi)
              best
              (if (>= (* x x) best)
                  best
                  (let ((y-upper (min hi (u32-quot (- best 1) x))))
                    (if (< y-upper x)
                        (search-rows (+ x 1) best)
                        (let ((row-best (rec search-column ((y x))
                                             (if (> y y-upper)
                                                 u32-max
                                                 (let ((prod (* x y)))
                                                   (if (is-pal prod)
                                                       prod
                                                       (search-column (+ y 1))))))))
                          (if (== row-best u32-max)
                              (search-rows (+ x 1) best)
                              (search-rows (+ x 1) row-best)))))))))

 (inline)
 (declare largest-search (U32 -> U32 -> U32))
 (define (largest-search lo hi)
     (rec search-rows ((x hi) (best 0))
          (if (< x lo)
              best
              (if (<= (* x hi) best)
                  best
                  (let ((y-lower (max x (+ (u32-quot best x) 1))))
                    (if (> y-lower hi)
                        (search-rows (- x 1) best)
                        (let ((row-best (rec search-column ((y hi))
                                             (if (< y y-lower)
                                                 0
                                                 (let ((prod (* x y)))
                                                   (if (is-pal prod)
                                                       prod
                                                       (search-column (- y 1))))))))
                          (if (== row-best 0)
                              (search-rows (- x 1) best)
                              (search-rows (- x 1) row-best)))))))))

 (inline)
 (declare smallest (U32 -> U32 -> (Optional (Tuple U32 (array:LispArray U32)))))
 (define (smallest lo hi)
     (let ((p (smallest-search lo hi)))
       (if (== p u32-max)
           None
           (Some (Tuple p (collect-factor-pairs p lo hi))))))

 (inline)
 (declare largest (U32 -> U32 -> (Optional (Tuple U32 (array:LispArray U32)))))
 (define (largest lo hi)
     (let ((p (largest-search lo hi)))
       (if (== p 0)
           None
           (Some (Tuple p (collect-factor-pairs p lo hi))))))

 (inline)
 (declare smallest-product-or-zero (U32 -> U32 -> U32))
 (define (smallest-product-or-zero lo hi)
     (let ((p (smallest-search lo hi)))
       (if (== p u32-max) 0 p)))

 (inline)
 (declare largest-product-or-zero (U32 -> U32 -> U32))
 (define (largest-product-or-zero lo hi)
     (largest-search lo hi))

 (inline)
 (declare sum-pairs ((array:LispArray U32) -> U32))
 (define (sum-pairs v)
     (let ((n (array:length v)))
       (rec accumulate-pair-sums ((i 0) (acc 0))
            (if (>= i n)
                acc
                (let ((x (array:aref v i))
                      (y (array:aref v (+ i 1))))
                  (accumulate-pair-sums (+ i 2) (+ acc (+ x y))))))))

 (inline)
 (declare do-iters-largest (U32 -> U32 -> U32 -> (Tuple U32 U32)))
 (define (do-iters-largest lo hi iters)
     (let ((iterate-largest (fn (n current-max acc cnt)
                                (if (>= n iters)
                                    (Tuple acc cnt)
                                    (let ((res (largest lo current-max)))
                                      (match res
                                        ((Some r)
                                         (let ((prod (tuple:fst r))
                                               (pairs (tuple:snd r))
                                               (sum-pairs-val (sum-pairs pairs))
                                               (acc-new (+ (+ (+ acc prod) sum-pairs-val) cnt))
                                               (cnt-new (+ cnt 1))
                                               (next-max (if (<= current-max lo)
                                                             hi
                                                             (- current-max 1))))
                                           (iterate-largest (+ n 1) next-max acc-new cnt-new)))
                                        (_ (let ((next-max (if (<= current-max lo)
                                                               hi
                                                               (- current-max 1))))
                                             (iterate-largest (+ n 1) next-max acc cnt)))))))))
       (let ((base (match (largest lo hi)
                     ((Some r) (tuple:fst r))
                     (_ 0))))
         (Tuple base (tuple:fst (iterate-largest 0 hi 0 0))))))

 (inline)
 (declare do-iters-smallest (U32 -> U32 -> U32 -> (Tuple U32 U32)))
 (define (do-iters-smallest lo hi iters)
     (let ((iterate-smallest (fn (n current-min acc cnt)
                                 (if (>= n iters)
                                     (Tuple acc cnt)
                                     (let ((res (smallest current-min hi)))
                                       (match res
                                         ((Some r)
                                          (let ((prod (tuple:fst r))
                                                (pairs (tuple:snd r))
                                                (sum-pairs-val (sum-pairs pairs))
                                                (acc-new (+ (+ (+ acc prod) sum-pairs-val) cnt))
                                                (cnt-new (+ cnt 1))
                                                (next-min (if (>= current-min hi)
                                                              lo
                                                              (+ current-min 1))))
                                            (iterate-smallest (+ n 1) next-min acc-new cnt-new)))
                                         (_ (let ((next-min (if (>= current-min hi)
                                                                lo
                                                                (+ current-min 1))))
                                              (iterate-smallest (+ n 1) next-min acc cnt)))))))))
       (let ((base (match (smallest lo hi)
                     ((Some r) (tuple:fst r))
                     (_ 0))))
         (Tuple base (tuple:fst (iterate-smallest 0 lo 0 0))))))
 )
