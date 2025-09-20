(in-package #:coalton-palindrome)
(named-readtables:in-readtable coalton:coalton)

;; Override Coalton's library optimization settings to disable safety checks
(cl:declaim (cl:optimize cl:speed (cl:safety 0) (cl:debug 0)))

(coalton-toplevel

;; ---------- UFix-optimized arithmetic helpers ----------

 (inline)
 (declare ufix-quot (UFix -> UFix -> UFix))
 (define (ufix-quot a b)
   "Fast UFix division using direct CL truncate with proper fixnum types"
   (lisp UFix (a b)
         (cl:locally (cl:declare (cl:optimize cl:speed (cl:safety 0))
                                 (cl:type cl:fixnum a b))
           (cl:truncate a b))))

 (inline)
 (declare ufix-isqrt (UFix -> UFix))
 (define (ufix-isqrt n)
   "Fast UFix integer square root using direct CL isqrt with proper fixnum types"
   (lisp UFix (n)
         (cl:locally (cl:declare (cl:optimize cl:speed (cl:safety 0))
                                 (cl:type cl:fixnum n))
           (cl:isqrt n))))

 ;; ---------- Simple palindrome check ----------

 (inline)
 (declare has-even-digits (UFix -> Boolean))
 (define (has-even-digits n)
     (cond
       ((< n 10) False)
       ((< n 100) True)
       ((< n 1000) False)
       ((< n 10000) True)
       ((< n 100000) False)
       ((< n 1000000) True)
       ((< n 10000000) False)
       ((< n 100000000) True)
       ((< n 1000000000) False)
       ((< n 10000000000) True)
       ((< n 100000000000) False)
       ((< n 1000000000000) True)
       ((< n 10000000000000) False)
       ((< n 100000000000000) True)
       ((< n 1000000000000000) False)
       ((< n 10000000000000000) True)
       ((< n 100000000000000000) False)
       (True True)))

 (inline)
 (declare is-pal (UFix -> Boolean))
 (define (is-pal n)
     (cond
       ((< n 10) True)
       ((and (nonzero? n) (zero? (mod n 10))) False)
       ((and (>= n 11) (has-even-digits n) (nonzero? (mod n 11))) False)
       (True
        (rec half-reverse ((m n) (rev 0))
             (if (<= m rev)
                 (or (== m rev)
                     (== m (ufix-quot rev 10)))
                 (let ((digit (mod m 10)))
                   (half-reverse (ufix-quot m 10) (+ (* rev 10) digit))))))))

 ;; ---------- Simple factor pair collection ----------


 (inline)
 (declare collect-factor-pairs (UFix -> UFix -> UFix -> (array:LispArray UFix)))
 (define (collect-factor-pairs prod lo hi)
     (let ((x-min (max lo (ufix-quot (+ prod (- hi 1)) hi)))
           (x-max (min hi (ufix-isqrt prod))))
       (let ((buff (array:make-uninitialized 6)))
         (rec fill ((x x-min) (i 0))
              (if (> x x-max)
                  (let ((out (array:make-uninitialized i)))
                    (rec copy ((j 0))
                         (when (< j i)
                           (let ((v (array:aref buff j)))
                             (array:set! out j v)
                             (copy (+ j 1)))))
                    out)
                  (let ((divisible (zero? (mod prod x))))
                    (if divisible
                        (let ((y (ufix-quot prod x))
                              (in-range (and (>= y lo) (<= y hi))))
                          (when in-range
                            (let ((j i))
                              (array:set! buff j x)
                              (array:set! buff (+ j 1) y)))
                          (fill (+ x 1) (if in-range (+ i 2) i)))
                        (fill (+ x 1) i))))))))

 ;; ---------- Smallest/Largest palindrome search ----------
 ;; Ported directly from Haskell with same variable names and control flow

 (inline)
 (declare smallest (UFix -> UFix -> (Optional (Tuple UFix (array:LispArray UFix)))))
 (define (smallest lo hi)
     (let ((max-bound 4611686018427387903))  ; maxBound for UFix (2^62 - 1)
       (rec search-rows ((x lo) (best max-bound))
                (if (> x hi)
                    ;; End of search - return best if found
                    (if (== best max-bound)
                        None
                        (Some (Tuple best (collect-factor-pairs best lo hi))))
                    (if (>= (* x x) best)
                        ;; Early termination - x*x >= best
                        (if (== best max-bound)
                            None
                            (Some (Tuple best (collect-factor-pairs best lo hi))))
                        ;; Search this row for palindromes
                        (let ((y-upper (min hi (ufix-quot (- best 1) x))))
                          (if (< y-upper x)
                              ;; No valid y values in this row
                              (search-rows (+ x 1) best)
                              ;; Scan column for palindromes
                              (let ((result (rec search-column ((y x) (row-best max-bound))
                                                 (if (> y y-upper)
                                                     (if (== row-best max-bound)
                                                         None
                                                         (Some row-best))
                                                     (let ((prod (* x y)))
                                                       (if (is-pal prod)
                                                           (Some prod)
                                                           (search-column (+ y 1) row-best)))))))
                                (match result
                                  ((Some new-best) (search-rows (+ x 1) new-best))
                                  (_ (search-rows (+ x 1) best)))))))))))

 (inline)
 (declare largest (UFix -> UFix -> (Optional (Tuple UFix (array:LispArray UFix)))))
 (define (largest lo hi)
     (rec search-rows ((x hi) (best 0))
              (if (< x lo)
                  ;; End of search - return best if found
                  (if (== best 0)
                      None
                      (Some (Tuple best (collect-factor-pairs best lo hi))))
                  (if (<= (* x hi) best)
                      ;; Early termination - x*hi <= best
                      (if (== best 0)
                          None
                          (Some (Tuple best (collect-factor-pairs best lo hi))))
                      (if (== x 0)
                          ;; No valid factors when x == 0
                          None
                          ;; Search this row for palindromes
                          (let ((y-lower (max x (+ (ufix-quot best x) 1))))
                            (if (> y-lower hi)
                                ;; No valid y values in this row
                                (search-rows (- x 1) best)
                                ;; Scan column for palindromes (completely inlined)
                                (let ((result (rec search-column ((y hi) (row-best 0))
                                                   (if (< y y-lower)
                                                       (if (== row-best 0)
                                                           None
                                                           (Some row-best))
                                                       (let ((prod (* x y)))
                                                         (if (is-pal prod)
                                                             (Some prod)
                                                             (search-column (- y 1) row-best)))))))
                                  (match result
                                    ((Some new-best) (search-rows (- x 1) new-best))
                                    (_ (search-rows (- x 1) best)))))))))))

 ;; ---------- Helper functions for accumulator logic ----------

 (inline)
 (declare sum-pairs ((array:LispArray UFix) -> UFix))
 (define (sum-pairs v)
     (let ((n (array:length v)))
       (rec accumulate-pair-sums ((i 0) (acc 0))
            (if (>= i n)
                acc
                (let ((x (array:aref v i))
                      (y (array:aref v (+ i 1))))
                  (accumulate-pair-sums (+ i 2) (+ acc (+ x y))))))))

 ;; ---------- Benchmarking functions ----------

 (inline)
 (declare do-iters-largest (UFix -> UFix -> UFix -> (Tuple UFix UFix)))
 (define (do-iters-largest min-val max-val iters)
     (let ((range-count (if (>= max-val min-val)
                            (+ (- max-val min-val) 1)
                            0)))
       (if (== range-count 0)
           (Tuple 0 0)
           (let ((iterate-largest (fn (n current-max acc cnt)
                                      (if (>= n iters)
                                          (Tuple acc cnt)
                                          (let ((res (largest min-val current-max)))
                                            (match res
                                              ((Some r)
                                               (let ((prod (tuple:fst r))
                                                     (pairs (tuple:snd r))
                                                     (sum-pairs-val (sum-pairs pairs))
                                                     (acc-new (+ (+ (+ acc prod) sum-pairs-val) cnt))
                                                     (cnt-new (+ cnt 1))
                                                     (next-max (if (<= current-max min-val)
                                                                   max-val
                                                                   (- current-max 1))))
                                                 (iterate-largest (+ n 1) next-max acc-new cnt-new)))
                                              (_ (let ((next-max (if (<= current-max min-val)
                                                                     max-val
                                                                     (- current-max 1))))
                                                   (iterate-largest (+ n 1) next-max acc cnt)))))))))
             (let ((acc0 (tuple:fst (iterate-largest 0 max-val 0 0))))
               (match (largest min-val max-val)
                 ((Some r) (Tuple (tuple:fst r) acc0))
                 ((None) (Tuple 0 acc0))))))))

 (inline)
 (declare do-iters-smallest (UFix -> UFix -> UFix -> (Tuple UFix UFix)))
 (define (do-iters-smallest min-val max-val iters)
     (let ((range-count (if (>= max-val min-val)
                            (+ (- max-val min-val) 1)
                            0)))
       (if (== range-count 0)
           (Tuple 0 0)
           (let ((iterate-smallest (fn (n current-min acc cnt)
                                       (if (>= n iters)
                                           (Tuple acc cnt)
                                           (let ((res (smallest current-min max-val)))
                                             (match res
                                               ((Some r)
                                                (let ((prod (tuple:fst r))
                                                      (pairs (tuple:snd r))
                                                      (sum-pairs-val (sum-pairs pairs))
                                                      (acc-new (+ (+ (+ acc prod) sum-pairs-val) cnt))
                                                      (cnt-new (+ cnt 1))
                                                      (next-min (if (>= current-min max-val)
                                                                    min-val
                                                                    (+ current-min 1))))
                                                  (iterate-smallest (+ n 1) next-min acc-new cnt-new)))
                                               (_ (let ((next-min (if (>= current-min max-val)
                                                                      min-val
                                                                      (+ current-min 1))))
                                                    (iterate-smallest (+ n 1) next-min acc cnt)))))))))
             (let ((acc0 (tuple:fst (iterate-smallest 0 min-val 0 0))))
               (match (smallest min-val max-val)
                 ((Some r) (Tuple (tuple:fst r) acc0))
                 ((None) (Tuple 0 acc0))))))))

 ;; ---------- CL-friendly wrappers ----------

 (declare smallest-product-or-zero (UFix -> UFix -> UFix))
 (define (smallest-product-or-zero lo hi)
     (match (smallest lo hi)
       ((Some t) (tuple:fst t))
       (_ 0)))

 (declare largest-product-or-zero (UFix -> UFix -> UFix))
 (define (largest-product-or-zero lo hi)
     (match (largest lo hi)
       ((Some t) (tuple:fst t))
       (_ 0)))
 )
