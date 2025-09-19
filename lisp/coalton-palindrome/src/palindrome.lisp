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
(declare collect-zero-factor-pairs (UFix -> (array:LispArray UFix)))
(define (collect-zero-factor-pairs hi)
  ;; Direct array construction instead of list building
  (let ((arr (array:make-uninitialized (* 2 (+ hi 1)))))
    (rec fill ((y 0) (idx 0))
      (if (> y hi)
          arr
          (let ((_1 (array:set! arr idx 0))
                (_2 (array:set! arr (+ idx 1) y)))
            (fill (+ y 1) (+ idx 2)))))))

(inline)
(declare collect-factor-pairs (UFix -> UFix -> UFix -> (array:LispArray UFix)))
(define (collect-factor-pairs prod lo hi)
  (if (zero? prod)
      (collect-zero-factor-pairs hi)
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
                      (fill (+ x 1) i)))))))))

;; ---------- Smallest/Largest palindrome search ----------
;; Ported directly from Haskell with same variable names and control flow

(inline)
(declare smallest (UFix -> UFix -> (Optional (Tuple UFix (array:LispArray UFix)))))
(define (smallest min-val max-val)
  (match (search-smallest min-val max-val)
    ((Some prod) (Some (Tuple prod (collect-factor-pairs prod min-val max-val))))
    (_ None)))

(inline)
(declare search-smallest (UFix -> UFix -> (Optional UFix)))
(define (search-smallest min-val max-val)
  (search-rows-for-smallest min-val (+ (* max-val max-val) 1) max-val))

(declare search-rows-for-smallest (UFix -> UFix -> UFix -> (Optional UFix)))
(define (search-rows-for-smallest x best max-val)
  (if (> x max-val)
      (if (== best (+ (* max-val max-val) 1)) None (Some best))
      (if (>= (* x x) best)
          (if (== best (+ (* max-val max-val) 1)) None (Some best))
          (match (search-row-for-smallest x best max-val)
            ((Some new-best) (search-rows-for-smallest (+ x 1) new-best max-val))
            (_ (search-rows-for-smallest (+ x 1) best max-val))))))

(declare search-row-for-smallest (UFix -> UFix -> UFix -> (Optional UFix)))
(define (search-row-for-smallest x-val current-best max-val)
  (let ((y-upper (min max-val (ufix-quot (- current-best 1) x-val))))
    (if (< y-upper x-val)
        None
        (search-column-for-smallest x-val y-upper))))

(declare search-column-for-smallest (UFix -> UFix -> (Optional UFix)))
(define (search-column-for-smallest x-val y-upper)
  (rec scan-column-ascending ((y x-val))
    (if (> y y-upper)
        None
        (let ((prod (* x-val y)))
          (if (is-pal prod)
              (Some prod)
              (scan-column-ascending (+ y 1)))))))

(inline)
(declare largest (UFix -> UFix -> (Optional (Tuple UFix (array:LispArray UFix)))))
(define (largest min-val max-val)
  (match (search-largest min-val max-val)
    ((Some prod) (Some (Tuple prod (collect-factor-pairs prod min-val max-val))))
    (_ None)))

(inline)
(declare search-largest (UFix -> UFix -> (Optional UFix)))
(define (search-largest min-val max-val)
  (search-rows-for-largest max-val 0 min-val max-val))

(declare search-column-for-largest (UFix -> UFix -> UFix -> (Optional UFix)))
(define (search-column-for-largest x-val y-lower max-val)
  (rec scan-column-descending ((y max-val))
    (if (< y y-lower)
        None
        (let ((prod (* x-val y)))
          (if (is-pal prod)
              (Some prod)
              (scan-column-descending (- y 1)))))))

(declare search-row-for-largest (UFix -> UFix -> UFix -> (Optional UFix)))
(define (search-row-for-largest x-val best max-val)
  (let ((y-lower (max x-val (+ (ufix-quot best x-val) 1))))
    (if (> y-lower max-val)
        None
        (search-column-for-largest x-val y-lower max-val))))

(declare search-rows-for-largest (UFix -> UFix -> UFix -> UFix -> (Optional UFix)))
(define (search-rows-for-largest x best min-val max-val)
  (if (< x min-val)
      (if (== best 0) None (Some best))
      (if (<= (* x max-val) best)
          (if (== best 0) None (Some best))
          (match (search-row-for-largest x best max-val)
            ((Some new-best) (search-rows-for-largest (- x 1) new-best min-val max-val))
            ((None) (search-rows-for-largest (- x 1) best min-val max-val))))))

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
