(defpackage #:coalton-palindrome
  (:documentation "Coalton palindromic products implementation")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec  #:coalton-library/vector)
   (#:array #:coalton-library/lisparray)
   (#:math #:coalton-library/math/arith)
   (#:integral #:coalton-library/math/integral)
   (#:cell #:coalton-library/cell)
   (#:str  #:coalton-library/string)
   (#:tuple #:coalton-library/tuple))
  (:export
   #:has-even-digits
   #:is-pal
   #:collect-factor-pairs
   #:smallest
   #:largest
   #:smallest-search
   #:largest-search
   #:sum-pairs
   #:do-iters-largest
   #:do-iters-smallest
   #:smallest-product-or-zero
   #:largest-product-or-zero))
