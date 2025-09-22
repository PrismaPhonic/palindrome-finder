(in-package :pp-fast)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; Canonical unsigned types
(deftype word32 () '(unsigned-byte 32))
(deftype word64 () '(unsigned-byte 64))

(defmacro with-divmod ((q r) (num den) &body body)
  `(multiple-value-bind (,q ,r) (truncate ,num ,den)
     (declare (type word32 ,q ,r))
     ,@body))

;; Tight summation for (simple-array word32 (*)) -> word64 (macro to avoid return boxing)
(defmacro sum-ub32-vector (vec)
  (let ((v (gensym "V")) (len (gensym "LEN")) (acc (gensym "ACC")) (i (gensym "I")))
    `(let ((,v ,vec))
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type (or null (simple-array word32 (*))) ,v))
       (let ((,acc 0))
         (declare (type word64 ,acc))
         (when ,v
           (let ((,len (length ,v)))
             (declare (type fixnum ,len))
             (loop for ,i of-type fixnum from 0 below ,len do
               (incf ,acc (the word64 (aref ,v ,i))))))
         (the word64 ,acc)))))

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
