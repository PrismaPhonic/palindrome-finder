 (defpackage :pp-util
   (:use :cl)
   (:export :with-fast-args-parse :read-monotonic-ns :time-diff-ns))

(in-package :pp-util)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro with-fast-args-parse ((line a b) &body body)
  (let ((s (gensym "S"))
        (len (gensym "LEN"))
        (sp1 (gensym "SP1"))
        (sp2 (gensym "SP2"))
        (end-a (gensym "END-A")))
    `(let* ((,s ,line)
            (,len (length ,s)))
       (declare (type simple-string ,s)
                (type fixnum ,len)
                (optimize (speed 3) (safety 0) (debug 0)))
       (let* ((,sp1 (position #\Space ,s :start 0 :end ,len))
              (,sp2 (and ,sp1 (position #\Space ,s :start (1+ ,sp1) :end ,len)))
              (,end-a (or ,sp2 ,len))
              (,a (and ,sp1 (parse-integer ,s :start (1+ ,sp1) :end ,end-a)))
              (,b (and ,sp2 (parse-integer ,s :start (1+ ,sp2) :end ,len))))
         ,@body))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (unless (fboundp '%clock-gettime)
    (sb-alien:define-alien-type timespec
        (sb-alien:struct timespec
          (tv_sec sb-alien:long)
          (tv_nsec sb-alien:long)))
    (sb-alien:define-alien-type clockid-t sb-alien:int)
    (sb-alien:define-alien-routine ("clock_gettime" %clock-gettime) sb-alien:int
      (clock-id clockid-t)
      (tp (* timespec)))))

#+(and sbcl linux)
(defconstant +clock-monotonic+ 1)

#+(and sbcl (not linux))
(error "CLOCK_MONOTONIC constant not defined for this platform")

#+sbcl
(declaim (inline read-monotonic-ns))

#+sbcl
(defun read-monotonic-ns ()
  (sb-alien:with-alien ((ts timespec))
    (unless (zerop (%clock-gettime +clock-monotonic+ (sb-alien:addr ts)))
      (error "clock_gettime failed"))
    (+ (* (sb-alien:slot ts 'tv_sec) 1000000000)
       (sb-alien:slot ts 'tv_nsec))))

#+sbcl
(declaim (inline time-diff-ns))

#+sbcl
(defun time-diff-ns (start end)
  (- end start))

#-sbcl
(declaim (inline read-monotonic-ns))

#-sbcl
(defun read-monotonic-ns ()
  (get-internal-real-time))

#-sbcl
(declaim (inline time-diff-ns))

#-sbcl
(defun time-diff-ns (start end)
  (let ((ticks-per-second internal-time-units-per-second))
    (truncate (* (- end start)
                 (/ 1000000000d0 ticks-per-second)))))
