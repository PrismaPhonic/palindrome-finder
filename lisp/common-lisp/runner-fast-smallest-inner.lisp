(defpackage :pp-runner-fast-smallest-inner
  (:use :cl)
  (:export :main))
(in-package :pp-runner-fast-smallest-inner)

(load "../gc.lisp")
(load "args.lisp")
(load "../utility.lisp")

(use-package :pp-util)

#-sbcl
(declaim (ftype (function (pp-fast::word32 pp-fast::word32 pp-fast::word32)
                          (values pp-fast::word32 (unsigned-byte 64)))
                %do-iters))

(defun %do-iters (min max iters)
  (declare (type pp-fast::word32 min max iters))
  (let* ((acc 0)
         (cnt 0)
         (current-min min))
    (declare (type (unsigned-byte 64) acc cnt)
             (type pp-fast::word32 current-min))
    (loop for n of-type pp-fast::word32 from 0 below iters do
      (multiple-value-bind (p vec) (pp-fast:smallest-inner current-min max)
        (when p
          (let ((sum 0))
            (declare (type (unsigned-byte 64) sum)
                     (type (simple-array (unsigned-byte 32) (*)) vec)
                     (type pp-fast::word32 p))
            (setf sum (the pp-fast::word64 (pp-fast::sum-ub32-vector vec)))
            (setf acc (the pp-fast::word64 (+ acc sum)))
            (setf acc (the pp-fast::word64 (+ acc (the pp-fast::word64 p))))
            (setf acc (the pp-fast::word64 (+ acc cnt)))
            (incf cnt))))
      (setf current-min (if (>= current-min max)
                            min
                            (the pp-fast::word32 (1+ current-min)))))
    (multiple-value-bind (p _vec) (pp-fast:smallest-inner min max)
      (declare (ignore _vec))
      (values (or p 0) acc))))

(defun run-with-timing (min max iters)
  (declare (type pp-fast::word32 min max iters))
  #+sbcl
  (let ((start (pp-util:read-monotonic-ns)))
    (multiple-value-bind (product acc) (%do-iters min max iters)
      (declare (type pp-fast::word32 product)
               (type (unsigned-byte 64) acc))
      (let ((elapsed (the (unsigned-byte 64)
                          (pp-util:time-diff-ns start (pp-util:read-monotonic-ns)))))
        (values product acc elapsed))))
  #-sbcl
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (product acc) (%do-iters min max iters)
      (let* ((end (get-internal-real-time))
             (elapsed-ns (truncate (* (- end start)
                                      (/ 1000000000d0
                                         internal-time-units-per-second)))))
        (values product acc elapsed-ns)))))

(defun server-loop ()
  (pp-gc:prepare-gc-for-bench)
  (let ((min nil) (max nil))
    (loop for line = (read-line *standard-input* nil nil)
          while line do
            (with-fast-args-parse (line a b)
              (let ((c (schar line 0)))
                (cond
                  ((char= c #\I)
                   (setf min a max b)
                   (format t "OK~%") (finish-output))
                  ((char= c #\W)
                   (%do-iters min max a)
                   (format t "OK~%") (finish-output))
                  ((char= c #\R)
                   (multiple-value-bind (p acc elapsed-ns) (run-with-timing min max a)
                     (format t "OK ~A ~A ~D~%" p acc elapsed-ns)
                     (finish-output)))
                  ((char= c #\Q) (return))))))))

(defun main ()
  (let* ((argv #+sbcl sb-ext:*posix-argv*))
    (if (and argv (find "--server" argv :test #'string=))
        (server-loop)
        (let* ((len (length argv))
               (min (the (unsigned-byte 32) (parse-integer (svref argv (- len 3)))))
               (max (the (unsigned-byte 32) (parse-integer (svref argv (- len 2)))))
               (iters (the (unsigned-byte 32) (max 1 (parse-integer (svref argv (- len 1)))))))
          (pp-gc:prepare-gc-for-bench)
          (format t "~D~%" (%do-iters min max iters))))))


