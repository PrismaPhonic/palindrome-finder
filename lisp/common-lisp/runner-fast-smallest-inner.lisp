(defpackage :pp-runner-fast-smallest-inner
  (:use :cl)
  (:export :main))
(in-package :pp-runner-fast-smallest-inner)

(load "../gc.lisp")
(load "args.lisp")

(defun %do-iters (min max iters)
  (declare (type fixnum min max iters))
  (let* ((acc 0)
         (cnt 0)
         (current-min min))
    (declare (type fixnum acc cnt current-min))
    (loop for n fixnum from 0 below iters do
      (multiple-value-bind (p vec) (pp-fast:smallest-inner current-min max)
        (when p
          (let ((sum 0))
            (declare (type fixnum p sum)
                     (type (simple-array (signed-byte 64) (*)) vec))
            (loop for i from 0 below (length vec) by 2 do
              (when (< (1+ i) (length vec))
                (incf sum (+ (aref vec i) (aref vec (1+ i))))))
            (incf acc (+ p sum cnt))
            (incf cnt))))
      (setf current-min (if (>= current-min max)
                            min
                            (the fixnum (1+ current-min)))))
    (multiple-value-bind (p _vec) (pp-fast:smallest-inner min max)
      (declare (ignore _vec))
      (values (or p 0) acc))))

(defun server-loop ()
  (pp-gc:prepare-gc-for-bench)
  (let ((min nil) (max nil))
    (loop for line = (read-line *standard-input* nil nil)
          while line do
            (let* ((parts (uiop:split-string line :separator " "))
                   (cmd   (string-upcase (first parts))))
              (cond
                ((string= cmd "INIT")
                 (setf min (parse-integer (second parts))
                       max (parse-integer (third parts)))
                 (format t "OK~%") (finish-output))
                ((string= cmd "WARMUP")
                 (let ((iters (parse-integer (second parts))))
                   (when (and min max) (%do-iters min max iters))
                   (format t "OK~%") (finish-output)))
                ((string= cmd "RUN")
                 (let ((iters (parse-integer (second parts))))
                   (if (and min max)
                       (multiple-value-bind (p acc) (%do-iters min max iters)
                         (format t "OK ~A ~A~%" p acc))
                       (format t "ERR NOTINIT~%"))
                   (finish-output)))
                ((string= cmd "QUIT") (return))
                (t (format t "ERR BADCMD~%") (finish-output)))))))

(defun main ()
  (let* ((argv #+sbcl sb-ext:*posix-argv*))
    (if (and argv (find "--server" argv :test #'string=))
        (server-loop)
        (multiple-value-bind (min max iters) (pp-args:parse-min-max-iters)
          (pp-gc:prepare-gc-for-bench)
          (format t "~D~%" (%do-iters min max iters))))))


