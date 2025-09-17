(defpackage :pp-runner-fast-largest-inner
  (:use :cl)
  (:export :main))
(in-package :pp-runner-fast-largest-inner)

(load "gc.lisp")
(load "args.lisp")

(defun %do-iters (min max iters)
  (declare (type fixnum min max iters))
  (let* ((range-count (- max min -1))
         (iters-per-range (truncate iters range-count))
         (remainder (mod iters range-count))
         (acc 0)
         (cnt 0))
    (declare (type fixnum range-count iters-per-range remainder acc cnt))
    ;; Base iterations: run iters-per-range times for each range
    (loop for idx from 0 below range-count do
      (let ((current-max (- max idx)))
        (loop for j from 0 below iters-per-range do
          (multiple-value-bind (p vec) (pp-fast:largest-inner min current-max)
            (when p
              (let ((sum 0))
                (declare (type fixnum p sum)
                         (type (simple-array (signed-byte 64) (*)) vec))
                (loop for i from 0 below (length vec) by 2 do
                  (when (< (1+ i) (length vec))
                    (incf sum (+ (aref vec i) (aref vec (1+ i))))))
                (incf acc (+ p sum cnt))
                (incf cnt)))))))
    ;; Remainder iterations: run 1 additional time for first remainder ranges
    (loop for idx from 0 below remainder do
      (let ((current-max (- max idx)))
        (multiple-value-bind (p vec) (pp-fast:largest-inner min current-max)
          (when p
            (let ((sum 0))
              (declare (type fixnum p sum)
                       (type (simple-array (signed-byte 64) (*)) vec))
              (loop for i from 0 below (length vec) by 2 do
                (when (< (1+ i) (length vec))
                  (incf sum (+ (aref vec i) (aref vec (1+ i))))))
              (incf acc (+ p sum cnt))
              (incf cnt))))))
    ;; Return the result for the original range and the accumulator
    (multiple-value-bind (p _vec) (pp-fast:largest-inner min max)
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
                         (format t "OK ~D ~D~%" p acc))
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
