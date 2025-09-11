(defpackage :pp-args
  (:use :cl)
  (:export :parse-min-max-iters))
(in-package :pp-args)

(defun parse-min-max-iters ()
  (let* ((argv #+sbcl sb-ext:*posix-argv*))
    (let ((args (subseq argv (- (length argv) 3))))
      (unless (= (length args) 3)
        (format t "usage: ~A <min> <max> <iterations>~%" (first argv))
        (uiop:quit 2))
      (values (parse-integer (first args))
              (parse-integer (second args))
              (max 1 (parse-integer (third args)))))))
