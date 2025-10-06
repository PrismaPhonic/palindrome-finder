(defpackage :coalton-palindrome-largest
  (:use :cl)
  (:export :main))
(in-package :coalton-palindrome-largest)

;; Override Coalton's library optimization settings to disable safety checks
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load "../gc.lisp")
(load "../utility.lisp")

(use-package :pp-util)

(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32))
                          (values (unsigned-byte 32) (unsigned-byte 32)))
                %do-iters))

(defun %do-iters (min max iters)
  (declare (type (unsigned-byte 32) min max iters))
  (let ((result (coalton-palindrome:do-iters-largest min max iters)))
    (values (the (unsigned-byte 32) (coalton-library/tuple:fst result))
            (the (unsigned-byte 32) (coalton-library/tuple:snd result)))))

(defun run-with-timing (min max iters)
  (declare (type (unsigned-byte 32) min max iters))
  (let ((start (pp-util:read-monotonic-ns)))
    (multiple-value-bind (prod acc) (%do-iters min max iters)
      (let* ((end (pp-util:read-monotonic-ns))
             (elapsed (pp-util:time-diff-ns start end)))
        (values prod acc elapsed)))))

(defun %parse-u32 (s)
  (the (unsigned-byte 32) (parse-integer s)))

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
                   (multiple-value-bind (prod acc elapsed)
                       (run-with-timing min max a)
                     (format t "OK ~D ~D ~D~%" prod acc elapsed))
                   (finish-output))
                  ((char= c #\Q) (return))))))))

(defun main ()
  (let* ((argv #+sbcl sb-ext:*posix-argv*))
    (if (and argv (find "--server" argv :test #'string=))
        (server-loop)
        (let* ((len (length argv))
               (min (%parse-u32 (svref argv (- len 3))))
               (max (%parse-u32 (svref argv (- len 2))))
               (iters (the (unsigned-byte 32) (max 1 (parse-integer (svref argv (- len 1)))))))
          (pp-gc:prepare-gc-for-bench)
          (format t "~D~%" (%do-iters min max iters))))))
