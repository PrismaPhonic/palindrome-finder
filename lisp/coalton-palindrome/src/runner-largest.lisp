(defpackage :coalton-palindrome-largest
  (:use :cl)
  (:export :main))
(in-package :coalton-palindrome-largest)

;; Override Coalton's library optimization settings to disable safety checks
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load "../gc.lisp")
(load "../utility.lisp")

(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32))
                          (values (unsigned-byte 32) (unsigned-byte 32)))
                %do-iters))

(defun %do-iters (min max iters)
  (declare (type (unsigned-byte 32) min max iters))
  (let ((result (coalton-palindrome:do-iters-largest min max iters)))
    (values (the (unsigned-byte 32) (coalton-library/tuple:fst result))
            (the (unsigned-byte 32) (coalton-library/tuple:snd result)))))

(defun %parse-u32 (s)
  (the (unsigned-byte 32) (parse-integer s)))

(defun server-loop ()
  (pp-gc:prepare-gc-for-bench)
  (let ((min nil) (max nil))
    (loop for line = (read-line *standard-input* nil nil)
          while line do
            (with-fast-command-parse (line cmd a b)
              (cond
                ((and (>= (length cmd) 4) (string= cmd "INIT" :end1 4))
                 (setf min a max b)
                 (format t "OK~%") (finish-output))
                ((and (>= (length cmd) 6) (string= cmd "WARMUP" :end1 6))
                 (when (and min max) (%do-iters min max a))
                 (format t "OK~%") (finish-output))
                ((and (>= (length cmd) 3) (string= cmd "RUN" :end1 3))
                 (if (and min max)
                     (multiple-value-bind (p acc) (%do-iters min max a)
                       (format t "OK ~D ~D~%" p acc))
                     (format t "ERR NOTINIT~%"))
                 (finish-output))
                ((and (>= (length cmd) 4) (string= cmd "QUIT" :end1 4)) (return))
                (t (format t "ERR BADCMD~%") (finish-output)))))))

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
