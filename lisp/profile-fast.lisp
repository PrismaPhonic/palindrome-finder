;;;; profile-fast.lisp — profile pp-fast:{smallest,largest}-inner with SB-SProf
;;;; Recompile with PALINDROMEP notinline so samples attribute correctly.

(require :asdf)
(require :uiop)
(require :sb-sprof)

;; Load pp-fast from this directory
(let ((here (uiop:pathname-directory-pathname *load-truename*)))
  (uiop:chdir here)
  (asdf:load-asd (merge-pathnames "pp-fast.asd" here))
  (asdf:load-system "pp-fast"))

(defpackage :pp-prof
  (:use :cl)
  (:import-from :sb-sprof :start-profiling :stop-profiling :reset :report))
(in-package :pp-prof)

;;; ------------------------- Hard-coded params -------------------------
(defparameter +smallest-min+ 910)
(defparameter +smallest-max+ 999)
(defparameter +smallest-iters+ 8000000)

(defparameter +largest-min+ 100)
(defparameter +largest-max+ 999)
(defparameter +largest-iters+ 2000000)

(defparameter +interval+ 0.0002d0)   ; 200 µs

;;; -------------------- Recompile with NOTINLINE on pal ----------------
(defun recompile-hot-with-pal-notinline ()
  "Ensure callers don’t inline PALINDROMEP by recompiling them after NOTINLINE."
  ;; Tell compiler to not inline
  (declaim (notinline pp-fast::palindromep))
  ;; Recompile palindromep itself
  (compile 'pp-fast::palindromep)
  ;; Recompile callers so they call, not inline
  (declaim (notinline pp-fast:smallest-inner pp-fast:largest-inner))
  (compile 'pp-fast:smallest-inner)
  (compile 'pp-fast:largest-inner)
  (format t "~&[profile] Recompiled with NOTINLINE palindromep.~%"))

(defun %maybe-gc-full ()
  (ignore-errors (funcall (find-symbol "GC" "SB-EXT") :full t)))

(defun run-many (fn min max iters)
  (declare (type function fn) (type fixnum min max iters))
  (let ((last-prod nil) (last-vec nil))
    (dotimes (i iters)
      (declare (fixnum i))
      (multiple-value-setq (last-prod last-vec) (funcall fn min max)))
    (values last-prod last-vec)))

(defun profile-one (label fn min max iters &key (interval 0.001d0) (max-samples 200000))
  (format t "~&[profile] ~A  min=~D max=~D iters=~D interval=~F~%"
          label min max iters interval)
  ;; Warm up (JIT) before sampling
  (run-many fn min max (min 100 iters))
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode :cpu :sample-interval interval :max-samples max-samples)
  (unwind-protect
       (progn
         (run-many fn min max iters)
         (sb-sprof:stop-profiling)
         (terpri) (format t "~&----- ~A (flat) -----~%" label)
         (sb-sprof:report :type :flat :stream *standard-output*)
         (terpri) (format t "~&----- ~A (call-graph) -----~%" label)
         (sb-sprof:report :type :graph :stream *standard-output*))
    (ignore-errors (sb-sprof:stop-profiling))))

(defun main ()
  (recompile-hot-with-pal-notinline)
  (%maybe-gc-full)

  ;; Profile SMALLEST (heavy palindromep pressure)
  (profile-one "pp-fast:smallest-inner"
               #'pp-fast:smallest-inner
               +smallest-min+ +smallest-max+ +smallest-iters+
               :interval +interval+)

  ;; Profile LARGEST (few palindromep calls due to pruning)
  (profile-one "pp-fast:largest-inner"
               #'pp-fast:largest-inner
               +largest-min+ +largest-max+ +largest-iters+
               :interval +interval+)

  (format t "~&[profile] done.~%"))

(main)
