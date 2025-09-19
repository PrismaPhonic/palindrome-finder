(require :asdf)
(require :uiop)

(let* ((here (uiop:pathname-directory-pathname *load-truename*))
       (runner "src/runner-smallest.lisp")
       (asd    "coalton-palindrome.asd")
       (out    #P"../../target-bin/palprod-coalton-smallest"))
  (uiop:chdir here)
  (ensure-directories-exist out)

  ;; Configure Coalton for release (production) build before loading systems
  (let ((config '((:compiler-mode          "release")
                  (:print-unicode          t)
                  (:perform-specialization t)
                  (:perform-inlining       t)
                  (:emit-type-annotations  t)
                  (:print-types            nil)
                  (:print-rewrites         nil))))
    (setf (symbol-plist ':coalton-config) nil)
    (dolist (kv config)
      (setf (get ':coalton-config (first kv)) (second kv))))

  ;; Help SBCL derive and keep types in hot code
  (setf sb-ext:*derive-function-types* t
        sb-ext:*block-compile-default* :specified)


  ;; Load Coalton palindrome system
  (declaim (optimize (speed 3) (safety 0) (debug 0)))
  (asdf:load-asd (merge-pathnames asd here))
  (asdf:load-system "coalton-palindrome")

  ;; Load the runner file from THIS directory
  (let ((runner-path (merge-pathnames runner here)))
    (format t ";; Loading ~A~%" runner-path)
    (load runner-path))

  ;; Sanity: confirm the runner package and MAIN
  (let ((pkg (find-package :coalton-palindrome-smallest)))
    (unless pkg
      (error "Runner package COALTON-PALINDROME-SMALLEST not found."))
    (let* ((main-sym (intern "MAIN" pkg))
           (main-fn  (and (fboundp main-sym) (symbol-function main-sym))))
      (unless main-fn
        (error "Symbol MAIN not fbound in COALTON-PALINDROME-SMALLEST."))
      (format t ";; Saving image to ~A~%" out)
      (sb-ext:save-lisp-and-die out
                                :executable t
                                :toplevel main-fn
                                :compression 2
                                :purify t))))
