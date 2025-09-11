(require :asdf)
(require :uiop)

(let* ((here (uiop:pathname-directory-pathname *load-truename*))
       (runner "runner-comm-largest-inner.lisp")
       (out    #P"../target-bin/palprod-comm-largest-inner"))
  (uiop:chdir here)
  (ensure-directories-exist out)

  ;; Load system so PP-COMM exists
  (asdf:load-asd (merge-pathnames "pp-comm.asd" here))
  (asdf:load-system "pp-comm")

  ;; Load the runner file from THIS directory
  (let ((runner-path (merge-pathnames runner here)))
    (format t ";; Loading ~A~%" runner-path)
    (load runner-path))

  ;; Sanity: confirm the runner package and MAIN
  (let ((pkg (find-package :pp-runner-comm-largest-inner)))
    (unless pkg
      (error "Runner package PP-RUNNER-COMM-LARGEST-INNER not found."))
    (let* ((main-sym (intern "MAIN" pkg))
           (main-fn  (and (fboundp main-sym) (symbol-function main-sym))))
      (unless main-fn
        (error "Symbol MAIN not fbound in PP-RUNNER-COMM-LARGEST-INNER."))
      (format t ";; Saving image to ~A~%" out)
      (sb-ext:save-lisp-and-die out
                                :executable t
                                :toplevel main-fn
                                :compression 2
                                :purify t))))
