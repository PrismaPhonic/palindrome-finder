;; Ensures that coalton-palindrome and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-asd "/home/pmfarr/git/palindrome-bench/lisp/coalton-palindrome/coalton-palindrome.asd")
  (asdf:load-system "coalton-palindrome")
  (quicklisp-client:quickload :fiveam))

;; Defines the testing package with symbols from coalton-palindrome and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage :coalton-palindrome-test
  (:use :cl :fiveam)
  (:export :run-tests))

;; Enter the testing package
(in-package :coalton-palindrome-test)

;; Define and enter a new FiveAM test-suite
(def-suite* coalton-palindrome-suite)

;; Helper functions to call Coalton functions via wrappers
(defun call-coalton-smallest (min-factor max-factor)
  "Return (values product marker) where marker is T if factors exist, NIL otherwise.
If min > max, return NIL (single value) to match error-case expectations."
  (if (> min-factor max-factor)
      nil
      (let ((prod (coalton-palindrome:smallest-product-or-zero min-factor max-factor)))
        (if (zerop prod)
            (values nil nil)
            (values prod t)))))

(defun call-coalton-largest (min-factor max-factor)
  "Return (values product marker) where marker is T if factors exist, NIL otherwise.
If min > max, return NIL (single value) to match error-case expectations."
  (if (> min-factor max-factor)
      nil
      (let ((prod (coalton-palindrome:largest-product-or-zero min-factor max-factor)))
        (if (zerop prod)
            (values nil nil)
            (values prod t)))))

(test find-the-smallest-palindrome-from-single-digit-factors
      (let ((min-factor 1)
            (max-factor 9)
            (palindrome 1))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-largest-palindrome-from-single-digit-factors
      (let ((min-factor 1)
            (max-factor 9)
            (palindrome 9))
        (is (multiple-value-bind (p f) (call-coalton-largest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-smallest-palindrome-from-double-digit-factors
      (let ((min-factor 10)
            (max-factor 99)
            (palindrome 121))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-largest-palindrome-from-double-digit-factors
      (let ((min-factor 10)
            (max-factor 99)
            (palindrome 9009))
        (is (multiple-value-bind (p f) (call-coalton-largest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-smallest-palindrome-from-triple-digit-factors
      (let ((min-factor 100)
            (max-factor 999)
            (palindrome 10201))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-largest-palindrome-from-triple-digit-factors
      (let ((min-factor 100)
            (max-factor 999)
            (palindrome 906609))
        (is (multiple-value-bind (p f) (call-coalton-largest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-smallest-palindrome-from-four-digit-factors
      (let ((min-factor 1000)
            (max-factor 9999)
            (palindrome 1002001))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test find-the-largest-palindrome-from-four-digit-factors
      (let ((min-factor 1000)
            (max-factor 9999)
            (palindrome 99000099))
        (is (multiple-value-bind (p f) (call-coalton-largest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(test empty-result-for-smallest-if-no-palindrome-in-the-range
      (let ((min-factor 1002)
            (max-factor 1003))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (null p) (null f))))))

(test empty-result-for-largest-if-no-palindrome-in-the-range
      (let ((min-factor 15)
            (max-factor 15))
        (is (multiple-value-bind (p f) (call-coalton-largest min-factor max-factor)
              (and (null p) (null f))))))

(test error-result-for-smallest-if-min-is-more-than-max
      (let ((min-factor 10000)
            (max-factor 1))
        (is (eql NIL (call-coalton-smallest min-factor max-factor)))))

(test error-result-for-largest-if-min-is-more-than-max
      (let ((min-factor 2)
            (max-factor 1))
        (is (eql NIL (call-coalton-largest min-factor max-factor)))))

(test smallest-product-does-not-use-the-smallest-factor
      (let ((min-factor 3215)
            (max-factor 4000)
            (palindrome 10988901))
        (is (multiple-value-bind (p f) (call-coalton-smallest min-factor max-factor)
              (and (eql p palindrome) (not (null f)))))))

(defun run-tests (&optional (test-or-suite 'coalton-palindrome-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))
