(asdf:defsystem #:coalton-palindrome
  :description "Coalton palindromic products implementation"
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "palindrome")))

(asdf:defsystem #:coalton-palindrome/largest
  :description "Coalton largest palindrome runner"
  :depends-on (#:coalton-palindrome)
  :pathname "src/"
  :components ((:file "runner-largest")))

(asdf:defsystem #:coalton-palindrome/smallest
  :description "Coalton smallest palindrome runner"
  :depends-on (#:coalton-palindrome)
  :pathname "src/"
  :components ((:file "runner-smallest")))
