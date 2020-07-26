;;;; lisp-cond-restarts.asd

(asdf:defsystem lisp-cond-restarts
  :description "Conditions and Restarts"
  :author "Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>"
  :license  "CC BY 4.0"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre :hash-set)
  :components ((:file "package")
               (:file "lisp-cond-restarts")))
