(in-package #:cl-user)

(defpackage #:loopus
  (:use #:closer-common-lisp)
  (:shadow #:variable)
  (:export
   ;; polynomial.lisp
   #:*default-polynomial-representation*
   #:variable
   #:variablep
   #:variable-name
   #:variable-number
   #:polynomial
   #:polynomialp
   #:polynomial+
   #:polynomial-
   #:polynomial*
   ;; other
   #:for
   ))
