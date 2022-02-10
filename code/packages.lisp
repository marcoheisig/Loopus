(in-package #:cl-user)

(defpackage #:xyz.heisig.loopus
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
   ;; macros.lisp
   #:for
   #:begin))
