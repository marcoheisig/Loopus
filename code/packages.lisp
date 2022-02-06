(in-package #:cl-user)

(defpackage #:loopus
  (:use #:closer-common-lisp)
  (:shadow #:variable)
  (:local-nicknames (#:dlist #:loopus.dlist))
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
