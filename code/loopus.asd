(defsystem #:Loopus
  :description "A portable loop optimization framework for Common Lisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "closer-mop"
   "trivia"
   "trivial-cltl2"
   "trivial-macroexpand-all"
   "typo"
   "cl-isl")

  :serial t
  :components
  ((:file "packages")
   (:module "math"
    :components
    ((:file "variable")
     (:file "polynomial")))
   (:module "ir"
    :components
    ((:file "ir")
     (:file "lexenv")
     (:file "ir-convert")
     (:file "ir-specialize")
     (:file "ir-remove-dead-code")
     (:file "ir-eliminate-common-subexpressions")
     (:file "ir-split-loops")
     (:file "ir-isl-optimize")
     (:file "input-loopus-ir")
     (:file "output-loopus-ir")
     (:file "ir-unroll-loops")
     (:file "ir-vectorize")
     (:file "ir-optimize")
     (:file "ir-expand")))
   (:file "macros")))
