(defsystem #:Loopus
  :description "A portable loop optimization framework for Common Lisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "bordeaux-threads"
   "trivia"
   "trivial-cltl2"
   "trivial-macroexpand-all")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "variable")
   (:file "polynomial")
   (:file "ir")
   (:file "lexenv")
   (:file "ir-convert")
   (:file "ir-specialize")
   (:file "ir-hoist-loop-invariant-code")
   (:file "ir-eliminate-common-subexpressions")
   (:file "ir-split-loops")
   (:file "ir-unroll-loops")
   (:file "ir-vectorize")
   (:file "ir-optimize")
   (:file "ir-expand")
   (:file "macros")))
