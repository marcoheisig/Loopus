(defsystem #:loopus
  :description "A portable loop optimization framework for Common Lisp."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "bordeaux-threads"
   "loopus.dlist"
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
   (:file "ir-optimize")
   (:file "ir-expand")
   (:file "macros")))
