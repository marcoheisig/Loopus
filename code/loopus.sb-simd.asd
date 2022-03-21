(defsystem #:loopus.sb-simd
  :description "SIMD vectorization for Loopus"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "loopus"
   "typo.sb-simd"
   "trivia")

  :serial t
  :components
  ((:module "ir" :components ((:file "sb-simd")))))
