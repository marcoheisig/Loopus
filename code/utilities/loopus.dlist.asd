(defsystem #:loopus.dlist
  :description "A doubly-linked list implementation for Loopus."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria")

  :serial t
  :components
  ((:file "dlist")))
