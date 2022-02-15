(in-package #:cl-user)

(progn
  (defpackage #:loopus
    (:use #:closer-common-lisp)
    #1=
    (:export
     #:for
     #:begin))

  (defpackage #:loopus.internals
    (:use #:closer-common-lisp #:loopus)
    (:import-from #:trivia #:place #:<> #:access)
    (:shadow #:variable)
    #1#
    ;; Mathematical Functions (loopus.math)
    #2=
    (:export
     #:*default-polynomial-representation*
     #:variable
     #:variablep
     #:variable-name
     #:variable-number
     #:polynomial
     #:polynomialp
     #:polynomial+
     #:polynomial-
     #:polynomial*)
    ;; The Loopus Intermediate Representation (loopus.ir)
    #3=
    (:export
     #:ir-node
     #:ir-node-p
     #:ir-node-dominator
     #:ir-node-inputs
     #:ir-node-outputs
     #:ir-node-successor
     #:ir-node-predecessor

     #:ir-initial-node
     #:ir-initial-node-p
     #:ir-final-node
     #:ir-final-node-p
     #:make-ir-initial-and-ir-final-node

     #:ir-loop
     #:ir-loop-p
     #:ir-loop-body-initial-node
     #:ir-loop-variable

     #:ir-if
     #:ir-if-p
     #:ir-if-then-initial-node
     #:ir-if-else-initial-node

     #:ir-construct
     #:ir-construct-p

     #:ir-enclose
     #:ir-enclose-p
     #:ir-enclose-argument-values
     #:ir-enclose-body-initial-node

     #:ir-value
     #:ir-value-p
     #:ir-value-producer
     #:ir-value-users
     #:ir-value-declared-type
     #:ir-value-derived-type

     #:insert-ir-node-before
     #:insert-ir-node-after
     #:extract-ir-node
     #:map-ir-node-successors
     #:map-ir-node-predecessors
     #:map-ir-nodes

     #:ir-convert-in-environment
     #:ir-optimize
     #:ir-specialize
     #:ir-remove-dead-code
     #:ir-hoist-loop-invariant-code
     #:ir-eliminate-common-subexpressions
     #:ir-split-loops
     #:ir-unroll-loops
     #:ir-vectorize
     #:ir-expand))

  (defpackage #:loopus.math
    (:use #:closer-common-lisp #:loopus.internals)
    (:import-from #:trivia #:place #:<> #:access)
    (:shadow #:variable)
    #2#)

  (defpackage #:loopus.ir
    (:use #:closer-common-lisp #:loopus.internals)
    (:import-from #:trivia #:place #:<> #:access)
    (:shadow #:variable)
    #3#))
