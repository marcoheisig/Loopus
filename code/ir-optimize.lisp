(in-package #:Loopus)

(defun ir-optimize (ir)
  (ir-vectorize
   (ir-unroll-loops
    (ir-split-loops
     (ir-eliminate-common-subexpressions
      (ir-hoist-loop-invariant-code
       (ir-remove-dead-code
        (ir-specialize ir))))))))
