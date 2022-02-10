(in-package #:loopus)

(defun ir-optimize (ir)
  (ir-vectorize
   (ir-unroll-loops
    (ir-split-loops
     (ir-eliminate-common-subexpressions
      (ir-hoist-loop-invariant-code
       (ir-specialize ir)))))))
