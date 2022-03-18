(in-package #:loopus.ir)

(defun ir-optimize (ir)
  (ir-vectorize
   (ir-unroll-loops
    (ir-split-loops
     (ir-remove-dead-code
      (ir-eliminate-common-subexpressions
       (ir-specialize ir)))))))
