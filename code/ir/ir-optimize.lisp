(in-package #:loopus.ir)

;; Nothing
(defun ir-optimize (ir)
  ir)

;; No isl
;; No common sub expression. Specialize makes (aref array idx) pure and
;; sub expression delete it
(defun ir-optimize (ir)
  (ir-vectorize
   (ir-remove-dead-code
    (ir-specialize ir))))

;; Isl
(defun ir-optimize (ir)
  (ir-vectorize
   (ir-isl-optimize
    (ir-remove-dead-code
     (ir-specialize ir)))))
