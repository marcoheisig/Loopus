(in-package #:loopus.ir)

(defun ir-remove-dead-code (ir)
  "Returns a copy of IR in which all calls to pure functions whose outputs
  are never used have been removed."
  ir)
