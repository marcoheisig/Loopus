(in-package #:loopus)

(defun ir-hoist-loop-invariant-code (ir)
  "Returns a copy of IR in which each call to a pure function has been
  moved outside of each loop that none of its inputs depends on."
  ir)
