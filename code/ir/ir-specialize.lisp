(in-package #:loopus.ir)

(defun ir-specialize (ir)
  "Returns a copy of IR in which the derived type of each value is refined
  based on the knowledge about its producer, and where each function call
  has been replaced by the most specific function that has identical
  semantics but for the (possibly refined) type of its inputs."
  ir)
