(in-package #:Loopus)

(defun ir-split-loops (ir)
  "Returns a copy of IR in which some loops with a predicate that depend
  only on the iteration space are eliminated by splitting the loop into one
  loop where the predicate is always true, and one loop where the predicate
  is always false."
  ir)
