(in-package #:loopus.ir)

(defun ir-vectorize (ir)
  "Returns a copy of IR in which sufficiently simple inner loops have been
  replaced by a vectorized loop and a reminder loop."
  (let ((*ir-value-copies* (make-hash-table :test #'eq)))
    (copy-ir-block 'ir-vectorize ir nil)))


