(in-package #:loopus.ir)

(defun ir-specialize (ir)
  "Returns a copy of IR in which the derived type of each value is refined
  based on the knowledge about its producer, and where each function call
  has been replaced by the most specific function that has identical
  semantics but for the (possibly refined) type of its inputs."
  (let ((*ir-value-copies* (make-hash-table :test #'eq)))
    (copy-ir-block 'ir-specialize ir nil)))

(defun find-specialized-value (ir-value)
  (multiple-value-bind (copy present-p)
      (gethash ir-value *ir-value-copies*)
    (if (not present-p)
        (error "Reference to IR value that hasn't been specialized upon yet: ~S"
               ir-value)
        copy)))

(defmethod copy-ir-node
    ((context (eql 'ir-specialize))
     (ir-call ir-call))
  (let* ((outputs (ir-node-outputs ir-call))
         (max-typed-outputs (if (eql outputs '*) 0 (length outputs))))
    ;; Wrappers can be either IR nodes or IR values.
    (labels ((wrapper-nth-value-ntype (n wrapper)
               (etypecase wrapper
                 (ir-value (if (zerop n)
                               (ir-value-derived-ntype wrapper)
                               (typo:type-specifier-ntype 'null)))
                 (ir-call
                  (let ((outputs (ir-node-outputs wrapper)))
                    (if (< n (length outputs))
                        (ir-value-derived-ntype (nth n outputs))
                        (typo:type-specifier-ntype 'null))))))
             (wrap-constant (constant)
               (let ((ir-value (make-instance 'ir-value :derived-ntype (typo:ntype-of constant))))
                 (make-instance 'ir-construct
                   :form `',constant
                   :outputs (list ir-value))
                 ir-value))
             (wrapper-outputs (wrapper)
               (etypecase wrapper
                 (ir-value (list wrapper))
                 (ir-node (ir-node-outputs wrapper))))
             (wrap-function (fnrecord wrappers mandatory optional rest)
               (let* ((inputs (loop for wrapper in wrappers
                                    for outputs = (wrapper-outputs wrapper)
                                    do (assert (consp outputs))
                                    collect (first outputs)))
                      (n-mandatory (length mandatory))
                      (n-optional (length optional))
                      (n-provided (if rest (1- multiple-values-limit) (+ n-mandatory n-optional)))
                      (n-outputs (min n-provided max-typed-outputs))
                      (ntypes (make-array n-outputs))
                      (index 0))
                 (loop for ntype in mandatory while (< index n-outputs) do
                   (setf (svref ntypes index)
                         ntype)
                   (incf index))
                 (loop for ntype in optional while (< index n-outputs) do
                   (setf (svref ntypes index)
                         (typo:ntype-union ntype (typo:type-specifier-ntype 'null)))
                   (incf index))
                 (loop while (< index n-outputs) do
                   (setf (svref ntypes index)
                         (the typo:ntype rest)) ; TODO
                   (incf index))
                 (make-instance 'ir-call
                   :fnrecord fnrecord
                   :inputs inputs
                   :outputs
                   (if (eql outputs '*)
                       '*
                        (loop for ntype across ntypes
                              collect
                              (make-instance 'ir-value
                                :derived-ntype ntype)))))))
      (let* ((wrapper
               (typo:specialize
                (ir-call-fnrecord ir-call)
                (mapcar #'find-specialized-value (ir-node-inputs ir-call))
                :wrap-constant #'wrap-constant
                :wrap-function #'wrap-function
                :wrapper-nth-value-ntype #'wrapper-nth-value-ntype))
             (outputs (wrapper-outputs wrapper)))
        (loop for ir-value in (ir-node-outputs ir-call)
              for output in outputs do
                (setf (gethash ir-value *ir-value-copies*)
                      output))))))

(defmethod copy-ir-node
    ((context (eql 'ir-specialize))
     (ir-if ir-if))
  (let* ((node (make-instance 'ir-node))
         (then (copy-ir-block context (ir-if-then ir-if) node))
         (else (copy-ir-block context (ir-if-else ir-if) node))
         (node-outputs (ir-node-outputs ir-if))
         (then-outputs (ir-node-outputs (ir-node-predecessor (ir-final-node then))))
         (else-outputs (ir-node-outputs (ir-node-predecessor (ir-final-node then)))))
    (change-class node 'ir-if
      :then then
      :else else
      :inputs (list (find-specialized-value (first (ir-node-inputs ir-if))))
      :outputs
      (if (eql node-outputs '*)
          '*
           (loop for output in node-outputs
                 collect
                 (copy-ir-value
                  nil
                  output
                  (typo:ntype-union
                   (if (null then-outputs)
                       (typo:universal-ntype)
                       (ir-value-derived-ntype (pop then-outputs)))
                   (if (null else-outputs)
                       (typo:universal-ntype)
                       (ir-value-derived-ntype (pop else-outputs))))))))))
