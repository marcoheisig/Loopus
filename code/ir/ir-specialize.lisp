(in-package #:loopus.ir)

(defun ir-specialize (ir)
  "Returns a copy of IR in which the derived type of each value is refined
  based on the knowledge about its producer, and where each function call
  has been replaced by the most specific function that has identical
  semantics but for the (possibly refined) type of its inputs."
  (let ((*ir-value-copies* (make-hash-table :test #'eq)))
    (ir-specialize-block ir nil)))

(defgeneric ir-specialize-block (ir-node dominator))

(defgeneric ir-specialize-node (ir-node))

(defun find-specialized-value (ir-value)
  (multiple-value-bind (copy present-p)
      (gethash ir-value *ir-value-copies*)
    (if (not present-p)
        (error "Reference to IR value that hasn't been specialized upon yet: ~S"
               ir-value)
        copy)))

(defmethod ir-specialize-block (ir-node dominator)
  (multiple-value-bind (initial-node final-node)
      (make-ir-initial-and-ir-final-node dominator)
    (let ((*predecessor* initial-node)
          (*successor* final-node))
      (map-block-inner-nodes #'ir-specialize-node ir-node))
    initial-node))

(defmethod ir-specialize-node ((ir-loop ir-loop))
  (destructuring-bind (start end step)
      (ir-node-inputs ir-loop)
    (let* ((start (find-specialized-value start))
           (step (find-specialized-value step))
           (end (find-specialized-value end))
           (variable (copy-ir-value nil (ir-loop-variable ir-loop)))
           (node (make-instance 'ir-node))
           (body (ir-specialize-block (ir-loop-body ir-loop) node)))
      (change-class node 'ir-loop
        :variable variable
        :inputs (list start end step)
        :body body))))

(defmethod ir-specialize-node ((ir-call ir-call))
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

(defmethod ir-specialize-node ((ir-if ir-if))
  (let* ((node (make-instance 'ir-node))
         (then (ir-specialize-block (ir-if-then ir-if) node))
         (else (ir-specialize-block (ir-if-else ir-if) node))
         (then-outputs (ir-node-outputs (ir-node-predecessor (ir-final-node then))))
         (else-outputs (ir-node-outputs (ir-node-predecessor (ir-final-node then)))))
    (change-class node 'ir-if
      :then then
      :else else
      :inputs (list (find-specialized-value (first (ir-node-inputs ir-if))))
      :outputs
      (loop for output in (ir-node-outputs ir-if)
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
                  (ir-value-derived-ntype (pop else-outputs)))))))))

(defmethod ir-specialize-node ((ir-construct ir-construct))
  (make-instance 'ir-construct
    :form (ir-construct-form ir-construct)
    :outputs
    (loop for output in (ir-node-outputs ir-construct)
          collect
          (copy-ir-value nil output))))

(defmethod ir-specialize-node ((ir-enclose ir-enclose))
  (let ((node (make-instance 'ir-node)))
    (change-class node 'ir-enclose
      :argument-values (mapcar (alexandria:curry #'copy-ir-value nil)
                               (ir-enclose-argument-values ir-enclose))
      :body (ir-specialize-block (ir-enclose-body ir-enclose) node))))
