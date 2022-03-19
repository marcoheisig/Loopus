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
         (n-outputs (if (eql outputs '*) '* (length (ir-node-outputs ir-call))))
         (max-typed-outputs (if (integerp n-outputs) n-outputs (1- multiple-values-limit))))
    ;; Wrappers can be either IR nodes or IR values.
    (labels ((make-value (ntype)
               (make-instance 'ir-value
                 :derived-ntype ntype))
             (wrapper-nth-value-ntype (n wrapper)
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
               (let ((ir-value (make-value (typo:ntype-of constant))))
                 (make-instance 'ir-construct
                   :form `',constant
                   :outputs (list ir-value))
                 ir-value))
             (wrapper-outputs (wrapper expected-values)
               (if (eql expected-values '*)
                   '()
                   (let ((outputs
                           (etypecase wrapper
                             (ir-value (list wrapper))
                             (ir-node (ir-node-outputs wrapper)))))
                     (if (<= expected-values (length outputs))
                         (subseq outputs 0 expected-values)
                         (let ((default (make-value (typo:ntype-of nil))))
                           (make-instance 'ir-construct
                             :form 'nil
                             :outputs (list default))
                           (replace (make-list expected-values :initial-element default)
                                    outputs))))))
             (wrap-function (fnrecord wrappers mandatory optional rest)
               (make-instance 'ir-call
                 :fnrecord fnrecord
                 :inputs
                 (loop for wrapper in wrappers
                       collect (first (wrapper-outputs wrapper 1)))
                 :outputs
                 (let ((index 0))
                   (flet ()
                     (append
                      (loop for ntype in mandatory
                            do (incf index)
                            collect (make-value ntype))
                      (loop for ntype in optional
                            do (incf index)
                            collect (make-value (typo:ntype-union ntype (typo:type-specifier-ntype 'null))))
                      (unless (eql outputs '*)
                        (loop while (< index max-typed-outputs)
                              for ntype = (typo:ntype-union rest (typo:type-specifier-ntype 'null))
                              do (incf index)
                              collect (make-value ntype)))))))))
      (let ((wrapper
              (typo:specialize
               (ir-call-fnrecord ir-call)
               (mapcar #'find-specialized-value (ir-node-inputs ir-call))
               :wrap-constant #'wrap-constant
               :wrap-function #'wrap-function
               :wrapper-nth-value-ntype #'wrapper-nth-value-ntype)))
        (replace-node-outputs ir-call (wrapper-outputs wrapper n-outputs))))))

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
