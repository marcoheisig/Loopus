(in-package #:loopus.ir)

(defvar *ir-node-liveness*)

(defgeneric ir-node-alive-p (ir-node))

(defun ir-remove-dead-code (ir)
  "Returns a copy of IR in which all calls to pure functions whose outputs
  are never used have been removed."
  (let ((*ir-node-liveness* (make-hash-table :test #'eq))
        (*ir-value-copies* (make-hash-table :test #'eq)))
    (copy-ir-block 'ir-remove-dead-code ir nil)))

(defmethod copy-ir-node :around
    ((context (eql 'ir-remove-dead-code))
     (ir-node ir-node))
  (when (ir-node-alive-p ir-node)
    (call-next-method)))

(defmethod ir-node-alive-p :around ((ir-node ir-node))
  (values
   (alexandria:ensure-gethash
    ir-node
    *ir-node-liveness*
    (call-next-method))))

(defmethod ir-node-alive-p ((ir-initial-node ir-initial-node))
  ;; An initial node is alive if at least one inner node in its block is
  ;; alive.
  (block nil
    (map-block-inner-nodes
     (lambda (node)
       (when (ir-node-alive-p node)
         (return t)))
     ir-initial-node)
    (return nil)))

(defmethod ir-node-alive-p ((ir-node-with-outputs ir-node-with-outputs))
  ;; A node with outputs is alive if any of its outputs has a user that is
  ;; alive.
  (let ((outputs (ir-node-outputs ir-node-with-outputs)))
    (or (eql outputs '*)
        (loop for output in outputs
                thereis
                (loop for user in (ir-value-users output)
                        thereis (ir-node-alive-p user))))))

(defmethod ir-node-alive-p ((ir-loop ir-loop))
  ;; A loop is a live if its body is alive.
  (ir-node-alive-p (ir-loop-body ir-loop)))

(defmethod ir-node-alive-p ((ir-call ir-call))
  ;; A call is alive it is not pure.
  (or (not (typo:fnrecord-purep (ir-call-fnrecord ir-call)))
      (call-next-method)))

(defmethod ir-node-alive-p ((ir-if ir-if))
  ;; An if node is alive if either of its branches is alive.
  (or (ir-node-alive-p (ir-if-then ir-if))
      (ir-node-alive-p (ir-if-else ir-if))))
