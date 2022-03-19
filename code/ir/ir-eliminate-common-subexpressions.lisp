(in-package #:loopus.ir)

;;; A hash table, mapping from IR nodes to their depth.
(defvar *ir-node-depth*)

;;; The depth on the IR block being processed right now.
(defvar *depth*)

(defgeneric ir-node-depth (ir-node))

(defgeneric ir-value-depth (ir-node))

(defun ir-eliminate-common-subexpressions (ir)
  "Returns a copy of IR in which all calls to the same, pure function with
  the same arguments have been replaced by a single call, and where that
  call has been moved to the outermost block it depends on."
  (let ((*ir-value-copies* (make-hash-table :test #'eq))
        (*ir-node-depth* (make-hash-table :test #'eq)))
    (copy-ir-block 'ir-eliminate-common-subexpressions ir nil)))

(defmethod copy-ir-block :around
    ((context (eql 'ir-eliminate-common-subexpressions))
     (ir-initial-node ir-initial-node)
     dominator)
  (let ((*depth* (ir-node-depth ir-initial-node)))
    (call-next-method)))

(defmethod copy-ir-node :around
    ((context (eql 'ir-eliminate-common-subexpressions))
     (ir-node ir-node))
  ;; Go to the outermost block that this call depends on.
  (let ((*blocks* (nthcdr (- *depth* (ir-node-depth ir-node)) *blocks*)))
    (call-next-method)))

(defmethod copy-ir-node
    ((context (eql 'ir-eliminate-common-subexpressions))
     (ir-call ir-call))
  (let ((fnrecord (ir-call-fnrecord ir-call))
        (inputs (ir-node-inputs ir-call))
        (outputs (ir-node-outputs ir-call)))
    (when (typo:fnrecord-purep fnrecord)
      ;; Check whether an existing call node can be reused.
      (map-block-inner-nodes
       (lambda (node)
         (when (and (ir-call-p node)
                    (eq (ir-call-fnrecord node) fnrecord)
                    (if (eql outputs '*)
                        (eql (ir-node-outputs node) '*)
                        (= (length (ir-node-outputs node))
                           (length outputs)))
                    (= (length (ir-node-inputs node))
                       (length inputs))
                    (loop for input in inputs
                          for other-input in (ir-node-inputs node)
                          always (eq (copy-ir-value context input) other-input)))
           (replace-node-outputs ir-call (ir-node-outputs node))
           (return-from copy-ir-node node)))
       *initial-node*))
    (call-next-method)))

(defmethod copy-ir-node
    ((context (eql 'ir-eliminate-common-subexpressions))
     (ir-construct ir-construct))
  ;; Check whether there is an existing construct node that can be reused.
  (map-block-inner-nodes
   (lambda (node)
     (when (and (ir-construct-p node)
                (equal (ir-construct-form node)
                       (ir-construct-form ir-construct)))
       (replace-node-outputs ir-construct (ir-node-outputs node))
       (return-from copy-ir-node node)))
   *initial-node*)
  (call-next-method))

(defmethod ir-value-depth ((ir-value ir-value))
  (let ((producer (ir-value-producer ir-value)))
    (if (ir-loop-p producer)
        (1+ (ir-node-depth producer))
        (ir-node-depth producer))))

(defmethod ir-node-depth ((ir-initial-node ir-initial-node))
  (do ((dominator (ir-node-dominator ir-initial-node) (ir-node-dominator dominator))
       (depth 0 (1+ depth)))
      ((null dominator) depth)))

(defmethod ir-node-depth ((ir-node ir-node))
  (ir-node-depth (ir-initial-node ir-node)))

(defmethod ir-node-depth ((ir-call ir-call))
  (if (typo:fnrecord-purep (ir-call-fnrecord ir-call))
      (reduce #'max (ir-node-inputs ir-call)
              :key #'ir-value-depth
              :initial-value 0)
      (call-next-method)))

(defmethod ir-node-depth ((ir-construct ir-construct))
  0)
