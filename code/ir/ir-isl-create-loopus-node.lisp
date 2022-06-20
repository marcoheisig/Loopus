(in-package :loopus.ir)

;; Takes a cl-isl node (for instance a loop node), and create a loopus nodes for it
(defgeneric execute-node (node))

;; Loop block
(defmethod execute-node ((node isl:for-node))
  (let* ((old-hashtable (alexandria:copy-hash-table *ir-value-copies*)) ; lexical scope
         (variable-isl (isl:for-node-get-iterator node))
         (possible-loop-variables (append possible-loop-variables (list (isl:id-expr-get-id variable-isl))))
         (start-value (isl:for-node-get-init node))
         (end-condition (isl:for-node-get-cond node))
         (increment (isl:for-node-get-inc node))
         (body (isl:for-node-get-body node))
         ;; Generation of the nodes
         (start (execute-expr start-value))
         (end (create-end-value end-condition variable-isl))
         (step (execute-expr increment))
         (variable (create-loop-var variable-isl))
         ;;(_ (ins2 variable variable-isl *id-to-nodes* possible-loop-variables))
         (loop-node (make-instance 'ir-node)))
    (change-class loop-node 'ir-loop
                  :variable variable
                  :inputs (list start end step)
                  :direction (if (eql (type-of increment) 'isl:int-expr)
                                 (let ((value (isl:value-object (isl:int-expr-get-value increment))))
                                   (if (> value 0) :ascending
                                       (if (< value 0) :descending
                                           :unknown)))
                                 :unknown)
                  :body (make-instance 'ir-initial-node :dominator loop-node))
    (setf (slot-value variable '%producer) loop-node)
    (let ((*depth-loop-variables* (cons variable *depth-loop-variables*))
          (_ (setf (gethash node *ir-value-copies*) variable))
          (*current-depth* (1+ *current-depth*)))
      ;; Recursion
      (setf (slot-value loop-node '%body) (my-main body loop-node))
      ;; Restore state before we leave the loop
      (setf *ir-value-copies* old-hashtable)
      (delete-loop-var variable-isl)
      loop-node)))

;; A single statement
(defmethod execute-node ((node isl:user-node))
  (let* ((node (isl:user-node-get-expr node))
         (how-many-args (isl:op-expr-get-n-arg node))
         ;; We go 2 go because we have a counter between each loop variable in the domain
         (args (loop for i from 0 below how-many-args by 2 collect (isl:op-expr-get-op-arg node i)))
         (counter-value (isl:value-object
                         (isl:int-expr-get-value
                          (isl:op-expr-get-op-arg node 1))))
         (old-node (gethash counter-value *id-to-expression*))
         (old-code (ir-node-inputs old-node))
         (idx (mapcar (lambda (c)
                        (position (isl:id-expr-get-id c)
                         possible-loop-variables))
                      (cdr args)))
         (old-code (let* ((cp *depth-loop-variables*)
                          ;; todo refactor this
                          (_ (setf *depth-loop-variables*
                                   (reverse
                                    (loop for e in idx collect (nth e (reverse *depth-loop-variables*))))))
                          (r (loop for e in old-code collect (copy-ir-node 'output e)))
                          (_ (setf *depth-loop-variables* cp)))
                     r))
         )
    (let ((c (make-instance 'ir-call
                            :fnrecord (ir-call-fnrecord old-node)
                            :inputs old-code
                            :outputs nil;;(ir-node-outputs old-node)
                            )))
      c)))

;; A block is a sequence of instructions
(defmethod execute-node ((node isl:block-node))
  (mapcar #'execute-node
          (isl:ast-node-list-elements (isl:block-node-getlist node))))

;; End of execute-node
;; Now some utilities

;; To create the end value of a loop. Depending on if it's "<" or "<=" we may need to add 1 to the end (because loopus is "<" only)
;; Also do some assertion because we don't support every loop possible
(defun create-end-value (end-condition variable)
  (let* ((_ (assert (= 2 (isl:op-expr-get-n-arg end-condition))))
         (a (isl:op-expr-get-op-arg end-condition 0))
         (b (isl:op-expr-get-op-arg end-condition 1))
         (_ (assert (isl:ast-expr-equal-p a variable))))
    (cond
      ((isl:op-le-p end-condition) (execute-expr (isl:create-ast-expr-from-add
                                                   (isl:create-ast-expr-from-val (isl:value 1)) b)))
      ((isl:op-lt-p end-condition) (execute-expr b))
      ;; Not sure what to do
      ;; isl:op-ge-p
      ;; isl:op-gt-p
      (t (break "~a not supported yet for end conditon of loop" end-condition)))))


;; To copy loopus things
;; Todo rewrite them
(defmethod copy-ir-node ((context (eql 'output)) (ir ir-value))
  (copy-ir-value context ir))

(defmethod copy-ir-value ((context (eql 'output)) (ir-value ir-value) &optional (ntype (typo:universal-ntype)))
  (let ((d (gethash (ir-value-producer ir-value) *depth-node*)))
    (if d
        (nth d (reverse *depth-loop-variables*))
        ;;(copy-ir-value nil ir-value)
        (let ((found (gethash ir-value *ir-value-copies*))
              (res
                (let* ((declared-type (ir-value-declared-type ir-value))
                       (declared-ntype (typo:type-specifier-ntype declared-type))
                       (derived-ntype (ir-value-derived-ntype ir-value)))
                  (values
                   (alexandria:ensure-gethash
                    ir-value
                    *ir-value-copies*
                    (make-instance 'ir-value
                                   :declared-type declared-type
                                   :derived-ntype
                                   (typo:ntype-intersection
                                    declared-ntype
                                    (typo:ntype-intersection derived-ntype ntype))))))))
          (when (and (not found) (ir-construct-p (ir-value-producer ir-value)))
            (make-instance 'ir-construct
                           :form (ir-construct-form (ir-value-producer ir-value))
                           :outputs (list res)))
          (when (and (not found) (ir-call-p (ir-value-producer ir-value)))
            (copy-ir-node context (ir-value-producer ir-value)))
          res)
        ;;(call-next-method)
        )))

