(in-package :loopus.ir)

;; Takes a cl-isl node (for instance a loop node), and create a loopus nodes for it
(defgeneric execute-node (node))

;; Loop block
(defmethod execute-node ((node isl:for-node))
  (let* ((old-hashtable (alexandria:copy-hash-table *ir-value-copies*)) ; lexical scope
         (variable-isl (isl:for-node-get-iterator node))
         (possible-loop-variables (append possible-loop-variables (list (isl:id-expr-get-id variable-isl))))
         (start-value (isl:for-node-get-init node))
         (test-ast (isl:for-node-get-cond node))
         (increment (isl:for-node-get-inc node))
         (body-ast (isl:for-node-get-body node))
         ;; Generation of the nodes
         (start (execute-expr start-value))
         (step (execute-expr increment))
         (variable (create-loop-var variable-isl))
         (loop (make-instance 'ir-node))
         (body (make-ir-initial-and-ir-final-node loop))
         (test (make-ir-initial-and-ir-final-node loop)))
    (let ((*blocks* (cons (ir-final-node test) *blocks*)))
      (execute-expr test-ast))
    (let ((*depth-loop-variables* (cons variable *depth-loop-variables*))
          (*current-depth* (1+ *current-depth*))
          (*blocks* (cons (ir-final-node body) *blocks*)))
      (setf (gethash node *ir-value-copies*) variable)
      (execute-node body-ast))
    (change-class loop 'ir-loop
                  :variable variable
                  :inputs (list start step)
                  :test test
                  :body body)
    ;; Restore state before we leave the loop
    (setf *ir-value-copies* old-hashtable)
    (delete-loop-var variable-isl)
    loop))

;; A single statement
(defmethod execute-node ((node isl:user-node))
  (let* ((node (isl:user-node-get-expr node))
         (how-many-args (isl:op-expr-get-n-arg node))
         ;; We go 2 go because we have a counter between each loop variable in the domain
         (args (loop for i from 0 below how-many-args by 2 collect (isl:op-expr-get-op-arg node i)))
         (counter-value (isl:value-object
                         (isl:int-expr-get-value
                          (isl:op-expr-get-op-arg node (1- how-many-args)))))
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
                     r)))
    (make-instance 'ir-call
                  :fnrecord (ir-call-fnrecord old-node)
                  :inputs old-code
                  :outputs nil
                  ;; Nil because we split each expression into subexpressions ?
                  ;; doen't work with multiple return value ?
                  ;; todo
                  )))

;; A block is a sequence of instructions
;; Todo lexical scope ?
(defmethod execute-node ((node isl:block-node))
  (mapcar #'execute-node
          (isl:ast-node-list-elements (isl:block-node-getlist node))))

;; End of execute-node
;; Now some utilities

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

