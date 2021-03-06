(in-package :loopus.ir)

;; Takes a cl-isl expression (for instance "1+2"), and create loopus nodes for it
(defgeneric execute-expr (expr))

;; Simple integer
(defmethod execute-expr ((expr isl:int-expr))
  (let* ((v (isl:int-expr-get-value expr))
         (v (isl:value-object v)))
    (let* ((construct (make-instance 'ir-node))
           (answer (make-instance 'ir-value
                                  :declared-type `(eql ,v)
                                  :derived-ntype (typo:ntype-of v)))
           (*blocks* (last *blocks*)))
      (change-class construct 'ir-construct
                    :form `',v
                    :outputs (list answer))
      answer)))

;; Variable
(defmethod execute-expr ((expr isl:id-expr))
  ;; Simple loop variable
  (if (position (isl:id-expr-get-id expr) possible-loop-variables)
      (create-loop-var expr)
      ;; Otherwise it's a constant value
      (let* ((name (symbol-name (isl:identifier-name (isl:id-expr-get-id expr))))
             ;; If it's a free variable we modify the value we use, otherwise it'll be name
             (answer (gethash (gethash name *free-variable-to-index*) *position-to-loopusvariable*))
             (value (if answer answer name)))
        (alexandria:ensure-gethash ; is it actually usefull?
         value *id-to-nodes*
         (let* ((construct (make-instance 'ir-node))
                (answer (make-instance 'ir-value
                                       ;;:declared-type v;;v ;;??
                                       ;;:derived-ntype nil;;v ;;??
                                       ))
                (*blocks* (last *blocks*)))
           (change-class construct 'ir-construct
                         :form value
                         :outputs (list answer))
           answer)))))

;; Function call
(defmethod execute-expr ((expr isl:ast-expr))
  (let* ((answer (make-instance 'ir-value)))
    (make-instance 'ir-call
                   :fnrecord (make-instance 'typo:fnrecord :name (isl:op-expr-get-operator expr) :function #'+) ;;todo place the real function here instead of +
                   :inputs (mapcar #'execute-expr (isl:op-expr-get-list-args expr))
                   :outputs (list answer))
    answer))
