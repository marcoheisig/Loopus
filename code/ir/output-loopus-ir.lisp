(ql:quickload :cl-isl)

(in-package :loopus.ir)

(defparameter node nil)
(defparameter *ir-value-copies* nil)

(defparameter possible-loop-variables nil)

(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(defparameter *values* nil)
(defparameter *depth-loop-variables* nil) ;; map of int to loop variable

(defparameter *current-depth* nil)

(defparameter *position-to-loopusvariable* nil)


(defun ins2 (&rest rest)
  (break "Inspect ~a" rest))
(defun ins (e)
  (break "Inspect ~a" e))

;; Takes a cl-isl node (for instance a loop node), and create a loopus nodes for it
;; Will call the function below
(defgeneric execute-node (node))

;; Takes a cl-isl expression (for instance "1+2"), and create loopus nodes for it
;; Won't call the function above
(defgeneric execute-expr (expr))

;; Entry point of the program. Takes a cl-isl ast, call execute-node on it, and returns a loopus ast
;; Sequence of instructions are "block" in the cl-isl ast, and execute-node on a block calls recursively
;; itself on every statement of the block. Hence a single call to execute-node in this function
(defun my-main (node dominator)
  (unless dominator (setf *values* '()))
  (unless dominator (setf *depth-loop-variables* '()))
  (multiple-value-bind (ir-initial-node ir-final-node)
      (make-ir-initial-and-ir-final-node dominator)
    (print node)
    (let ((*blocks* (cons ir-final-node *blocks*)))
      (execute-node node))
    ir-initial-node))

;; todo refactor this
(defun is-member (v l)
  (if l
      (if (string= v (car (first l)))
          (cdr (first l))
          (is-member v (cdr l)))
      nil))

(defun create-loop-var (v)
  (let ((v (if (symbolp v) v (isl::identifier-name (isl::id-expr-get-id v)))))
    (if (is-member v *values*)
        (is-member v *values*)
        (let ((answer (make-instance 'ir-value)))
          (setf *values* (cons (cons v answer) *values*))
          answer))))
(defun delete-loop-var (v)
  (setf *values* (cdr *values*)))
;;;;;;;;;;;;;;;
;; Execute-expr
;;;;;;;;;;;;;;;

;;todo one file per thing?


;; Creation of integer
(defmethod execute-expr ((expr isl::int-expr))
  (let* ((v (isl:int-expr-get-value expr))
         (v (isl:value-object v)))
    (let* ((construct (make-instance 'ir-node))
           (answer (make-instance 'ir-value
                                  :declared-type `(eql ,v)
                                  )))
      (change-class construct 'ir-construct
                    :form `',v
                    :outputs (list answer))
      answer)))

;; Creation of a variable
(defmethod execute-expr ((expr isl:id-expr))
  (let* ((v (isl:id-expr-get-id expr))
         (v (isl:identifier-name v)))
    ;; Simple loop variable
    (if (position (isl:id-expr-get-id expr) possible-loop-variables)
        (create-loop-var v)
        ;; If it's a free variable we modify the value we use, otherwise it'll be v
        (let* ((answer (gethash
                        (gethash (symbol-name v) *free-variable-to-index*)
                        *position-to-loopusvariable*))
               (value (if answer answer v)))
          ;; Otherwise it's a constant value
          (if nil ;;(is-member v *values*) - todo
              (progn
                (is-member v *values*)
                )
              (let* ((construct (make-instance 'ir-node))
                     (answer (make-instance 'ir-value
                                            ;;:declared-type v;;v ;;??
                                            ;;:derived-ntype nil;;v ;;??
                                            )))
                (change-class construct 'ir-construct
                              :form value
                              :outputs (list answer))
                (setf *values* (cons (cons v answer) *values*))
                answer))))))

;; Todo the type?
;; For a function call generate the appropriate ir-call
(defmethod execute-expr ((expr isl::ast-expr))
  (let* ((answer (make-instance 'ir-value)))
    (make-instance 'ir-call
                   :fnrecord (make-instance 'typo:fnrecord :name (isl:op-expr-get-operator expr) :function #'+) ;;todo
                   :inputs  (mapcar #'execute-expr (isl:op-expr-get-list-args expr))
                   :outputs (list answer))
    answer))


;;;;;;;;;;;;;;;
;; Execute-node
;;;;;;;;;;;;;;;

(defun plus-expr (expr1 expr2)
  (let* ((answer (make-instance 'ir-value))
         (_ (make-instance 'ir-call
                        :fnrecord (make-instance 'typo:fnrecord :name '+ :function #'+)
                        :inputs (list expr1 expr2)
                        :outputs (list answer))))
    ;;(setf *values* (cons (cons v answer) *values*))
    answer))

(defmethod execute-node ((node isl::for-node))
  (let* ((variable (isl::for-node-get-iterator node))
         (possible-loop-variables (append possible-loop-variables (list (isl:id-expr-get-id variable))))
         (start-value (isl::for-node-get-init node))
         (end-condition (isl::for-node-get-cond node))
         ;;(_ (assert (cl-isl::%isl-ast-expr-is-equal variable (cl-isl::%isl-ast-expr-get-op-arg end-condition 0))))
         ;; todo assert
         (increment (isl::for-node-get-inc node))
         (end-value (isl::op-expr-get-op-arg end-condition 1))
         (body (isl::for-node-get-body node)))
    ;; Generation of the nodes
    (let* ((variable (create-loop-var variable))
           (start (execute-expr start-value))
           (end-minus-step (execute-expr end-value))
           (step (execute-expr increment))
           (end (plus-expr end-minus-step step))
           ;; We need to add step to end, because loopus generate code with "< end" constraint, and cl-isl has "<= end"
           (loop-node (make-instance 'ir-node)))
      (change-class loop-node 'ir-loop
                    :variable variable
                    :inputs (list start end step)
                    :direction (if (eql (type-of increment) 'isl::int-expr)
                                   (let ((value (isl::value-object (isl::int-expr-get-value increment))))
                                     (if (> value 0) :ascending
                                         (if (< value 0) :descending
                                             :unknown)))
                                   :unknown)
                    :body (make-instance 'ir-initial-node :dominator loop-node))
      (setf (slot-value variable '%producer) loop-node)
      (let ((*depth-loop-variables* (cons variable *depth-loop-variables*))
            (_ (setf (gethash node *ir-value-copies*) variable))
            (*current-depth* (1+ *current-depth*)))
        ;;todo
        (setf (slot-value loop-node '%body) (my-main body loop-node))
        (delete-loop-var variable)
        loop-node))))


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

#+or(defmethod copy-ir-value (context (ir-value ir-value) &optional (ntype (typo:universal-ntype)))
  ;;(copy-ir-node context (ir-value-producer ir-value))
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
    res))

(defmethod execute-node ((node isl::user-node))
  (let* ((node (isl::user-node-get-expr node))
         (how-many-args (isl::op-expr-get-n-arg node))
         ;; Todo resolve this thing
         (how-many-args (* 2 (1+ *current-depth*)))
         (args (loop for i from 0 below how-many-args by 2 collect (isl::op-expr-get-op-arg node i)))
         (counter-value (isl::value-object
                         (isl::int-expr-get-value
                          (isl::op-expr-get-op-arg node 1))))
         (old-node (gethash counter-value *id-to-expression*))
         (old-code (ir-node-inputs old-node))
         (idx (mapcar (lambda (c)
                        (position (isl::id-expr-get-id c)
                         possible-loop-variables))
                      (cdr args)))
         ;;(_ (ins2 old-code *depth-loop-variables*))
         ;;(_ (ins idx))
         (old-code (let* ((cp *depth-loop-variables*)
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

(defmethod execute-node ((node isl::block-node))
  (mapcar #'execute-node
          (isl::ast-node-list-elements (isl::block-node-getlist node))))
