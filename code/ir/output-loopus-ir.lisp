(ql:quickload :cl-isl)

(in-package :loopus.ir)

(defparameter node nil)
(defparameter *ir-value-copies* nil) ;;(make-hash-table))

(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(defparameter *values* nil)
(defparameter *depth-loop-variables* nil) ;; map of int to loop variable

(defparameter *current-depth* nil)

(defun ins2 (&rest rest)
  (break "Inspect ~a" rest))
(defun ins (e)
  (break "Inspect ~a" e))


(defun my-main (node dominator)
  (unless dominator (setf *values* '()))
  (unless dominator (setf *depth-loop-variables* '()))
  (multiple-value-bind (ir-initial-node ir-final-node)
      (make-ir-initial-and-ir-final-node dominator)
    (print node)
    (let ((*blocks* (cons ir-final-node *blocks*)))
      (execute-node node))
    ir-initial-node))

(defgeneric execute-node (node))
(defgeneric execute-expr (expr))


(defun is-member (v l)
  (if l
      (if (string= v (car (first l)))
          (cdr (first l))
          (is-member v (cdr l)))
      nil))

(defmethod execute-expr ((expr isl::id-expr))
  (let* ((v (isl::id-expr-get-id expr))
         (v (isl::identifier-name v)))
    (format t "Creation of a value ~a" v)
    (if nil ;;(is-member v *values*)
        (progn
          (is-member v *values*)
          )
        (let ((answer (make-instance 'ir-value
                                     ;;:declared-type v;;v ;;??
                                     ;;:derived-ntype nil;;v ;;??
                                     )))
          (setf *values* (cons (cons v answer) *values*))
          answer))))

(defmethod execute-expr ((expr isl::int-expr))
  (let* ((v (isl::int-expr-get-value expr))
         (v (isl::value-object v)))
    (format t "Creation of an integer value ~a" v)
    (let* ((construct (make-instance 'ir-node))
           (answer (make-instance 'ir-value
                                  :declared-type `(eql ,v)
                                  )))
      (change-class construct 'ir-construct
                    :form `',v
                    :outputs (list answer))
      answer)))

(defmethod execute-node ((node isl::for-node))
  (let* ((variable (isl::for-node-get-iterator node))
         (start-value (isl::for-node-get-init node))
         (end-condition (isl::for-node-get-cond node))
         ;;(_ (assert (cl-isl::%isl-ast-expr-is-equal variable (cl-isl::%isl-ast-expr-get-op-arg end-condition 0))))
         ;; todo assert
         (end-value (isl::op-expr-get-op-arg end-condition 1))
         (increment (isl::for-node-get-inc node))
         (body (isl::for-node-get-body node)))
    ;; Generation of the nodes
    (let* ((variable (execute-expr variable))
           (start (execute-expr start-value))
           (end (execute-expr end-value))
           (step (execute-expr increment))
           (loop-node (make-instance 'ir-node)))
      (change-class loop-node 'ir-loop
                    :variable variable
                    :inputs (list start end step)
                    :body (make-instance 'ir-node))
      (setf (slot-value variable '%producer) loop-node)
      (push variable *depth-loop-variables*)
      (setf (gethash node *ir-value-copies*) variable)
      (incf *current-depth*)
      ;;todo
      (setf (slot-value loop-node '%body) (my-main body loop-node))
      loop-node)))


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
         (how-many-args (+ 2 *current-depth*))
         (args (loop for i from 1 below how-many-args collect (isl::op-expr-get-op-arg node i)))
         (counter-value (isl::value-object
                         (isl::int-expr-get-value
                          (isl::op-expr-get-op-arg node 1))))
         (old-node (gethash counter-value *id-to-expression*))
         (old-code (ir-node-inputs old-node))
         (idx (mapcar (lambda (c)
                        (position
                         (isl::identifier-name
                          (isl::id-expr-get-id c))
                         (mapcar #'read-from-string
                                 (list "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"))
                        ))
                      (cdr args)))
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
