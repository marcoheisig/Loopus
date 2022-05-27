(in-package #:loopus.ir)

;;; The Loopus IR, consists of a sequence of nodes that are connected via
;;; their successor and predecessor slot.  Each such chain of nodes starts
;;; with an initial node and ends with a final node.  When control is
;;; transferred to a node, it reads its (possibly empty) list of input
;;; values, computes its output values and transfers control to its
;;; successor.  The node that precedes the final node returns its outputs
;;; as multiple values.  Some nodes, such as IR-IF, IR-ENCLOSE, and
;;; IR-LOOP, have a reference to one or more other initial nodes that are
;;; processed specially.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special Variables

;; A list of all surrounding final nodes, sorted by dominance, starting
;; with the innermost surrounding block.
(defvar *blocks* '())

(define-symbol-macro *final-node* (first *blocks*))

(define-symbol-macro *initial-node* (ir-initial-node *final-node*))

;;; A hash table, mapping from IR values to their copy.
(defvar *ir-value-copies*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric ir-node-p (object) (:method ((object t)) nil))

(defgeneric ir-node-dominator (ir-node))

(defgeneric ir-node-inputs (ir-node))

(defgeneric ir-node-outputs (ir-node))

(defgeneric ir-node-successor (ir-node))

(defgeneric ir-node-predecessor (ir-node))

(defgeneric ir-node-values-type (ir-node))


(defgeneric ir-initial-node (ir-node))

(defgeneric ir-initial-node-p (object) (:method ((object t)) nil))

(defgeneric ir-final-node (ir-node))

(defgeneric ir-final-node-p (object) (:method ((object t)) nil))

(defgeneric make-ir-initial-and-ir-final-node (dominator))


(defgeneric ir-loop-p (object) (:method ((object t)) nil))

(defgeneric ir-loop-body (ir-loop))

(defgeneric ir-loop-variable (ir-loop))


(defgeneric ir-call-p (object) (:method ((object t)) nil))

(defgeneric ir-call-fnrecord (ir-call))


(defgeneric ir-if-p (object) (:method ((object t)) nil))

(defgeneric ir-if-then (ir-if))

(defgeneric ir-if-else (ir-if))


(defgeneric ir-construct-p (object) (:method ((object t)) nil))

(defgeneric ir-construct-form (ir-construct))


(defgeneric ir-enclose-p (object) (:method ((object t)) nil))

(defgeneric ir-enclose-argument-values (ir-enclose))

(defgeneric ir-enclose-body (ir-enclose))


(defgeneric ir-value-p (object) (:method ((object t)) nil))

(defgeneric ir-value-producer (ir-value))

(defgeneric ir-value-users (ir-value))

(defgeneric ir-value-declared-type (ir-value))

(defgeneric ir-value-derived-ntype (ir-value))

(defgeneric ir-value-declare-type (ir-value type-specifier))


(defgeneric map-block-inner-nodes (function ir-node))

(defgeneric insert-ir-node-before (ir-node future-successor))

(defgeneric insert-ir-node-after (ir-node future-predecessor))

(defgeneric extract-ir-node (ir-node))

(defgeneric copy-ir-value (context ir-value &optional ntype))

(defgeneric copy-ir-node (context ir-node))

(defgeneric copy-ir-block (context ir-node dominator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-node ()
  ())

(defclass ir-value ()
  ((%declared-type
    :initarg :declared-type
    :initform 't
    :reader ir-value-declared-type)
   (%derived-ntype
    :initarg :derived-ntype
    :initform (typo:universal-ntype)
    :type typo:ntype
    :reader ir-value-derived-ntype)
   (%producer
    :type ir-node
    :reader ir-value-producer)
   (%users
    :initform '()
    :reader ir-value-users)))

(defmethod print-object ((ir-value ir-value) stream)
  (print-unreadable-object (ir-value stream :type t)
    (let ((t1 (ir-value-declared-type ir-value))
          (t2 (typo:ntype-type-specifier (ir-value-derived-ntype ir-value))))
      (format stream "~S" (if (subtypep t2 t1) t2 t1)))))

(defun ensure-ir-value-producer (ir-value producer)
  (if (slot-boundp ir-value '%producer)
      (let ((other-producer (slot-value ir-value '%producer)))
        (unless (eq producer other-producer)
          (error "Attempt to redefine the producer of ~S from ~S to ~S."
                 ir-value other-producer producer)))
      (setf (slot-value ir-value '%producer) producer)))

(defun ensure-ir-value-user (ir-value user)
  (pushnew user (slot-value ir-value '%users)))

(defmethod ir-value-declare-type ((ir-value ir-value) new-type)
  (let ((old-type (ir-value-declared-type ir-value)))
    (cond ((subtypep old-type new-type)
           (values))
          ((subtypep new-type old-type)
           (setf (slot-value ir-value '%declared-type)
                 new-type))
          ((subtypep `(and ,new-type ,old-type) nil)
           (error "Incompatible declared types ~S and ~S."
                  new-type old-type))
          (t
           (setf (slot-value ir-value '%declared-type)
                 `(and ,new-type ,old-type))))
    ir-value))

(defclass ir-node-with-dominator (ir-node)
  (;; The IR node 'above' this one, i.e., the unique loop or if node that
   ;; has a reference to this node, or some predecessor of this node.
   (%dominator
    :initarg :dominator
    :initform (alexandria:required-argument :dominator)
    :type (or null ir-node)
    :reader ir-node-dominator)))

(defclass ir-node-with-inputs (ir-node)
  (;; The list of values that are used by this node.
   (%inputs
    :initarg :inputs
    :initform (alexandria:required-argument :inputs)
    :type list
    :reader ir-node-inputs)))

(defmethod shared-initialize :after
    ((ir-node-with-inputs ir-node-with-inputs) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (input (ir-node-inputs ir-node-with-inputs))
    (ensure-ir-value-user input ir-node-with-inputs)))

(defclass ir-node-with-outputs (ir-node)
  (;; The list of values that are produced by this node, or, if the node
   ;; occurs as the final node of a block in a context where the number of
   ;; expected values is not known, the symbol *.
   (%outputs
    :initarg :outputs
    :initform (alexandria:required-argument :outputs)
    :type (or list (eql *))
    :reader ir-node-outputs)))

(defmethod shared-initialize :after
    ((ir-node-with-outputs ir-node-with-outputs) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((outputs (ir-node-outputs ir-node-with-outputs)))
    (unless (eql outputs '*)
      (dolist (output outputs)
        (ensure-ir-value-producer output ir-node-with-outputs)))))

(defclass ir-node-with-successor (ir-node)
  (;; The IR node that this node transfers control to.
   (%successor
    :initarg :successor
    :initform *final-node*
    :type ir-node
    :reader ir-node-successor
    :writer (setf ir-node-%successor))))

(defclass ir-node-with-predecessor (ir-node)
  (;; The IR node that this node receives control form.
   (%predecessor
    :initarg :predecessor
    :initform (ir-node-predecessor *final-node*)
    :type ir-node
    :reader ir-node-predecessor
    :writer (setf ir-node-%predecessor))))

(defclass ir-initial-node (ir-node-with-dominator ir-node-with-successor)
  (;; Each initial node has a reference to its corresponding final node.
   (%final-node
    :initarg :final-node
    :type ir-final-node
    :reader ir-final-node)))

(defclass ir-final-node (ir-node-with-predecessor)
  (;; Each final node has a reference to its corresponding initial node.
   (%initial-node
    :initarg :initial-node
    :type ir-node
    :reader ir-initial-node)))

(defclass ir-inner-node (ir-node-with-predecessor ir-node-with-successor)
  ())

(defmethod shared-initialize :after
    ((inner-node ir-inner-node) slot-names &key &allow-other-keys)
  (insert-ir-node-before inner-node *final-node*))

;;; A loop node is an inner node with three inputs (start, end, and
;;; step), zero outputs, and a control flow node that marks the beginning
;;; of its body.  When control is transferred to the loop node, it
;;; repeatedly evaluates its body in an environment where the loop variable
;;; is bound to successive elements of the iteration space.
(defclass ir-loop (ir-inner-node ir-node-with-inputs)
  ((%variable
    :initarg :variable
    :initform (alexandria:required-argument :variable)
    :type ir-value
    :reader ir-loop-variable)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :type ir-initial-node
    :reader ir-loop-body)))

(defmethod shared-initialize :after
    ((ir-loop ir-loop) slot-names &key &allow-other-keys)
  (ensure-ir-value-producer (ir-loop-variable ir-loop) ir-loop))

;;; A call node is defined by an fnrecord that denotes a function, and
;;; further inputs that serve as the arguments of that function.  It has
;;; some number of outputs (whose number need not fit to the number of
;;; values produced by the function).  When control is transferred to it,
;;; it binds each output to the corresponding value obtained by invoking
;;; the function on the arguments.  Outputs with no corresponding value are
;;; bound to NIL.
(defclass ir-call (ir-inner-node ir-node-with-inputs ir-node-with-outputs)
  ((%fnrecord
    :initarg :fnrecord
    :initform nil
    :type (or typo:fnrecord null)
    :reader ir-call-fnrecord)))

;;; An if node is an inner node with one input that is the generalized
;;; boolean to test, some number of outputs, a reference to the initial
;;; node of its then part, and a reference to the initial node of its else
;;; part.  When control is transferred to it, it transfers control to its
;;; then initial node if the input is true, and to its else initial node if
;;; the input is false.  Then it binds its outputs to the values produced
;;; by whatever chain of nodes was chosen.
(defclass ir-if (ir-inner-node ir-node-with-inputs ir-node-with-outputs)
  ((%then
    :initarg :then
    :initform (alexandria:required-argument :then)
    :type list
    :reader ir-if-then)
   (%else
    :initarg :else
    :initform (alexandria:required-argument :else)
    :type list
    :reader ir-if-else)))

;;; A construct node is an inner node with no inputs and some number of
;;; outputs.  When control is transferred to it, it binds each output to
;;; the corresponding value obtained by evaluating its form.
;;;
;;; Construct nodes are used to handle constants, or references to
;;; functions or variables from outside of the loop nest.
(defclass ir-construct (ir-inner-node ir-node-with-outputs)
  ((%form
    :initarg :form
    :initform (alexandria:required-argument :form)
    :reader ir-construct-form)))

;;; An enclose node is an inner node that has no inputs, and a single
;;; output that is the closure with the given arguments and body.
(defclass ir-enclose (ir-inner-node)
  ((%inputs :initform '() :type null)
   ;; A list of IR values that are bound on each invocation of that
   ;; function.
   (%argument-values
    :initarg :argument-values
    :initform (alexandria:required-argument :argument-values)
    :type list
    :reader ir-enclose-argument-values)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :reader ir-enclose-body)))

(defmethod shared-initialize :after
    ((ir-enclose ir-enclose) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (argument-value (ir-enclose-argument-values ir-enclose))
    (ensure-ir-value-producer argument-value ir-enclose)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod ir-node-p ((ir-node ir-node))
  t)

(defmethod ir-node-dominator ((ir-node-with-predecessor ir-node-with-predecessor))
  ;; Only the initial node has a direct reference to its dominator.  All
  ;; other nodes have to follow their chain of predecessors.
  (ir-node-dominator (ir-node-predecessor ir-node-with-predecessor)))

(defmethod ir-node-dominator ((ir-final-node ir-final-node))
  ;; The final node doesn't have to chase the chain of predecessors,
  ;; because it has a direct reference to the initial node.
  (ir-node-dominator (ir-initial-node ir-final-node)))

(defmethod ir-node-inputs ((ir-node ir-node))
  '())

(defmethod ir-node-outputs ((ir-node ir-node))
  '())

(defmethod ir-initial-node ((ir-initial-node ir-initial-node))
  ir-initial-node)

(defmethod ir-initial-node ((ir-node-with-predecessor ir-node-with-predecessor))
  ;; Follow the chain of predecessors.
  (ir-initial-node (ir-node-predecessor ir-node-with-predecessor)))

(defmethod ir-initial-node-p ((ir-initial-node ir-initial-node))
  t)

(defmethod ir-final-node ((ir-final-node ir-final-node))
  ir-final-node)

(defmethod ir-final-node ((ir-node-with-successor ir-node-with-successor))
  ;; Follow the chain of successors.
  (ir-final-node (ir-node-successor ir-node-with-successor)))

(defmethod ir-final-node-p ((ir-final-node ir-final-node))
  t)

(defmethod make-ir-initial-and-ir-final-node (dominator)
  (let ((initial-node (make-instance 'ir-node))
        (final-node (make-instance 'ir-node)))
    (change-class final-node 'ir-final-node
      :initial-node initial-node
      :predecessor initial-node)
    (change-class initial-node 'ir-initial-node
      :dominator dominator
      :successor final-node
      :final-node final-node)
    (values initial-node final-node)))

(defmethod ir-loop-p ((ir-loop ir-loop))
  t)

(defmethod ir-call-p ((ir-call ir-call))
  t)

(defmethod ir-if-p ((ir-if ir-if))
  t)

(defmethod ir-construct-p ((ir-construct ir-construct))
  t)

(defmethod ir-enclose-p ((ir-enclose ir-enclose))
  t)

(defmethod ir-value-p ((ir-value ir-value))
  t)

(defmethod ir-node-values-type ((ir-node ir-node))
  '(values))

(defmethod ir-node-values-type ((ir-node-with-outputs ir-node-with-outputs))
  (let ((outputs (ir-node-outputs ir-node-with-outputs)))
    (if (eql outputs '*)
        '(values)
         `(values ,@(mapcar #'ir-value-declared-type outputs)))))

(defmethod ir-value-derived-type (ir-value)
  (typo:ntype-type-specifier
   (ir-value-derived-ntype ir-value)))

(defmethod map-block-inner-nodes (function (ir-node ir-node))
  (map-block-inner-nodes function (ir-initial-node ir-node)))

(defmethod map-block-inner-nodes (function (initial-node ir-initial-node))
  (let ((final-node (ir-final-node initial-node)))
    (loop for node = (ir-node-successor initial-node)
            then (ir-node-successor node)
          until (eq node final-node) do
            (funcall function node))))

(defmethod insert-ir-node-before
    ((ir-node ir-inner-node)
     (future-successor ir-node-with-predecessor))
  (let ((a (ir-node-predecessor future-successor))
        (b ir-node)
        (c future-successor))
    (setf (ir-node-%successor b) c)
    (setf (ir-node-%predecessor b) a)
    (setf (ir-node-%successor a) b)
    (setf (ir-node-%predecessor c) b))
  ir-node)

(defmethod insert-ir-node-after
    ((ir-node ir-inner-node)
     (future-predecessor ir-node-with-successor))
  (let ((a future-predecessor)
        (b ir-node)
        (c (ir-node-successor future-predecessor)))
    (setf (ir-node-%successor b) c)
    (setf (ir-node-%predecessor b) a)
    (setf (ir-node-%successor a) b)
    (setf (ir-node-%predecessor c) b))
  ir-node)

(defmethod extract-ir-node
    ((ir-node ir-inner-node))
  (let ((a (ir-node-predecessor ir-node))
        (b ir-node)
        (c (ir-node-successor ir-node)))
    (setf (ir-node-%successor a) c)
    (setf (ir-node-%predecessor c) a)
    (setf (ir-node-%successor b) b)
    (setf (ir-node-%predecessor b) b)
    ir-node))

(defmethod copy-ir-value (context (ir-value ir-value) &optional (ntype (typo:universal-ntype)))
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
                      (typo:ntype-intersection derived-ntype ntype)))))))

(defmethod copy-ir-node (context (ir-loop ir-loop))
  (let* ((variable (copy-ir-value context (ir-loop-variable ir-loop)))
         (ir-node (make-instance 'ir-node-with-outputs
                    :outputs (list variable))))
    (change-class ir-node 'ir-loop
      :inputs (mapcar (alexandria:curry #'copy-ir-value context) (ir-node-inputs ir-loop))
      :variable variable
      :body (copy-ir-block context (ir-loop-body ir-loop) ir-node))))

(defmethod copy-ir-node (context (ir-call ir-call))
  (with-accessors ((fnrecord ir-call-fnrecord)
                   (inputs ir-node-inputs)
                   (outputs ir-node-outputs)) ir-call
    (make-instance 'ir-call
      :fnrecord fnrecord
      :inputs (mapcar (alexandria:curry #'copy-ir-value context) inputs)
      :outputs
      (if (eql outputs '*)
          '*
          (mapcar (alexandria:curry #'copy-ir-value context) outputs)))))

(defmethod copy-ir-node (context (ir-if ir-if))
  (let ((ir-node (make-instance 'ir-node)))
    (change-class ir-node 'ir-if
      :inputs (mapcar (alexandria:curry #'copy-ir-value context)
                      (ir-node-inputs ir-if))
      :outputs (mapcar (alexandria:curry #'copy-ir-value context)
                       (ir-node-outputs ir-if))
      :then (copy-ir-block context (ir-if-then ir-if) ir-node)
      :else (copy-ir-block context (ir-if-else ir-if) ir-node))))

(defmethod copy-ir-node (context (ir-construct ir-construct))
  (make-instance 'ir-construct
    :form (ir-construct-form ir-construct)
    :outputs (mapcar (alexandria:curry #'copy-ir-value context)
                     (ir-node-outputs ir-construct))))

(defmethod copy-ir-node (context (ir-enclose ir-enclose))
  (let ((ir-node (make-instance 'ir-node)))
    (change-class ir-node 'ir-enclose
      :body (copy-ir-block context (ir-enclose-body ir-enclose) ir-node)
      :argument-values (mapcar (alexandria:curry #'copy-ir-value context)
                               (ir-enclose-argument-values ir-enclose)))))

(defmethod copy-ir-block (context (ir-node ir-node) dominator)
  (multiple-value-bind (ir-initial-node ir-final-node)
      (make-ir-initial-and-ir-final-node dominator)
    (let ((*blocks* (cons ir-final-node *blocks*)))
      (map-block-inner-nodes (alexandria:curry #'copy-ir-node context) ir-node))
    ir-initial-node))

(defun replace-node-outputs (node replacement-outputs)
  (let* ((node-outputs (ir-node-outputs node)))
    (unless (and (eql node-outputs '*)
                 (eql replacement-outputs '*))
      (loop for node-output in node-outputs
            for replacement-output in replacement-outputs do
              (setf (gethash node-output *ir-value-copies*)
                    replacement-output)))))
