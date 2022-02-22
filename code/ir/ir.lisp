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
;;; Generic Functions

(defgeneric ir-node-p (object) (:method ((object t)) nil))

(defgeneric ir-node-dominator (ir-node))

(defgeneric ir-node-inputs (ir-node))

(defgeneric ir-node-outputs (ir-node))

(defgeneric ir-node-successor (ir-node))

(defgeneric ir-node-predecessor (ir-node))


(defgeneric ir-initial-node (ir-node))

(defgeneric ir-initial-node-p (object) (:method ((object t)) nil))

(defgeneric ir-final-node (ir-node))

(defgeneric ir-final-node-p (object) (:method ((object t)) nil))

(defgeneric make-ir-initial-and-ir-final-node (dominator))


(defgeneric ir-loop-p (object) (:method ((object t)) nil))

(defgeneric ir-loop-body-initial-node (ir-loop))

(defgeneric ir-loop-variable (ir-loop))


(defgeneric ir-if-p (object) (:method ((object t)) nil))

(defgeneric ir-if-then-initial-node (ir-if))

(defgeneric ir-if-else-initial-node (ir-if))


(defgeneric ir-construct-p (object) (:method ((object t)) nil))

(defgeneric ir-construct-form (ir-construct))


(defgeneric ir-enclose-p (object) (:method ((object t)) nil))

(defgeneric ir-enclose-argument-values (ir-enclose))

(defgeneric ir-enclose-body-initial-node (ir-enclose))


(defgeneric ir-value-p (object) (:method ((object t)) nil))

(defgeneric ir-value-producer (ir-value))

(defgeneric ir-value-users (ir-value))

(defgeneric ir-value-declared-type (ir-value))

(defgeneric ir-value-derived-type (ir-value))

(defgeneric ir-value-declare-type (ir-value type-specifier))


(defgeneric insert-ir-node-before (ir-node future-successor))

(defgeneric insert-ir-node-after (ir-node future-predecessor))

(defgeneric extract-ir-node (ir-node))

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
    :type (or null ir-loop)
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
  (;; The list of values that are produced by this node.  One peculiarity
   ;; should be mentioned here: If the node is the final node of a
   ;; sequence, i.e., if its successor is NIL, its outputs are meaningless.
   ;; It may return any number of values in an unspecified way.
   (%outputs
    :initarg :outputs
    :initform (alexandria:required-argument :outputs)
    :type list
    :reader ir-node-outputs)))

(defmethod shared-initialize :after
    ((ir-node-with-outputs ir-node-with-outputs) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (output (ir-node-outputs ir-node-with-outputs))
    (ensure-ir-value-producer output ir-node-with-outputs)))

(defclass ir-node-with-successor (ir-node)
  (;; The IR node that this node transfers control to.
   (%successor
    :initarg :successor
    :initform (alexandria:required-argument :successor)
    :type ir-node
    :reader ir-node-successor
    :writer (setf ir-node-%successor))))

(defclass ir-node-with-predecessor (ir-node)
  (;; The IR node that this node receives control form.
   (%predecessor
    :initarg :predecessor
    :initform (alexandria:required-argument :predecessor)
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

;;; A loop node is an inner node with three inputs (start, end, and
;;; step), zero outputs, and a control flow node that marks the beginning
;;; of its body.  When control is transferred to the loop node, it
;;; repeatedly evaluates its body in an environment where the loop variable
;;; is bound to successive elements of the iteration space.
(defclass ir-loop (ir-inner-node ir-node-with-inputs)
  ((%variable
    :initarg :variable
    :type ir-value
    :reader ir-loop-variable)
   (%body-initial-node
    :initarg :body-initial-node
    :initform (alexandria:required-argument :body-initial-node)
    :type ir-initial-node
    :reader ir-loop-body)))

(defmethod shared-initialize :after
    ((ir-loop ir-loop) slot-names &key &allow-other-keys)
  (ensure-ir-value-producer (ir-loop-variable ir-loop) ir-loop))

;;; A call node has a first input that is a function, and further inputs
;;; that serve as the arguments of that function.  It has some number of
;;; outputs (whose number need not fit to the number of values produced by
;;; the function).  When control is transferred to it, it binds each output
;;; to the corresponding value obtained by invoking the function on the
;;; arguments.  Outputs with no corresponding value are bound to NIL.
(defclass ir-call (ir-inner-node ir-node-with-inputs ir-node-with-outputs)
  ())

;;; An if node is an inner node with one input that is the generalized
;;; boolean to test, some number of outputs, a reference to the initial
;;; node of its then part, and a reference to the initial node of its else
;;; part.  When control is transferred to it, it transfers control to its
;;; then initial node if the input is true, and to its else initial node if
;;; the input is false.  Then it binds its outputs to the values produced
;;; by whatever chain of nodes was chosen.
(defclass ir-if (ir-inner-node ir-node-with-inputs ir-node-with-outputs)
  ((%then-initial-node
    :initarg :then-initial-node
    :initform (alexandria:required-argument :then-initial-node)
    :type list
    :reader ir-if-then-initial-node)
   (%else-initial-node
    :initarg :else-initial-node
    :initform (alexandria:required-argument :else-initial-node)
    :type list
    :reader ir-if-else-initial-node)))

;;; A construct node is an inner node with no inputs and some number of
;;; outputs.  When control is transferred to it, it binds each output to
;;; the corresponding value obtained by evaluating its form.
;;;
;;; Construct nodes are used to handle constants, or references to
;;; functions or variables from outside of the loop nest.
(defclass ir-construct (ir-inner-node ir-node-with-outputs)
  ((%inputs :type null :initform '())
   (%form
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
   (%body-initial-node
    :initarg :body-initial-node
    :initform (alexandria:required-argument :body-initial-node)
    :reader ir-enclose-body-initial-node)))

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

(defmethod ir-if-p ((ir-if ir-if))
  t)

(defmethod ir-construct-p ((ir-construct ir-construct))
  t)

(defmethod ir-enclose-p ((ir-enclose ir-enclose))
  t)

(defmethod ir-value-p ((ir-value ir-value))
  t)

(defmethod ir-value-derived-type (ir-value)
  (typo:ntype-type-specifier
   (ir-value-derived-ntype ir-value)))

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
