(in-package #:loopus)

(defvar *dominator* nil)

;;; This is a very simple IR, consisting of a sequence of nodes that are
;;; connected via their successor and predecessor slot.  The node whose
;;; predecessor is NIL is called the initial node.  The node whose
;;; successor is NIL is called the final node.  When control is transferred
;;; to a node, it reads its (possibly empty) list of input values, computes
;;; its output values and transfers control to its successor.  The final
;;; node returns its outputs.  Some nodes, such as IR-IF, IR-ENCLOSE, and
;;; IR-LOOP, have a reference to one or more other initial IR nodes that
;;; are processed specially.

(defclass ir-node ()
  (;; The IR node that this node transfers control to.
   (%successor
    :initform nil
    :type (or null ir-node)
    :reader ir-node-successor)
   ;; The IR node that this node receives control from.
   (%predecessor
    :initform nil
    :type (or null ir-node)
    :reader ir-node-predecessor)
   ;; The IR node 'above' this one, i.e., the unique loop or if node that
   ;; has a reference to this node or some predecessor of this node.
   (%dominator
    :initarg :dominator
    :initform *dominator*
    :type (or null ir-loop)
    :reader ir-node-dominator)
   ;; The list of values that are used by this node.
   (%inputs
    :initarg :inputs
    :initform (alexandria:required-argument :inputs)
    :type list
    :reader ir-node-inputs)
   ;; The list of values that are produced by this node.  One peculiarity
   ;; should be mentioned here: If the node is the final node of a
   ;; sequence, i.e., if its successor is NIL, its outputs are meaningless.
   ;; It may return any number of values in an unspecified way.
   (%outputs
    :initarg :outputs
    :initform (alexandria:required-argument :outputs)
    :type list
    :reader ir-node-outputs)))

(defgeneric ir-node-p (x)
  (:method ((x t)) nil)
  (:method ((x ir-node)) t))

(defgeneric add-control-flow-edge (from to)
  (:method ((from ir-node) (to ir-node))
    (let ((predecessor (ir-node-predecessor to)))
      (if (null predecessor)
          (setf (slot-value to '%predecessor) from)
          (unless (eq predecessor from)
            (error "Attempt to redefine the predecessor of ~S from ~S to ~S."
                   to predecessor from))))
    (let ((successor (ir-node-successor from)))
      (if (null successor)
          (setf (slot-value from '%successor) to)
          (unless (eq successor to)
            (error "Attempt to redefine the successor of ~S from ~S to ~S."
                   from successor to))))
    (values)))

(defclass ir-value ()
  ((%declared-type
    :initarg :declared-type
    :initform 't
    :reader ir-value-declared-type)
   (%derived-type
    :initarg :derived-type
    :initform 't
    :reader ir-value-derived-type)
   (%producer
    :type ir-node
    :reader ir-value-producer)
   (%users
    :initform '()
    :reader ir-value-users)))

(defgeneric ir-value-p (x)
  (:method ((x t)) nil)
  (:method ((x ir-value)) t))

(defun ensure-ir-value-producer (ir-value producer)
  (if (slot-boundp ir-value '%producer)
      (let ((other-producer (slot-value ir-value '%producer)))
        (unless (eq producer other-producer)
          (error "Attempt to redefine the producer of ~S from ~S to ~S."
                 ir-value other-producer producer)))
      (setf (slot-value ir-value '%producer) producer)))

(defun ensure-ir-value-user (ir-value user)
  (pushnew user (slot-value ir-value '%users)))

(defmethod shared-initialize :after ((ir-node ir-node) slot-names &key &allow-other-keys)
  (dolist (output (ir-node-outputs ir-node))
    (ensure-ir-value-producer output ir-node))
  (dolist (input (ir-node-inputs ir-node))
    (ensure-ir-value-user input ir-node)))

;;; An initial node has no inputs, no outputs, and no predecessor and does
;;; nothing but transfer its control to its successor.
(defclass ir-initial-node (ir-node)
  ((%inputs :initform '() :type null)
   (%outputs :initform '() :type null)
   (%predecessor :type null)))

;;; A loop node has three inputs (start, end, and step), zero outputs, one
;;; successor, and a control flow node that marks the beginning of its
;;; body.  When control is transferred to the loop node, it repeatedly
;;; evaluates its body in an environment where the loop variable is bound
;;; to successive elements of the iteration space.
(defclass ir-loop (ir-node)
  ((%variable
    :initarg :variable
    :type ir-value
    :reader ir-loop-variable)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :type ir-initial-node
    :reader ir-loop-body)))

;;; Ensure that the loop is the producer of its variable.
(defmethod shared-initialize :after ((ir-loop ir-loop) slot-names &key &allow-other-keys)
  (ensure-ir-value-producer (ir-loop-variable ir-loop) ir-loop))

;;; A call has first input that is a function, and further inputs that
;;; serve as the arguments of that function, and some number of outputs
;;; (whose number need not fit to the number of arguments of the function).
;;; When control is transferred to it, it binds each output to the
;;; corresponding value obtained by invoking the function on the arguments.
(defclass ir-call (ir-node)
  ())

;;; An if node has one input that is the generalized boolean to test, some
;;; number of outputs, a then node, and an else node.  When control is
;;; transferred to it, it transfers control to its then node if the input
;;; is true, and to its then node if the input is false.  Then it binds its
;;; outputs to the values produced by whatever chain of nodes was chosen.
(defclass ir-if (ir-node)
  ((%then-node
    :initarg :then-node
    :initform (alexandria:required-argument :then-node)
    :type list
    :reader ir-if-then-node)
   (%else-node
    :initarg :else-node
    :initform (alexandria:required-argument :else-node)
    :type list
    :reader ir-if-else-node)))

;;; A construct node has zero inputs and some number of outputs.  When
;;; control is transferred to it, it binds each output to the corresponding
;;; value obtained by evaluating its form.
;;;
;;; Construct nodes are used to handle constants, or references to
;;; functions or variables from outside of the loop nest.
(defclass ir-construct (ir-node)
  ((%inputs :type null :initform '())
   (%form
    :initarg :form
    :initform (alexandria:required-argument :form)
    :reader ir-construct-form)))

;;; An enclose node creates a function at run time.
(defclass ir-enclose (ir-node)
  ((%inputs :initform '() :type null)
   ;; A list of IR values that are bound on each invocation of that
   ;; function.
   (%arguments
    :initarg :arguments
    :initform (alexandria:required-argument :arguments)
    :type list
    :reader ir-enclose-arguments)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :reader ir-enclose-body)))
