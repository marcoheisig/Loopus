(in-package #:loopus)

(defvar *dominating-loop* nil)

;;; This is a very simple IR, consisting of a directed graph of nodes that
;;; are connected via their lists of successors and predecessors.  When
;;; control is transferred to a node, it reads its (possibly empty) list of
;;; input values, computes its output values and determines which successor
;;; it transfers control to.

(defclass ir-node ()
  ((%dominating-loop
    :initarg :dominating-loop
    :initform *dominating-loop*
    :type (or null ir-loop)
    :reader ir-node-dominating-loop)
   (%inputs
    :initarg :inputs
    :initform (alexandria:required-argument :inputs)
    :type list
    :reader ir-node-inputs)
   (%outputs
    :initarg :outputs
    :initform (alexandria:required-argument :outputs)
    :type list
    :reader ir-node-outputs)
   (%successors
    :initform '()
    :type list
    :reader ir-node-successors)
   (%predecessors
    :initform '()
    :type list
    :reader ir-node-predecessors)))

(defgeneric ir-node-p (x)
  (:method ((x t)) nil)
  (:method ((x ir-node)) t))

(define-modify-macro appendf (&rest args) append)

(defgeneric add-control-flow-edge (from to)
  (:method ((from ir-node) (to ir-node))
    (appendf (slot-value to '%predecessors) (list from))
    (appendf (slot-value from '%successors) (list to))))

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

(defgeneric add-declaration (ir-value declared-type)
  (:method ((ir-value ir-value) declared-type)
    (break "TODO")))

;;; The initial node of an IR graph.  It has no inputs, no outputs, and
;;; simply transfers control to its sole successor.
(defclass ir-initial-node (ir-node)
  ((%inputs :type null :initform '())
   (%outputs :type null :initform '())))

;;; A loop node has three inputs (start, step, and end), zero outputs, one
;;; successor, and a control flow node that marks the beginning of its
;;; body.  When control is transferred to the loop node, it repeatedly
;;; evaluates its body in an environment where the loop variable is bound
;;; to successive elements of the iteration space, and finally transfers
;;; control to its sole successor.
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
;;; serve as the arguments of that function, some number of outputs (whose
;;; number need not fit to the number of arguments of the function), and a
;;; single successor.  When control is transferred to it, it binds each
;;; output to the corresponding value obtained by invoking the function on
;;; the arguments and transfers control to its successor.
(defclass ir-call (ir-node)
  ())

;;; An if node has one input (the value to test), some number of outputs,
;;; and two successors (then and else).  When control is transferred to it,
;;; it transfers control to its first successor if the input is true, and
;;; to its second successor if the input is false.  Then it binds its
;;; outputs to the values produced by whatever successor was chosen.
(defclass ir-if (ir-node)
  ((%then-outputs
    :initarg :then-outputs
    :initform (alexandria:required-argument :then-outputs)
    :type list
    :reader ir-if-then-outputs)
   (%else-outputs
    :initarg :else-outputs
    :initform (alexandria:required-argument :else-outputs)
    :type list
    :reader ir-if-else-outputs)))

;;; A construct node has zero inputs, some number of outputs, and a single
;;; successor.  When control is transferred to it, it binds each output to
;;; the corresponding value obtained by evaluating its form and transfers
;;; control to its successor.
;;;
;;; Construct nodes are used to handle constants, or references to
;;; variables from outside of the loop nest.
(defclass ir-construct (ir-node)
  ((%inputs :type null :initform '())
   (%form
    :initarg :form
    :initform (alexandria:required-argument :form)
    :reader ir-leaf-form)))

;;; An enclose node creates a function at run time.
(defclass ir-enclose (ir-node)
  (;; A list of IR values that are bound on each invocation of that
   ;; function.
   (%arguments
    :initarg :arguments
    :initform (alexandria:required-argument :arguments)
    :type list
    :reader ir-enclose-arguments)
   (%lexenv
    :initarg :lexenv
    :initform (alexandria:required-argument :lexenv)
    :reader ir-enclose-lexenv)
   (%initial-node
    :initarg :initial-node
    :initform (alexandria:required-argument :initial-node)
    :reader ir-enclose-initial-node)))
