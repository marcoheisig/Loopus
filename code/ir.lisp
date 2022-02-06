(in-package #:loopus)

;;; This is a very simple IR, consisting only of blocks, calls, and values.
;;; We distinguish impure calls, i.e. calls to functions with side-effects
;;; such as reductions or stores, and pure calls.  Each block contains a
;;; sequence of impure calls that may or may not reference values from
;;; other pure or impure calls.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Blocks

(defgeneric ir-block-immediate-dominator (ir-block))

(defgeneric ir-block-depth (ir-block))

(defgeneric ir-block-effects (ir-block))

(defgeneric ir-block-add-effect (ir-block effect))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ir-block ()
    (;; The IR block dominating this one, or NIL if this block is the
     ;; outermost IR block.
     (%immediate-dominator
      :initarg :immediate-dominator
      :initform nil
      :type (or ir-block null)
      :reader ir-block-immediate-dominator)
     ;; A doubly-linked list of impure calls carried by this block.
     (%effect-dlist
      :initarg :effect-dlist
      :initform (dlist:dlist)
      :type dlist:dlist
      :reader ir-block-effect-dlist))))

(defmethod ir-block-depth (ir-block)
  (trivia:match (ir-block-immediate-dominator ir-block)
    ((eql nil) 0)
    ((ir-block :immediate-dominator d) (1+ (ir-block-depth d)))))

(defmethod ir-block-effects (ir-block)
  (dlist:list-from-dlist
   (ir-block-effect-dlist ir-block)))

(defmethod ir-block-add-effect (ir-block effect)
  (check-type effect ir-impure-call)
  (dlist:dlist-push-back effect (ir-block-effect-dlist ir-block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loops

(defgeneric ir-loop-variable (ir-loop))

(defgeneric ir-loop-start (ir-loop))

(defgeneric ir-loop-step (ir-loop))

(defgeneric ir-loop-end (ir-loop))

(defclass ir-loop (ir-block)
  ((%variable
    :initarg :variable
    :initform (alexandria:required-argument :variable)
    :type ir-loop-variable
    :reader ir-loop-variable)
   (%start
    :initarg :start
    :initform (alexandria:required-argument :start)
    :type polynomial
    :reader ir-loop-start)
   (%step
    :initarg :step
    :initform (alexandria:required-argument :step)
    :type polynomial
    :reader ir-loop-step)
   (%end
    :initarg :end
    :initform (alexandria:required-argument :end)
    :type polynomial
    :reader ir-loop-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calls

(defgeneric ir-call-function-name (ir-call))

(defgeneric ir-call-arguments (ir-call))

(defgeneric ir-call-results (ir-call))

(defclass ir-call ()
  ((%function-name
    :initarg :function-name
    :initform (alexandria:required-argument :function-name)
    :type function-name
    :reader ir-call-function)
   (%arguments
    :initarg :arguments
    :initform (alexandria:required-argument :arguments)
    :type list
    :reader ir-call-arguments)
   (%results
    :initarg :results
    :initform (alexandria:required-argument :results)
    :type list
    :reader ir-call-results)))

(defclass ir-impure-call (ir-call)
  ())

(defclass ir-pure-call (ir-call)
  ())

(defclass ir-load (ir-pure-call)
  ())

(defclass ir-store (impure-call)
  ())

(defclass ir-reduction (impure-call)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Values

(defgeneric ir-value-name (ir-value))

(defgeneric ir-value-declared-type (ir-value))

(defgeneric ir-value-derived-type (ir-value))

(defgeneric (setf ir-value-derived-type) (type ir-value))

(defgeneric ir-value-users (ir-value))

(defgeneric ir-unknown-form (ir-unknown))

(defgeneric ir-result-definition (ir-result))

(defgeneric ir-constant-value (ir-constant))

(defgeneric ir-function-lambda-list (ir-function))

(defgeneric ir-function-body (ir-function))

(defgeneric ir-function-lexenv (ir-function))

(defclass ir-value ()
  ((%declared-type
    :initarg :type
    :initform (alexandria:required-argument :declared-type)
    :reader ir-value-declared-type)
   (%derived-type
    :initarg :type
    :initform 't
    :accessor ir-value-derived-type)))

(defclass ir-loop-variable (ir-value)
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type variable-name
    :reader ir-loop-variable-name)
   (%loop
    :initarg :loop
    :initform nil
    :type (or ir-loop null)
    :reader ir-loop-variable-loop)))

(defclass ir-unknown (ir-value)
  ((%form
    :initarg :form
    :initform (alexandria:required-argument :form)
    :reader ir-unknown-form)))

(defclass ir-result (ir-value)
  ((%definition
    :initarg :definition
    :initform (alexandria:required-argument :definition)
    :type ir-call
    :reader ir-result-definition)))

(defclass ir-constant (ir-value)
  ((%value
    :initarg :value
    :initform (alexandria:required-argument :value)
    :type t
    :reader ir-constant-value)))

(defclass ir-function (ir-value)
  ((%lambda-list
    :initarg :lambda-list
    :initform (alexandria:required-argument :lambda-list)
    :reader ir-function-lambda-list)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :reader ir-function-body)
   (%lexenv
    :initarg :lexenv
    :initform (alexandria:required-argument :lexenv)
    :type lexenv
    :reader ir-function-lexenv)))
