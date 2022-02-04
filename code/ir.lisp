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

(defclass ir-block ()
  (;; The IR block dominating this one, or NIL if this block is the
   ;; outermost IR block.
   (%immediate-dominator
    :initarg :immediate-dominator
    :initform (alexandria:required-argument :immediate-dominator)
    :type (or ir-block null)
    :reader ir-block-immediate-dominator)
   ;; A list of impure calls carried by this block.
   (%effects
    :initarg :effects
    :initform (alexandria:required-argument :effects)
    :type list
    :reader ir-block-effects)))

(defmethod ir-block-depth (ir-block)
  (trivia:match (ir-block-immediate-dominator ir-block)
    ((eql nil) 0)
    ((ir-block :immediate-dominator d) (1+ (ir-block-depth d)))))

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
    :type variable
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

(defgeneric ir-call-block (ir-call))

(defgeneric ir-call-depth (ir-call))

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
    :reader ir-call-results)
   (%block
    :initarg :block
    :initform (alexandria:required-argument :block)
    :type ir-block
    :reader ir-call-block)))

(defmethod ir-call-depth ((ir-call ir-call))
  (ir-block-depth (ir-call-block ir-call)))

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

(defgeneric ir-value-type (ir-value))

(defgeneric ir-value-definition (ir-value))

(defgeneric ir-value-users (ir-value))

(defclass ir-value ()
  ((%name
    :initarg :name
    :initform (gensym "V")
    :type symbol
    :reader ir-value-name)
   (%type
    :initarg :type
    :initform 't
    :accessor ir-value-type)
   (%definition
    :initarg :definition
    :initform (alexandria:required-argument :definition)
    :type ir-call
    :reader ir-value-definition)
   (%users
    :initarg :users
    :initform '()
    :type list
    :reader ir-value-users)))
