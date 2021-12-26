(in-package #:loopus)

;;; This is a very simple IR, consisting only of nodes and blocks.  Each
;;; node corresponds to a function call that produces zero or more values.
;;; We distinguish effect nodes, such as reductions or stores, and pure
;;; nodes.  Each block contains a sequence of effect nodes.  The arguments
;;; of an effect node can be pure nodes or effect nodes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Blocks

(defclass ir-block ()
  (;; The IR block dominating this one, or NIL if this block is the
   ;; outermost IR block.
   (%dominator
    :initarg :immediate-dominator
    :initform (alexandria:required-argument :immediate-dominator)
    :type (or ir-block null)
    :reader ir-block-immediate-dominator)
   ;; A list of IR nodes carried by this block that have an effect.
   (%effects
    :initarg :effects
    :initform (alexandria:required-argument :effects)
    :type list
    :reader ir-block-effects)))

(defclass loop-block (ir-block)
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
    :reader ir-loop-end)
   (%unroll
    :initarg :unroll
    :initform 1
    :type unsigned-byte
    :reader ir-loop-unroll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Nodes

(defclass ir-node ()
  ((%arguments
    )
   (%block
    )
   (%result-variables
    )))

(defclass effect-node (ir-node)
  ())

(defclass pure-node (ir-node)
  ())

(defclass store-node (effect-node)
  ())

(defclass reduction-node (effect-node)
  ())

(defclass invoke-block-node (effect-node)
  ())

(defclass call-node (pure-node)
  ())
