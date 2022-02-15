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


(defgeneric insert-ir-node-before (ir-node future-successor))

(defgeneric insert-ir-node-after (ir-node future-predecessor))

(defgeneric extract-ir-node (ir-node))
