(in-package :loopus.ir)

;; Used in the node creation I think, not sure
(defparameter node nil)

;; The hashtable used by copy-ir-node
(defparameter *ir-value-copies* nil)

;; Is a list of loop variable in the cl-isl ast. New variable are pushed at the END of the list
(defparameter possible-loop-variables nil)

;; string of the identifier to ir node - todo do it with identifier instead of strings
;; Used for loop variables
(defparameter *id-to-nodes* nil)

;; Map of int to loop variable
(defparameter *depth-loop-variables* nil)

;; Depth we are currently at in the ast
(defparameter *current-depth* nil)

;; Hashtable of depth to the associated loopus node which is loop variable
(defparameter *position-to-loopusvariable* nil)

;; Add/remove from *id-to-nodes*
;; Now it's with the string of the name
;; (Because the lisp object wrapping the identifier changes even if it's the same identifier)
(defun create-loop-var (loop-variable)
  (let ((loop-variable (isl:identifier-name (isl:id-expr-get-id loop-variable))))
    (alexandria:ensure-gethash loop-variable *id-to-nodes* (make-instance 'ir-value))))
(defun delete-loop-var (loop-variable)
  (let ((loop-variable (isl:identifier-name (isl:id-expr-get-id loop-variable))))
    (remhash loop-variable *id-to-nodes*)))

;; Entry point of the program. Takes a cl-isl ast, call execute-node on it, and returns a loopus ast
;; Sequence of instructions are "block" in the cl-isl ast, and execute-node on a block calls recursively
;; itself on every statement of the block. Hence a single call to execute-node in this function
(defun my-main (node dominator)
  (multiple-value-bind (ir-initial-node ir-final-node)
      (make-ir-initial-and-ir-final-node dominator)
    (let ((*blocks* (cons ir-final-node *blocks*)))
      (execute-node node))
    ir-initial-node))
