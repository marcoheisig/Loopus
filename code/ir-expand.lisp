(in-package #:loopus)

;; A hash table, mapping from values to symbols.
(defvar *ir-expand-value-names*)

(defun value-name (value)
  (check-type value ir-value)
  (alexandria:ensure-gethash value *ir-expand-value-names* (gensym "V")))

(defgeneric ir-expand-node (ir-node))

(defun ir-expand (ir)
  (let ((*ir-expand-value-names* (make-hash-table :test #'eq))
        (*gensym-counter* 0))
    (ir-expand-initial-node ir)))

(defmethod ir-expand-node ((ir-initial-node ir-initial-node))
  `(basic-block
    ,@(loop for node = (ir-node-successor ir-initial-node) then (ir-node-successor node)
            until (null node)
            collect (ir-expand-node node))))

(defmethod ir-expand-node ((ir-loop ir-loop))
  (let ((index (value-name (ir-loop-variable ir-loop))))
    (destructuring-bind (start end step) (ir-node-inputs ir-loop)
      `(()
        (let ((,index ,(value-name start)))
          (declare (fixnum ,index))
          (loop until (= ,index ,(value-name end)) do
            (progn ,(ir-expand-node (ir-loop-body ir-loop)))
            (incf ,index ,(value-name step))))))))

(defmethod ir-expand-node ((ir-call ir-call))
  `(,(mapcar #'value-name (ir-node-outputs ir-call))
    (funcall ,@(mapcar #'value-name (ir-node-inputs ir-call)))))

(defmethod ir-expand-node ((ir-if ir-if))
  `(,(mapcar #'value-name (ir-node-outputs ir-if))
    (if ,(value-name (first (ir-node-inputs ir-if)))
        ,(ir-expand-node (ir-if-then-node ir-if))
        ,(ir-expand-node (ir-if-else-node ir-if)))))

(defmethod ir-expand-node ((ir-construct ir-construct))
  `(,(mapcar #'value-name (ir-node-outputs ir-construct))
    ,(ir-construct-form ir-construct)))

(defmethod ir-expand-node ((ir-enclose ir-enclose))
  `(,(mapcar #'value-name (ir-node-outputs ir-enclose))
    (lambda ,(mapcar #'value-name (ir-enclose-arguments ir-enclose))
      ,(ir-expand-node (ir-enclose-body ir-enclose)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Basic Block Macro

(defmacro basic-block (&rest body)
  (expand-basic-block body '()))

(defun expand-basic-block (body values)
  (trivia:ematch body
    ((list)
     `(values ,@values))
    ((list* (list (list) form)
            rest)
     `(progn ,form ,(expand-basic-block rest '())))
    ((list* (list (list variable) form)
            rest)
     (expand-basic-block/let* (list (list variable form)) rest))
    ((list* (list (list* variables) form)
            rest)
     `(multiple-value-bind ,variables ,form
        ,(expand-basic-block rest variables)))
    ((list* malformed _)
     (error "Malformed block component: ~S" malformed))))

(defun expand-basic-block/let* (reversed-bindings body)
  (trivia:ematch body
    ((list)
     `(let* ,(reverse reversed-bindings)
        (values ,(first (first reversed-bindings)))))
    ((list* (list (list) form)
            rest)
     `(let* ,(reverse reversed-bindings)
        ,form
        ,(expand-basic-block rest '())))
    ((list* (list (list variable) form) rest)
     (expand-basic-block/let* (list* (list variable form) reversed-bindings) rest))
    ((list* (list (list* variables) form) rest)
     `(let* ,(reverse reversed-bindings)
        (multiple-value-bind ,variables ,form
          ,(expand-basic-block rest variables))))
    ((list* malformed _)
     (error "Malformed block component: ~S" malformed))))
