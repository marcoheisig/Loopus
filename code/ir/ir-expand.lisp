(in-package #:loopus.ir)

;; A hash table, mapping from values to symbols.
(defvar *ir-expand-value-names*)

(defun value-name (value)
  (check-type value ir-value)
  (alexandria:ensure-gethash value *ir-expand-value-names* (gensym "V")))

(defgeneric ir-expand-node (ir-node))

(defun ir-expand (ir)
  (let ((*ir-expand-value-names* (make-hash-table :test #'eq))
        (*gensym-counter* 0))
    (ir-expand-node ir)))

(defmethod ir-expand-node ((ir-initial-node ir-initial-node))
  (let ((final-node (ir-final-node ir-initial-node)))
    `(basic-block
      ,@(loop for node = (ir-node-successor ir-initial-node) then (ir-node-successor node)
              until (eq node final-node)
              collect (ir-expand-node node)))))

(defmethod ir-expand-node ((ir-loop ir-loop))
  (with-accessors ((inputs ir-node-inputs)
                   (variable ir-loop-variable)
                   (body ir-loop-body)
                   (test ir-loop-test)) ir-loop
    (destructuring-bind (start step) inputs
      (let ((variable (value-name variable))
            (start (value-name start))
            (step (value-name step)))
        `(() (loop for ,variable fixnum = ,start then (+ ,variable ,step)
                   while ,(ir-expand-node test)
                   do ,(ir-expand-node body)))))))

(defmethod ir-expand-node ((ir-call ir-call))
  (let* ((fnrecord (ir-call-fnrecord ir-call))
         (outputs (ir-node-outputs ir-call)))
    `(,(if (eql outputs '*) '() (mapcar #'value-name outputs))
      (the ,(ir-node-values-type ir-call)
           ,(let ((name (typo:fnrecord-name fnrecord))
                  (arguments (mapcar #'value-name (ir-node-inputs ir-call))))
              (cond ((null name)
                     `(funcall ,(typo:fnrecord-function fnrecord) ,@arguments))
                    ((symbolp name)
                     `(,name ,@arguments))
                    (t
                     `(funcall (function ,name) ,@arguments))))))))

(defmethod ir-expand-node ((ir-if ir-if))
  `(,(mapcar #'value-name (ir-node-outputs ir-if))
    (the ,(ir-node-values-type ir-if)
         (if ,(value-name (first (ir-node-inputs ir-if)))
             ,(ir-expand-node (ir-if-then ir-if))
             ,(ir-expand-node (ir-if-else ir-if))))))

(defmethod ir-expand-node ((ir-construct ir-construct))
  `(,(mapcar #'value-name (ir-node-outputs ir-construct))
    (the ,(ir-node-values-type ir-construct)
         ,(ir-construct-form ir-construct))))

(defmethod ir-expand-node ((ir-enclose ir-enclose))
  `(,(mapcar #'value-name (ir-node-outputs ir-enclose))
    (the ,(ir-node-values-type ir-enclose)
         (lambda ,(mapcar #'value-name (ir-enclose-argument-values ir-enclose))
           ,(ir-expand-node (ir-enclose-body ir-enclose))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Basic Block Macro

(defmacro basic-block (&rest body)
  (if (null body)
      `(values)
      (expand-basic-block body)))

(defun expand-basic-block (body)
  (trivia:ematch body
    ((list (list (list* _) form))
     form)
    ((list* (list (list) form)
            rest)
     `(progn ,form ,(expand-basic-block rest)))
    ((list* (list (list variable) form)
            rest)
     (expand-basic-block/let* (list (list variable form)) rest))
    ((list* (list (list* variables) form)
            rest)
     `(multiple-value-bind ,variables ,form
        ,(expand-basic-block rest)))
    ((list* malformed _)
     (error "Malformed block component: ~S" malformed))))

(defun expand-basic-block/let* (reversed-bindings body)
  (trivia:ematch body
    ((list (list (list* _) form))
     `(let* ,(reverse reversed-bindings)
        ,form))
    ((list* (list (list) form)
            rest)
     `(let* ,(reverse reversed-bindings)
        ,form
        ,(expand-basic-block rest)))
    ((list* (list (list variable) form) rest)
     (expand-basic-block/let* (list* (list variable form) reversed-bindings) rest))
    ((list* (list (list* variables) form) rest)
     `(let* ,(reverse reversed-bindings)
        (multiple-value-bind ,variables ,form
          ,(expand-basic-block rest))))
    ((list* malformed _)
     (error "Malformed block component: ~S" malformed))))
