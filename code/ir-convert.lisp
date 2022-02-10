(in-package #:Loopus)

;;; This file contains the machinery for converting a loop nest to Loopus
;;; IR.  This is essentially a variation of EVAL for a very limited subset
;;; of Common Lisp, except that it doesn't actually compute a result but
;;; assemble IR nodes.

;;; The most recent IR node that has been created.
(defvar *predecessor*)

;;; An EQL hash table, mapping from each constant to the IR value resulting
;;; from constructing that constant.
(defvar *constants*)

;;; An EQ hash table, mapping from variables to the IR value resulting from
;;; a reference to that variable.
(defvar *variable-references*)

;;; An EQUAL hash table, mapping from function names to the IR value
;;; resulting from a reference to that function.
(defvar *function-references*)

(defun ir-convert-in-environment (form env &optional (expected-values '*))
  (let* ((initial-node (make-instance 'ir-initial-node))
         (*predecessor* initial-node)
         (*constants* (make-hash-table))
         (*variable-references* (make-hash-table :test #'eq))
         (*function-references* (make-hash-table :test #'equal)))
    (ir-convert
     ;; TODO It would be better not to use macroexpand-all, but to expand
     ;; things ourselves.  Otherwise we risk that an implementation expands
     ;; macros in a way that relies on internals we can't handle.
     (trivial-macroexpand-all:macroexpand-all
      ;; Each occurrence of the FOR macro is turned into a call to the
      ;;  function %FOR.  This function is not defined, but handled specially
      ;;  by the IR conversion process.
      `(macrolet ((for ((variable start end &optional (step 1)) &body body)
                    `(%for ',variable ,start ,end ,step (locally ,@body))))
         ,form)
      env)
     (make-lexenv env)
     expected-values)
    initial-node))

(defgeneric ir-convert-constant (constant))

(defgeneric ir-convert-symbol (symbol lexenv expected-values))

(defgeneric ir-convert-compound-form (operator rest lexenv expected-values))

(defmacro ensure-expected-values
    (expected-values &body body)
  (alexandria:once-only (expected-values)
    (alexandria:with-gensyms (thunk values)
      `(let ((,thunk (lambda () ,@body)))
         (case expected-values
           ((*) (funcall ,thunk))
           ((1) (let ((value (funcall ,thunk)))
                  (assert (ir-value-p value))
                  (values value)))
           (otherwise
            (let ((,values (multiple-value-list (funcall ,thunk))))
              (assert (every #'ir-value-p ,values))
              (values-list
               (loop repeat ,expected-values
                     collect
                     (cond ((null ,values)
                            (ir-convert-constant nil))
                           ((consp ,values)
                            (pop ,values))))))))))))

(defun make-outputs (expected-values)
  (if (eq expected-values '*)
      '()
       (loop repeat expected-values
             collect (make-instance 'ir-value))))

(defun ir-convert (form lexenv &optional (expected-values 1))
  (ensure-expected-values expected-values
    (if (atom form)
        (if (symbolp form)
            (ir-convert-symbol form lexenv expected-values)
            (ir-convert-constant form))
        (ir-convert-compound-form (first form) (rest form) lexenv expected-values))))

(defun push-node (node)
  (add-control-flow-edge *predecessor* node)
  (setf *predecessor* node))

;;; Conversion of Constants

(defmethod ir-convert-constant ((constant t))
  (values
   (alexandria:ensure-gethash
    constant
    *constants*
    (let ((value (make-instance 'ir-value :derived-type `(eql ,constant))))
      (push-node
       (make-instance 'ir-construct
         :form `',constant
         :outputs (list value)))
      value))))

;;; Conversion of Symbols

(defmethod ir-convert-symbol ((variable-name symbol) lexenv expected-values)
  ;; We might want to need this once we can handle macroexpansion (and, in
  ;; particular, symbol macroexpansion) ourselves.
  (declare (ignore expected-values))
  (let* ((env (lexenv-parent lexenv))
         (vrecords (lexenv-vrecords lexenv))
         (vrecord (find variable-name vrecords :key #'vrecord-name)))
    (if (not (null vrecord))
        (vrecord-value vrecord)
        (multiple-value-bind (kind localp alist)
            (trivial-cltl2:variable-information variable-name env)
          (declare (ignore localp alist))
          (case kind
            (:special
             (error "Cannot (yet) handle special variables."))
            ((:lexical :global :constant)
             (if (constantp variable-name)
                 (ir-convert-constant (eval variable-name))
                 (values
                  (alexandria:ensure-gethash
                   variable-name
                   *variable-references*
                   (let ((value (make-instance 'ir-value)))
                     (push-node
                      (make-instance 'ir-construct
                        :form variable-name
                        :outputs (list value)))
                     value)))))
            (t
             (error "Reference to undefined variable ~S."
                    variable-name)))))))

;;; Conversion of Compound Forms

(defmethod ir-convert-compound-form
    ((operator symbol) rest lexenv expected-values)
  (ir-convert `(funcall (function ,operator) ,@rest) lexenv expected-values))

(defmethod ir-convert-compound-form
    ((_ (eql 'funcall)) rest lexenv expected-values)
  (let ((outputs (make-outputs expected-values)))
    (push-node
     (make-instance 'ir-call
       :inputs (mapcar (lambda (form) (ir-convert form lexenv)) rest)
       :outputs outputs))
    (values-list outputs)))

(defmethod ir-convert-compound-form
    ((_ (eql 'macrolet)) rest lexenv expected-values)
  ;; We have already processed everything with MACROEXPAND-ALL, so we can
  ;; just convert the macrolet's body.
  (ir-convert `(locally ,@(rest rest)) lexenv expected-values))

(defmethod ir-convert-compound-form
    ((_ (eql 'function)) rest lexenv expected-values)
  (trivia:match rest
    ((list (and function-name (type function-name)))
     (let* ((env (lexenv-parent lexenv))
            (frecords (lexenv-frecords lexenv))
            (frecord (find function-name frecords :key #'frecord-name :test #'equal)))
       (if (not (null frecord))
           (frecord-value frecord)
           (multiple-value-bind (kind localp alist)
               (trivial-cltl2:function-information function-name env)
             (declare (ignore localp alist))
             (case kind
               (:special-form
                (error "Invalid reference to the special form ~S."
                       function-name))
               (:function
                (values
                 (alexandria:ensure-gethash
                  function-name
                  *function-references*
                  (let ((value (make-instance 'ir-value)))
                    (push-node
                     (make-instance 'ir-construct
                       :form `(function ,function-name)
                       :outputs (list value)))
                    value))))
               (t
                (error "Reference to the undefined function ~S."
                       function-name)))))))
    ((list (list* 'lambda (list* lambda-list) body))
     (when (intersection lambda-list lambda-list-keywords)
       (error "Lambda list keywords aren't supported, yet."))
     (multiple-value-bind (forms declarations) (alexandria:parse-body body)
       (declare (ignore declarations))
       (let* ((value (make-instance 'ir-value))
              (arguments
                (loop repeat (length lambda-list)
                      collect (make-instance 'ir-value)))
              (lexenv
                (augment-lexenv
                 lexenv
                 (loop for variable in lambda-list
                       for value in arguments
                       collect (make-vrecord variable value))
                 '()))
              (body-node
                (let* ((initial-node (make-instance 'ir-initial-node))
                       (*predecessor* initial-node))
                  (ir-convert `(locally ,@forms) lexenv '*)
                  initial-node)))
         (push-node (make-instance 'ir-enclose
                      :outputs (list value)
                      :arguments arguments
                      :body body-node))
         value)))
    (_ (error "Malformed function special form :~S."
              `(function ,@rest)))))

(defmethod ir-convert-compound-form
    ((_ (eql 'progn)) rest lexenv expected-values)
  (ir-convert-progn rest lexenv expected-values))

(defun ir-convert-progn (forms lexenv expected-values)
  (dolist (form (butlast forms))
    (ir-convert form lexenv 0))
  (ir-convert (first (last forms)) lexenv expected-values))

(defmethod ir-convert-compound-form
    ((_ (eql 'locally)) rest lexenv expected-values)
  (multiple-value-bind (body-forms declarations)
      (alexandria:parse-body rest)
    (declare (ignore declarations))
    (ir-convert-progn body-forms lexenv expected-values)))

(defmethod ir-convert-compound-form
    ((_ (eql 'let)) rest lexenv expected-values)
  (unless (and (consp rest) (listp (first rest)))
    (error "Malformed let form: ~S" `(let ,@rest)))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn
     forms
     (augment-lexenv
      lexenv
      (loop for (name form) in (mapcar #'canonicalize-binding (first rest))
            collect
            (make-vrecord name (ir-convert form lexenv)))
      '())
     expected-values)))

(defmethod ir-convert-compound-form
    ((_ (eql 'let*)) rest lexenv expected-values)
  (unless (and (consp rest) (listp (first rest)))
    (error "Malformed let* form: ~S" `(let* ,@rest)))
  (loop for (name form) in (mapcar #'canonicalize-binding (first rest)) do
    (setf lexenv
          (augment-lexenv
           lexenv
           (list (make-vrecord name (ir-convert form lexenv)))
           '())))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn forms lexenv expected-values)))

(defun canonicalize-binding (binding)
  (trivia:match binding
    ((type variable-name)
     (list binding nil))
    ((list (and name (type variable-name)) form)
     (list name form))
    (_ (error "Malformed binding: ~S" binding))))

(defmethod ir-convert-compound-form
    ((_ (eql 'flet)) rest lexenv expected-values)
  (unless (and (consp rest)
               (listp (first rest)))
    (error "Malformed flet form: ~S" `(flet ,@rest)))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn
     forms
     (augment-lexenv
      lexenv
      '()
      (loop for definition in (first rest)
            collect
            (trivia:match definition
              ((list* (and function-name (type function-name)) (list* lambda-list) body)
               (make-frecord
                function-name
                (ir-convert `(function (lambda ,lambda-list ,@body)) lexenv)))
              (_ (error "Malformed flet definition: ~S"
                        definition)))))
     expected-values)))

(defmethod ir-convert-compound-form
    ((_ (eql 'the)) rest lexenv expected-values)
  (unless (= 2 (length rest))
    (error "Malformed the form: ~S" `(the ,@rest)))
  ;; TODO
  (ir-convert (second rest) lexenv expected-values))

(defmethod ir-convert-compound-form
    ((_ (eql 'if)) rest lexenv expected-values)
  (unless (<= 2 (length rest) 3)
    (error "Malformed IF form: ~S" `(if ,@rest)))
  (destructuring-bind (test then &optional else) rest
    (let* ((test-value (ir-convert test lexenv))
           (node (make-instance 'ir-node
                   :inputs (list test-value)
                   :outputs '())))
      (multiple-value-bind (then-outputs then-node)
          (let* ((initial-node (make-instance 'initial-node))
                 (*predecessor* initial-node)
                 (*dominator* node))
            (values
             (multiple-value-list
              (ir-convert then lexenv expected-values))
             initial-node))
        (multiple-value-bind (else-outputs else-node)
            (let* ((initial-node (make-instance 'initial-node))
                   (*predecessor* initial-node)
                   (*dominator* node))
              (values
               (multiple-value-list
                (ir-convert else lexenv expected-values))
               initial-node))
          (let ((outputs
                  ;; TODO
                  (loop for then-output in then-outputs
                        for else-output in else-outputs
                        collect
                        (make-instance 'ir-value
                          :derived-type
                          `(or ,(ir-value-derived-type then-output)
                               ,(ir-value-derived-type else-output))))))
            (change-class node 'ir-if
              :then-node then-node
              :else-node else-node
              :outputs outputs)
            (push-node node)
            (values-list outputs)))))))

(defmethod ir-convert-compound-form
    ((_ (eql '%for)) rest lexenv expected-values)
  (declare (ignore expected-values)) ; Loops return nothing.
  (destructuring-bind (quoted-variable-name start end step body-form) rest
    (let* ((variable-name (second quoted-variable-name))
           (variable (make-instance 'ir-value))
           (body (make-instance 'ir-initial-node))
           (loop (make-instance 'ir-loop
                   :inputs (list (ir-convert start lexenv)
                                 (ir-convert end lexenv)
                                 (ir-convert step lexenv))
                   :outputs '()
                   :variable variable
                   :body body)))
      (let ((*dominator* loop)
            (*predecessor* body)
            (lexenv (augment-lexenv lexenv (list (make-vrecord variable-name variable)) '())))
        (ir-convert body-form lexenv 0))
      (push-node loop)
      (values))))
