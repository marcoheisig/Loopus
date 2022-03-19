(in-package #:loopus.ir)

;;; This file contains the machinery for converting a loop nest to Loopus
;;; IR.  This is essentially a variation of EVAL for a very limited subset
;;; of Common Lisp, except that it doesn't actually compute a result but
;;; assemble IR nodes.

;;; Hash tables for caching IR constructs.
(defvar *ir-convert-constants*)
(defvar *ir-convert-variables*)
(defvar *ir-convert-function-names*)

(defun ir-convert-in-environment (form env &optional (expected-values '*))
  (multiple-value-bind (initial-node final-node)
      (make-ir-initial-and-ir-final-node nil)
    (let ((*ir-convert-constants* (make-hash-table :test #'eql))
          (*ir-convert-variables* (make-hash-table :test #'eq))
          (*ir-convert-function-names* (make-hash-table :test #'equal))
          (*blocks* (cons final-node *blocks*)))
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
      (values initial-node final-node))))

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
      '*
       (loop repeat expected-values collect (make-instance 'ir-value))))

(defun ir-convert (form lexenv &optional (expected-values 1))
  (ensure-expected-values expected-values
    (if (atom form)
        (if (symbolp form)
            (ir-convert-symbol form lexenv expected-values)
            (ir-convert-constant form))
        (ir-convert-compound-form (first form) (rest form) lexenv expected-values))))

;;; Conversion of Constants

(defun ir-convert-constant (constant)
  (values
   (alexandria:ensure-gethash
    constant
    *ir-convert-constants*
    (let* ((*blocks* (last *blocks*))
           (value (make-instance 'ir-value :declared-type `(eql ,constant))))
      (make-instance 'ir-construct
        :form `',constant
        :outputs (list value))
      value))))

(defun ir-convert-variable (variable-name &optional (declared-type t))
  (values
   (alexandria:ensure-gethash
    variable-name
    *ir-convert-variables*
    (let* ((*blocks* (last *blocks*))
           (value (make-instance 'ir-value :declared-type declared-type)))
      (make-instance 'ir-construct
        :form variable-name
        :outputs (list value))
      value))))

(defun ir-convert-function (function-name)
  (values
   (alexandria:ensure-gethash
    function-name
    *ir-convert-function-names*
    (let* ((*blocks* (last *blocks*))
           (value (make-instance 'ir-value
                    :declared-type 'function
                    :derived-ntype
                    (if (fboundp function-name)
                        (typo:ntype-of (fdefinition function-name))
                        (typo:type-specifier-ntype  'function)))))
      (make-instance 'ir-construct
        :form `(function ,function-name)
        :outputs (list value))
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
          (declare (ignore localp))
          (case kind
            (:special
             (error "Cannot (yet) handle special variables."))
            ((:lexical :global :constant)
             (if (constantp variable-name)
                 (ir-convert-constant (eval variable-name))
                 (ir-convert-variable
                  variable-name
                  `(and ,@(loop for (key . value) in alist
                                when (eq key 'type)
                                  collect value)))))
            (t
             (ir-convert-variable variable-name)))))))

(defun map-declaration-specifiers (function declarations)
  (dolist (declaration declarations)
    (trivia:match declaration
      ((list* 'declare declaration-specifiers)
       (dolist (declaration-specifier declaration-specifiers)
         (funcall function declaration-specifier)))
      (_ (error "Malformed declaration: ~S" declaration)))))

(defparameter *declaration-identifiers*
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

(defun map-type-declarations (function declarations)
  (map-declaration-specifiers
   (lambda (declaration-specifier)
     (trivia:match declaration-specifier
       ((list* 'type type-specifier variables)
        (dolist (variable variables)
          (funcall function variable type-specifier)))
       ((list* type-specifier variables)
        (when (member type-specifier *declaration-identifiers*)
          (trivia.fail:fail))
        (dolist (variable variables)
          (funcall function variable type-specifier)))))
   declarations))

(defun handle-type-declarations (declarations lexenv)
  (map-type-declarations
   (lambda (variable type)
     (ir-value-declare-type (ir-convert variable lexenv) type))
   declarations))

;;; Conversion of Compound Forms

(defmethod ir-convert-compound-form
    ((operator symbol) rest lexenv expected-values)
  (ir-convert `(funcall (function ,operator) ,@rest) lexenv expected-values))

(defmethod ir-convert-compound-form
    ((_ (eql 'funcall)) rest lexenv expected-values)
  (let ((outputs (make-outputs expected-values)))
    (make-instance 'ir-call
      :fnrecord (typo:ensure-fnrecord 'funcall)
      :inputs (mapcar (lambda (form) (ir-convert form lexenv)) rest)
      :outputs outputs)
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
                (ir-convert-function function-name))
               (t
                (let ((value (make-instance 'ir-value :declared-type 'function)))
                  (make-instance 'ir-construct
                    :form `(function ,function-name)
                    :outputs (list value))
                  value)))))))
    ((list (list* 'lambda (list* lambda-list) body))
     (when (intersection lambda-list lambda-list-keywords)
       (error "Lambda list keywords aren't supported, yet."))
     (multiple-value-bind (forms declarations) (alexandria:parse-body body)
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
              (ir-enclose (make-instance 'ir-node))
              (body-node (make-ir-initial-and-ir-final-node ir-enclose)))
         (handle-type-declarations declarations lexenv)
         ;; Convert the body.
         (let ((*blocks* (cons (ir-final-node body-node) *blocks*)))
           (ir-convert `(locally ,@forms) lexenv '*))
         (change-class ir-enclose 'ir-enclose
           :outputs (list value)
           :arguments arguments
           :body body-node)
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
    (handle-type-declarations declarations lexenv)
    (ir-convert-progn body-forms lexenv expected-values)))

(defmethod ir-convert-compound-form
    ((_ (eql 'let)) rest lexenv expected-values)
  (unless (and (consp rest) (listp (first rest)))
    (error "Malformed let form: ~S" `(let ,@rest)))
  (multiple-value-bind (forms declarations)
      (alexandria:parse-body (rest rest))
    (let ((lexenv
            (augment-lexenv
             lexenv
             (loop for (name form) in (mapcar #'canonicalize-binding (first rest))
                   collect
                   (make-vrecord name (ir-convert form lexenv)))
             '())))
      (handle-type-declarations declarations lexenv)
      (ir-convert-progn forms lexenv expected-values))))

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
  (multiple-value-bind (forms declarations)
      (alexandria:parse-body (rest rest))
    (handle-type-declarations declarations lexenv)
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
  (let ((lexenv
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
                             definition)))))))
    (multiple-value-bind (forms declarations)
        (alexandria:parse-body (rest rest))
      (handle-type-declarations declarations lexenv)
      (ir-convert-progn forms lexenv expected-values))))

(defmethod ir-convert-compound-form
    ((_ (eql 'the)) rest lexenv expected-values)
  (unless (= 2 (length rest))
    (error "Malformed the form: ~S" `(the ,@rest)))
  (multiple-value-bind (required optional rest restp)
      (parse-values-type-specifier (first rest))
    (let* ((values (multiple-value-list (ir-convert (second rest) lexenv expected-values)))
           (rest values))
      (loop for type in required while values do
        (ir-value-declare-type (pop rest) type))
      (loop for type in optional while values do
        (ir-value-declare-type (pop rest) `(or ,type null)))
      (when restp
        (loop while values do
          (ir-value-declare-type (pop rest) rest)))
      (values-list values))))

(defun parse-values-type-specifier (type-specifier)
  (trivia:match type-specifier
    ((list* 'values rest)
     (let ((required '())
           (optional '())
           (rest-type nil)
           (rest-type-p nil))
       (labels ((fail ()
                  (error "Invalid values type specifier: ~S"
                         type-specifier))
                (process-required (rest)
                  (unless (null rest)
                    (let ((first (first rest))
                          (rest (rest rest)))
                      (case first
                        (&rest (process-rest rest))
                        (&optional (process-optional rest))
                        (otherwise
                         (push first required)
                         (process-required rest))))))
                (process-optional (rest)
                  (unless (null rest)
                    (let ((first (first rest))
                          (rest (rest rest)))
                      (case first
                        (&rest (process-rest rest))
                        (&optional (fail))
                        (otherwise
                         (push first optional)
                         (process-optional rest))))))
                (process-rest (rest)
                  (when (null rest) (fail))
                  (let ((first (first rest))
                        (rest (rest rest)))
                    (unless (null rest) (fail))
                    (case first
                      (&rest (fail))
                      (&optional (fail))
                      (otherwise
                       (setf rest-type-p t)
                       (setf rest-type first))))))
         (process-required rest)
         (values (reverse required)
                 (reverse optional)
                 rest-type
                 rest-type-p))))
    (_ (values (list type-specifier) '() nil nil))))

(defmethod ir-convert-compound-form
    ((_ (eql 'if)) rest lexenv expected-values)
  (unless (<= 2 (length rest) 3)
    (error "Malformed IF form: ~S" `(if ,@rest)))
  (destructuring-bind (test then &optional else) rest
    (let* ((test-value (ir-convert test lexenv))
           (ir-if (make-instance 'ir-node))
           (then-node (make-ir-initial-and-ir-final-node ir-if))
           (else-node (make-ir-initial-and-ir-final-node ir-if))
           (outputs (make-outputs expected-values)))
      (let ((*blocks* (cons (ir-final-node then-node) *blocks*)))
        (ir-convert then lexenv expected-values))
      (let ((*blocks* (cons (ir-final-node else-node) *blocks*)))
        (ir-convert else lexenv expected-values))
      (change-class ir-if 'ir-if
        :inputs (list test-value)
        :outputs outputs
        :then then-node
        :else else-node)
      (values-list outputs))))

(defmethod ir-convert-compound-form
    ((_ (eql '%for)) rest lexenv expected-values)
  (declare (ignore expected-values))    ; Loops return nothing.
  (destructuring-bind (quoted-variable-name start end step body-form) rest
    (let* ((variable-name (second quoted-variable-name))
           (variable (make-instance 'ir-value))
           (ir-loop (make-instance 'ir-node))
           (body (make-ir-initial-and-ir-final-node ir-loop))
           (start-value (ir-convert start lexenv))
           (end-value (ir-convert end lexenv))
           (step-value (ir-convert step lexenv)))
      (ir-value-declare-type variable 'fixnum)
      (let ((lexenv (augment-lexenv lexenv (list (make-vrecord variable-name variable)) '()))
            (*blocks* (cons (ir-final-node body) *blocks*)))
        (ir-convert body-form lexenv 0))
      (change-class ir-loop 'ir-loop
        :inputs (list start-value end-value step-value)
        :variable variable
        :body body)
      (values))))

(defmethod ir-convert-compound-form
    ((_ (eql 'quote)) rest lexenv expected-values)
  (unless (= 1 (length rest))
    (error "Malformed QUOTE form: ~S" `',@rest))
  (ir-convert-constant (first rest)))
