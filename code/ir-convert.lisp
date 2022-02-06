(in-package #:loopus)

;;; This file contains the machinery for converting a loop nest to Loopus
;;; IR.  This is essentially a variation of EVAL for a very limited subset
;;; of Common Lisp, except that it doesn't actually compute a result but
;;; assemble IR nodes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexical Environments

(defgeneric lexenv-frecord (lexenv function-name))

(defgeneric lexenv-vrecord (lexenv variable-name))

(defgeneric lexenv-add (lexenv vrecords frecords))

(defclass lexenv ()
  (;; The host environment surrounding this lexenv.
   (%parent
    :initform nil
    :reader lexenv-parent)
   (%frecord-table
    :initform (make-hash-table :test #'equal)
    :reader lexenv-frecord-table)
   (%vrecord-table
    :initform (make-hash-table :test #'eq)
    :reader lexenv-vrecord-table)
   (%vrecords
    :initform '()
    :reader lexenv-vrecords)
   (%frecords
    :initform '()
    :reader lexenv-frecords)))

(defclass frecord ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type function-name
    :reader frecord-name)
   (%value
    :initarg :value
    :initform (alexandria:required-argument :value)
    :type (or ir-function ir-unknown)
    :reader frecord-value)))

(defun make-frecord (function-name value)
  (check-type function-name function-name)
  (check-type value ir-value)
  (make-instance 'frecord
    :name function-name
    :value value))

(defun frecordp (x)
  (typep x 'frecord))

(defmethod lexenv-frecord ((lexenv lexenv) function-name)
  (check-type function-name function-name)
  (or (find-if
       (lambda (frecord)
         (equal (frecord-name frecord) function-name))
       (lexenv-frecords lexenv))
      (multiple-value-bind (kind localp alist)
          (trivial-cltl2:function-information function-name (lexenv-parent lexenv))
        (declare (ignore localp alist))
        (case kind
          (:special-form
           (error "Invalid reference to the special form ~S."
                  function-name))
          (:function
           (alexandria:ensure-gethash
            function-name
            (lexenv-frecord-table lexenv)
            (make-frecord
             function-name
             (make-instance 'ir-unknown
               :form `(function ,function-name)))))
          (t
           (error "Reference to the undefined function ~S."
                  function-name))))))

(defclass vrecord ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type variable-name
    :reader vrecord-name)))

(defun make-vrecord (variable-name value)
  (check-type variable-name variable-name)
  (check-type value ir-value)
  (make-instance 'vrecord
    :name variable-name
    :value value))

(defun vrecordp (x)
  (typep x 'vrecord))

(defmethod lexenv-vrecord ((lexenv lexenv) variable-name)
  (check-type variable-name variable-name)
  (or
   (find-if
    (lambda (record)
      (and (vrecordp record)
           (eq (vrecord-name record) variable-name)))
    (lexenv-vrecords lexenv))
   (let ((env (lexenv-parent lexenv)))
     (multiple-value-bind (kind localp alist)
         (trivial-cltl2:variable-information variable-name env)
       (declare (ignore localp alist))
       (case kind
         (:special
          (error "Cannot (yet) handle special variables."))
         ((:lexical :global)
          (alexandria:ensure-gethash
           variable-name
           (lexenv-vrecord-table lexenv)
           (make-vrecord
            variable-name
            (make-instance 'ir-unknown
              :form variable-name))))
         (:constant
          (alexandria:ensure-gethash
           variable-name
           (lexenv-vrecord-table lexenv)
           (make-vrecord
            variable-name
            (if (constantp variable-name)
                (make-instance 'ir-constant
                  :value (eval variable-name))
                (make-instance 'ir-unknown
                  :name variable-name)))))
         (t
          (error "Reference to undefined variable ~S."
                 variable-name)))))))

(defmethod lexenv-add (lexenv vrecords frecords)
  (make-instance 'lexenv
    :parent (lexenv-parent lexenv)
    :vrecord-table (lexenv-vrecord-table lexenv)
    :frecord-table (lexenv-frecord-table lexenv)
    :vrecords (append vrecords (lexenv-vrecords lexenv))
    :frecords (append frecords (lexenv-frecords lexenv))))

(defun make-lexenv (&optional environment)
  (make-instance 'lexenv
    :parent environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Conversion

(defvar *current-block*)

(defgeneric ir-convert-atom (atom lexenv))

(defgeneric ir-convert-compound-form (operator rest lexenv))

(defgeneric ir-convert-funcall (function-name arguments))

(defun ir-convert (form lexenv)
  (if (atom form)
      (ir-convert-atom form lexenv)
      (ir-convert-compound-form (first form) (rest form) lexenv)))

(defun ir-convert-in-environment (form env)
  (let ((*current-block* (make-instance 'ir-block)))
    (ir-convert
     (trivial-macroexpand-all:macroexpand-all
      ;; Each occurrence of the FOR macro is turned into a call to the
      ;;  function %FOR.  This function is not defined, but handled
      ;;  specially by the IR conversion process.
      `(macrolet ((for ((variable start end &optional (step 1)) &body body)
                    `(%for ',variable ,start ,end ,step (locally ,@body))))
         ,form)
      env)
     (make-lexenv env))))

(defmethod ir-convert-compound-form
    ((symbol symbol) rest lexenv)
  (if (special-operator-p symbol)
      (error "Cannot handle the special operator ~S." symbol)
      (ir-convert-funcall
       symbol
       (mapcar (lambda (form) (ir-convert form lexenv)) rest))))

(defmethod ir-convert-compound-form
    ((_ (eql 'function)) rest lexenv)
  (trivia:match rest
    ((list (type function-name))
     (make-instance 'ir-unknown
       :form `(function ,@rest)))
    ((list (list* 'lambda (list* lambda-list) body))
     (multiple-value-bind (forms declarations) (alexandria:parse-body body)
       (declare (ignore declarations))
       (make-instance 'ir-function
         :lexenv lexenv
         :lambda-list lambda-list
         :body forms)))
    (_ (error "Malformed function special form :~S."
              `(function ,@rest)))))

(defmethod ir-convert-compound-form
    ((_ (eql 'progn)) rest lexenv)
  (ir-convert-progn rest lexenv))

(defun ir-convert-progn (forms lexenv)
  (loop for form in (butlast forms) do
    (ir-convert form lexenv))
  (ir-convert (first (last forms)) lexenv))

(defmethod ir-convert-compound-form
    ((_ (eql 'locally)) rest lexenv)
  (multiple-value-bind (body-forms declarations) (parse-body rest)
    (declare (ignore declarations))
    (ir-convert-progn body-forms lexenv)))

(defmethod ir-convert-compound-form
    ((_ (eql 'let)) rest lexenv)
  (unless (and (consp rest) (listp (first rest)))
    (error "Malformed let form: ~S" `(let ,@rest)))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn
     forms
     (lexenv-add
      lexenv
      (loop for (name form) in (mapcar #'canonicalize-binding (first rest))
            collect
            (make-vrecord name (ir-convert form lexenv)))
      '()))))

(defmethod ir-convert-compound-form
    ((_ (eql 'let*)) rest lexenv)
  (unless (and (consp rest) (listp (first rest)))
    (error "Malformed let* form: ~S" `(let* ,@rest)))
  (loop for (name form) in (mapcar #'canonicalize-binding (first rest)) do
    (setf lexenv
          (lexenv-add
           lexenv
           (list (make-vrecord name (ir-convert form lexenv)))
           '())))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn forms lexenv)))

(defun canonicalize-binding (binding)
  (trivia:match binding
    ((type variable-name)
     (list binding nil))
    ((list (and name (type variable-name)) form)
     (list name form))
    (_ (error "Malformed binding: ~S" binding))))

(defmethod ir-convert-compound-form
    ((_ (eql 'flet)) rest lexenv)
  (unless (and (consp rest)
               (listp (first rest)))
    (error "Malformed flet form: ~S" `(flet ,@rest)))
  (multiple-value-bind (forms declarations) (alexandria:parse-body (rest rest))
    (declare (ignore declarations))
    (ir-convert-progn
     forms
     (lexenv-add
      lexenv
      '()
      (loop for definition in (first rest)
            collect
            (trivia:match definition
              ((list* (and function-name (type function-name))
                      (list* lambda-list)
                      body)
               (multiple-value-bind (forms declarations) (alexandria:parse-body body)
                 (declare (ignore declarations))
                 (make-frecord
                  function-name
                  (make-instance 'ir-function
                    :lambda-list lambda-list
                    :body forms
                    :lexenv lexenv))))
              (_ (error "Malformed flet definition: ~S"
                        definition))))))))

(defmethod ir-convert-compound-form
    ((_ (eql 'the)) rest lexenv)
  (unless (= 2 (length rest))
    (error "Malformed the form: ~S" `(the ,@rest)))
  (ir-convert (second rest) lexenv))

(defmethod ir-convert-compound-form
    ((_ (eql '%for)) rest lexenv)
  (destructuring-bind (quoted-variable start end step body-form) rest
    (let* ((variable-name (second quoted-variable))
           (variable
             (make-instance 'ir-loop-variable
               :name variable-name))
           (*current-block*
             (make-instance 'ir-loop
               :variable variable
               :start (ir-convert start lexenv)
               :end (ir-convert end lexenv)
               :step (ir-convert step lexenv)
               :immediate-dominator *current-block*)))
      (setf (slot-value variable '%loop) *current-block*)
      (ir-convert
       body-form
       (lexenv-add lexenv (list (make-vrecord variable-name variable)) '())))))
