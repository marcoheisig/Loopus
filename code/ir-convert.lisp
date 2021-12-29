(in-package #:loopus)

;;; This file contains the machinery for turning a loop nest into graph of
;;; IR blocks and nodes.  This is essentially a variation of EVAL for a
;;; very limited subset of Common Lisp, except that it doesn't actually
;;; compute a result but assemble data flow graph nodes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexical Environments

(defgeneric lexenv-frecord (lexenv function-name))

(defgeneric lexenv-vrecord (lexenv variable-name))

(defgeneric lexenv-add (lexenv records))

(defclass lexenv ()
  ((%records
    :initform '()
    :reader lexenv-records)))

(defclass record ()
  ())

(defclass frecord (record)
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type function-name
    :reader frecord-name)
   (%lambda-list
    :initarg :lambda-list
    :initform (alexandria:required-argument :lambda-list)
    :reader frecord-lambda-list)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :reader frecord-body)
   (%lexenv
    :initarg :lexenv
    :initform (alexandria:required-argument :lexenv)
    :reader frecord-lexenv)))

(defclass vrecord (record)
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type variable-name
    :reader vrecord-name)
   (%variable
    :initarg :variable
    :initform (alexandria:required-argument :variable)
    :type variable
    :reader vrecord-variable)))

(defun frecordp (x)
  (typep x 'frecord))

(defun vrecordp (x)
  (typep x 'vrecord))

(defmethod lexenv-frecord ((lexenv lexenv) function-name)
  (check-type function-name function-name)
  (find-if
   (lambda (record)
     (and (frecordp record)
          (equal (frecord-name record) function-name)))
   (lexenv-records lexenv)))

(defmethod lexenv-vrecord ((lexenv lexenv) variable-name)
  (check-type variable-name variable-name)
  (find-if
   (lambda (record)
     (and (vrecordp record)
          (eq (vrecord-name record) variable-name)))
   (lexenv-records lexenv)))

(defmethod lexenv-add (lexenv records)
  (make-instance 'lexenv
    :records (append records (lexenv-records lexenv))))

(defvar *null-lexenv* (make-instance 'lexenv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Conversion

(defgeneric ir-convert-atom (atom lexenv))

(defgeneric ir-convert-compound-form (operator rest lexenv))

(defgeneric ir-convert-funcall (function-name arguments lexenv))

(defun ir-convert (form lexenv)
  (if (atom form)
      (ir-convert-atom form lexenv)
      (ir-convert-compound-form (first form) (rest form) lexenv)))

(defmethod ir-convert-compound-form
    ((symbol symbol) rest lexenv)
  (if (special-operator-p symbol)
      (error "Cannot handle the special operator ~S." symbol)
      (ir-convert-funcall
       symbol
       (mapcar (lambda (form) (ir-convert form lexenv)) rest)
       lexenv)))

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
    (ir-convert-progn body-forms lexenv)))

(defmethod ir-convert-compound-form
    ((_ (eql 'let)) rest lexenv)
  (break "TODO"))

(defmethod ir-convert-compound-form
    ((_ (eql 'let*)) rest lexenv)
  (break "TODO"))

(defmethod ir-convert-compound-form
    ((_ (eql 'flet)) rest lexenv)
  (break "TODO"))

(defmethod ir-convert-compound-form
    ((_ (eql 'the)) rest lexenv)
  (break "TODO"))

(defmethod ir-convert-compound-form
    ((_ (eql 'function)) rest lexenv)
  (error "Cannot optimize the FUNCTION special form in this position."))

(defmethod ir-convert-compound-form
    ((_ (eql 'funcall)) rest lexenv)
  (flet ((convert (x) (ir-convert x lexenv)))
    (when (null rest)
      (error "FUNCALL requires at least one argument."))
    (let ((function-form (first rest))
          (arguments (rest rest)))
      (unless (and (consp function-form)
                   (= (list-length function-form) 2)
                   (member (first function-form) '(function quote)))
        (error "Cannot handle calls to ~S." function-form))
      (let ((callee (second function-form)))
        ;; Process a LAMBDA expression as the equivalent LET expression.
        (if (eq (first function-form) 'function)
            (if (lambda-expression-p callee)
                (let ((lambda-list (second callee))
                      (body (rest (rest callee))))
                  (unless (null (intersection lambda-list lambda-list-keywords))
                    (error "Cannot handle the lambda list ~S."
                           lambda-list))
                  (unless (= (length lambda-list) (length rest))
                    (error "Wrong number of arguments in ~S."
                           (list* 'funcall rest)))
                  (convert `(let ,(mapcar #'list lambda-list rest) ,@body)))
                (ir-convert-funcall callee (mapcar #'convert arguments) lexenv))
            (ir-convert-funcall callee (mapcar #'convert arguments) *null-lexenv*))))))
