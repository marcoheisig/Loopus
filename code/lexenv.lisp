(in-package #:Loopus)

;;; The lexical environments used for IR conversion.

(defgeneric lexenv-frecord (lexenv function-name))

(defgeneric lexenv-vrecord (lexenv variable-name))

(defgeneric augment-lexenv (lexenv vrecords frecords))

(defclass lexenv ()
  (;; The host environment surrounding this lexenv.
   (%parent
    :initarg :parent
    :initform nil
    :reader lexenv-parent)
   (%vrecords
    :initarg :vrecords
    :initform '()
    :reader lexenv-vrecords)
   (%frecords
    :initarg :frecords
    :initform '()
    :reader lexenv-frecords)))

(defun make-lexenv (&optional environment)
  (make-instance 'lexenv
    :parent environment))

(defmethod augment-lexenv (lexenv vrecords frecords)
  (make-instance 'lexenv
    :parent (lexenv-parent lexenv)
    :vrecords (append vrecords (lexenv-vrecords lexenv))
    :frecords (append frecords (lexenv-frecords lexenv))))

(defclass frecord ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type function-name
    :reader frecord-name)
   (%value
    :initarg :value
    :initform (alexandria:required-argument :value)
    :reader frecord-value)))

(defun make-frecord (function-name value)
  (check-type function-name function-name)
  (check-type value ir-value)
  (make-instance 'frecord
    :name function-name
    :value value))

(defun frecordp (x)
  (typep x 'frecord))

(defclass vrecord ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type variable-name
    :reader vrecord-name)
   (%value
    :initarg :value
    :initform (alexandria:required-argument :value)
    :reader vrecord-value)))

(defun make-vrecord (variable-name value)
  (check-type variable-name variable-name)
  (check-type value ir-value)
  (make-instance 'vrecord
    :name variable-name
    :value value))

(defun vrecordp (x)
  (typep x 'vrecord))
