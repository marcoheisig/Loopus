(in-package #:loopus)

(defgeneric variable-name (v))

(defgeneric variable-number (v))

(let ((counter 0)
      (lock (bordeaux-threads:make-lock)))
  (defun next-variable-number ()
    (bordeaux-threads:with-lock-held (lock)
      (incf counter))))

(defclass variable ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type symbol
    :reader variable-name)
   (%number
    :initform (next-variable-number)
    :type unsigned-byte
    :reader variable-number)))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type t :identity nil)
    (write (expression-from-variable variable) :stream stream)))

(defun expression-from-variable (variable)
  (let* ((number (variable-number variable))
         (symbol (variable-name variable))
         (name (symbol-name symbol))
         (package (symbol-package symbol)))
    (intern (format nil "~A<~D>" name number)
            package)))
