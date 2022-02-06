(in-package #:loopus)

(deftype non-nil-symbol ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or non-nil-symbol (cons (eql setf) (cons non-nil-symbol null))))

(deftype variable-name ()
  'non-nil-symbol)

(defun canonicalize-binding (binding)
  (typecase binding
    (symbol
     (list binding nil))
    ((cons symbol (cons t null))
     binding)
    (otherwise
     (error "Malformed binding: ~S" binding))))

;;; Returns two values - the body forms and a list containing all
;;; declaration specifiers.
(defun parse-body (body &key (allow-documentation nil))
  (let ((declaration-specifiers '()))
    (loop for (item . rest) on body do
      ;; If documentation is allowed, skip it.
      (unless (and allow-documentation (stringp item))
        (if (and (consp item) (eq (first item) 'declare))
            (loop for declaration-specifier in (rest item) do
              (push declaration-specifier declaration-specifiers))
            (return-from parse-body
              (values rest (reverse declaration-specifiers))))))
    (values '() (reverse declaration-specifiers))))
