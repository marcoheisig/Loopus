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
