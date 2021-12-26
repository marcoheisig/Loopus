(in-package #:loopus)

;;; Variables

(defvar *default-polynomial-representation* 'expanded-polynomial)

;;; Generic Functions

(defgeneric polynomialp (x))

(defgeneric one-arg-polynomial- (p))

(defgeneric two-arg-polynomial+ (p1 p2))

(defgeneric two-arg-polynomial* (p1 p2))

(defgeneric polynomial-expression (p))

(defgeneric coerce-to-polynomial (x representation))

(defclass polynomial ()
  ())

;;; Methods

(defmethod print-object ((polynomial polynomial) stream)
  (print-unreadable-object (polynomial stream :type t :identity nil)
    (write (expression-from-polynomial polynomial) :stream stream)))

(defmethod polynomialp ((object t)) nil)

(defmethod polynomialp ((polynomial polynomial)) t)

(defmethod two-arg-polynomial+ ((p1 polynomial) (p2 polynomial))
  (if (eq (class-of p1) (class-of p2))
      (call-next-method)
      (two-arg-polynomial+ p1 (coerce-to-polynomial p2 p1))))

(defmethod two-arg-polynomial* ((p1 polynomial) (p2 polynomial))
  (if (eq (class-of p1) (class-of p2))
      (call-next-method)
      (two-arg-polynomial* p1 (coerce-to-polynomial p2 p1))))

(defmethod coerce-to-polynomial ((x t) (representation symbol))
  (let ((class (find-class representation nil)))
    (if (not class)
        (call-next-method)
        (coerce-to-polynomial x (class-prototype class)))))

(defmethod coerce-to-polynomial ((expression list) (representation polynomial))
  (polynomial-from-expression expression))

(defmethod coerce-to-polynomial ((x polynomial) (prototype polynomial))
  (if (typep x (class-of prototype))
      x
      (call-next-method)))

(defmethod coerce-to-polynomial :around ((x t) (prototype polynomial))
  (let ((result (call-next-method)))
    (unless (typep result (class-of prototype))
      (error "Returned a ~A where a ~A was expected.  This is a bug."
             (class-name (class-of result))
             (class-name (class-of prototype))))
    result))

;;; Functions

(defun polynomial (object &optional (representation nil representation-supplied-p))
  (if (not representation-supplied-p)
      (if (polynomialp object)
          object
          (coerce-to-polynomial object *default-polynomial-representation*))
      (coerce-to-polynomial object representation)))

(defun polynomial- (polynomial &rest more-polynomials)
  (if (null more-polynomials)
      (one-arg-polynomial- polynomial)
      (reduce #'two-arg-polynomial+
              more-polynomials
              :key #'one-arg-polynomial-
              :initial-value polynomial)))

(defun polynomial+ (&rest polynomials)
  (case (length polynomials)
    (0 (polynomial 0))
    (1 (polynomial (first polynomials)))
    (otherwise
     (reduce #'two-arg-polynomial+ polynomials))))

(defun polynomial* (&rest polynomials)
  (case (length polynomials)
    (0 (polynomial 1))
    (1 (polynomial (first polynomials)))
    (otherwise
     (reduce #'two-arg-polynomial* polynomials))))

(defun polynomial-from-expression
    (expression &key (environment '()) (representation *default-polynomial-representation*))
  (labels ((lookup (symbol)
             (let ((entry (assoc symbol environment)))
               (if (consp entry)
                   (cdr entry)
                   (let ((variable (make-instance 'variable :name symbol)))
                     (push (cons symbol variable) environment)
                     variable))))
           (convert (x)
             (if (atom x)
                 (if (symbolp x)
                     (coerce-to-polynomial (lookup x) representation)
                     (coerce-to-polynomial x representation))
                 (let ((operator (first x))
                       (arguments (mapcar #'convert (rest x))))
                   (case operator
                     (+ (apply #'polynomial+ arguments))
                     (- (apply #'polynomial- arguments))
                     (* (apply #'polynomial* arguments))
                     (otherwise
                      (error "Cannot convert calls to ~A into a polynomial."
                             operator)))))))
    (convert expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expanded Polynomial
;;;
;;; Each expanded polynomial is represented as a list of addends.  Each
;;; added is a list of the form (C V1 ... VN), where C is an integer and
;;; where V1 to VN are (possibly duplicate) variables.  The value of an
;;; addend is computed by multiplying the integer C and the values of each
;;; variable V1 to VN.  The value of the entire polynomial is computed by
;;; summing up the values of each addend.
;;;
;;; By convention, each addend must have a nonzero first entry, and all
;;; variables must be sorted by their number such that variables with a
;;; lower number come first.  Furthermore, no two addends of an expanded
;;; polynomial must have the same list of variables, and all addends must
;;; be sorted such that for any two addends, the one a lower number in the
;;; first differing variable comes first.

(defclass expanded-polynomial (polynomial)
  ((%addends
    :initarg :addends
    :initform (alexandria:required-argument :addends)
    :type list
    :reader expanded-polynomial-addends)))

(defun make-addend (constant variables)
  (list* constant variables))

(defun addend-constant (addend)
  (the integer (first addend)))

(defun addend-variables (addend)
  (the list (rest addend)))

(defun canonicalize-addend (addend)
  (let ((constant (addend-constant addend))
        (variables (addend-variables addend)))
    (if (loop for (v1 v2) on variables
              until (not v2)
              always (<= (variable-number v1)
                         (variable-number v2)))
        addend
        (make-addend constant (sort (copy-list variables) #'<= :key #'variable-number)))))

(defun addend<= (a1 a2)
  (let ((a1-variables (addend-variables a1))
        (a2-variables (addend-variables a2)))
    (loop
      (when (null a1-variables) (return t))
      (when (null a2-variables) (return nil))
      (let ((v1 (pop a1-variables))
            (v2 (pop a2-variables)))
        (unless (eq v1 v2)
          (unless (<= (variable-number v1)
                      (variable-number v2))
            (return nil)))))))

(defun canonicalize-addends (addends)
  (let ((result '())
        (previous nil))
    (loop for addend in (sort (mapcar #'canonicalize-addend addends) #'addend<=) do
      (cond ((not previous)
             (setf previous addend))
            ((equal (addend-variables previous)
                    (addend-variables addend))
             (setf previous
                   `(,(+ (addend-constant previous)
                         (addend-constant addend))
                     ,@(addend-variables previous))))
            (t
             (unless (zerop (addend-constant previous))
               (push previous result))
             (setf previous addend))))
    (unless (or (not previous)
                (zerop (addend-constant previous)))
      (push previous result))
    result))

(defmethod two-arg-polynomial+
    ((p1 expanded-polynomial)
     (p2 expanded-polynomial))
  (make-instance 'expanded-polynomial
    :addends
    (canonicalize-addends
     (append (expanded-polynomial-addends p1)
             (expanded-polynomial-addends p2)))))

(defmethod one-arg-polynomial-
    ((p expanded-polynomial))
  (make-instance 'expanded-polynomial
    :addends
    (loop for addend in (expanded-polynomial-addends p)
          collect
          (make-addend (- (addend-constant addend))
                       (addend-variables addend)))))

(defmethod two-arg-polynomial*
    ((p1 expanded-polynomial)
     (p2 expanded-polynomial))
  (make-instance 'expanded-polynomial
    :addends
    (let ((addends '()))
      (loop for a1 in (expanded-polynomial-addends p1) do
        (loop for a2 in (expanded-polynomial-addends p2) do
          (push (make-addend (* (addend-constant a1)
                                (addend-constant a2))
                             (append (addend-variables a1)
                                     (addend-variables a2)))
                addends)))
      (canonicalize-addends addends))))

(defmethod coerce-to-polynomial
    ((integer integer)
     (prototype expanded-polynomial))
  (make-instance 'expanded-polynomial
    :addends (if (zerop integer)
                 (list)
                 (list (make-addend integer '())))))

(defmethod coerce-to-polynomial
    ((variable variable)
     (prototype expanded-polynomial))
  (make-instance 'expanded-polynomial
    :addends (list (make-addend 1 (list variable)))))

(defmethod expression-from-polynomial
    ((p expanded-polynomial))
  (expression-from-addends (expanded-polynomial-addends p)))

(defun expression-from-addends (addends)
  (cond ((null addends)
         0)
        ((null (rest addends))
         (expression-from-addend (first addends)))
        (t
         `(+ ,@(mapcar #'expression-from-addend addends)))))

(defun expression-from-addend (addend)
  (let ((constant (addend-constant addend))
        (variables (addend-variables addend)))
    (cond ((zerop constant)
           0)
          ((null variables)
           constant)
          ((and (null (cdr variables))
                (= constant 1))
           (expression-from-variable (first variables)))
          ((= constant 1)
           `(* ,@(mapcar #'expression-from-variable variables)))
          (t
           `(* ,constant ,@(mapcar #'expression-from-variable variables))))))
