(in-package #:loopus.ir)

;; Some function to compute properties about the loop, and then isl-optimize

;; Max dimension of array access on read/write access (print array) doesn't count
(defgeneric compute-max-array-dimension (ir))
(defmethod compute-max-array-dimension ((ir ir-loop))
  (compute-max-array-dimension (ir-loop-body ir)))
(defmethod compute-max-array-dimension ((ir ir-if))
  (max
   (compute-max-array-dimension (ir-if-else ir))
   (compute-max-array-dimension (ir-if-else ir))))
;; todo refactor this and the update-node on ir-call in input.lisp
(defmethod compute-max-array-dimension ((ir ir-call))
  (let ((args (ir-node-inputs ir))
        (is-aref (eql 'aref (typo:fnrecord-name (ir-call-fnrecord ir))))
        (is-setf (equal '(setf aref) (typo:fnrecord-name (ir-call-fnrecord ir)))))
    (cond
      ;; args is either (aref array idx...) or ((setf aref) value array idx...)
      (is-aref (length (cdr args))) ; it's (aref array idx1 idx2 ...)
      (is-setf (length (cddr args))) ; it's ((setf aref) value array idx1 idx2), hence cddr
      (t 0))))
(defmethod compute-max-array-dimension ((ir ir-initial-node))
  (let ((value 0))
    (map-block-inner-nodes
     (lambda (ir) (setf value (max value (compute-max-array-dimension ir))))
     ir)
    value))
(defmethod compute-max-array-dimension ((ir ir-node)) 0)

;; Max number of free variable
(defgeneric compute-max-free-variable (ir))
(defmethod compute-max-free-variable ((ir ir-loop))
  (compute-max-free-variable (ir-loop-body ir)))
(defmethod compute-max-free-variable ((ir ir-if))
  (+ (compute-map-free-variable (ir-if-else ir))
     (compute-map-free-variable (ir-if-else ir))))
(defmethod compute-max-free-variable ((ir ir-initial-node))
  (let ((value 0))
    (map-block-inner-nodes
     (lambda (ir) (setf value (+ value (compute-max-free-variable ir))))
     ir)
    value))
(defmethod compute-max-free-variable ((ir ir-construct))
  1) ; todo be 1 iif it may be free variable, 0 otherwise
(defmethod compute-max-free-variable ((ir ir-node)) 0)


;; This code setf a lot of variables
;; Each of them have a quick comment of "what it does"
;; This commment is next to the "defparameter" of the variable
(defun ir-isl-optimize (ir)
  "Returns a copy of IR where it's reordered by isl"

  ;; First, allocate the memory
  (let* ((*size-domain* (1+ (* 2 (compute-max-loop-depth ir))))
         ;;(ins *size-domain*)
         (*space-domain* (isl:create-space-set 0 *size-domain*))
         (*size-range* (1+ (compute-max-array-dimension ir)))
         ;;(ins *size-range*)
         (*space-range* (isl:create-space-set 0 *size-range*))
         (*space-map-domain-range* (isl:create-space-map 0 *size-domain* *size-range*))
         (*space-map-schedule* (isl:create-space-map 0 *size-domain* *size-domain*))
         (*size-free-parameters* (compute-max-free-variable ir))
         ;;(ins *size-free-parameters*)
         ;; Add parameters from free variables
         (*free-variable-to-index* (make-hash-table :test 'equal)))
    (loop for i below *size-free-parameters* do
      (let ((id (isl:make-gensym-identifier 'free-variable)))
        (setf (gethash (symbol-name (isl:identifier-name id)) *free-variable-to-index*) i)
        (setf *space-domain* (isl:space-add-param-id *space-domain* id))
        (setf *space-range* (isl:space-add-param-id *space-range* id))
        (setf *space-map-domain-range* (isl:space-add-param-id *space-map-domain-range* id))
        (setf *space-map-schedule* (isl:space-add-param-id *space-map-schedule* id))))
    (let* ((*construct-to-identifier* nil)
           (position-next-free-variable nil)
           (*set-domain* (isl:union-set-empty *space-domain*))
           (*map-read* (isl:union-map-empty *space-map-domain-range*))
           (*map-write* (isl:union-map-empty *space-map-domain-range*))
           (*map-schedule* (isl:union-map-empty *space-map-schedule*))
           ;; End of allocation of memory

           ;; Special parameters - first phase
           (*construct-to-identifier* (make-hash-table))
           (position-next-free-variable -1) ; -1 because we use the return value of incf, so the first use returns 0
           (*counter-range* 0)
           (*all-irnodes* (make-hash-table))
           (*loop-variables* '())
           (*loop-bounds* '())
           (*counter-domain* '())
           (*global-counter* 0)
           (*id-to-expression* (make-hash-table))
           (*depth-node* (make-hash-table))
           (*current-depth* 0)
           (*node-to-read* (make-hash-table))
           (*node-to-write* (make-hash-table)))
      ;; End of setf special parameters
      ;; First phase
      (map-block-inner-nodes #'update-node ir)
      (print "Domain, read, write, and schedule:")
      (print *set-domain*)
      (print *map-read*)
      (print *map-write*)
      (print *map-schedule*)
      ;; End of first phase
      (let* ((node nil)
             (*ir-value-copies* (make-hash-table))
             (possible-loop-variables nil)
             (*id-to-nodes* (make-hash-table :test 'equal))
             (*depth-loop-variables* '())
             (*current-depth* 0)
             (*position-to-loopusvariable* (make-hash-table)))
        (maphash (lambda (key value) (setf (gethash value *position-to-loopusvariable*) key)) *construct-to-identifier*)
        ;; End of setf special parameters - Begin of second phase
        (let ((init-node (isl::generate-debug-ast *set-domain* *map-read* *map-write* *map-schedule*))
              (node (isl:generate-optimized-ast *set-domain* *map-read* *map-write* *map-schedule*)))
          (isl:pretty-print-node init-node)
          (isl:pretty-print-node node)
          (print "ok")
          (let ((r (my-main node nil)))
            (print (ir-expand r))
            r))))))

;; (defun ir-isl-optimize (ir) ir)

;; utilities - to remove
(defun ins (e)
  (break "Inspect ~a" e))
(defun ins2 (&rest rest)
  (break "Inspect ~a" rest))
