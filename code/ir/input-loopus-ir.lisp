(ql:quickload :cl-isl)

(in-package :loopus.ir)

(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))
(declaim (optimize (debug 3)))

;; UTILITIES
(defun ins (e)
  (break "Inspect ~a" e))

;; To create points on the domain side
;; 1 global time
;; 1 for each variable
(defparameter *size-domain* 5)
(defparameter *space-domain* (isl:create-space-set 0 *size-domain*))

;; To create points on the range side
(defparameter *size-range* 5)
;; The max of the size needed for each read/write
;; A[i, j] consumes 3 spot
(defparameter *space-range* (isl:create-space-set 0 *size-range*))

;; The space of maps (domain -> range)
(defparameter *space-map-domain-range* (isl:create-space-map 0 *size-domain* *size-range*))

;; The space of schedule (domain -> domain)
(defparameter *space-map-schedule* (isl:create-space-map 0 *size-domain* *size-domain*))

;; Definition of variables that will hold the set/map of domain/read/write/schedule
;; defparameter
(defparameter *set-domain* (isl:union-set-empty *space-domain*))
(defparameter *map-read* (isl:union-map-empty *space-map-domain-range*))
(defparameter *map-write* (isl:union-map-empty *space-map-domain-range*))
(defparameter *map-schedule* (isl:union-map-empty *space-map-schedule*))



;; Modify a map to add it another map (union of both) (same for push-set)
(defmacro push-map (map object)
  ;; Todo, verify the space of the point is the same as the space of the map ?
  `(setf ,map (isl:union-map-union ,map ,object)))
(defmacro push-set (set object)
  `(setf ,set (isl:union-set-union ,set ,object)))



;; Generate an unique number for each ir-node
(defparameter *counter-range* nil) ; the value they'll have. It's just increment by 1 each time
(defparameter *all-irnodes* nil) ; the map ir-node -> int (this unique value)
(defun uniquenumber (producer)
  (let ((v (if ; todo refactor ?
            (ir-construct-p producer)
            (ir-construct-form producer)
            producer)))
    (values
     (alexandria:ensure-gethash v *all-irnodes* (incf *counter-range*)))))

;; List of all loop-variables of loops we are currently in
(defparameter *loop-variables* nil)
(defun is-loop-variable (node)
  (position node *loop-variables*))

;; List of loop bounds
(defparameter *loop-bounds* nil)


;;;;;;;;;;;;;;;
;; DOMAIN
;;;;;;;;;;;;;;;

;; To create the set [*counter*, loop-var] : start <= loop-var < end
;; We start from universe { [*, *] }
;; Add the first constraint to have { [*counter-domain*, *] }
;; Add constraint for each loop-var to have { [*counter*, loop-var] : start <= loop-var < end }

(defparameter *counter-domain* nil)
(defun create-new-point-domain ()
  ;; The structure we want to have: just below. Start from universe, and create each part
  ;; [*counter-domain* only once, i for i in loop-var, -1 for the rest]
  (let* ((result (isl:basic-set-universe *space-domain*))
         (*space-domain* (isl:local-space-from-space *space-domain*))
         ;; [*, *] - universe
         (bot (isl::make-equality-constraint *space-domain*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-set 0 (isl:value -1)))
         (_ (setf result (isl:basic-set-add-constraint result bot))))
    ;; Now we have [*counter-domain*, *]
    (loop for p below *current-depth* do
      (let* ((bounds (nth p (reverse *loop-bounds*)))
             ;; the variable at the very left is the outer loop, so it's the good order
             (start-value (first bounds))
             (end-value (second bounds))
             (bot (isl::make-inequality-constraint *space-domain*))
             (bot (isl::inequality-constraint-set-constant bot (isl:value (- 0 start-value))))
             (bot (isl::inequality-constraint-set-coefficient bot :dim-set (1+ p) (isl:value 1)))
             (_ (setf result (isl:basic-set-add-constraint result bot)))
             ;; Creation of [*, i] : start <= i
             (bot (isl::make-inequality-constraint *space-domain*))
             (bot (isl::inequality-constraint-set-constant bot (isl:value (- end-value 1))))
             (bot (isl::inequality-constraint-set-coefficient bot :dim-set (1+ p) (isl:value -1)))
             ;; Creation of [*, i] : start <= i < end
             ;; End of this iteration: [*counter-domain*, i for one more variable] : start <= i < end
             (_ (setf result (isl:basic-set-add-constraint result bot))))))
    ;; Now we have [*counter-domain*, i for i in loop-var, *]
    (loop for p from (1+ *current-depth*) below *size-domain* do
      (let* ((bot (isl::make-equality-constraint *space-domain*))
             (bot (isl::equality-constraint-set-constant bot (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient bot :dim-set p (isl:value -1)))
             (_ (setf result (isl:basic-set-add-constraint result bot))))))
    ;; Now we have [*counter-domain* only once, i for i in loop-var, -1 for the rest]
    (isl:basic-set-union-set result)))



;;;;;;;;;;;;;;;
;; READ/WRITE
;;;;;;;;;;;;;;;

;; This will get called on each instruction that can read or write
;; Todo comment

(defun get-value (node)
  (let* ((producer (ir-value-producer node)))
    (uniquenumber producer)))
(defun create-new-point-range (&rest args)
  (let* ((result (isl:basic-map-universe *space-map-domain-range*))
         (*space-map-domain-range* (isl:local-space-from-space *space-map-domain-range*))
         (bot (isl::make-equality-constraint *space-map-domain-range*)))
    ;; First the array
    (setf bot (isl::equality-constraint-set-constant bot (isl:value (get-value (first args)))))
    (setf bot (isl::equality-constraint-set-coefficient bot :dim-out 0 (isl:value -1)))
    (setf result (isl:basic-map-add-constraint result bot))
    ;; We do all arguments of the read. So if (aref a b c 1 3) we do for a b c 1 3
    (loop for idx from 1 below (length args) do
      (let ((pos-variable (is-loop-variable (nth idx args)))
            (bot (isl::make-equality-constraint *space-map-domain-range*)))
        (if pos-variable
            ;; If we are a loop variable
            (progn
              (setf bot (isl::equality-constraint-set-coefficient bot :dim-in (- (- (length args) 1) pos-variable) (isl:value -1)))
              (setf bot (isl::equality-constraint-set-coefficient bot :dim-out idx (isl:value 1))))
            ;; Otherwise, we are a constant (or more but todo)
            (progn
              ;; Parse the value it has
              (let ((value (second (ir-value-declared-type (nth idx args)))))
                (setf bot (isl::equality-constraint-set-constant bot (isl:value value)))
                (setf bot (isl::equality-constraint-set-coefficient bot :dim-out idx (isl:value -1))))))
        (setf result (isl:basic-map-add-constraint result bot))))
    ;; Fill for the rest with a single value
    (loop for p from (length args) below *size-range* do
          (let* ((bot (isl::make-equality-constraint *space-map-domain-range*))
                 (bot (isl::equality-constraint-set-constant bot (isl:value -1)))
                 (bot (isl::equality-constraint-set-coefficient bot :dim-out p (isl:value -1)))
                 (_ (setf result (isl:basic-map-add-constraint result bot))))))
    result))



;;;;;;;;;;;;;;;
;; SCHEDULE
;;;;;;;;;;;;;;;

(defun create-map-schedule (&rest args)
  (let* ((result (isl:basic-map-universe *space-map-schedule*))
         (*space-map-schedule* (isl:local-space-from-space *space-map-schedule*))
         (bot (isl::make-equality-constraint *space-map-schedule*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-in 0 (isl:value -1)))
         (result (isl:basic-map-add-constraint result bot))
         (bot (isl::make-equality-constraint *space-map-schedule*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-out (- *size-domain* 1) (isl:value -1)))
         (result (isl:basic-map-add-constraint result bot)))
    ;; Loop for each variable
    (loop for idx from 0 below *current-depth* do
      ;; Now, for each loop variable, we map it to the correct left part
      (let* ((bot (isl::make-equality-constraint *space-map-schedule*))
             (pos-variable (is-loop-variable (nth idx (first args))))
             (bot (isl::equality-constraint-set-coefficient bot :dim-in
                                                      ;(- (- *size-domain* 1)
                                                         (+ 1 pos-variable)
                                                      ;   )
                                                      (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient bot :dim-out
                                                            (- (- *size-domain* 2) idx)
                                                            (isl:value 1)))
             (_ (setf result (isl:basic-map-add-constraint result bot))))))
    ;; Loop for the rest
    (loop for idx from *current-depth* below *size-domain* do
      (let* ((bot (isl::make-equality-constraint *space-map-schedule*))
             (bot (isl::equality-constraint-set-coefficient bot :dim-in
                                                            idx (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient bot :dim-out
                                                            (- (- *size-domain* 1) idx)
                                                            (isl:value 1)))
             (_ (setf result (isl:basic-map-add-constraint result bot))))))
    (isl:basic-map-union-map result)))



;; Function that'll be mapped on all ir nodes
(defgeneric update-node (node))
(defmethod update-node ((node ir-node)))

(defmacro my-incf (v)
  `(setf ,v (* (+ 1 ,v) 2)))
(defparameter *id-to-expression* nil) ; int -> loopus node
(defparameter *depth-node* nil) ; loopus for node -> depth
(defparameter *current-depth* nil)


;; Function call
;; Right now, only check if it's aref/setf, otherwise it does nothing
(defmethod update-node ((node ir-call))
  (let* ((function-call node)
         (args (ir-node-inputs node))
         (is-aref (eql 'aref (typo:fnrecord-name (ir-call-fnrecord node))))
         (is-setf (equal '(setf aref) (typo:fnrecord-name (ir-call-fnrecord node))))
         (current-timestamp (create-new-point-domain)))
    ;; Current timestamp is the set of timestamp corresponding to this single instruction
    ;; If it's a instructon outisde a loop, the set will only have a single element
    ;; Otherwise if it's in a "i" loop, it'd be for instance { [0, i]: start <= i < end }
    ;; For each point of this set, a read/write operation is maybe performed
    ;; We want to add to *map-read/write* the map, for instance, { [0, i] -> A[i, 0] } if A[i, 0] is read

    ;; Will become (when (or "map read can be modified" "map write can be modified"))
    (when (or is-aref is-setf)
      ;; Add the loopus node to the hashtable
      (setf (gethash *counter-domain* *id-to-expression*) node)
      ;; Add to *set-domain*
      (push-set *set-domain* current-timestamp)
      ;; Add to *map-read* and/or *map-write*
      ;;todo refactor
      (let* ((new-map (apply #'create-new-point-range
                               (if is-aref
                                   ;; If it's an aref, just gives what follows aref
                                   ;; (aref a b c d e) -> args will be (a b c d e)
                                   (cons (first args) (reverse (cdr args)))
                                   ;; If it's an setf, it's ((setf aref) value a b c d e)
                                   ;; instead of (aref a b c d e) like above
                                   ;; So (cdr args) is (a b c d e)
                                   (cons (first (cdr args)) (reverse (cddr args)))
                                   )))
             (new-map (isl:basic-map-union-map new-map))
             (new-map (isl:union-map-intersect-domain new-map current-timestamp))
             )
        (when is-aref (push-map *map-read* new-map))
        (when is-setf (push-map *map-write* new-map)))
      ;; Add to *map-schedule*
      (push-map *map-schedule*
                (isl:union-map-intersect-domain
                 (create-map-schedule *loop-variables*)
                 current-timestamp))
      (my-incf *counter-domain*)
      )
    ))

;; Todo handle lexical scope
(defmethod update-node ((node ir-loop))
  ;; First, we add informations (the current loop variable, the depth, etc...)
  ;; And then, last s-expr, call recursively on the body of the loop!
  ;; List of loop variables
  (push
   (ir-loop-variable node)
   *loop-variables*)
  ;; Current depth we are in
  (setf (gethash node *depth-node*) *current-depth*)
  (incf *current-depth*)
  ;; Loop bounds
  (let* ((inputs (ir-node-inputs node))
         (start (second (ir-value-declared-type (first inputs))))
         (end (second (ir-value-declared-type (second inputs)))))
    ;; todo step too ?
    (push (list start end) *loop-bounds*))
  ;; Recursive call
  (map-block-inner-nodes #'update-node (ir-loop-body node)))




(when nil

(print "--")
;;(setf *print-case* :downcase)
;;Loops over 1D, 2D, 3D, 4D rectangle.
;;Loop over triangle.
;;Loop over tetrahedra.
;;Loop with 'funny holes' (branches that depend on the indices).
;;Space filling curves

(let* ((sym (gensym "array"))
       (myloop
         (ir-convert-in-environment



          ;;[i, j] -> i*N + j

          #+or(loopus:for (k 0 10)
             (setf (aref ,sym k)
                   ;;k
                   ;;(+ 1 2)
                   ;;(+ 1 k)
                   (+ (* k 3) k)
                   ))

          #+or(loopus:for (i 0 4)
             (+ (aref ,sym i) 1))


          ;; créer i = 1+i

          ;;(loopus:for (k 0 4)
          `(loopus:for (i 0 5)
               (loopus:for (j 0 5)
                 (setf (aref ,sym i j) (+ (aref ,sym i j) i)))) ;;(* 10 j) (* 100 i)))))
          nil)))


            #+or(loopus:for (j 0 5)
                  ;;(setf (aref C i j) 0)
                  (loopus:for (i 0 5)
                    (setf (aref C i j) (+
                                        (aref C i j)
                                        (* (aref A i k)
                                           (aref B k j))))))

  (map-block-inner-nodes #'update-node myloop)

  (print *set-domain*)
  (print *map-read*)
  (print *map-write*)
  (print *map-schedule*)


  (let ((node (isl::get-new-result *set-domain* *map-read* *map-write* *map-schedule*))
        (start-node (isl::get-initial-result  *set-domain* *map-read* *map-write* *map-schedule*)))
    (isl:pretty-print-node start-node)
    (isl:pretty-print-node node)
    (print start-node)
    (print node)

    (print "end")
    (eval `(defparameter ,sym (make-array '(5 5) :initial-element 0)))
    (eval `(print ,sym))
    (eval (print (ir-expand (my-main node nil))))
    (eval `(print ,sym))))








#|


Domain := [n] -> {
[i1, i2, 0] : 0 <= i1 <= n and 0 <= i2 <= n;
[i1, i2, 2] : 0 <= i1 <= n and 0 <= i2 <= n;
};

Read := [n] -> {
[i1, i2, 0] -> [1, i1, i2]
} * Domain;

Write := [n] -> {
[i1, i2, 2] -> [1, i1, i2]
} * Domain;

Schedule := [n] -> {
[i1, i2, 2] -> [i1, i2, 2];
[i1, i2, 0] -> [i1, i2, 0]
};

-> good ast

same but [0, i1, i2] instead of [i1, i2, 0]
-> bad ast

|#
)


