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

;; How many free variable we can have
(defparameter *size-free-parameters* 5)

;; A hack. List of all loop variables
(defparameter possible-loop-variables (mapcar #'read-from-string (list "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9")))

;; Add parameters from free variables
;; Position can be found with the hashtable
(defparameter *free-variable-to-index* (make-hash-table :test 'equal))
(loop for i below *size-free-parameters* do
  (let ((id (isl::make-gensym-identifier 'free-variable)))
    (setf (gethash (symbol-name (isl:identifier-name id)) *free-variable-to-index*) i)
    (setf *space-domain* (isl::space-add-param-id *space-domain* id))
    (setf *space-range* (isl::space-add-param-id *space-range* id))
    (setf *space-map-domain-range* (isl::space-add-param-id *space-map-domain-range* id))
    (setf *space-map-schedule* (isl::space-add-param-id *space-map-schedule* id))))

;; hashtable of ir-construct-node to position of the identifier
(defparameter *construct-to-identifier* nil) ;; ir-construct-node to position (integer)
(defparameter position-next-free-variable nil) ;; at first it's *size-domain*. Gets incf each time



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
;; The function that does what is described just above is create-new-point-domain, 2 s-expr below

;; Add a constant value to the constraint. Can be a known value or a variable
(defun add-constant-constraint (constraint value i delta)
  ;; 3 cases:
  ;; + integer -> just add the value
  ;; + loop variable -> *loop-variables* has the first value the most inner loop so we need to reverse it
  ;; + free variable -> pick from *construct-to-identifier*

  ;; Delta is to fix the 1off error because constraint are <=
  ;; It's either 0 when start <= loop-var. Or -1 when loop-var < end
  ;; When it's integer we just add it. Otherwise we add it as a constant in the inequality
  (if (integerp value)
      ;; integer
      (isl::inequality-constraint-set-constant constraint (isl:value (+ delta (* i value))))
      (let ((idx-loop-variable (position value (reverse *loop-variables*))))
        (if idx-loop-variable
            ;; loop variable
            (isl::inequality-constraint-set-coefficient
             (isl::inequality-constraint-set-constant constraint (isl:value delta)) ; Add the -1 constant in the inequality
             :dim-set (1+ idx-loop-variable)
             (isl:value i))
            (let ((idx-free-variable
                    ;; position-next-free-variable is incremented only when not found
                    (alexandria:ensure-gethash
                     (ir-construct-form (ir-value-producer value))
                     *construct-to-identifier*
                     (incf position-next-free-variable))))
              (if idx-free-variable
                  ;; free variable
                    (isl::inequality-constraint-set-coefficient
                     (isl::inequality-constraint-set-constant constraint (isl:value delta))
                     :dim-param idx-free-variable
                     (isl:value i))
                  (break "can't happen")))))))

(defparameter *counter-domain* nil)
(defun create-new-point-domain ()
  ;; The structure we want to have: see commentary above. Start from universe, and create each part
  ;; [*counter-domain* only once, i for i in loop-var, -1 for the rest] -- One s-expr for each part
  ;; *counter-domain only once part
  (let* ((result (isl:basic-set-universe *space-domain*))
         (*space-domain* (isl:local-space-from-space *space-domain*))
         ;;(result (add-all-loop-identifier result))
         ;; [*, *] - universe
         (bot (isl::make-equality-constraint *space-domain*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-set 0 (isl:value -1)))
         (_ (setf result (isl:basic-set-add-constraint result bot))))
    ;; Now we have [*counter-domain*, *]
    ;; Part for each loop var
    (loop for p below *current-depth* do
      (let* ((bounds (nth p (reverse *loop-bounds*)))
             ;; The variable at the very left is the outer loop, so it's the good order
             (start-value (first bounds))
             (end-value (second bounds))
             (bot (isl::make-inequality-constraint *space-domain*))
             (bot (add-constant-constraint bot start-value -1 0))
             (bot (isl::inequality-constraint-set-coefficient bot :dim-set (1+ p) (isl:value 1)))
             (_ (setf result (isl:basic-set-add-constraint result bot)))
             ;; Creation of [*, i] : start <= i
             (bot (isl::make-inequality-constraint *space-domain*))
             (bot (add-constant-constraint bot end-value 1 -1))
             (bot (isl::inequality-constraint-set-coefficient bot :dim-set (1+ p) (isl:value -1)))
             ;; Creation of [*, i] : start <= i < end
             ;; The "<" comes from the -1 in add-constant-constraint. We actually create i <= end - 1
             ;; End of this iteration: [*counter-domain*, i for one more variable] : start <= i < end
             (_ (setf result (isl:basic-set-add-constraint result bot))))))
    ;; Now we have [*counter-domain*, i for i in loop-var, *]
    ;; Part to fill the rest
    (loop for p from (1+ *current-depth*) below *size-domain* do
      (let* ((bot (isl::make-equality-constraint *space-domain*))
             (bot (isl::equality-constraint-set-constant bot (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient bot :dim-set p (isl:value -1)))
             (_ (setf result (isl:basic-set-add-constraint result bot))))))
    ;; Now we have [*counter-domain* only once, i for i in loop-var, -1 for the rest]. What we wanted
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

;; I'm a clown and it's just (identity domain domain) ?

(defun create-map-schedule (&rest args)
  (let* ((result (isl:basic-map-universe *space-map-schedule*))
         (*space-map-schedule* (isl:local-space-from-space *space-map-schedule*))
         (bot (isl::make-equality-constraint *space-map-schedule*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-in 0 (isl:value -1)))
         (result (isl:basic-map-add-constraint result bot))
         (bot (isl::make-equality-constraint *space-map-schedule*))
         (bot (isl::equality-constraint-set-constant bot (isl:value *counter-domain*)))
         (bot (isl::equality-constraint-set-coefficient bot :dim-out 0 (isl:value -1)))
         (result (isl:basic-map-add-constraint result bot)))
    ;; Loop for each variable
    (loop for idx from 0 below *current-depth* do
      ;; Now, for each loop variable, we map it to the correct left part
      (let* ((bot (isl::make-equality-constraint *space-map-schedule*))
             (pos-variable (is-loop-variable (nth idx (first args))))
             (bot (isl::equality-constraint-set-coefficient
                   bot
                   :dim-in (+ 1 pos-variable)
                   (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient
                   bot
                   :dim-out (1+ idx)
                   (isl:value 1)))
             (_ (setf result (isl:basic-map-add-constraint result bot))))))
    ;; Loop for the rest
    (loop for idx from *current-depth* below *size-domain* do
      (let* ((bot (isl::make-equality-constraint *space-map-schedule*))
             (bot (isl::equality-constraint-set-coefficient
                   bot
                   :dim-in idx
                   (isl:value -1)))
             (bot (isl::equality-constraint-set-coefficient
                   bot
                   :dim-out idx
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
      (let* ((what-is-read/wrote-in-order
               ;; todo comment why we do this
               (if is-aref
                   ;; If it's an aref, just gives what follows aref
                   ;; (aref a b c d e) -> args will be (a b c d e)
                   (cons (first args) (reverse (cdr args)))
                   ;; If it's an setf, it's ((setf aref) value a b c d e)
                   ;; instead of (aref a b c d e) like above
                   ;; So (cdr args) is (a b c d e)
                   (cons (first (cdr args)) (reverse (cddr args)))
                   ))
             (map-of-read/write (apply #'create-new-point-range what-is-read/wrote-in-order))
             (map-of-read/write (isl:basic-map-union-map map-of-read/write))
             (map-of-read/write (isl:union-map-intersect-domain map-of-read/write current-timestamp)))
        (when is-aref (push-map *map-read* map-of-read/write))
        (when is-setf (push-map *map-write* map-of-read/write)))
      ;; Add to *map-schedule*
      (push-map *map-schedule*
                (isl:union-map-intersect-domain
                 (create-map-schedule *loop-variables*)
                 current-timestamp))
      (my-incf *counter-domain*))))

(defun parse-bound (value)
  (if (typo.ntype:eql-ntype-p (ir-value-derived-ntype value))
      ;(typo:eql-ntype-object
      (second (ir-value-derived-type value))
      value)) ;;todo
;;      (ir-construct-form (ir-value-producer value))))

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
         (start (parse-bound (first inputs)))
         (end (parse-bound (second inputs))))
    ;;(ins end)
    ;; todo step too ?
    (push (list start end) *loop-bounds*))
  ;; Recursive call
  (map-block-inner-nodes #'update-node (ir-loop-body node)))
