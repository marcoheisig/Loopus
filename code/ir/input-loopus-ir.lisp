(ql:quickload :cl-isl)

(in-package :loopus.ir)

(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))
(declaim (optimize (debug 3)))

;; UTILITIES
(defun ins (e)
  (break "Inspect ~a" e))


;; To create points on the domain side
(defparameter *size-domain* nil)
;; Needs to be even. 2 per loop variable
(defparameter *space-domain* nil)

;; To create points on the range side
(defparameter *size-range* nil)
;; A[i, j] consumes 3 spot (1 for the array, 1 for i, 1 for j)
(defparameter *space-range* nil)

;; The space of maps (domain -> range)
(defparameter *space-map-domain-range* nil)

;; The space of schedule (domain -> domain)
(defparameter *space-map-schedule* nil)

;; How many free variable we can have
(defparameter *size-free-parameters* nil)

;; Add parameters from free variables
;; Position can be found with the hashtable
(defparameter *free-variable-to-index* nil)

;; hashtable of ir-construct-node to position of the identifier
(defparameter *construct-to-identifier* nil) ;; ir-construct-node to position (integer)
(defparameter position-next-free-variable nil) ;; at first it's *size-domain*. Gets incf each time


;; Definition of variables that will hold the set/map of domain/read/write/schedule
(defparameter *set-domain* nil)
(defparameter *map-read* nil)
(defparameter *map-write* nil)
(defparameter *map-schedule* nil)



;; Modify a map to add it another map (union of both) (same for push-set)
(defmacro push-map (map object)
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

;; We want to create the set [*counter*1, loop-var1, *counter*2, loop-var2, ...] : start <= loop-var1 < end; start <= ...
;; We start from universe { [*, *] }
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
      (let ((idx-loop-variable (position value *loop-variables*)))
        (if idx-loop-variable
            ;; loop variable
            (isl::inequality-constraint-set-coefficient
             (isl::inequality-constraint-set-constant constraint (isl:value delta)) ; Add the -1 constant in the inequality
             :dim-set (1+ idx-loop-variable)
             (isl:value i))
            (let ((idx-free-variable
                    (alexandria:ensure-gethash ; position-next-free-variable is incremented only when not found
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
  (let* ((result (isl:basic-set-universe *space-domain*))
         (*space-domain* (isl:local-space-from-space *space-domain*)))
    ;; Part for each loop var
    (loop for p below (* 2 *current-depth*) by 2 do
      ;; First, the creation of the global counter, and then the loop variable
      (let* (;; Creation of the counter
             (constraint (isl::make-equality-constraint *space-domain*))
             (constraint (isl::equality-constraint-set-constant constraint (isl:value *counter-domain*)))
             (constraint (isl::equality-constraint-set-coefficient constraint :dim-set p (isl:value -1)))
             (_ (setf result (isl:basic-set-add-constraint result constraint)))
             ;; Creation of the variable
             (bounds (nth (/ p 2) (reverse *loop-bounds*)))
             (p (1+ p))
             ;; The variable at the very left is the outer loop, so it's the good order
             (start-value (first bounds))
             (end-value (second bounds))
             (constraint (isl::make-inequality-constraint *space-domain*))
             (constraint (add-constant-constraint constraint start-value -1 0))
             (constraint (isl::inequality-constraint-set-coefficient constraint :dim-set p (isl:value 1)))
             (_ (setf result (isl:basic-set-add-constraint result constraint)))
             ;; Creation of [*, i] : start <= i
             (constraint (isl::make-inequality-constraint *space-domain*))
             (constraint (add-constant-constraint constraint end-value 1 -1))
             (constraint (isl::inequality-constraint-set-coefficient constraint :dim-set p (isl:value -1)))
             ;; Creation of [*, i] : start <= i < end
             ;; The "<" comes from the -1 in add-constant-constraint. We actually create i <= end - 1
             ;; End of this iteration: [*counter-domain*, i for one more variable] : start <= i < end
             (_ (setf result (isl:basic-set-add-constraint result constraint))))))
    ;; Now we have [*counter-domain*, i, ...]
    ;; Part to fill the rest
    (loop for p from (* 2 *current-depth*) below *size-domain* do
      (let* ((constraint (isl::make-equality-constraint *space-domain*))
             (constraint (isl::equality-constraint-set-constant constraint (isl:value -1)))
             (constraint (isl::equality-constraint-set-coefficient constraint :dim-set p (isl:value -1)))
             (_ (setf result (isl:basic-set-add-constraint result constraint))))))
    ;; Now we have what we wanted
    (isl:basic-set-union-set result)))



;;;;;;;;;;;;;;;
;; READ/WRITE
;;;;;;;;;;;;;;;

;; This will get called on each instruction that can read or write

;; todo this + 2 variable per loop depth

;; First, create the affine expression
(defun affine-expression-from-loopus-ast (ast local-space)
  ;; maybe ir-if
  ;; todo generic function
  ;; If it's a call, we do a recursive call to ourself :-)
  (if (ir-call-p (ir-value-producer ast))
      (let* ((the-call (ir-value-producer ast))
             (a (first (ir-node-inputs the-call)))
             (b (second (ir-node-inputs the-call)))
             ;; We call recursively on a and b. "this" is the current function
             (this (lambda (arg) (affine-expression-from-loopus-ast arg local-space)))
             (new-a (funcall this a))
             (new-b (funcall this b)))
        ;; If one of the expression isn't recognized, we are not recognized too
        (if (not (and new-a new-b))
            nil
            ;; integer+ integer- integer* takes 2 arguments due to typo
            ;; todo generalize by checking if types and number of arguments are ok to do the thing below
            (case (typo:fnrecord-name (ir-call-fnrecord the-call))
              (typo:integer+ (isl:affine-add new-a new-b))
              (typo:integer- (isl:affine-sub new-a new-b))
              (typo:integer* (isl:affine-mul new-a new-b))
              ;; otherwise universe set todo
              ;; todo rationnal
              #+or(typo:integer/ (isl:affine-div new-a new-b))
              (otherwise
               ;; Otherwise, we don't know/recognize what it is. Return the universe
               nil))))
        ;; Otherwise, base case
      (let ((pos-variable (is-loop-variable ast)))
        (if pos-variable
            (isl:create-var-affine local-space :dim-set (1+ (* 2 pos-variable)))
            (isl:create-val-affine local-space (isl:value (second (ir-value-declared-type ast)))))))) ; todo derived type

(defun get-value (node)
  (let* ((producer (ir-value-producer node)))
    (uniquenumber producer)))
(defun create-new-point-range-new (&rest args)
  ;; Creation of the result map, and adding the constraint of the array
  (let* ((result (isl:basic-map-universe *space-map-domain-range*))
         (local-space (isl:local-space-from-space *space-map-domain-range*))
         (local-space-domain (isl:local-space-from-space *space-domain*))
         (constraint (isl:make-equality-constraint local-space))
         (constraint (isl:equality-constraint-set-constant constraint (isl:value (get-value (first args)))))
         (constraint (isl:equality-constraint-set-coefficient constraint :dim-out 0 (isl:value -1)))
         (result (isl:basic-map-add-constraint result constraint)))
    ;; The, we do all arguments of the read. So if (aref a b c 1 3) we do for a b c 1 3
    (loop for idx from 1 below (length args) do
      ;; For everything we read, we create an affixe expression of what it is, and create the associated map
      (let* ((affine-expression (affine-expression-from-loopus-ast (nth idx args) local-space-domain))
             (new-map (if affine-expression
                          (isl:basic-map-from-affine affine-expression)
                          (isl:basic-map-universe *space-map-domain-range*)))
             ;; The map we just created is [o0, o1, ...] -> [our expression]
             ;; (Unless affine-expression is nil (not recognized), and then we already have the good map)
             ;; We need to extend the range to obtain [o0, o1, ...] -> [i0, our expression, ...]
             ;; We add first everything before; then everything after
             ;; idx here is (+ 1 (1- idx)) ; 1 is for the array, (1- idx) is the every loop variable before
             (_ (when affine-expression
                  (setf new-map (isl:basic-map-insert-dimension new-map :dim-out 0 idx))))
             ;; About 0, and (1+ idx). It's the insertion position.
             ;; The final result we want is [smth, our expression, smth]
             ;; So first, we insert before, hence the 0
             ;; Then, we insert just after, hence (1+ idx). We inserted idx elements, so we have (1+ idx) total elements
             (_ (when affine-expression
                  (setf new-map (isl:basic-map-insert-dimension new-map :dim-out (1+ idx) (- *size-range* (1+ idx)))))))
        (setf result (isl:basic-map-intersect result new-map))))
    ;; Fill for the rest with a single value
    (loop for p from (length args) below *size-range* do
      (let* ((constraint (isl:make-equality-constraint local-space))
             (constraint (isl:equality-constraint-set-constant constraint (isl:value -1)))
             (constraint (isl:equality-constraint-set-coefficient constraint :dim-out p (isl:value -1))))
        (setf result (isl:basic-map-add-constraint result constraint))))
    result))


;; Old version
#+or(defun create-new-point-range (&rest args)
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

(defun create-map-schedule (timestamp)
  (isl:union-set-identity timestamp))

;; Old version
;; I'm a clown and it's just (identity domain domain) ?
#+or(defun create-map-schedule (&rest args)
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
             ;; Old version
             ;;(map-of-read/write (apply #'create-new-point-range what-is-read/wrote-in-order))
             ;;(map-of-read/write (isl:basic-map-union-map map-of-read/write))
             ;;(map-of-read/write (isl:union-map-intersect-domain map-of-read/write current-timestamp))
             ;; End of old version
             (map-of-read/write (isl:basic-map-union-map (apply #'create-new-point-range-new what-is-read/wrote-in-order)))
             (map-of-read/write (isl:union-map-intersect-domain map-of-read/write current-timestamp)))
        (when is-aref (push-map *map-read* map-of-read/write))
        (when is-setf (push-map *map-write* map-of-read/write)))
      ;; Add to *map-schedule*
      (push-map *map-schedule* (create-map-schedule current-timestamp))
                #+or(isl:union-map-intersect-domain
                 (create-map-schedule *loop-variables*)
                 current-timestamp)
      (my-incf *counter-domain*))))

;; todo
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
  (let* ((*loop-variables* (append *loop-variables* (list (ir-loop-variable node))))
         ;; Current depth we are in
         (old-hash-table (alexandria:copy-hash-table *depth-node*))
         (_ (setf (gethash node *depth-node*) *current-depth*))
         (*current-depth* (1+ *current-depth*))
         ;; Loop bounds
         (inputs (ir-node-inputs node))
         (start (parse-bound (first inputs)))
         (end (parse-bound (second inputs)))
         ;; todo step too ?
         (*loop-bounds* (cons (list start end) *loop-bounds*)))
    ;; Recursive call
    (map-block-inner-nodes #'update-node (ir-loop-body node))
    ;; No need to restore the hashtable, every node is different ?
    ;;(setf *depth-node* old-hash-table)
    ))
