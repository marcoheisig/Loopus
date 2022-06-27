(in-package :loopus.ir)

(declaim (optimize (debug 3)))
#|
(macrolet ((acc (v) `(setf (aref accumulator 0) (+ (aref accumulator 0) ,v))))
  (let* ((dim 10)
         (v-start 2)
         (v-end 5)
         ;; Single loop over arrays
         (1d (make-array dim))
         (2d (make-array (list dim dim)))
         (2dd (make-array (list dim dim)))
         (3d (make-array (list dim dim dim)))
         ;(4d (make-array (list dim dim dim dim)))
         ;; Matrix multiplication
         (dim-C-x 5)
         (dim-C-y 5)
         (dim-temp 5)
         ;; C = A * B
         ;; A has dim (x, temp) and B (temp, y)
         (matrix-A (make-array (list dim-C-x dim-temp)))
         (matrix-B (make-array (list dim-temp dim-C-y)))
         (matrix-C (make-array (list dim-C-x dim-C-y)))
         (matrix-C-check (make-array (list dim-C-x dim-C-y)))
         ;; Various variables
         (accumulator (make-array 1))) ; todo later variable to 1d array to


    ;;(loop i (loop j (aref i i))) -> memory proximity is weird (same for j)
    ;;the first variable is the most outer loop
    ;; With this memory it's working fine
    ;;(memory-proximity (union-map-from-str "{ [0, i1, 0, i3, -1, -1, -1, -1] -> [0, 1 + i1, 0, i3, -1, -1, -1, -1] : 0 <= i1 <= 8 and 0 <= i3 <= 9 }"))


    ;; Doesnt work when it's { [0, i1, 0, i3, -1, -1, -1, -1] -> [0, 1 + i1, 0, o3, -1, -1, -1, -1] : 0 <= i1 <= 8 and 0 <= i3 <= 9 and 0 <= o3 <= 9 }>
    ;; Which is the correct thing, and should still reorder :(
    ;; todo

    ;; Index which are an operation
    (progn
      (print "1d")
      (print 1d)
      (loopus:for (i 0 9)
        ;(setf (aref 1d i #+or(/ 2 (+ 1 i))) i)
        (loopus:for (j 0 10)
          (setf (aref 2d i j) j))
        (loopus:for (k 0 10)
          (setf (aref 2d i k) k)))
      ;; todo when we use j here, the error is weird (hard to understand we use a non-defined variable)
      (print 1d))


    ;; Sum over arrays
    ;; 1D
   (progn
      (print "1d")
      (print 1d)
      (loopus:for (i 2 v-end) ;; doesn't work anymore with v-start because of step
        (setf (aref 1d i) 2))
      (print 1d)


      ;;(setf (aref accumulator 0) 1)
      #+or(loopus:for (i 0 10)
        (acc (aref 1d i)))
      (print 1d)
      (print accumulator))

    ;; 2D
    ;; aref not taking into accoutn?
    (progn
      (print "2d")
      (loop for i below 10 do
        (loop for j below 10 do
          (setf (aref 2d i j) 0)))
      (loop for i from 2 below 9 do
        (loop for j from 1 below i by 2 do
          (setf (aref 2d j i) (+ i j))))
      (print 2d)
      (loop for i below 10 do
        (loop for j below 10 do
          (setf (aref 2d i j) 0)))
      ;; todo
      ;; if step is not known, the loop direction is unknown, and not sure what I should do
      ;; but the user probably know the loop direction anyway, maybe better to ask him
      (loopus:for (i 2 9)
        (loopus:for (j 1 i 2)
          (setf (aref 2d j i) (+ i j)))
        #+or(loopus:for (j 1 i)
          (setf (aref 2dfefef i j) (+ i j))))
       #+or(loopus:for (i 2 9)
         (loopus:for (j 1 i)
           (setf (row-major-aref 2d (+ (* j (array-dimension 2d 1)) i))
                 (+ i j))))
      (print 2d)
      #+or(loopus:for (i 5 10)
        (loopus:for (j 5 10)
          ;;(acc (aref 2d i j))))
          ;;(setf (aref 2d i j) 1)
          ;;(setf (aref 2d j i) 1)
          (SETF (AREF ACCUMULATOR 0) (+ (aref accumulator 0) (AREF |2D| i j)))
          ))
      #+or(loopus:for (j 0 10)
        (loopus:for (i 0 5)
          ;;(acc (aref 2d i j))))
          (SETF (AREF ACCUMULATOR 0) (+ (AREF ACCUMULATOR 0) (AREF |2D| I J)))))

      ;; Doesnt work because WaW dependancies

      (print accumulator))
;; todo - reverse on ast generation for proximity
    ;; 3D
    #+or(progn
      (print "3d")
      (loopus:for (i 0 10)
        (loopus:for (j 0 10)
          (loopus:for (k 0 10)
            (setf (aref 3d i j k) (+ i j k)))))
      #+or(loopus:for (i 0 10)
        (loopus:for (j 0 10)
          (loopus:for (k 0 10)
            (acc (aref 3d i j k)))))
      (print 3d)
      (print accumulator))

    ;; Matrix multiplication
    #+or(progn
      ;; Init
      #+or(loopus:for (i 0 5)
        (loopus:for (j 0 5)
          (setf (aref matrix-A i j) (random 10))
          (setf (aref matrix-B i j) (random 10))
          (setf (aref matrix-C i j) 0)
          (setf (aref matrix-C-check i j) 0)))
      (loop for i below 5 do
        (loop for j below 5 do
          (setf (aref matrix-A i j) (random 10))
          (setf (aref matrix-B i j) (random 10))
          (setf (aref matrix-C i j) 0)
          (setf (aref matrix-C-check i j) 0)))
      ;; Mul
      (time
       (loopus:for (k 0 5)
         (loopus:for (j 0 5)
           (loopus:for (i 0 5)
             (setf (aref matrix-C i j)
                   (+ (aref matrix-C i j)
                      (* (aref matrix-A i k)
                         (aref matrix-B k j))))))))
      (time
       (loop for i below 4 do
         (loop for j below 4 do
           (loop for k below 4 do
             (setf (aref matrix-C-check i j)
                   (+ (aref matrix-C-check i j)
                      (* (aref matrix-A i k)
                         (aref matrix-B k j))))))))
      (print matrix-A)
      (print matrix-B)
      (print matrix-C)
      (print matrix-C-check))

    ;; Loop over triangles
    ;; Todo

    ;; End
    ))

|#
#|
;; Test of expressions that aren't a read/write
(progn
  (defun mm (arg) (1+ arg))
  (let ((2d (make-array '(2 2))))
    (loop for i below 2 do (loop for j below 2 do (setf (aref 2d i j) (+ i (* 10 j)))))
    (loop for i below 2 do
      (loop for j below 2 do
        (print (aref 2d j i))))
    (print "--")
    (loopus:for (i 0 2)
      (loopus:for (j 0 2)
        (aref 2d j i)))
    (loopus:for (i 0 2)
      (loopus:for (j 0 2)
        (aref 2d j i)
        (aref 2d j i))))
  (loopus:for (i 0 10) (print (mm i)))
  (loopus:for (i 0 10)
    (print i)
(print 2)
(loopus:for (i 0 2)
(loopus:for (j 0 2)
(print (+ 1 2 3 4 5))
(row-major-aref 2d (+ (* i 2) j))))))

|#


(defun print3 (a b)
  (setf a b))

(progn
  (defun mm (arg) (1+ arg))
  (let ((2d (make-array '(10 10)))
        (ss 1))
    #+or(loop for i from ss below 10 do
      (loop for j from 0 below 10 do
        (setf (row-major-aref 2d (+ (* j 10) i)) (+ j (* 10 i)))))
   (loopus:for (i ss 10)
      (loopus:for (j 0 10)
        (setf (row-major-aref 2d (+ (* j 10) i)) (+ j (* 10 i)))))
    ;; (loop for i below 10 do (loop for j below 10 do (setf (aref 2d i j) (+ i (* 10 j)))))
    (loop for i from 0 below 10 do
      (loop for j from 1 below 10 do
        (print (aref 2d j i))))
    (print "--")))


#+or(let ((start 2)
      (end 10)
      (step 2))
  (loopus:for (i start end)
    (loopus:for (j 0 i)
      (print j)
      (print i))))
