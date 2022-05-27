(declaim (optimize (debug 3)))

(macrolet ((acc (v) `(setf (aref accumulator 0) (+ (aref accumulator 0) ,v))))
  (let* ((dim 10)
         ;; Single loop over arrays
         (1d (make-array dim))
         (2d (make-array (list dim dim)))
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
    ;; Sum over arrays

    ;; 1D
    (progn
      (print "1d")
      ;;(loopus:for (i 0 10) (print i))
      (loopus:for (i 0 10)
        (setf (aref 1d i) i))
      (setf (aref accumulator 0) 1)
      (loopus:for (i 0 10)
        (acc (aref 1d i)))
      (print 1d)
      (print accumulator))

    ;; 2D
    ;; aref not taking into accoutn?
    (progn
      (print "2d")
      (loopus:for (j 0 10)
        (loopus:for (i 0 5)
          (setf (aref 2d i j) (+ i j))))
      (loopus:for (i 0 10)
        (loopus:for (j 0 5)
          (setf (aref 2d i j) (+ i j))))
      #+or(loopus:for (i 5 10)
        (loopus:for (j 5 10)
          ;;(acc (aref 2d i j))))
          ;;(setf (aref 2d i j) 1)
          ;;(setf (aref 2d j i) 1)
          (SETF (AREF ACCUMULATOR 0) (AREF |2D| i j))
          ))
      #+or(loopus:for (j 0 10)
        (loopus:for (i 0 5)
          ;;(acc (aref 2d i j))))
          (SETF (AREF ACCUMULATOR 0) (+ (AREF ACCUMULATOR 0) (AREF |2D| I J)))))
      (print 2d)
      (print accumulator))
;; todo - reverse on ast generation for proximity
    ;; 3D
    (progn
      (print "3d")
      (loopus:for (i 0 8)
        (loopus:for (j 0 9)
          (loopus:for (k 0 10)
            (setf (aref 3d i j k) (+ i j k)))))
      (loopus:for (i 0 10)
        (loopus:for (j 0 10)
          (loopus:for (k 0 10)
            (acc (aref 3d i j k)))))
      (print 3d)
      (print accumulator))

    ;; Matrix multiplication
    (progn
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
