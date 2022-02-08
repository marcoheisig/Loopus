(in-package #:loopus)

(defmacro begin (&body body)
  `(for (,(gensym) 0 1) ,@body))

(defmacro for (&whole form (variable start end &optional (step 1))
               &body body &environment env)
  (declare (ignorable variable start end step body))
  (ir-expand
   (ir-optimize
    (ir-convert-in-environment form env 0))))
