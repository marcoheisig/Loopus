(in-package #:loopus)

(defmacro begin (&body body &environment env)
  (ir-expand
   (ir-optimize
    (ir-convert
     (trivial-macroexpand-all:macroexpand-all
      `(macrolet ((for ((variable start end &optional (step 1)) &body body)
                    `(%for ',variable ,start ,end ,step (locally ,@body))))
         (locally ,@body))
      env)))))

(defmacro for (&whole form (variable start end &optional (step 1)) &body body)
  (declare (ignorable variable start end step body))
  `(begin ,form))

(defun %for (&rest rest)
  (declare (ignore rest))
  (error "The magic function %FOR should never appear in actual code."))

(defun ir-convert (form)
  (break "~S" form))
