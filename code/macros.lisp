(in-package #:loopus)

(defmacro begin (&body body)
  `(for (,(gensym) 0 1) ,@body))

(defmacro for (&whole form (variable start end &optional (step 1))
               &body body &environment env)
  (declare (ignorable variable start end step body))
  (ir-expand
   (ir-optimize
    (ir-convert
     (trivial-macroexpand-all:macroexpand-all
      ;; Each occurrence of the FOR macro is turned into a call to the
      ;;  function %FOR.  This function is not defined, but handled
      ;;  specially by the IR conversion process.
      `(macrolet ((for ((variable start end &optional (step 1)) &body body)
                    `(%for ',variable ,start ,end ,step (locally ,@body))))
         ,form)
      env)
     *null-lexenv*))))

