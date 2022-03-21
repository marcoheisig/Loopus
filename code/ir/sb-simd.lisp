(in-package #:loopus.ir)

(pushnew 'typo.common-lisp:integer+ sb-simd-vectorizer::*add-operators*)
(pushnew 'typo.common-lisp:integer* sb-simd-vectorizer::*mul-operators*)

(defclass ir-simd-loop (ir-loop)
  ())

(defmethod copy-ir-node
    ((context (eql 'ir-vectorize))
     (ir-loop ir-loop))
  (destructuring-bind (start end step) (ir-node-inputs ir-loop)
    (declare (ignore start end))
    (when (and (typo:eql-ntype-p (ir-value-derived-ntype step))
               (eql (typo:eql-ntype-object (ir-value-derived-ntype step)) 1))
      (map-block-inner-nodes
       (lambda (node)
         (unless (and (ir-call-p node)
                      (typo:fnrecord-name (ir-call-fnrecord node))
                      (or
                       (sb-simd-internals::find-function-record
                        (typo:fnrecord-name (ir-call-fnrecord node))
                        nil)
                       (member (typo:fnrecord-name (ir-call-fnrecord node))
                               '(typo.common-lisp:integer+ typo.common-lisp:integer*))))
           (return-from copy-ir-node (call-next-method))))
       (ir-loop-body ir-loop)))
    (let* ((variable (copy-ir-value context (ir-loop-variable ir-loop)))
           (ir-node (make-instance 'ir-node-with-outputs
                      :outputs (list variable))))
      (change-class ir-node 'ir-simd-loop
        :inputs (mapcar (alexandria:curry #'copy-ir-value context) (ir-node-inputs ir-loop))
        :variable variable
        :body (copy-ir-block context (ir-loop-body ir-loop) ir-node)))))

(defmethod ir-expand-node ((ir-simd-loop ir-simd-loop))
  (destructuring-bind (start end step) (ir-node-inputs ir-simd-loop)
    (declare (ignore step))
    `(()
      (sb-simd-vectorizer:do-vectorized
          (,(value-name (ir-loop-variable ir-simd-loop))
           ,(value-name start)
           ,(value-name end))
        ,(ir-expand-node (ir-loop-body ir-simd-loop))))))
