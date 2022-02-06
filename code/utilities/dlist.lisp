(defpackage #:loopus.dlist
  (:use #:common-lisp)
  (:export
   #:dlist
   #:dlistp
   #:dlist-empty-p
   #:dlist-first
   #:dlist-last
   #:dlist-push-back
   #:dlist-push-front
   #:dlist-pop-front
   #:dlist-pop-back
   #:list-from-dlist
   #:dnode
   #:dnodep
   #:dnode-next
   #:dnode-prev
   #:dnode-insert-after
   #:dnode-insert-before
   #:dnode-delete
   #:map-dlist-dnodes
   #:do-dlist-dnodes
   #:list-from-dlist))

(in-package #:loopus.dlist)

;;; A simple doubly-linked list implementation.

(defstruct (dnode
            (:predicate dnodep)
            (:copier nil)
            (:constructor make-dnode (value)))
  value
  (%prev nil :type (or null dnode))
  (%next nil :type (or null dnode)))

(defun dnode-prev (dnode)
  ;; Never hand out the dlist's head.
  (let ((prev (dnode-%prev dnode)))
    (if (eq (dnode-value prev) '.head.) nil prev)))

(defun dnode-next (dnode)
  ;; Never hand out the dlist's tail.
  (let ((next (dnode-%next dnode)))
    (if (eq (dnode-value next) '.tail.) nil next)))

(defun dnode-insert-after (dnode item)
  (let ((a dnode)
        (b (make-dnode item))
        (c (dnode-%next dnode)))
    (setf (dnode-%next b) c
          (dnode-%prev b) a)
    (setf (dnode-%next a) b
          (dnode-%prev c) b)
    b))

(defun dnode-insert-before (dnode item)
  (let ((a (dnode-%prev dnode))
        (b (make-dnode item))
        (c dnode))
    (setf (dnode-%next b) c
          (dnode-%prev b) a)
    (setf (dnode-%next a) b
          (dnode-%prev c) b)
    b))

(defun dnode-delete (dnode &optional (end-dnode nil end-dnode-p))
  "Delete DNODE from the containing doubly-linked list.  If the optional
argument END-DNODE is supplied, delete all dnodes from DNODE (inclusive) to
END-DNODE (exclusive)."
  (let ((a (dnode-%prev dnode))
        (b (if (not end-dnode-p) (dnode-%next dnode) end-dnode)))
    (setf (dnode-%next a) b)
    (setf (dnode-%prev b) a)
    (values)))

(defstruct (dlist
            (:predicate dlistp)
            (:constructor %make-dlist))
  (head (alexandria:required-argument :head)
   :type dnode
   :read-only t)
  (tail (alexandria:required-argument :tail)
   :type dnode
   :read-only t))

(defun dlist (&rest args)
  (let* ((head (make-dnode '.head.))
         (tail (make-dnode '.tail.))
         (dlist (%make-dlist
                 :head head
                 :tail tail)))
    (setf (dnode-%prev tail) head)
    (setf (dnode-%next head) tail)
    (loop for arg in args do
      (dlist-push-back dlist arg))
    dlist))

(defun dlist-empty-p (dlist)
  (eq (dnode-%next (dlist-head dlist))
      (dlist-tail dlist)))

(defun dlist-push-front (dlist item)
  (dnode-insert-after (dlist-head dlist) item))

(defun dlist-push-back (dlist item)
  (dnode-insert-before (dlist-tail dlist) item))

(defun dlist-first (dlist)
  (dnode-next (dlist-head dlist)))

(defun dlist-last (dlist)
  (dnode-prev (dlist-tail dlist)))

(defun dlist-pop-front (dlist)
  (let ((dnode (dlist-first dlist)))
    (if (not dnode)
        (values nil nil)
        (values (dnode-delete dnode) t))))

(defun dlist-pop-back (dlist)
  (let ((dnode (dlist-last dlist)))
    (if (not dnode)
        (values nil nil)
        (values (dnode-delete dnode) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

(defun map-dlist-dnodes (function dlist &key (from-end nil))
  (let ((head (dlist-head dlist))
        (tail (dlist-tail dlist)))
    (if (not from-end)
        (loop for dnode = (dnode-%next head) then (dnode-%next dnode)
              until (eq dnode tail)
              do (funcall function dnode))
        (loop for dnode = (dnode-%prev tail) then (dnode-%prev dnode)
              until (eq dnode head)
              do (funcall function dnode)))))

(defmacro do-dlist-dnodes ((dnode dlist &key (from-end nil) (result nil)) &body body)
  (check-type dnode symbol)
  `(block nil
     (map-dlist-dnodes
      (lambda (,dnode) ,@body)
      ,dlist
      :from-end ,from-end)
     ,result))

(defun list-from-dlist (dlist)
  (let ((result '()))
    (do-dlist-dnodes (dnode dlist :from-end t :result result)
      (push (dnode-value dnode) result))))
