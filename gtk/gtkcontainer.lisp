;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <espejohn@online.no>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; $Id: gtkcontainer.lisp,v 1.2 2000-10-05 17:21:04 espen Exp $

(in-package "GTK")


(defmethod initialize-instance ((container container) &rest initargs
				&key children)
  (declare (ignore initargs))
  (call-next-method)
  (dolist (child children)
    (cond
     ((consp child)
      (container-add container (first child))
      (setf
       (slot-value (first child) 'child-slots)
       (apply
	#'make-instance
	(slot-value (class-of container) 'child-class)
	:parent container :child (first child) (cdr child))))
     (t
      (container-add container child)))))



(define-foreign ("gtk_container_child_getv" container-child-get-arg) () nil
  (container container)
  (child widget)
  (1 unsigned-int)
  (arg arg))

(define-foreign ("gtk_container_child_setv" container-child-set-arg) () nil
  (container container)
  (child widget)
  (1 unsigned-int)
  (arg arg))

(defun container-child-arg (container child name)
  (with-gc-disabled
    (let ((arg (arg-new 0)))
      (setf (arg-name arg) name)
      (container-child-get-arg container child arg) ; probably memory leak
      (let ((type (type-from-number (arg-type arg))))
	(prog1
	    (arg-value arg type)
	  (arg-free arg nil))))))

(defun (setf container-child-arg) (value container child name)
  (with-gc-disabled
    (let ((arg (arg-new 0)))
      (setf (arg-name arg) name)
      (container-child-get-arg container child arg) ; probably memory leak
      (let ((type (type-from-number (arg-type arg))))
	(setf (arg-value arg type) value)
	(container-child-set-arg container child arg)
	(arg-free arg nil))))
  value)


(define-foreign container-add () nil
  (container container)
  (widget widget))

(define-foreign container-remove () nil
  (container container)
  (widget widget))

(define-foreign container-check-resize () nil
  (container container))

(define-foreign ("gtk_container_foreach_full" %container-foreach)
    (container function) nil
  (container container)
  (0 unsigned-long)
  (*callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-marshal* pointer))

(defun map-container (seqtype func container)
  (case seqtype
    ((nil)
     (%container-foreach container func)
     nil)
    (list
     (let ((list nil))
       (%container-foreach
	container
	#'(lambda (child)
	    (push (funcall func child) list)))
       (nreverse list)))
    (t
     (let ((seq (make-sequence seqtype (container-num-children container)))
	   (index 0))
       (%container-foreach
	container
	#'(lambda (child)
	    (setf (elt seq index) (funcall func child))
	    (incf index)))
       seq))))

(defmacro do-container ((var container &optional (result nil)) &body body)
  (let ((continue (make-symbol "CONTINUE")))
    `(let ((,continue t))
       (%container-foreach
	,container
	#'(lambda (,var)
	    (when ,continue
	      (setq ,continue nil)
	      (block nil
		,@body
		(setq ,continue t)))))
       ,result)))

(define-foreign container-children () (glist widget)
  (container container))

(defun (setf container-children) (children container)
  (dolist (child (container-children container))
    (container-remove container child))
  (dolist (child children)
    (container-add container child))
  children)

;; Should be implemented as a foreign function
(defun container-num-children (container)
  (length (container-children container)))

(define-foreign container-resize-children () nil
  (container container))
