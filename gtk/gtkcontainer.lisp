;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gtkcontainer.lisp,v 1.4 2001-10-21 23:20:13 espen Exp $

(in-package "GTK")

(defmethod initialize-instance ((container container) &rest initargs)
  (call-next-method)
  (dolist (child (get-all initargs :child))
    (apply #'container-add container (mklist child))))


(defbinding %container-add () nil
  (container container)
  (widget widget))

(defun container-add (container widget &rest args)
  (%container-add container widget)
  (when args
    (setf
     (slot-value widget 'child-slots)
     (apply
      #'make-instance
      (gethash (class-of container) *container-to-child-class-mappings*)
      :parent container :child widget args))))


(defbinding %container-remove () nil
  (container container)
  (widget widget))

(defun container-remove (container widget)
  (%container-remove container widget)
  (slot-makunbound widget 'child-slots))


(defbinding container-check-resize () nil
  (container container))

(defbinding (%container-foreach "gtk_container_foreach_full")
    (container function) nil
  (container container)
  (0 unsigned-long)
  (*callback-marshal* pointer)
  ((register-callback-function function) pointer)
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

(defbinding %container-get-children () (glist widget)
  (container container))

(defmethod container-children ((container container))
  (%container-get-children container))

(defmethod (setf container-children) (children (container container))
  (dolist (child (container-children container))
    (container-remove container child))
  (dolist (child children)
    (container-add container child))
  children)

;; Should be implemented as a foreign function
(defun container-num-children (container)
  (length (container-children container)))

(defbinding container-resize-children () nil
  (container container))
