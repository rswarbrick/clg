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

;; $Id: gtkcontainer.lisp,v 1.13 2004-12-17 00:15:16 espen Exp $

(in-package "GTK")


(defmethod shared-initialize ((container container) names &rest initargs 
			      &key child children child-args)
  (declare (ignore child children))
  (call-next-method)
  (initial-add container 
   #'(lambda (container args) 
       (apply #'container-add container (append (mklist args) child-args)))
   initargs :child :children))


(defbinding %container-add () nil
  (container container)
  (widget widget))

(defmethod container-add ((container container) (widget widget) &rest args)
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

(defmethod container-remove ((container container) (widget widget))
  (%container-remove container widget)
  (slot-makunbound widget 'child-slots))


(defbinding %container-child-get-property () nil
  (container container)
  (child widget)
  (property-name string)
  (value gvalue))

(defbinding %container-child-set-property () nil
  (container container)
  (child widget)
  (property-name string)
  (value gvalue))
  

(defbinding container-check-resize () nil
  (container container))

(def-callback-marshal %foreach-callback (nil widget))

(defbinding %container-foreach (container callback-id) nil
  (container container)
  ((callback %foreach-callback) pointer)
  (callback-id unsigned-int))

(defun container-foreach (container function)
  (with-callback-function (id function)
    (%container-foreach container id)))

(defun map-container (seqtype func container)
  (case seqtype
    ((nil)
     (container-foreach container func)
     nil)
    (list
     (let ((list nil))
       (container-foreach
	container
	#'(lambda (child)
	    (push (funcall func child) list)))
       (nreverse list)))
    (t
     (let ((seq (make-sequence seqtype (container-length container)))
	   (index 0))
       (container-foreach
	container
	#'(lambda (child)
	    (setf (elt seq index) (funcall func child))
	    (incf index)))
       seq))))

(defmacro do-container ((var container &optional (result nil)) &body body)
  (let ((continue (make-symbol "CONTINUE")))
    `(let ((,continue t))
       (container-foreach
	,container
	#'(lambda (,var)
	    (when ,continue
	      (setq ,continue nil)
	      (block nil
		,@body
		(setq ,continue t)))))
       ,result)))

;; (defbinding %container-get-children () (glist widget)
;;   (container container))

(defmethod container-children ((container container))
;;   (%container-get-children container)
  (map-container 'list #'identity container))

(defmethod (setf container-children) (children (container container))
  (dolist (child (container-children container))
    (container-remove container child))
  (dolist (child children)
    (container-add container child))
  children)

(defun container-length (container)
  (let ((n 0))
    (container-foreach container
     #'(lambda (child)
	 (declare (ignore child))
	 (incf n)))
    n))

(defbinding container-resize-children () nil
  (container container))

(defbinding container-propagate-expose () nil
  (container container)
  (child widget)
  (event gdk:expose-event))


(defbinding %container-get-focus-chain () boolean
  (container container)
  (focusable-widgets (glist widget) :out))

(defun container-focus-chain (container)
  (multiple-value-bind (chain-set-p focusable-widgets)
      (%container-get-focus-chain container)
    (and chain-set-p focusable-widgets)))

(defbinding %container-set-focus-chain () nil
  (container container)
  (focusable-widgets (glist widget)))

(defbinding %container-unset-focus-chain () nil
  (container container))

(defun (setf container-focus-chain) (focusable-widgets container)
  (if (null focusable-widgets)
      (%container-unset-focus-chain container)
    (%container-set-focus-chain container focusable-widgets)))
