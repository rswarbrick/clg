;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2005 Espen S. Johnsen <espen@users.sf.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; $Id: gtkcontainer.lisp,v 1.25 2008-03-06 22:02:08 espen Exp $

(in-package "GTK")

(defgeneric container-add (container widget &rest args))
(defgeneric container-remove (container widget))
(defgeneric container-all-children (container))
(defgeneric container-internal-children (container))
(defgeneric (setf container-children) (children container))


(defun initial-add (object function initargs key pkey)
  (loop 
   as (initarg value . rest) = initargs then rest
   do (cond
       ((eq initarg key) (funcall function object value))
       ((eq initarg pkey) (mapc #'(lambda (value)
				    (funcall function object value))
				value)))
       while rest))

(defun initial-apply-add (object function initargs key pkey)
  (initial-add object #'(lambda (object value)
			  (apply function object (mklist value)))
	       initargs key pkey))


(defmethod shared-initialize ((container container) names &rest initargs 
			      &key child children child-args 
			      (show-children nil show-children-p))
  (declare (ignore names child children))
  (when show-children-p
    (if (not show-children)
	(setf (user-data container 'show-recursive-p) nil)
      (signal-connect container 'show #'container-show-recursive 
       :object t :remove t)))

  (call-next-method)
  (initial-add container 
   #'(lambda (container args) 
       (apply #'container-add container (append (mklist args) child-args)))
   initargs :child :children))


(defmethod compute-signal-function ((container container) signal function object args)
  (declare (ignore signal))
  (if (eq object :children)
      #'(lambda (&rest emission-args)
	  (let ((all-args (nconc (rest emission-args) args)))
	    (container-foreach container
	     #'(lambda (child)
		 (apply function child all-args)))))
    (call-next-method)))


(defbinding %container-add () nil
  (container container)
  (widget widget))

(defun find-child-class (container-class)
  (or
   (gethash container-class *container-to-child-class-mappings*)
   (setf (gethash container-class *container-to-child-class-mappings*)
    (or
     (when (eq container-class (find-class 'container))
       (find-class 'container-child))
     (find-child-class (find-class (supertype container-class)))))))

(defun init-child-slots (container child args)
  (when args
    (setf
     (slot-value child 'child-properties)
     (apply
      #'make-instance (find-child-class (class-of container))
      :parent container :child child args))))

(defmethod container-add ((container container) (widget widget) &rest args)
  (%container-add container widget)
  (init-child-slots container widget args)
  widget)

(defmethod container-add ((container container) (widgets list) &rest args)
  (dolist (widget widgets)
    (apply #'container-add container widget args)))

(defbinding %container-remove () nil
  (container container)
  (widget widget))

(defmethod container-remove ((container container) (widget widget))
  (%container-remove container widget)
  (slot-makunbound widget 'child-properties))


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

(define-callback-marshal %foreach-callback nil (widget))

(defbinding %container-foreach (container callback-id) nil
  (container container)
  (%foreach-callback callback)
  (callback-id unsigned-int))

(defun container-foreach (container function)
  (with-callback-function (id function)
    (%container-foreach container id)))

(defbinding %container-forall (container callback-id) nil
  (container container)
  (%foreach-callback callback)
  (callback-id unsigned-int))

(defun container-forall (container function)
  (with-callback-function (id function)
    (%container-forall container id)))

(defun map-container (seqtype func container)
  (case seqtype
    ((nil)
     (container-foreach container func)
     nil)
    (list
     (let ((list nil))
       (container-foreach container
	#'(lambda (child)
	    (push (funcall func child) list)))
       (nreverse list)))
    (t
     (let ((seq (make-sequence seqtype (container-length container)))
	   (index 0))
       (container-foreach container
	#'(lambda (child)
	    (setf (elt seq index) (funcall func child))
	    (incf index)))
       seq))))

(defmethod container-all-children ((container container))
  (let ((internal ()))
    (container-forall container 
     #'(lambda (child)
	 (push child internal)))
    (nreverse internal)))

(defmethod container-internal-children ((container container))
  (let ((external-children (container-children container))
	(all-children (container-all-children container)))
    (loop
     for child in all-children
     unless (find child external-children)
     collect child)))

(defmethod (setf container-children) (children (container container))
  (dolist (child (container-children container))
    (container-remove container child))
  (dolist (child children)
    (apply #'container-add container (mklist child)))
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

(defgeneric container-show-recursive (container))

(defmethod container-show-recursive ((container container))
  "Recursively show any child widgets except widgets explicit hidden during construction."
  (labels ((recursive-show (widget)
	     (when (typep widget 'container)
	       (if (not (user-data-p widget 'show-recursive-p))
		   (container-foreach widget #'recursive-show)
		 (unset-user-data widget 'show-recursive-p)))
	     (unless (widget-hidden-p widget)
	       (widget-show widget))))
    (container-foreach container #'recursive-show)))
