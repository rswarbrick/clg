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

;; $Id: gtkcontainer.lisp,v 1.19 2006-02-19 19:31:14 espen Exp $

(in-package "GTK")

(defmethod shared-initialize ((container container) names &rest initargs 
			      &key child children child-args 
			           (show-children nil show-children-p))
  (declare (ignore child children))
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


(defmethod compute-signal-function ((container container) signal function object)
  (if (eq object :children)
      #'(lambda (&rest args)
	  (mapc #'(lambda (child)
		    (apply function child (rest args)))
		(container-children container)))
    (call-next-method)))


(defbinding %container-add () nil
  (container container)
  (widget widget))

(defmethod container-add ((container container) (widget widget) &rest args)
  (%container-add container widget)
  (when args
    (setf
     (slot-value widget 'child-properties)
     (apply
      #'make-instance
      (gethash (class-of container) *container-to-child-class-mappings*)
      :parent container :child widget args))))

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

(defmethod container-children ((container container))
  (map-container 'list #'identity container))

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
  "Recursively shows any child widgets except widgets explicit hidden during construction."
  (labels ((recursive-show (widget)
	     (when (typep widget 'container)
	       (if (not (user-data-p widget 'show-recursive-p))
		   (container-foreach widget #'recursive-show)
		 (unset-user-data widget 'show-recursive-p)))
	     (unless (widget-hidden-p widget)
	       (widget-show widget))))
    (container-foreach container #'recursive-show)))
