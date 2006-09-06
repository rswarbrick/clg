;; Common Lisp bindings for GTK+ 2.x
;; Copyright 2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: ginspect.lisp,v 1.12 2006-09-06 11:07:36 espen Exp $

#+sbcl(require :gtk)
#+cmu(asdf:oos 'asdf:load-op :gtk)

(defpackage "GINSPECT"
  (:use "COMMON-LISP" "GFFI" "GLIB" "GTK" #+cmu"PCL" #+sbcl"SB-PCL" #+clisp"MOP")
  #+clisp(:shadowing-import-from "MOP" "SLOT-DEFINITION-TYPE")
  (:export "GINSPECT" "GINSPECT-TOPLEVELS"))

(in-package "GINSPECT")

(defvar *ginspect-unbound-object-marker* 
  #+(or cmu clisp)(gensym "UNBOUND-OBJECT-")
  #+sbcl sb-impl::*inspect-unbound-object-marker*)


(defgeneric insert-object (object store parent &optional prefix))
(defgeneric insert-parts (object store parent))
(defgeneric object-has-parts-p (object))
(defgeneric decompose-describe-object (object))


;; A container to hold lisp objects "inside" the tree store
(defclass object-container (gobject)
  ((object :initarg :object))
  (:metaclass gobject-class))


(defun ginspect (object)
  (let* ((store (make-instance 'tree-store 
		 :column-types '(string string gobject boolean)
		 :column-names '(name pprinted object expanded)))
	 (view (make-instance 'tree-view :model store :headers-visible nil)))

    (let ((column (make-instance 'tree-view-column))
 	  (name (make-instance 'cell-renderer-text))
 	  (object (make-instance 'cell-renderer-text)))	  
      (tree-view-append-column view column)
      (cell-layout-pack column name :expand nil)
      (cell-layout-add-attribute column name 'text (column-index store 'name))
      (cell-layout-pack column object :expand t)
      (cell-layout-add-attribute column object 'text (column-index store 'pprinted)))

    (insert-object object store nil)

    (signal-connect view 'row-expanded 
     #'(lambda (iter path)
	 (when (setf 
		(tree-model-value store iter 'expanded)
		(not (tree-model-value store iter 'expanded)))
	   (multiple-value-bind (valid child-iter) 
	       (tree-model-iter-children store iter)
	     ;; Remove old children
	     (when valid
	       (loop while (tree-store-remove store child-iter))))
	   (let ((container (tree-model-value store iter 'object)))
	     (insert-parts (slot-value container 'object) store iter))
	   (tree-view-expand-row view path nil))))

    (make-instance 'dialog
     :title "Object Inspector" :show-children t :visible t
     :default-width 600 :default-height 600
     :button (list "gtk-close" #'widget-destroy :object t)
     :child (make-instance 'scrolled-window 
	     :hscrollbar-policy :automatic :child view))))


(defmethod decompose-describe-object ((object t))
  #+cmu
  (destructuring-bind (description named-p &rest parts) 
      (inspect::describe-parts object)
    (if (equal parts (list object))
	(values description nil nil)
      (values description named-p parts)))
  #+sbcl(sb-impl::inspected-parts object)
  #+clisp(values (format nil "The object is an ATOM of type ~A" (type-of object) nil nil)))

(defmethod decompose-describe-object ((object (eql t)))
  (values (call-next-method) nil nil))

(defmethod decompose-describe-object ((object (eql nil)))
  (values (call-next-method) nil nil))

(defun propper-list-p (object)
  (and (listp object) (null (cdr (last object)))))

(defmethod decompose-describe-object ((object cons))
  (if (propper-list-p object)
      (values (call-next-method) nil object)
    (values "The object is a CONS." nil (list (car object) (cdr object)))))

#+(or cmu sbcl)
(defmethod decompose-describe-object ((object #+cmu alien:system-area-pointer
					      #+sbcl sb-alien:system-area-pointer))
  (values "The object is a SYSTEM-AREA-POINTER" nil nil))

(defmethod decompose-describe-object ((object (eql *ginspect-unbound-object-marker*)))
  (values "The slot is unbound" nil nil))

#+(or cmu clisp)
(defmethod decompose-describe-object ((object symbol))
  (values 
   (call-next-method) t
   (list
    (cons "Name" (symbol-name object)) 
    (cons "Package" (symbol-package object))
    (cons "Value" (if (boundp object)
		      (symbol-value object)
		    *ginspect-unbound-object-marker*))
    (cons "Function" (if (fboundp object)
			 (symbol-function  object)
		       *ginspect-unbound-object-marker*))
    (cons "Plist" (symbol-plist object)))))

(defmethod decompose-describe-object ((object standard-object))
  (values 
   (format nil "The instance is an object of type ~A" 
    (class-name (class-of object)))
   t
   (loop
    for slotd in (class-slots (class-of object))
    when (slot-readable-p slotd)
    collect (let* ((slot-name (slot-definition-name slotd))
		   (slot-value (if (slot-boundp object slot-name)
				   (slot-value object slot-name)
				 *ginspect-unbound-object-marker*)))
	      (cons (string slot-name) slot-value)))))

#+clisp
(defmethod decompose-describe-object ((object vector))
  (values   
   (format nil "The object is a ~A of length ~A" (type-of object) (length object))
   nil
   (coerce object 'list)))


(defmethod object-has-parts-p ((object t))
  (nth-value 2 (decompose-describe-object object)))

(defmethod object-has-parts-p ((object cons))
  t)

(defmethod object-has-parts-p ((object standard-object))
  (class-slots (class-of object)))

(defmethod object-has-parts-p ((object vector))
  (not (zerop (length object))))


(defmethod object-to-string ((object t))
  (with-output-to-string (stream)
    (write object :stream stream :lines 1 :right-margin 80)))

(defmethod object-to-string ((object (eql *ginspect-unbound-object-marker*)))
  "<unbound>")

(defmethod insert-object ((object t) store parent &optional (name ""))
  (let ((container (make-instance 'object-container :object object))
	(has-parts (object-has-parts-p object)))
    (let ((iter (tree-store-append store parent 
		 (vector name (object-to-string object) 
			 container (not has-parts)))))
      (when has-parts
	;; Insert dummy child
	(tree-store-append store iter (vector "" "" container t))))))

(defmethod insert-parts :around ((object t) store parent)
  (when (object-has-parts-p object)
    (call-next-method)))

(defmethod insert-parts ((object t) store parent)
  (multiple-value-bind (description named-p parts) 
      (decompose-describe-object object)
    (declare (ignore description))
    (loop
     for part in parts
     do (if named-p
	    (insert-object (cdr part) store parent (string (car part)))
	  (insert-object part store parent)))))


(defun ginspect-toplevels ()
  (ginspect (window-list-toplevels)))
