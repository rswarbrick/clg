;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <espen@users.sourceforge.org>
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

;; $Id: gtkobject.lisp,v 1.26 2005-03-11 16:48:15 espen Exp $


(in-package "GTK")


;;;; Misc utils

; (defun name-to-string (name)
;   (substitute #\_ #\- (string-downcase (string name))))

; (defun string-to-name (name &optional (package "KEYWORD"))
;   (intern (substitute #\- #\_ (string-upcase name)) package))



;;;; Superclass for the gtk class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library 
   #.(concatenate 'string (pkg-config:pkg-variable "gtk+-2.0" "libdir") 
		          "/libgtk-x11-2.0.so"))

  (defclass %object (gobject)
    ()
    (:metaclass gobject-class)
    (:gtype |gtk_object_get_type|)))


(defmethod initialize-instance ((object %object) &rest initargs &key signal)
  (declare (ignore signal))
  (call-next-method)
  (reference-foreign (class-of object) (proxy-location object))
  (dolist (signal-definition (get-all initargs :signal))
    (apply #'signal-connect object signal-definition)))

(defmethod initialize-instance :around ((object %object) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (%object-sink object))

(defbinding %object-sink () nil
  (object %object))

;;;; Main loop and event handling

(declaim (inline events-pending-p main-iteration))

(defbinding events-pending-p () boolean)

(defbinding get-current-event () gdk:event)

(defbinding main-do-event () nil
  (event gdk:event))

(defbinding main () nil)

(defbinding main-level () int)

(defbinding main-quit () nil)

(defbinding main-iteration-do (&optional (blocking t)) boolean
  (blocking boolean))

(defun main-iterate-all (&rest args)
  (declare (ignore args))
  (loop
   while (events-pending-p)
   do (main-iteration-do nil)))


;;;; Metaclass for child classes
 
(defvar *container-to-child-class-mappings* (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass child-class (virtual-slots-class)
    ())

  (defclass direct-child-slot-definition (direct-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname)))

  (defclass effective-child-slot-definition (effective-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname)))


(defmethod shared-initialize ((class child-class) names &key container)
  (call-next-method)
  (setf
   (gethash (find-class (first container)) *container-to-child-class-mappings*)
    class))

(defmethod direct-slot-definition-class ((class child-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-child-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class child-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-child-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class child-class) direct-slotds)
  (if (eq (most-specific-slot-value direct-slotds 'allocation) :property)
      (nconc 
       (list :pname (most-specific-slot-value direct-slotds 'pname))
       (call-next-method))
    (call-next-method)))

(progn
  #+cmu(declaim (optimize (inhibit-warnings 3)))
  #+sbcl(declaim (muffle-conditions compiler-note))
  (defun %container-child-get-property (parent child pname gvalue))
  (defun %container-child-set-property (parent child pname gvalue)))


(defmethod initialize-internal-slot-functions ((slotd effective-child-slot-definition))
  (let ((type (slot-definition-type slotd))
	(pname (slot-definition-pname slotd)))
    (setf 
     (slot-value slotd 'getter)
     #'(lambda (object)
	 (with-slots (parent child) object	   
	   (let ((gvalue (gvalue-new type)))
	     (%container-child-get-property parent child pname gvalue)
	     (unwind-protect
		 (funcall (reader-function type) gvalue +gvalue-value-offset+)
	       (gvalue-free gvalue t))))))
    
    (setf 
     (slot-value slotd 'setter)
     #'(lambda (value object)
	 (with-slots (parent child) object	   
	   (let ((gvalue (gvalue-new type)))
	     (funcall (writer-function type) value gvalue +gvalue-value-offset+)
	     (%container-child-set-property parent child pname gvalue)
	     (gvalue-free gvalue t)
	     value)))))
 
  (call-next-method)))


(defmethod add-reader-method ((class child-class) generic-function slot-name)
  (add-method
   generic-function
   (make-instance 'standard-method
    :specializers (list (find-class 'widget))
    :lambda-list '(widget)
    :function #'(lambda (args next-methods)
		  (declare (ignore next-methods))
		  (child-property-value (first args) slot-name)))))

(defmethod add-writer-method
    ((class child-class) generic-function slot-name)
  (add-method
   generic-function
   (make-instance 'standard-method
    :specializers (list (find-class t) (find-class 'widget))
    :lambda-list '(value widget)
    :function #'(lambda (args next-methods)
		  (declare (ignore next-methods))
		  (destructuring-bind (value widget) args
		    (setf (child-property-value widget slot-name) value))))))


(defmethod validate-superclass ((class child-class) (super standard-class))
  ;(subtypep (class-name super) 'container-child)
  t)


(defclass container-child ()
  ((parent :initarg :parent :type container)
   (child :initarg :child :type widget)))


;;;;

(defbinding %container-class-list-child-properties () pointer
  (class pointer)
  (n-properties unsigned-int :out))

(defun query-container-class-child-properties (type-number)
  (let ((class (type-class-ref type-number)))
    (multiple-value-bind (array length)
	(%container-class-list-child-properties class)
      (unwind-protect
	  (map-c-vector 'list #'identity array 'param length)
	(deallocate-memory array)))))

(defun default-container-child-name (container-class)
  (intern (format nil "~A-CHILD" container-class)))

(defun expand-container-type (type forward-p options)
  (let* ((class (type-from-number type))
	 (super (supertype type))
	 (child-class (default-container-child-name class)))
    (if forward-p 
	(expand-gobject-type type t options)
      `(progn
	 ,(expand-gobject-type type nil options)
	 (defclass ,child-class (,(default-container-child-name super))
	   ,(slot-definitions child-class 
	     (query-container-class-child-properties type) nil)
	   (:metaclass child-class)
	   (:container ,class))))))


(register-derivable-type 'container "GtkContainer" 'expand-container-type 'gobject-dependencies)
