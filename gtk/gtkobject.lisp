;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gtkobject.lisp,v 1.42 2007-06-06 10:43:54 espen Exp $


(in-package "GTK")


;;;; Superclass for the gtk class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library gtk "libgtk-2.0")

  (defclass %object (gobject)
    ()
    (:metaclass gobject-class)
    (:gtype |gtk_object_get_type|)))


(defmethod initialize-instance ((object %object) &rest initargs &key signal)
  (declare (ignore signal))
  (call-next-method)
  (dolist (signal-definition (get-all initargs :signal))
    (apply #'signal-connect object signal-definition)))

(defmethod initialize-instance :around ((object %object) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  ;; Add a temorary reference which will be removed when the object is
  ;; sinked
  (funcall (reference-function '%object) (foreign-location object))
  (%object-sink object))

(defbinding %object-sink () nil
  (object %object))

;;;; Main loop and event handling

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
   do (main-iteration-do nil))
  #+clisp 0)


;;;; Metaclass for child classes
 
(defvar *container-to-child-class-mappings* (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass container-child-class (virtual-slots-class)
    ())

  (defclass direct-child-slot-definition (direct-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname)))

  (defclass effective-child-slot-definition (effective-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname))))


(defmethod shared-initialize ((class container-child-class) names &key container)
  (declare (ignore names))
  (call-next-method)
  (setf
   (gethash (find-class (first container)) *container-to-child-class-mappings*)
    class)
  #+clisp
  (loop
   for slotd in (class-direct-slots class)
   when (typep slotd 'direct-child-slot-definition)
   do (loop
       for reader in (slot-definition-readers slotd)
       do (add-reader-method class 
	   (ensure-generic-function reader :lambda-list '(object)) 
	   (slot-definition-name slotd)))
      (loop
       for writer in (slot-definition-writers slotd)
       do (add-writer-method class 
	   (ensure-generic-function writer :lambda-list '(value object)) 
	   (slot-definition-name slotd)))))

(defmethod direct-slot-definition-class ((class container-child-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-child-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class container-child-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-child-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class container-child-class) direct-slotds)
  (if (eq (slot-definition-allocation (first direct-slotds)) :property)
      (nconc 
       (list :pname (most-specific-slot-value direct-slotds 'pname))
       (call-next-method))
    (call-next-method)))

(defmethod slot-readable-p ((slotd effective-child-slot-definition))
  (declare (ignore slotd))
  t)

(defmethod compute-slot-reader-function ((slotd effective-child-slot-definition) &optional signal-unbound-p)
  (declare (ignore signal-unbound-p))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (reader (reader-function type :ref :get)))
    #'(lambda (object)
	(with-slots (parent child) object	   
	  (with-memory (gvalue +gvalue-size+)
	    (glib::%gvalue-init gvalue (find-type-number type))
	    (%container-child-get-property parent child pname gvalue)
	    (funcall reader gvalue +gvalue-value-offset+))))))

(defmethod slot-writable-p ((slotd effective-child-slot-definition))
  (declare (ignore slotd))
  t)

(defmethod compute-slot-writer-function ((slotd effective-child-slot-definition))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (writer (writer-function type :temp t))
	 (destroy (destroy-function type :temp t)))
    #'(lambda (value object)
	(with-slots (parent child) object	   
	  (with-memory (gvalue +gvalue-size+)
	    (glib::%gvalue-init gvalue (find-type-number type))
	    (funcall writer value gvalue +gvalue-value-offset+)
	    (%container-child-set-property parent child pname gvalue)
	    (funcall destroy gvalue +gvalue-value-offset+))
	  value))))


(defmethod add-reader-method ((class container-child-class) generic-function slot-name #?(sbcl>= 1 0 2)slot-documentation)
  (add-method
   generic-function
   (make-instance 'standard-method
    :specializers (list (find-class 'widget))
    :lambda-list '(widget)
    :documentation (or #?(sbcl>= 1 0 2)slot-documentation "automatically generated reader method")
    :function #'(lambda (args next-methods)
		  (declare (ignore next-methods))
		  (child-property-value (first args) slot-name)))))

(defmethod add-writer-method ((class container-child-class) generic-function slot-name #?(sbcl>= 1 0 2)slot-documentation)
  (add-method
   generic-function
   (make-instance 'standard-method
    :specializers (list (find-class t) (find-class 'widget))
    :lambda-list '(value widget)
    :documentation (or #?(sbcl>= 1 0 2)slot-documentation "automatically generated reader method")
    :function #'(lambda (args next-methods)
		  (declare (ignore next-methods))
		  (destructuring-bind (value widget) args
		    (setf (child-property-value widget slot-name) value))))))


(defmethod validate-superclass ((class container-child-class) (super standard-class))
  ;(subtypep (class-name super) 'container-child)
  t)


(defclass container-child (virtual-slots-object)
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
	   (:metaclass container-child-class)
	   (:container ,class))))))

(defun container-child-class (container-class)
  (gethash container-class *container-to-child-class-mappings*))

(defun container-dependencies (type options)
  (delete-duplicates 
   (append
    (gobject-dependencies type options)
    (mapcar #'param-value-type (query-container-class-child-properties type)))))

(register-derivable-type 'container "GtkContainer" 'expand-container-type 'container-dependencies)
