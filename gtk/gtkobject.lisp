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

;; $Id: gtkobject.lisp,v 1.14 2002-03-24 13:02:43 espen Exp $


(in-package "GTK")


;;;; Misc utils

; (defun name-to-string (name)
;   (substitute #\_ #\- (string-downcase (string name))))

; (defun string-to-name (name &optional (package "KEYWORD"))
;   (intern (substitute #\- #\_ (string-upcase name)) package))



;;;; Superclass for the gtk class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library "libgtk-x11-2.0.so"
   :ignore ("gtk_window_get_type_hint"))

  (defclass %object (gobject)
    ()
    (:metaclass gobject-class)
    (:alien-name "GtkObject")))


(defmethod shared-initialize ((object %object) names &rest initargs
			      &allow-other-keys)
  (declare (ignore names))
  (call-next-method)
  (object-ref object) ; inc ref count before sinking
  (%object-sink object)
  (dolist (signal-definition (get-all initargs :signal))
    (apply #'signal-connect object signal-definition)))

(defmethod initialize-proxy ((object %object) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (%object-sink object))

(defbinding %object-sink () nil
  (object %object))


;;;; Main loop, timeouts and idle functions

(declaim (inline events-pending-p main-iteration))

(defbinding (events-pending-p "gtk_events_pending") () boolean)

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
  (when (events-pending-p)
    (main-iteration-do nil)
    (main-iterate-all)))

(system:add-fd-handler (gdk:connection-number) :input #'main-iterate-all)
(setq lisp::*periodic-polling-function* #'main-iterate-all)
(setq lisp::*max-event-to-sec* 0)
(setq lisp::*max-event-to-usec* 1000)



;;;; Metaclass for child classes
 
(defvar *container-to-child-class-mappings* (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass child-class (virtual-slot-class))

  (defclass direct-child-slot-definition (direct-virtual-slot-definition)
    ((pname :reader slot-definition-pname)))

  (defclass effective-child-slot-definition
    (effective-virtual-slot-definition)))


(defmethod shared-initialize ((class child-class) names &rest initargs
			      &key container)
  (declare (ignore initargs))
  (call-next-method)
  (setf
   (gethash (find-class (first container)) *container-to-child-class-mappings*)
    class))

(defmethod initialize-instance  ((slotd direct-child-slot-definition)
				 &rest initargs &key pname)
  (declare (ignore initargs))
  (call-next-method)
  (if pname
      (setf (slot-value slotd 'pname) pname)
    ; ???
    (error "Need pname for slot with allocation :property")))

(defmethod direct-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-child-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-child-slot-definition))
    (t (call-next-method))))

(progn
  (declaim (optimize (ext:inhibit-warnings 3)))
  (defun %container-child-get-property (parent child pname gvalue))
  (defun %container-child-set-property (parent child pname gvalue)))


(defmethod compute-virtual-slot-accessors
    ((class child-class) (slotd effective-child-slot-definition) direct-slotds)

  (with-slots (type) slotd
    (let ((pname (slot-definition-pname (first direct-slotds)))
	  (type-number (find-type-number type)))
      (list
       #'(lambda (object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
	       (let ((gvalue (gvalue-new type-number)))
		 (%container-child-get-property parent child pname gvalue)
		 (unwind-protect
		     (funcall
		      (intern-reader-function type)
		      gvalue +gvalue-value-offset+)
		   (gvalue-free gvalue t))))))
       #'(lambda (value object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
	      (let ((gvalue (gvalue-new type-number)))
		(funcall
		 (intern-writer-function type)
		 value gvalue +gvalue-value-offset+)
		(%container-child-set-property parent child pname gvalue)
		(funcall
		 (intern-destroy-function type)
		 gvalue +gvalue-value-offset+)
		(gvalue-free gvalue nil)
		value))))))))


(defmethod pcl::add-reader-method ((class child-class) generic-function slot-name)
  (add-method
   generic-function
   (make-instance 'standard-method
		  :specializers (list (find-class 'widget))
		  :lambda-list '(widget)
		  :function #'(lambda (args next-methods)
				(declare (ignore next-methods))
				(child-slot-value (first args) slot-name)))))

(defmethod pcl::add-writer-method
    ((class child-class) generic-function slot-name)
  (add-method
   generic-function
   (make-instance 'standard-method
		  :specializers (list (find-class t) (find-class 'widget))
		  :lambda-list '(value widget)
		  :function #'(lambda (args next-methods)
				(declare (ignore next-methods))
				(destructuring-bind (value widget) args
				  (setf
				   (child-slot-value widget slot-name)
				   value))))))


(defmethod validate-superclass ((class child-class) (super pcl::standard-class))
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
	  (map-c-array 'list #'identity array 'param length)
	(deallocate-memory array)))))

(defun default-container-child-name (container-class)
  (intern (format nil "~A-CHILD" container-class)))

(defun expand-container-type (type-number &optional slots)
  (let* ((class (type-from-number type-number))
	 (super (supertype type-number))
	 (child-class (default-container-child-name class))
	 (expanded-child-slots
	  (mapcar
	   #'(lambda (param)
	       (with-slots (name flags value-type documentation) param
	         (let* ((slot-name (default-slot-name name))
			(slot-type (type-from-number value-type #|t|#))
			(accessor
			 (default-slot-accessor class slot-name slot-type)))
		   `(,slot-name
		     :allocation :property
		     :pname ,name
		     ,@(cond
			((and
			  (member :writable flags)
			  (member :readable flags))
			 (list :accessor accessor))
			((member :writable flags)
			 (list :writer `(setf ,accessor)))
			((member :readable flags)
			 (list :reader accessor)))
		     ,@(when (or
			      (member :construct flags)
			      (member :writable flags))
			 (list :initarg (intern (string slot-name) "KEYWORD")))
		     :type ,slot-type
		     ,@(when documentation
			 (list :documentation documentation))))))
	   (query-container-class-child-properties type-number))))
    `(progn
       ,(expand-gobject-type type-number slots)
       (defclass ,child-class
	 (,(default-container-child-name super))
	 ,expanded-child-slots
	 (:metaclass child-class)
	 (:container ,class)))))

(register-derivable-type 'container "GtkContainer" 'expand-container-type)
