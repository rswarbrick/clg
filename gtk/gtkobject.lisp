;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gtkobject.lisp,v 1.9 2001-05-29 16:00:52 espen Exp $


(in-package "GTK")


;;;; Misc utils

; (defun name-to-string (name)
;   (substitute #\_ #\- (string-downcase (string name))))

; (defun string-to-name (name &optional (package "KEYWORD"))
;   (intern (substitute #\- #\_ (string-upcase name)) package))


;;; Argument stuff - to be removed soon

(deftype arg () 'pointer)

(defconstant +arg-type-offset+ 0)
(defconstant +arg-name-offset+ 4)
(defconstant +arg-value-offset+ 8)
(defconstant +arg-size+ 16)

(defbinding arg-new () arg
  (type type-number))

(defbinding %arg-free () nil
  (arg arg)
  (free-contents boolean))

(defun arg-free (arg free-contents &optional alien)
  (cond
   (alien (%arg-free arg free-contents))
   (t
    (unless (null-pointer-p arg)
      (when free-contents
	(funcall
	 (intern-destroy-function (type-from-number (arg-type arg)))
	 arg +arg-value-offset+))
      (deallocate-memory arg)))))

(defbinding %arg-reset () nil
  (arg arg))

(defun arg-name (arg)
  (funcall (intern-reader-function 'string) arg +arg-name-offset+))

(defun (setf arg-name) (name arg)
  (funcall (intern-writer-function 'string) name arg +arg-name-offset+)
  name)

(defun arg-type (arg)
  (system:sap-ref-32 arg +arg-type-offset+))

(defun (setf arg-type) (type arg)
  (setf (system:sap-ref-32 arg +arg-type-offset+) type))

(defun arg-value (arg &optional (type (type-from-number (arg-type arg))))
  (funcall (intern-reader-function type) arg +arg-value-offset+))

;; One should never call this function on an arg whose value is already set
(defun (setf arg-value)
    (value arg &optional (type (type-from-number (arg-type arg))))
  (funcall (intern-writer-function type) value arg +arg-value-offset+)
  value)

(defun (setf return-arg-value)
    (value arg &optional (type (type-from-number (arg-type arg))))
  ; this is probably causing a memory leak
  (funcall (intern-writer-function type) value (arg-value arg 'pointer) 0)
  value)

(defun arg-array-ref (arg0 index)
  (system:sap+ arg0 (* index +arg-size+)))


;;;; Superclass for the gtk class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library "/opt/gnome/lib/libgtk-x11-1.3.so")

  (defclass %object (gobject)
    ()
    (:metaclass gobject-class)
    (:alien-name "GtkObject")))


(defmethod shared-initialize ((object %object) names &rest initargs
			      &key signals)
  (declare (ignore initargs names))
  (call-next-method)
  (%object-sink object)
  (dolist (signal signals)
    (apply #'signal-connect object signal)))

(defmethod initialize-proxy ((object %object) &rest initargs &key location)
  (declare (ignore initargs))
  (call-next-method)
  (%object-sink location))

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

(system:add-fd-handler (gdk:event-poll-fd) :input #'main-iterate-all)
(setq lisp::*periodic-polling-function* #'main-iterate-all)
(setq lisp::*max-event-to-sec* 0)
(setq lisp::*max-event-to-usec* 1000)



;;;; Metaclass for child classes
 
(defvar *container-to-child-class-mappings* (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass child-class (virtual-slot-class))

  (defclass direct-child-slot-definition (direct-virtual-slot-definition)
    ((arg-name :reader slot-definition-arg-name)))

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
				 &rest initargs &key arg-name)
  (declare (ignore initargs))
  (call-next-method)
  (if arg-name
      (setf (slot-value slotd 'arg-name) arg-name)
    (error "Need argument name for slot with allocation :arg")))

(defmethod direct-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'direct-child-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'effective-child-slot-definition))
    (t (call-next-method))))

(defmethod compute-virtual-slot-accessor
    ((class child-class) (slotd effective-child-slot-definition) direct-slotds)
  (with-slots (type) slotd
    (let ((arg-name (slot-definition-arg-name (first direct-slotds)))
	  (type-number (find-type-number type))
; 	  (reader (intern-reader-function type))
; 	  (writer (intern-writer-function type))
; 	  (destroy (intern-destroy-function type))
	  )
      (list
       #'(lambda (object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
	       (let ((arg (arg-new type-number)))
		 (setf (arg-name arg) arg-name)
		 (%container-child-getv parent child arg)
		 (prog1
		     (funcall
		      (intern-reader-function type)
		      arg +arg-value-offset+)
		   (arg-free arg t t))))))
       #'(lambda (value object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
  	       (let ((arg (arg-new type-number)))
		 (setf (arg-name arg) arg-name)
		 (funcall
		  (intern-writer-function type)
		  value arg +arg-value-offset+)
		 (%container-child-setv parent child arg)
		 (funcall
		  (intern-destroy-function type)
		  arg +arg-value-offset+)
		 (arg-free arg nil)
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
  (subtypep (class-name super) 'container-child))


(defclass container-child ()
  ((parent :initarg :parent :type container)
   (child :initarg :child :type widget)))


;;;;

(defbinding %container-query-child-args () arg
  (type-number type-number)
  (nil null)
  (n-args unsigned-int :out))

(defun query-container-type-dependencies (type-number)
  (let ((child-slot-types ()))
    (multiple-value-bind (args n-args)
	(%container-query-child-args type-number)
      (dotimes (i n-args)
	(push (arg-type (arg-array-ref args i)) child-slot-types)))
    (delete-duplicates
     (append (query-object-type-dependencies type-number) child-slot-types))))

(defun default-container-child-name (container-class)
  (intern (format nil "~A-CHILD" container-class)))

(defun expand-container-type (type-number &optional slots)
  (let* ((class (type-from-number type-number))
	 (super (supertype type-number))
	 (child-class (default-container-child-name class))
	 (child-slots ()))
    (multiple-value-bind (args n-args)
	(%container-query-child-args type-number)
      (dotimes (i n-args)
	(let* ((arg (arg-array-ref args i))
	       (arg-name (arg-name arg))
	       (slot-name (default-slot-name
			    (subseq arg-name (+ (position #\: arg-name) 2))))
	       (type (type-from-number (arg-type arg) #|t|#)))
	  (push
	   `(,slot-name
	     :allocation :arg
	     :arg-name ,arg-name
	     :accessor ,(default-slot-accessor child-class slot-name type)
	     :initarg ,(intern (string slot-name) "KEYWORD")
	     :type ,type)
	   child-slots)))
      `(progn
	 ,(expand-gobject-type type-number slots)
	 (defclass ,child-class
	   (,(default-container-child-name super))
	   ,child-slots
	   (:metaclass child-class)
	   (:container ,class))))))

(register-derivable-type
 'container "GtkContainer"
 :query 'query-container-type-dependencies
 :expand 'expand-container-type)
