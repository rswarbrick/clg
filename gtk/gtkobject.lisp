;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: gtkobject.lisp,v 1.3 2000-08-16 17:30:36 espen Exp $


(in-package "GTK")

;;;; Misc utils

(defun name-to-string (name)
  (substitute #\_ #\- (string-downcase (string name))))

(defun string-to-name (name &optional (package "KEYWORD"))
  (intern (substitute #\- #\_ (string-upcase name)) package))


;;;; Argument stuff

(deftype arg () 'pointer)

(defconstant +arg-type-offset+ 0)
(defconstant +arg-name-offset+ 4)
(defconstant +arg-value-offset+ 8)
(defconstant +arg-size+ 16)

(define-foreign arg-new () arg
  (type type-number))

(define-foreign %arg-free () nil
  (arg arg)
  (free-contents boolean))

(defun arg-free (arg free-contents &optional alien)
  (cond
   (alien (%arg-free arg free-contents))
   (t
    (unless (null-pointer-p arg)
      (when free-contents
	(funcall
	 (get-destroy-function (type-from-number (arg-type arg)))
	 arg +arg-value-offset+))
      (deallocate-memory arg)))))

(define-foreign %arg-reset () nil
  (arg arg))

(defun arg-name (arg)
  (funcall (get-reader-function '(static string)) arg +arg-name-offset+))

(defun (setf arg-name) (name arg)
  (funcall (get-writer-function '(static string)) name arg +arg-name-offset+)
  name)

(defun arg-type (arg)
  (system:sap-ref-32 arg +arg-type-offset+))

(defun (setf arg-type) (type arg)
  (setf (system:sap-ref-32 arg +arg-type-offset+) type))

(defun arg-value (arg &optional (type (type-from-number (arg-type arg))))
  (funcall (get-reader-function type) arg +arg-value-offset+))

;; One should never call this function on an arg whose value is already set
(defun (setf arg-value)
    (value arg &optional (type (type-from-number (arg-type arg))))
  (funcall (get-writer-function type) value arg +arg-value-offset+)
  value)

(defun (setf return-arg-value)
    (value arg &optional (type (type-from-number (arg-type arg))))
  ; this is probably causing a memory leak
  (funcall (get-writer-function type) value (arg-value arg 'pointer) 0)
  value)

(defun arg-array-ref (arg0 index)
  (system:sap+ arg0 (* index +arg-size+)))


;;;; Superclass for the gtk class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass object (gobject)
    ()
;   ((flags
;     :allocation :alien
;     :accessor object-flags
;     :type object-flags))
    (:metaclass gobject-class)
    (:alien-name "GtkObject")))


(defmethod shared-initialize ((object object) names &rest initargs &key signals)
  (declare (ignore initargs names))
  (call-next-method)
  (dolist (signal signals)
    (apply #'signal-connect object signal)))


(defmethod initialize-instance :after ((object object) &rest initargs &key)
  (declare (ignore initargs))
  (object-default-construct object)
  (reference-instance object)
  (object-sink object))


(defmethod from-alien-initialzie-instance ((object object) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (object-sink object))


(define-foreign object-default-construct () nil
  (object object))

(define-foreign object-sink () nil
  (object object))

(define-foreign ("gtk_object_getv" object-get-arg) () nil
  (object object)
  (1 unsigned-int)
  (arg arg))

(define-foreign ("gtk_object_setv" object-set-arg) () nil
  (object object)
  (1 unsigned-int)
  (arg arg))

(defun object-arg (object name)
  (with-gc-disabled
    (let ((arg (arg-new 0)))
      (setf (arg-name arg) name)
      (object-get-arg object arg)
      (let ((type (type-from-number (arg-type arg))))
	(prog1
	    (arg-value arg type)
	  (arg-free arg t))))))

(defun (setf object-arg) (value object name)
  (with-gc-disabled
    (let ((arg (arg-new 0)))
      (setf (arg-name arg) name)
      (object-get-arg object arg)
      (let* ((type-number (arg-type arg))
	     (type (type-from-number type-number)))
	(%arg-reset arg)
	(setf (arg-type arg) type-number)
	(setf (arg-value arg type) value)
	(object-set-arg object arg)
	(arg-free arg t))))
  value)


;;;; Callback and user data mechanism

(declaim (fixnum *user-data-count*))

(defvar *user-data* (make-hash-table))
(defvar *user-data-count* 0)

(defun register-user-data (object &optional destroy-function)
  (check-type destroy-function (or null symbol function))
;  (incf *user-data-count*)
  (setq *user-data-count* (the fixnum (1+ *user-data-count*)))
  (setf
   (gethash *user-data-count* *user-data*)
   (cons object destroy-function))
  *user-data-count*)


(defun find-user-data (id)
  (check-type id fixnum)
  (multiple-value-bind (user-data p) (gethash id *user-data*)
    (values (car user-data) p)))


(defun register-callback-function (function)
  (check-type function (or null symbol function))
  ; We treat callbacks just as ordinary user data
  (register-user-data function))


(defun callback-trampoline (callback-id nargs arg-array)
  (declare (fixnum callback-id nargs))
  (let* ((return-arg (unless (null-pointer-p arg-array)
		       (arg-array-ref arg-array nargs)))
	 (return-type (if return-arg
			  (type-from-number (arg-type return-arg))
			nil))
	 (args nil)
	 (callback-function (find-user-data callback-id)))
    
    (dotimes (n nargs)
      (push (arg-value (arg-array-ref arg-array (- nargs n 1))) args))

    (labels ((invoke-callback ()
	       (restart-case
		   (unwind-protect
		       (let ((return-value (apply callback-function args)))
			 (when return-type
			   (setf (return-arg-value return-arg) return-value))))
		
		 (continue nil :report "Return from callback function"
		  (when return-type
		    (format
		     *query-io*
		     "Enter return value of type ~S: "
		     return-type)
		    (force-output *query-io*)
		    (setf
		     (return-arg-value return-arg)
		     (eval (read *query-io*)))))
		 (re-invoke nil :report "Re-invoke callback function"
		  (invoke-callback)))))
      (invoke-callback))))


(defun destroy-user-data (id)
  (check-type id fixnum)
  (let ((user-data (gethash id *user-data*)))
    (when (cdr user-data)
      (funcall (cdr user-data) (car user-data))))
  (remhash id *user-data*))


(defvar *callback-marshal* (system:foreign-symbol-address "callback_marshal"))
(defvar *destroy-marshal* (system:foreign-symbol-address "destroy_marshal"))

(defun after-gc-hook ()
  (setf
   (extern-alien "callback_trampoline" system-area-pointer)
   (make-pointer (kernel:get-lisp-obj-address #'callback-trampoline))
   (extern-alien "destroy_user_data" system-area-pointer)
   (make-pointer (kernel:get-lisp-obj-address #'destroy-user-data))))

(pushnew 'after-gc-hook ext:*after-gc-hooks*)
(after-gc-hook)



;;;; Main loop

(declaim (inline events-pending-p main-iteration))

(define-foreign ("gtk_events_pending" events-pending-p) () boolean)

(define-foreign main-do-event () nil
  (event gdk:event))

(define-foreign main () nil)

(define-foreign main-level () int)

(define-foreign main-quit () nil)

(define-foreign
    ("gtk_main_iteration_do" main-iteration) (&optional (blocking t)) boolean
  (blocking boolean))

(defun main-iterate-all (&rest args)
  (declare (ignore args))
  (when (events-pending-p)
    (main-iteration nil)
    (main-iterate-all)))

(system:add-fd-handler (gdk:event-poll-fd) :input #'main-iterate-all)
(setq lisp::*periodic-polling-function* #'main-iterate-all)
(setq lisp::*max-event-to-sec* 0)
(setq lisp::*max-event-to-usec* 1000)



;;;; Signals

(define-foreign %signal-emit-stop () nil
  (object object)
  (signal-id unsigned-int))

(define-foreign %signal-emit-stop-by-name (object signal) nil
  (object object)
  ((name-to-string signal) string))

(defun signal-emit-stop (object signal)
  (if (numberp signal)
      (%signal-emit-stop object signal)
    (%signal-emit-stop-by-name object signal)))

(define-foreign %signal-connect-full
    (object signal function after) unsigned-int
  (object object)
  ((name-to-string signal) string)
  (0 unsigned-long)
  (*callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-marshal* pointer)
  (nil boolean)
  (after boolean))

(defun signal-connect (object signal function
		       &key after ((:object callback-object)))
  (let* ((callback-object (if (eq callback-object t)
			      object
			    callback-object))
	 (callback-function
	  (if callback-object
	      #'(lambda (&rest args) (apply function callback-object args))
	    function)))
    (%signal-connect-full object signal callback-function after)))

(define-foreign signal-disconnect () nil
  (object object)
  (handler unsigned-int))

(define-foreign signal-handler-block () nil
  (object object)
  (handler unsigned-int))

(define-foreign signal-handler-unblock () nil
  (object object)
  (handler unsigned-int))


;;;; Metaclass used for subclasses of object

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass object-class (gobject-class))

  (defclass direct-object-slot-definition (direct-virtual-slot-definition))

  (defclass effective-object-slot-definition
    (effective-virtual-slot-definition)))


(defmethod initialize-instance :after ((slotd direct-object-slot-definition)
				   &rest initargs &key)
  (declare (ignore initargs))
  (unless (slot-boundp slotd 'location)
    (with-slots (pcl::name location pcl::class) slotd
      (setf
       location 
       (format nil "~A::~A"
        (alien-type-name (class-name pcl::class))
	(name-to-string pcl::name))))))


(defmethod direct-slot-definition-class ((class object-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'direct-object-slot-definition))
    (t (call-next-method))))


(defmethod effective-slot-definition-class ((class object-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'effective-object-slot-definition))
    (t (call-next-method))))
  

(defmethod compute-virtual-slot-location
    ((class object-class) (slotd effective-object-slot-definition)
     direct-slotds)
  (with-slots (type) slotd
    (let ((location (slot-definition-location (first direct-slotds)))
	  (type-number (find-type-number type))
	  (reader (get-reader-function type))
	  (writer (get-writer-function type))
	  (destroy (get-destroy-function type)))
      (list
       #'(lambda (object)
	   (with-gc-disabled
	     (let ((arg (arg-new type-number)))
	       (setf (arg-name arg) location)
	       (object-get-arg object arg)
	       (prog1
		   (funcall reader arg +arg-value-offset+)
		 (arg-free arg t t)))))
       #'(lambda (value object)
	   (with-gc-disabled
  	     (let ((arg (arg-new type-number)))
	       (setf (arg-name arg) location)
	       (funcall writer value arg +arg-value-offset+)
	       (object-set-arg object arg)
	       (funcall destroy arg +arg-value-offset+)
	       (arg-free arg nil)
	       value)))))))


(defmethod validate-superclass ((class object-class)
				(super pcl::standard-class))
  (subtypep (class-name super) 'object))
  

;;;; Metaclasses used for widgets and containers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass widget-class (object-class))

  (defclass container-class (widget-class)
    (child-class)))


(defvar *child-to-container-class-mappings* (make-hash-table))

(defmethod shared-initialize ((class container-class) names
			      &rest initargs &key name child-class)
  (declare (ignore initargs))
  (call-next-method)
  (with-slots ((child-class-slot child-class)) class
    (setf
     child-class-slot
     (or
      (first child-class)
      (intern (format nil "~A-CHILD" (or name (class-name class)))))
     (gethash child-class-slot *child-to-container-class-mappings*)
     class)))


(defmethod validate-superclass ((class widget-class)
				(super pcl::standard-class))
  (subtypep (class-name super) 'widget))

(defmethod validate-superclass ((class container-class)
				(super pcl::standard-class))
  (subtypep (class-name super) 'container))



;;;; Metaclass for child classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass child-class (virtual-class))

  (defclass direct-child-slot-definition (direct-virtual-slot-definition))

  (defclass effective-child-slot-definition
    (effective-virtual-slot-definition)))


(defmethod initialize-instance  ((slotd direct-child-slot-definition)
				 &rest initargs &key)
  (declare (ignore initargs))
  (call-next-method)
  (unless (slot-boundp slotd 'location)
    (with-slots (pcl::name location pcl::class) slotd
      (setf
       location 
       (format nil "~A::~A"
        (alien-type-name
	 (gethash (class-name pcl::class) *child-to-container-class-mappings*))
	(name-to-string pcl::name))))))


(defmethod direct-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'direct-child-slot-definition))
    (t (call-next-method))))


(defmethod effective-slot-definition-class ((class child-class) initargs)
  (case (getf initargs :allocation)
    (:arg (find-class 'effective-child-slot-definition))
    (t (call-next-method))))
  

(defmethod compute-virtual-slot-location
    ((class child-class) (slotd effective-child-slot-definition) direct-slotds)
  (with-slots (type) slotd
    (let ((location (slot-definition-location (first direct-slotds)))
	  (type-number (find-type-number type))
	  (reader (get-reader-function type))
	  (writer (get-writer-function type))
	  (destroy (get-destroy-function type)))
      (list
       #'(lambda (object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
	       (let ((arg (arg-new type-number)))
		 (setf (arg-name arg) location)
		 (container-child-get-arg parent child arg)
		 (prog1
		     (funcall reader arg +arg-value-offset+)
		   (arg-free arg t t))))))
       #'(lambda (value object)
	   (with-slots (parent child) object	   
	     (with-gc-disabled
  	       (let ((arg (arg-new type-number)))
		 (setf (arg-name arg) location)
		 (funcall writer value arg +arg-value-offset+)
		 (container-child-set-arg parent child arg)
		 (funcall destroy arg +arg-value-offset+)
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


