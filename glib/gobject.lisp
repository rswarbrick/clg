;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gobject.lisp,v 1.21 2004-11-09 12:47:44 espen Exp $

(in-package "GLIB")


;;;; Metaclass used for subclasses of gobject

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject-class (ginstance-class)
    ())

  (defmethod validate-superclass ((class gobject-class)
				(super pcl::standard-class))
;  (subtypep (class-name super) 'gobject)
    t))

(defclass direct-property-slot-definition (direct-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :initform t :reader slot-readable-p :initarg :readable)
   (writable :initform t :reader slot-writable-p :initarg :writable)
   (construct :initform nil :initarg :construct)))

(defclass effective-property-slot-definition (effective-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :reader slot-readable-p :initarg :readable)
   (writable :reader slot-writable-p :initarg :writable)
   (construct :initarg :construct)));)

(defbinding %object-ref () pointer
  (location pointer))

(defbinding %object-unref () nil
  (location pointer))

(defmethod reference-foreign ((class gobject-class) location)
  (declare (ignore class))
  (%object-ref location))

(defmethod unreference-foreign ((class gobject-class) location)
  (declare (ignore class))
  (%object-unref location))


; (defbinding object-class-install-param () nil
;   (class pointer)
;   (id unsigned-int)
;   (parameter parameter))

; (defbinding object-class-find-param-spec () parameter
;   (class pointer)
;   (name string))

(defun signal-name-to-string (name)
  (substitute #\_ #\- (string-downcase (string name))))


(defmethod direct-slot-definition-class ((class gobject-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-property-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-property-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class gobject-class) direct-slotds)
  (if (eq (most-specific-slot-value direct-slotds 'allocation) :property)
      (nconc 
       (list :pname (signal-name-to-string 
		     (most-specific-slot-value direct-slotds 'pname))
	     :readable (most-specific-slot-value direct-slotds 'readable)
	     :writable (most-specific-slot-value direct-slotds 'writable)
	     :construct (most-specific-slot-value direct-slotds 'construct))
       (call-next-method))
    (call-next-method)))


(defmethod initialize-internal-slot-functions ((slotd effective-property-slot-definition))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (type-number (find-type-number type)))
    (unless (slot-boundp slotd 'reader-function)
      (setf 
       (slot-value slotd 'reader-function)
       (if (slot-readable-p slotd)
	   (let () ;(reader (reader-function (type-from-number type-number))))
	     #'(lambda (object)
		 (let ((gvalue (gvalue-new type-number)))
		   (%object-get-property object pname gvalue)
		   (unwind-protect
			(funcall #|reader|# (reader-function (type-from-number type-number))  gvalue +gvalue-value-offset+)
		     (gvalue-free gvalue t)))))
	   #'(lambda (value object)
	       (error "Slot is not readable: ~A" (slot-definition-name slotd))))))
    
    (unless (slot-boundp slotd 'writer-function)
      (setf 
       (slot-value slotd 'writer-function)
       (if (slot-writable-p slotd)
	   (let ();; (writer (writer-function (type-from-number type-number)))
;; 		 (destroy (destroy-function (type-from-number type-number))))
	     #'(lambda (value object)
		 (let ((gvalue (gvalue-new type-number)))
		   (funcall #|writer|# (writer-function (type-from-number type-number)) value gvalue +gvalue-value-offset+)
		   (%object-set-property object pname gvalue)
;		   (funcall #|destroy|#(destroy-function (type-from-number type-number)) gvalue +gvalue-value-offset+)
		   (gvalue-free gvalue t)
		   value)))
 	   #'(lambda (value object)
 	       (error "Slot is not writable: ~A" (slot-definition-name slotd))))))
    
    (unless (slot-boundp slotd 'boundp-function)
      (setf 
       (slot-value slotd 'boundp-function)
       #'(lambda (object)
	   (declare (ignore object))
	   t))))
  (call-next-method))


;;;; Super class for all classes in the GObject type hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass gobject-class)
    (:alien-name "GObject")))

(defmethod initialize-instance ((object gobject) &rest initargs)
  ;; Extract initargs which we should pass directly to the GObeject
  ;; constructor
  (let* ((slotds (class-slots (class-of object)))
	 (args (loop 
		as tmp = initargs then (cddr tmp) while tmp
		as key = (first tmp)
		as value = (second tmp)
		as slotd = (find-if
			    #'(lambda (slotd)
				(member key (slot-definition-initargs slotd)))
			    slotds)
		when (and (typep slotd 'effective-property-slot-definition)
			  (slot-value slotd 'construct))
		collect (progn 
			  (remf initargs key)
			  (list 
			   (slot-definition-pname slotd)
			   (slot-definition-type slotd)
			   value)))))
    (if args
	(let* ((string-size (size-of 'string))
	       (string-writer (writer-function 'string))
	       (string-destroy (destroy-function 'string))
	       (params (allocate-memory 
			(* (length args) (+ string-size +gvalue-size+)))))
	  (loop
	   for (pname type value) in args
	   as tmp = params then (sap+ tmp (+ string-size +gvalue-size+))
	   do (funcall string-writer pname tmp)
	   (gvalue-init (sap+ tmp string-size) type value))
	  (unwind-protect
	       (setf  
		(slot-value object 'location) 
		(%gobject-newv (type-number-of object) (length args) params))
	    (loop
	     repeat (length args)
	     as tmp = params then (sap+ tmp (+ string-size +gvalue-size+))
	     do (funcall string-destroy tmp)
	     (gvalue-unset (sap+ tmp string-size)))
	    (deallocate-memory params)))
	(setf  
	 (slot-value object 'location) 
	 (%gobject-new (type-number-of object)))))

  (apply #'call-next-method object initargs))


(defmethod instance-finalizer ((instance gobject))
  (let ((location (proxy-location instance)))
    #'(lambda ()
	(remove-cached-instance location)
	(%object-unref location))))


(defbinding (%gobject-new "g_object_new") () pointer
  (type type-number)
  (nil null))

(defbinding (%gobject-newv "g_object_newv") () pointer
  (type type-number)
  (n-parameters unsigned-int)
  (params pointer))



;;;; Property stuff

(defbinding %object-set-property () nil
  (object gobject)
  (name string)
  (value gvalue))

(defbinding %object-get-property () nil
  (object gobject)
  (name string)
  (value gvalue))

(defbinding %object-notify () nil
  (object gobject)
  (name string))

(defbinding object-freeze-notify () nil
  (object gobject))

(defbinding object-thaw-notify () nil
  (object gobject))

(defbinding %object-set-qdata-full () nil
  (object gobject)
  (id quark)
  (data unsigned-long)
  (destroy-marshal pointer))


;;;; User data

(defun (setf object-data) (data object key &key (test #'eq))
  (%object-set-qdata-full
   object (quark-from-object key :test test)
   (register-user-data data) (callback %destroy-user-data))
  data)

(defbinding %object-get-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun object-data (object key &key (test #'eq))
  (find-user-data
   (%object-get-qdata object (quark-from-object key :test test))))


;;;;

(defbinding %object-class-list-properties () pointer
  (class pointer)
  (n-properties unsigned-int :out))


(defun %map-params (params length type inherited-p)
  (if inherited-p
      (map-c-vector 'list #'identity params 'param length)
    (let ((properties ()))
      (map-c-vector 'list 
       #'(lambda (param)
	   (when (eql (param-owner-type param) type)
	     (push param properties)))
       params 'param length)
      (nreverse properties))))

(defun query-object-class-properties (type &optional inherited-p)
  (let* ((type-number (find-type-number type))
	 (class (type-class-ref type-number)))
    (unwind-protect
	 (multiple-value-bind (array length)
	     (%object-class-list-properties class)
	   (unwind-protect
		(%map-params array length type-number inherited-p)
	     (deallocate-memory array)))
;      (type-class-unref type-number)
      )))


(defun default-slot-name (name)
  (intern (substitute #\- #\_ (string-upcase (string-upcase name)))))

(defun default-slot-accessor (class-name slot-name type)
  (intern
   (format
    nil "~A-~A~A" class-name slot-name
    (if (eq type 'boolean) "-P" ""))))


(defun slot-definition-from-property (class property)
  (with-slots (name flags value-type documentation) property
    (let* ((slot-name (default-slot-name name))
	   (slot-type (or (type-from-number value-type) value-type))
	   (accessor (default-slot-accessor class slot-name slot-type)))
      
      `(,slot-name
	:allocation :property :pname ,name
	
	;; accessors
	,@(cond
	   ((and
	     (member :writable flags) (member :readable flags)
	     (not (member :construct-only flags)))
	    (list :accessor accessor))
	   ((and (member :writable flags) (not (member :construct-only flags)))
	    (list :writer `(setf ,accessor)))
	   ((member :readable flags)
	    (list :reader accessor)))

	;; readable/writable/construct
	,@(when (or (not (member :writable flags))
		    (member :construct-only flags))
	    '(:writable nil))
	,@(when (not (member :readable flags))
	    '(:readable nil))
	,@(when (or (member :construct flags) 
		    (member :construct-only flags))
	    '(:construct t))
	
	;; initargs
	,@(when (or (member :construct flags)
		    (member :construct-only flags)
		    (member :writable flags))
	    (list :initarg (intern (string slot-name) "KEYWORD")))
	
	:type ,slot-type
	:documentation ,documentation))))


(defun slot-definitions (class properties slots)
  (loop 
   with manual-slots = slots
   for property in properties
   unless (find-if 
	   #'(lambda (slot)
	       (destructuring-bind (name &rest args) slot
		 (or 
		  (equal (param-name property) (getf args :pname))
		  (eq (default-slot-name (param-name property)) name))))
	   manual-slots)
   do (push (slot-definition-from-property class property) slots))
  (delete-if #'(lambda (slot) (getf (rest slot) :ignore)) slots))


(defun expand-gobject-type (type &optional options (metaclass 'gobject-class))
  (let ((supers (cons (supertype type) (implements type)))
	(class  (type-from-number type))
	(slots (getf options :slots)))    
    `(defclass ,class ,supers
      ,(slot-definitions class (query-object-class-properties type) slots)
      (:metaclass ,metaclass)
      (:alien-name ,(find-type-name type)))))


(register-derivable-type 'gobject "GObject" 'expand-gobject-type)
