;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: gtype.lisp,v 1.2 2000-08-15 14:42:34 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; 

(deftype type-number () '(unsigned 32))

(define-foreign ("g_type_name" alien-type-name) (type) (static string)
  ((find-type-number type) type-number))

(define-foreign %type-from-name () type-number
  (name string))

;(define-foreign type-parent () type-number
;  (type type-number))

(define-foreign type-instance-size (type) int
  ((find-type-number type) type-number))

(define-foreign type-create-instance (type) pointer
  ((find-type-number type) type-number))

(define-foreign type-free-instance () nil
  (instance pointer))


(defvar *type-to-number-hash* (make-hash-table))
(defvar *number-to-type-hash* (make-hash-table))

(defun type-number-from-alien-name (name &optional (error t))
  (if (string= name "invalid")
      0
    (let ((type-number (%type-from-name name)))
      (cond
       ((and (zerop type-number) error)
	(error "Invalid alien type name: ~A" name))
       ((zerop type-number) nil)
       (t type-number)))))

(defun (setf alien-type-name) (alien-name type)
  (let ((type-name (ensure-type-name type))
	(type-number (type-number-from-alien-name alien-name)))
    (setf (gethash type-number *number-to-type-hash*) type-name)
    (setf (gethash type-name *type-to-number-hash*) type-number)))

(defun (setf find-type-number) (type-number type)
  (setf (gethash (ensure-type-name type) *type-to-number-hash*) type-number))

(defun find-type-number (type)
  (etypecase type
    (integer type)
    (symbol (gethash type *type-to-number-hash*))
    (pcl::class (gethash (class-name type) *type-to-number-hash*))))
 
(defun type-from-number (type-number)
  (gethash type-number *number-to-type-hash*))

(defun type-number-of (object)
  (find-type-number (type-of object)))



;;;; Superclass for all metaclasses implementing some sort of virtual slots

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass virtual-class (pcl::standard-class))

  (defclass direct-virtual-slot-definition (standard-direct-slot-definition)
    ((location
      :reader slot-definition-location
      :initarg :location)))
  
  (defclass effective-virtual-slot-definition
    (standard-effective-slot-definition)))
  

(defmethod direct-slot-definition-class ((class virtual-class) initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'direct-virtual-slot-definition)
    (call-next-method)))


(defmethod effective-slot-definition-class ((class virtual-class) initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'effective-virtual-slot-definition)
    (call-next-method)))


(defun %direct-slot-definitions-slot-value (slotds slot &optional default)
  (let ((slotd
	 (find-if
	  #'(lambda (slotd)
	      (and
	       (slot-exists-p slotd slot)
	       (slot-boundp slotd slot)))
	  slotds)))
    (if slotd
	(slot-value slotd slot)
      default)))
  

(defgeneric compute-virtual-slot-location (class slotd direct-slotds))

(defmethod compute-virtual-slot-location
    ((class virtual-class)
     (slotd effective-virtual-slot-definition)
     direct-slotds)
    (let ((location
	   (%direct-slot-definitions-slot-value direct-slotds 'location)))
      (if (and location (symbolp location))
	  (list location `(setf ,location))
	location)))


(defmethod compute-effective-slot-definition
    ((class virtual-class) direct-slotds)
  (let ((slotd (call-next-method)))
    (when (typep slotd 'effective-virtual-slot-definition)
      (setf
       (slot-value slotd 'pcl::location)
       (compute-virtual-slot-location class slotd direct-slotds)))
    slotd))


(defmethod slot-value-using-class
    ((class virtual-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (let ((reader (first (slot-definition-location slotd))))
    (if reader
	(funcall reader object)
      (slot-unbound class object (slot-definition-name slotd)))))


(defmethod slot-boundp-using-class
    ((class virtual-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
   (and (first (slot-definition-location slotd)) t))
    


(defmethod (setf slot-value-using-class)
    (value (class virtual-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (let ((writer (second (slot-definition-location slotd))))
    (cond
     ((null writer)
      (error
       "Can't set read-only slot ~A in ~A"
       (slot-definition-name slotd)
       object))
     ((or (functionp writer) (symbolp writer))
      (funcall writer value object)
      object)
     (t
      (funcall (fdefinition writer) value object)
      object))))
	

(defmethod validate-superclass
    ((class virtual-class) (super pcl::standard-class))
  t)



;;;; Superclass for wrapping of C structures

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass alien-instance ()
    ((location
      :reader alien-instance-location
      :type system-area-pointer)))

  (defgeneric allocate-alien-storage (class))
  (defgeneric reference-instance (object))
  (defgeneric unreference-instance (object))
  (defgeneric from-alien-initialize-instance (object &rest initargs))
  (defgeneric instance-finalizer (object)))


(internal *instance-cache*)
(defvar *instance-cache* (make-hash-table :test #'eql))

(defun cache-instance (object)
  (setf
   (gethash (system:sap-int (alien-instance-location object)) *instance-cache*)
   (ext:make-weak-pointer object)))

(defun find-cached-instance (location)
  (let ((ref (gethash (system:sap-int location) *instance-cache*)))
    (when ref
      (ext:weak-pointer-value ref))))

(defun remove-cached-instance (location)
  (remhash (system:sap-int location) *instance-cache*))


(defmethod initialize-instance :before ((instance alien-instance)
					&rest initargs &key)
  (declare (ignore initargs))
  (setf
   (slot-value instance 'location)
   (allocate-alien-storage (class-of instance)))
  (cache-instance instance)
  (ext:finalize instance (instance-finalizer instance)))


(defmethod from-alien-initialize-instance ((instance alien-instance)
					   &rest initargs &key location)
  (declare (ignore initargs))
  (setf (slot-value instance 'location) location)
  (cache-instance instance))


(deftype-method translate-type-spec alien-instance (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)



;;;; Metaclass used for subclasses of alien-instance

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass alien-class (virtual-class)
    ((size
      :reader alien-class-size)))

  (defclass direct-alien-slot-definition (direct-virtual-slot-definition)
    ((allocation
      :initform :alien)
     (offset
      :reader slot-definition-offset
      :initarg :offset
      :initform 0)))
  
  (defclass effective-alien-slot-definition (effective-virtual-slot-definition)
    ((offset
      :reader slot-definition-offset)))
  
  (defclass effective-virtual-alien-slot-definition
    (effective-virtual-slot-definition))
 

  (defmethod alien-class-superclass ((class alien-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'alien-instance))
     (pcl::class-direct-superclasses class)))


  (defmethod shared-initialize ((class alien-class) names
				&rest initargs &key size alien-name name)
    (declare (ignore initargs))
    (call-next-method)

    (when alien-name
      (setf (alien-type-name (or name (class-name class))) (first alien-name)))
    (when size
      (setf (slot-value class 'size) (first size))))
    

  (defmethod shared-initialize :after ((class alien-class) names
				       &rest initargs &key)
    (declare (ignore initargs names))
    (let* ((super (alien-class-superclass class))
	   (actual-size
	    (if (eq (class-name super) 'alien-instance)
		0
	      (alien-class-size super))))
      (dolist (slotd (class-slots class))
	(when (eq (slot-definition-allocation slotd) :alien)
	  (with-slots (offset type) slotd
	    (setq actual-size (max actual-size (+ offset (size-of type)))))))
      (cond
       ((not (slot-boundp class 'size))
	(setf (slot-value class 'size) actual-size))
       ((> actual-size (slot-value class 'size))
	(warn "The actual size of class ~A is lager than specified" class)))))


  (defmethod direct-slot-definition-class ((class alien-class) initargs)
    (case (getf initargs :allocation)
      ((nil :alien) (find-class 'direct-alien-slot-definition))
;      (:instance (error "Allocation :instance not allowed in class ~A" class))
      (t (call-next-method))))


  (defmethod effective-slot-definition-class ((class alien-class) initargs)
    (case (getf initargs :allocation)
      (:alien (find-class 'effective-alien-slot-definition))
      (:virtual (find-class 'effective-virtual-alien-slot-definition))
      (t (call-next-method))))
  
  
  (defmethod compute-virtual-slot-location
      ((class alien-class) (slotd effective-alien-slot-definition)
       direct-slotds)
    (with-slots (offset type) slotd
      (setf offset (%direct-slot-definitions-slot-value direct-slotds 'offset))
      (let ((reader (get-reader-function type))
	    (writer (get-writer-function type))
	    (destroy (get-destroy-function type)))
	(list
	 #'(lambda (object)
	     (funcall reader (alien-instance-location object) offset))
	 #'(lambda (value object)
	     (let ((location (alien-instance-location object)))
	       (funcall destroy location offset)
	       (funcall writer value location offset)))))))
	     
  
  (defmethod compute-virtual-slot-location
      ((class alien-class)
       (slotd effective-virtual-alien-slot-definition)
       direct-slotds)
    (let ((location (call-next-method)))
      (if (or (stringp location) (consp location))
	  (destructuring-bind (reader &optional writer) (mklist location)
	    (with-slots (type) slotd
              (list
	       (if (stringp reader)
		   (let* ((alien-type (translate-type-spec type))
			  (alien
			   (alien::%heap-alien
			    (alien::make-heap-alien-info
			     :type (alien::parse-alien-type
				    `(function ,alien-type system-area-pointer))
			     :sap-form (system:foreign-symbol-address reader))))
			  (from-alien (get-from-alien-function type)))
		     #'(lambda (object)
			 (funcall
			  from-alien
			  (alien-funcall
			   alien (alien-instance-location object)))))
		 reader)
	       (if (stringp writer)
		   (let* ((alien-type (translate-type-spec type))
			  (alien
			   (alien::%heap-alien
			    (alien::make-heap-alien-info
			     :type (alien::parse-alien-type
				    `(function
				      void ,alien-type system-area-pointer))
			     :sap-form (system:foreign-symbol-address writer))))
			  (to-alien (get-to-alien-function type))
			  (cleanup  (get-cleanup-function type)))
		     #'(lambda (value object)
			 (let ((alien-value (funcall to-alien value))
			       (location (alien-instance-location object)))
			   (alien-funcall alien location alien-value)
			   (funcall cleanup alien-value))))
		 writer))))
	location)))


  (defmethod compute-slots ((class alien-class))
    ;; Translating the user supplied relative (to previous slot) offsets
    ;; to absolute offsets.
    ;; This code is broken and have to be fixed for real use.
    (with-slots (direct-slots) class
      (let* ((super (alien-class-superclass class))
	     (slot-offset
	      (if (eq (class-name super) 'alien-instance)
		  0
		(alien-class-size super))))
	(dolist (slotd direct-slots)
	  (when (eq (slot-definition-allocation slotd) :alien)
	    (with-slots (offset type) slotd
	      (setf
	       offset (+ slot-offset offset)
	       slot-offset (+ offset (size-of type)))))))
    
      ;; Reverse the direct slot definitions so the effective slots
      ;; will be in correct order.
      (setf direct-slots (reverse direct-slots))
      ;; This nreverse caused me so much frustration that I leave it
      ;; here just as a reminder of what not to do.
;      (setf direct-slots (nreverse direct-slots))
      )
    (call-next-method))


  (defmethod validate-superclass ((class alien-class)
				  (super pcl::standard-class))
     (subtypep (class-name super) 'alien-instance))

  (defgeneric make-instance-from-alien (class location &rest initargs &key)))

(defmethod make-instance-from-alien ((class symbol) location
				     &rest initargs &key)
  (apply #'make-instance-from-alien (find-class class) location initargs))

(defmethod make-instance-from-alien ((class alien-class) location
				     &rest initargs &key)
  (let ((instance (allocate-instance class)))
    (apply
     #'from-alien-initialize-instance
     instance :location location initargs)
    instance))

(defun ensure-alien-instance (class location &rest initargs)
  (or
   (find-cached-instance location)
   (apply #'make-instance-from-alien class location initargs)))

(defmethod allocate-alien-storage ((class alien-class))
  (allocate-memory (alien-class-size class)))



;;;; Superclass for wrapping structures with reference counting

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass alien-object (alien-instance)
    ()
    (:metaclass alien-class)
    (:size 0)))

(define-type-method-fun alien-ref (type-spec))
(define-type-method-fun alien-unref (type-spec))

(defmethod from-alien-initialize-instance ((object alien-object)
					   &rest initargs &key)
  (declare (ignore initargs))
  (call-next-method)
  (reference-instance object))

(defmethod instance-finalizer ((object alien-object))
  (let ((location (alien-instance-location object))
	(unref (fdefinition (alien-unref (class-of object)))))
    (declare (type system-area-pointer location) (type function unref))
    #'(lambda ()
	(remove-cached-instance location)
	(funcall unref location))))

(defmethod reference-instance ((object alien-object))
  (funcall (alien-ref (class-of object)) object)
  object)

(defmethod unreference-instance ((object alien-object))
  (funcall (alien-unref (class-of object)) object)
  nil)

(deftype-method translate-to-alien
    alien-object (type-spec object &optional copy)
  (if copy
      `(,(alien-ref type-spec) ,object)
    `(alien-instance-location ,object)))

(deftype-method translate-from-alien
    alien-object (type-spec location &optional alloc)
  (declare (ignore alloc))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-alien-instance ',type-spec location))))

(deftype-method
    cleanup-alien alien-object (type-spec sap &optional copied)
  (when copied
    `(let ((sap ,sap))
       (unless (null-pointer-p sap)
	 (,(alien-unref type-spec) sap)))))



;;;; Superclass for wrapping of non-refcounted structures

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass alien-structure (alien-instance)
    ((static
      :allocation :instance
      :reader alien-structure-static-p
      :initform nil
      :type boolean))
    (:metaclass alien-class)
    (:size 0)))

(define-type-method-fun alien-copier (type-spec))
(define-type-method-fun alien-deallocator (type-spec))

(defmethod from-alien-initialize-instance ((structure alien-structure)
					   &rest initargs &key static)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value structure 'static) static))

(defmethod instance-finalizer ((structure alien-structure))
  (let ((location (alien-instance-location structure)))
    (declare (type system-area-pointer location))
    (if (alien-structure-static-p structure)
	#'(lambda ()
	    (remove-cached-instance location))
      (let ((deallocator
	     (fdefinition (alien-deallocator (class-of structure)))))
	(declare (type function deallocator))
	#'(lambda ()
	    (remove-cached-instance location)
	    (funcall deallocator location))))))


(deftype-method alien-copier alien-structure (type-spec)
  (declare (ignore type-spec))
  'copy-memory)

(deftype-method alien-deallocator alien-structure (type-spec)
  (declare (ignore type-spec))
  'deallocate-memory)

(deftype-method translate-to-alien
    alien-structure (type-spec object &optional copy)
  `(let ((object ,object))
     (if (and ,copy (not (alien-structure-static-p object)))
	 (,(alien-copier type-spec)
	  `(alien-instance-location object)
	  ,(alien-class-size (find-class type-spec)))
       (alien-instance-location object))))

(deftype-method translate-from-alien
    alien-structure (type-spec location &optional (alloc :dynamic))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       ,(ecase alloc
	  (:dynamic `(ensure-alien-instance ',type-spec location))
	  (:static `(ensure-alien-instance ',type-spec location :static t))
	  (:copy `(ensure-alien-instance
		   ',type-spec
		   `(,(alien-copier type-spec)
		     location ,(alien-class-size (find-class type-spec)))))))))

(deftype-method cleanup-alien alien-structure (type-spec sap &optional copied)
  (when copied
    `(let ((sap ,sap))
       (unless (or
		(null-pointer-p sap)
		(alien-structure-static-p (find-cached-instance sap)))
	 (,(alien-deallocator type-spec) sap)))))



;;;; Superclass for static structures such as gdk:visual

(defclass static-structure (alien-structure)
  ()
  (:metaclass alien-class)
  (:size 0))


(defmethod from-alien-initialize-instance ((structure alien-structure)
				      &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value structure 'static) t))



;;;; Superclass wrapping types in the glib type system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gtype (alien-object)
    ()
    (:metaclass alien-class)
    (:size 4 #|(size-of 'pointer)|#)))


(defun %alien-instance-type-number (location)
  (let ((class (sap-ref-sap location 0)))
    (sap-ref-unsigned class 0)))


(deftype-method translate-from-alien gtype (type-spec location &optional alloc)
  (declare (ignore type-spec alloc))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-alien-instance
	(type-from-number (%alien-instance-type-number location))
	location))))



;;;; Metaclass for subclasses of gtype-class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gtype-class (alien-class)))


(defmethod shared-initialize ((class gtype-class) names
			      &rest initargs &key name)
  (declare (ignore initargs names))
  (call-next-method)
  (setf
   (slot-value class 'size)
   (type-instance-size (find-type-number (or name (class-name class))))))


(defmethod validate-superclass
    ((class gtype-class) (super pcl::standard-class))
  (subtypep (class-name super) 'gtype))


(defmethod allocate-alien-storage ((class gtype-class))
  (type-create-instance (find-type-number class)))


;;;; Initializing type numbers

(setf (alien-type-name 'invalid) "invalid")
(setf (alien-type-name 'char) "gchar")
(setf (alien-type-name 'unsigned-char) "guchar")
(setf (alien-type-name 'boolean) "gboolean")
(setf (alien-type-name 'int) "gint")
(setf (alien-type-name 'unsigned-int) "guint")
(setf (alien-type-name 'long) "glong")
(setf (alien-type-name 'unsigned-long) "gulong")
(setf (alien-type-name 'enum) "GEnum")
(setf (alien-type-name 'flags) "GFlags")
(setf (alien-type-name 'single-float) "gfloat")
(setf (alien-type-name 'double-float) "gdouble")
(setf (alien-type-name 'string) "gstring")
(setf (find-type-number 'fixnum) (find-type-number 'int))
