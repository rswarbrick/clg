;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: proxy.lisp,v 1.3 2001-05-04 16:56:34 espen Exp $

(in-package "GLIB")


;;;; Superclass for all metaclasses implementing some sort of virtual slots

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass virtual-slot-class (pcl::standard-class))

  (defclass direct-virtual-slot-definition (standard-direct-slot-definition)
    ((setter :reader slot-definition-setter :initarg :setter)
     (getter :reader slot-definition-getter :initarg :getter)))
  
  (defclass effective-virtual-slot-definition
    (standard-effective-slot-definition)))
  

(defmethod direct-slot-definition-class ((class virtual-slot-class) initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'direct-virtual-slot-definition)
    (call-next-method)))

(defmethod effective-slot-definition-class ((class virtual-slot-class) initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'effective-virtual-slot-definition)
    (call-next-method)))

(defun %most-specific-slot-value (slotds slot &optional default)
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
 
(defgeneric compute-virtual-slot-accessors (class slotd direct-slotds))

(defmethod compute-virtual-slot-accessors
    ((class virtual-slot-class)
     (slotd effective-virtual-slot-definition)
     direct-slotds)
    (let ((getter (%most-specific-slot-value direct-slotds 'getter))
	  (setter (%most-specific-slot-value direct-slotds 'setter)))
      (list getter setter)))

(defmethod compute-effective-slot-definition
    ((class virtual-slot-class) direct-slotds)
  (let ((slotd (call-next-method)))
    (when (typep slotd 'effective-virtual-slot-definition)
      (setf
       (slot-value slotd 'pcl::location)
       (compute-virtual-slot-accessors class slotd direct-slotds)))
    slotd))

(defmethod slot-value-using-class
    ((class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (let ((reader (first (slot-definition-location slotd))))
    (if reader
	(funcall reader object)
      (slot-unbound class object (slot-definition-name slotd)))))

(defmethod slot-boundp-using-class
    ((class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
   (and (first (slot-definition-location slotd)) t))
    
(defmethod (setf slot-value-using-class)
    (value (class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (let ((setter (second (slot-definition-location slotd))))
    (cond
     ((null setter)
      (error
       "Can't set read-only slot ~A in ~A"
       (slot-definition-name slotd)
       object))
     ((or (functionp setter) (symbolp setter))
      (funcall setter value object)
      value)
     (t
      (funcall (fdefinition setter) value object)
      value))))
	
(defmethod validate-superclass
    ((class virtual-slot-class) (super pcl::standard-class))
  t)


;;;; Proxy cache

(internal *instance-cache*)
(defvar *instance-cache* (make-hash-table :test #'eql))

(defun cache-instance (instance)
  (setf
   (gethash (system:sap-int (proxy-location instance)) *instance-cache*)
   (ext:make-weak-pointer instance)))

(defun find-cached-instance (location)
  (let ((ref (gethash (system:sap-int location) *instance-cache*)))
    (when ref
      (ext:weak-pointer-value ref))))

(defun remove-cached-instance (location)
  (remhash (system:sap-int location) *instance-cache*))



;;;; Proxy for alien instances

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy ()
    ((location :reader proxy-location :type system-area-pointer)))

  (defgeneric initialize-proxy (object &rest initargs))
  (defgeneric instance-finalizer (object)))


(defmethod initialize-instance :after ((instance proxy)
				       &rest initargs &key)
  (declare (ignore initargs))
  (cache-instance instance)
  (ext:finalize instance (instance-finalizer instance)))

(defmethod initialize-proxy ((instance proxy)
			     &rest initargs &key location weak-ref)
  (declare (ignore initargs))
  (setf 
   (slot-value instance 'location)
   (if weak-ref
       (funcall
	(proxy-class-copy (class-of instance))
	(type-of instance) location)
     location))
  (cache-instance instance)
  (ext:finalize instance (instance-finalizer instance)))

(defmethod instance-finalizer ((instance proxy))
  (let ((free (proxy-class-free (class-of instance)))
	(type (type-of instance))
	(location (proxy-location instance)))
    (declare
     (type symbol type)
     (type system-area-pointer location))
    #'(lambda ()
	(funcall free type location)
	(remove-cached-instance location))))


(deftype-method translate-type-spec proxy (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of proxy (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-from-alien
    proxy (type-spec location &optional weak-ref)
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-proxy-instance ',type-spec location ,weak-ref))))

(deftype-method translate-to-alien
    proxy (type-spec instance &optional weak-ref)
  (if weak-ref
      `(proxy-location ,instance)
    `(funcall
      (proxy-class-copy (find-class ',type-spec))
      ',type-spec (proxy-location ,instance))))

(deftype-method unreference-alien proxy (type-spec location)
  `(funcall (proxy-class-free (find-class ',type-spec)) ',type-spec ,location))

(defun proxy-instance-size (proxy)
  (proxy-class-size (class-of proxy)))

;;;; Metaclass used for subclasses of proxy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy-class (virtual-slot-class)
    ((size :reader proxy-class-size)
     (copy :reader proxy-class-copy)
     (free :reader proxy-class-free)))

  (defclass direct-alien-slot-definition (direct-virtual-slot-definition)
    ((allocation :initform :alien)
     (offset :reader slot-definition-offset :initarg :offset)))
  
  (defclass effective-alien-slot-definition (effective-virtual-slot-definition)
    ((offset :reader slot-definition-offset)))
  
  (defclass effective-virtual-alien-slot-definition
    (effective-virtual-slot-definition))


  (defmethod most-specific-proxy-superclass ((class proxy-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'proxy))
     (cdr (pcl::compute-class-precedence-list class))))

  (defmethod direct-proxy-superclass ((class proxy-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'proxy))
     (pcl::class-direct-superclasses class)))

  (defmethod shared-initialize ((class proxy-class) names
				&rest initargs &key size copy free)
    (declare (ignore initargs))
    (call-next-method)
    (cond
     (size (setf (slot-value class 'size) (first size)))
     ((slot-boundp class 'size) (slot-makunbound class 'size)))
    (cond
     (copy (setf (slot-value class 'copy) (first copy)))
     ((slot-boundp class 'copy) (slot-makunbound class 'copy)))
    (cond
     (free (setf (slot-value class 'free) (first free)))
     ((slot-boundp class 'free) (slot-makunbound class 'free))))

  (defmethod finalize-inheritance ((class proxy-class))
    (call-next-method)
    (let ((super (direct-proxy-superclass class)))
      (unless (typep super 'proxy)
 	(unless (or (slot-boundp class 'copy) (not (slot-boundp super 'copy)))
 	  (setf (slot-value class 'copy) (proxy-class-copy super)))
 	(unless (or (slot-boundp class 'free) (not (slot-boundp super 'free)))
 	  (setf (slot-value class 'free) (proxy-class-free super))))))

  (defmethod direct-slot-definition-class ((class proxy-class) initargs)
    (case (getf initargs :allocation)
      ((nil :alien) (find-class 'direct-alien-slot-definition))
;      (:instance (error "Allocation :instance not allowed in class ~A" class))
      (t (call-next-method))))

  (defmethod effective-slot-definition-class ((class proxy-class) initargs)
    (case (getf initargs :allocation)
      (:alien (find-class 'effective-alien-slot-definition))
      (:virtual (find-class 'effective-virtual-alien-slot-definition))
      (t (call-next-method))))
  
  (defmethod compute-virtual-slot-accessors
      ((class proxy-class) (slotd effective-alien-slot-definition)
       direct-slotds)
    (with-slots (offset type) slotd
      (let ((reader (intern-reader-function type))
	    (writer (intern-writer-function type))
	    (destroy (intern-destroy-function type)))
	(setf offset (slot-definition-offset (first direct-slotds)))
	(list
	 #'(lambda (object)
	     (funcall reader (proxy-location object) offset))
	 #'(lambda (value object)
	     (let ((location (proxy-location object)))
	       (funcall destroy location offset)
	       (funcall writer value location offset)))))))
 
  (defmethod compute-virtual-slot-accessors
      ((class proxy-class)
       (slotd effective-virtual-alien-slot-definition)
       direct-slotds)
    (destructuring-bind (getter setter) (call-next-method)
      (let ((class-name (class-name class)))
	(with-slots (type) slotd
	  (list
	   (if (stringp getter)
	       (mkbinding getter type class-name)
	     getter)
	   (if (stringp setter)
	       (let ((setter (mkbinding setter 'nil class-name type)))
		 #'(lambda (value object)
		     (funcall setter object value)))
	     setter))))))

  (defmethod compute-slots ((class proxy-class))
    (with-slots (direct-slots size) class
      (let ((current-offset
	     (proxy-class-size (most-specific-proxy-superclass class)))
	    (max-size 0))
	(dolist (slotd direct-slots)
	  (when (eq (slot-definition-allocation slotd) :alien)
	    (with-slots (offset type) slotd
	      (unless (slot-boundp slotd 'offset)
		(setf offset current-offset))
	      (setq current-offset (+ offset (size-of type)))
	      (setq max-size (max max-size current-offset)))))
	(unless (slot-boundp class 'size)
	  (setf size max-size))))
    (call-next-method))
   
  (defmethod validate-superclass ((class proxy-class)
				  (super pcl::standard-class))
    (subtypep (class-name super) 'proxy))
  
  (defmethod proxy-class-size (class)
    (declare (ignore class))
    0)

  (defgeneric make-proxy-instance (class location weak-ref
				   &rest initargs &key)))

(defmethod make-proxy-instance ((class symbol) location weak-ref
				&rest initargs &key)
  (apply #'make-proxy-instance (find-class class) location weak-ref initargs))

(defmethod make-proxy-instance ((class proxy-class) location weak-ref
				&rest initargs &key)
  (let ((instance (allocate-instance class)))
    (apply
     #'initialize-proxy
     instance :location location :weak-ref weak-ref initargs)
    instance))

(defun ensure-proxy-instance (class location weak-ref &rest initargs)
  (or
   (find-cached-instance location)
   (apply #'make-proxy-instance class location weak-ref initargs)))



;;;; Superclasses for wrapping of C structures

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass struct (proxy)
    ()
    (:metaclass proxy-class)
    (:copy %copy-struct)
    (:free %free-struct)))


(defmethod initialize-instance ((structure struct)
				&rest initargs)
  (declare (ignore initargs))
  (setf 
   (slot-value structure 'location)
   (allocate-memory (proxy-class-size (class-of structure))))
  (call-next-method))


(defun %copy-struct (type location)
  (copy-memory location (proxy-class-size (find-class type))))

(defun %free-struct (type location)
  (declare (ignore type))
  (deallocate-memory location))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass static (struct)
    ()
    (:metaclass proxy-class)))

(defun %copy-static (type location)
  (declare (ignore type))
  location)

(defun %free-static (type location)
  (declare (ignore type location))
  nil)
