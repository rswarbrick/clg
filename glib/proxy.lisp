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

;; $Id: proxy.lisp,v 1.2 2001-04-30 11:25:25 espen Exp $

(in-package "GLIB")


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
      value)
     (t
      (funcall (fdefinition writer) value object)
      value))))
	

(defmethod validate-superclass
    ((class virtual-class) (super pcl::standard-class))
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
    ((location
      :reader proxy-location
      :type system-area-pointer)))

  (defgeneric initialize-proxy (object &rest initargs))
  (defgeneric instance-finalizer (object)))


(defmethod initialize-instance :after ((instance proxy)
				       &rest initargs &key)
  (declare (ignore initargs))
  (cache-instance instance)
  (ext:finalize instance (instance-finalizer instance)))


(defmethod initialize-proxy ((instance proxy)
			     &rest initargs)
  (declare (ignore initargs))
  (cache-instance instance))


(defmethod instance-finalizer ((instance proxy))
  (let ((location (proxy-location instance)))
    #'(lambda ()
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



;;;; Metaclass used for subclasses of proxy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy-class (virtual-class)
    ((size :reader proxy-class-instance-size)))

  (defclass direct-alien-slot-definition (direct-virtual-slot-definition)
    ((allocation
      :initform :alien)
     (offset
      :reader slot-definition-offset
      :initarg :offset
      :initform 0)))
  
  (defclass effective-alien-slot-definition (effective-virtual-slot-definition)
    ((offset :reader slot-definition-offset)))
  
  (defclass effective-virtual-alien-slot-definition
    (effective-virtual-slot-definition))
 

  (defmethod most-specific-proxy-superclass ((class proxy-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'proxy))
     (cdr (pcl::compute-class-precedence-list class))))


  (defmethod shared-initialize ((class proxy-class) names
				&rest initargs &key size name)
    (declare (ignore initargs))
    (call-next-method)
    (when size
      (setf (slot-value class 'size) (first size))))
    

  (defmethod shared-initialize :after ((class proxy-class) names
				       &rest initargs &key)
    (declare (ignore initargs names))
    (let* ((super (most-specific-proxy-superclass class))
	   (actual-size
	    (if (eq (class-name super) 'proxy)
		0
	      (proxy-class-instance-size super))))
      (dolist (slotd (class-slots class))
	(when (eq (slot-definition-allocation slotd) :alien)
	  (with-slots (offset type) slotd
	    (setq actual-size (max actual-size (+ offset (size-of type)))))))
      (cond
       ((not (slot-boundp class 'size))
	(setf (slot-value class 'size) actual-size))
       ((> actual-size (slot-value class 'size))
	(warn "The actual size of class ~A is lager than specified" class)))))


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
  
  
  (defmethod compute-virtual-slot-location
      ((class proxy-class) (slotd effective-alien-slot-definition)
       direct-slotds)
    (with-slots (offset type) slotd
      (setf offset (%direct-slot-definitions-slot-value direct-slotds 'offset))
      (let ((reader (intern-reader-function type))
	    (writer (intern-writer-function type))
	    (destroy (intern-destroy-function type)))
	(list
	 #'(lambda (object)
	     (funcall reader (proxy-location object) offset))
	 #'(lambda (value object)
	     (let ((location (proxy-location object)))
	       (funcall destroy location offset)
	       (funcall writer value location offset)))))))
	     
  
  (defmethod compute-virtual-slot-location
      ((class proxy-class)
       (slotd effective-virtual-alien-slot-definition)
       direct-slotds)
    (let ((location (call-next-method))
	  (class-name (class-name class)))
      (if (or (stringp location) (consp location))
	  (destructuring-bind (reader &optional writer) (mklist location)
	    (with-slots (type) slotd
              (list
	       (if (stringp reader)
		   (mkbinding reader type class-name)
		 reader)
	       (if (stringp writer)
		   (let ((writer (mkbinding writer 'nil class-name type)))
		     #'(lambda (value object)
			 (funcall writer object value)))
		 writer))))
	location)))


  (defmethod compute-slots ((class proxy-class))
    ;; Translating the user supplied relative (to previous slot) offsets
    ;; to absolute offsets.
    ;; This code is broken and have to be fixed.
    (with-slots (direct-slots) class
      (let* ((super (most-specific-proxy-superclass class))
	     (slot-offset
	      (if (eq (class-name super) 'proxy)
		  0
		(proxy-class-instance-size super))))
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


  (defmethod validate-superclass ((class proxy-class)
				  (super pcl::standard-class))
     (subtypep (class-name super) 'proxy))

  (defgeneric make-proxy-instance (class location weak-ref &rest initargs &key)))


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


;;;; Superclass for wrapping of C structures

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass alien-structure (proxy)
    ()
    (:metaclass proxy-class)
    (:size 0)))


(defmethod initialize-instance ((structure alien-structure)
				&rest initargs)
  (declare (ignore initargs))
  (setf 
   (slot-value structure 'location)
   (allocate-memory (proxy-class-instance-size (class-of structure))))
  (call-next-method))


(defmethod initialize-proxy ((structure alien-structure)
			     &rest initargs &key location weak-ref)
  (declare (ignore initargs))
  (setf
   (slot-value structure 'location)
   (if weak-ref
       (copy-memory location (proxy-class-instance-size (class-of structure)))
     location))
  (call-next-method))


(defmethod instance-finalizer ((structure alien-structure))
  (let ((location (proxy-location structure)))
    (declare (type system-area-pointer location))
    #'(lambda ()
	(deallocate-memory location)
	(remove-cached-instance location))))


(deftype-method translate-to-alien
     alien-structure (type-spec object &optional weak-ref)
  (if weak-ref
      `(proxy-location ,object)
    `(copy-memory
      (proxy-location ,object)
      ,(proxy-class-instance-size (find-class type-spec)))))


(deftype-method unreference-alien alien-structure (type-spec c-struct)
  (declare (ignore type-spec))
  `(deallocate-memory ,c-struct))
