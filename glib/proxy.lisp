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

;; $Id: proxy.lisp,v 1.9 2004-10-28 19:29:00 espen Exp $

(in-package "GLIB")

(import 
'(pcl::initialize-internal-slot-functions
  pcl::compute-effective-slot-definition-initargs
  pcl::compute-slot-accessor-info
  pcl::reader-function pcl::writer-function pcl::boundp-function))

;;;; Superclass for all metaclasses implementing some sort of virtual slots

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass virtual-slot-class (standard-class) 
    ())

  (defclass direct-virtual-slot-definition (standard-direct-slot-definition)
    ((setter :reader slot-definition-setter :initarg :setter)
     (getter :reader slot-definition-getter :initarg :getter)
     (boundp :reader slot-definition-boundp :initarg :boundp)))
  
  (defclass effective-virtual-slot-definition (standard-effective-slot-definition)
    ((setter :reader slot-definition-setter :initarg :setter)
     (getter :reader slot-definition-getter :initarg :getter)
     (boundp :reader slot-definition-boundp :initarg :boundp)))

  (defun most-specific-slot-value (instances slot &optional default)
    (let ((object (find-if
		   #'(lambda (ob)
		       (and (slot-exists-p ob slot) (slot-boundp ob slot)))
		   instances)))
      (if object
	  (slot-value object slot)
	  default)))
)

  

(defmethod direct-slot-definition-class ((class virtual-slot-class) &rest initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'direct-virtual-slot-definition)
    (call-next-method)))

(defmethod effective-slot-definition-class ((class virtual-slot-class) &rest initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'effective-virtual-slot-definition)
    (call-next-method)))


(defmethod initialize-internal-slot-functions ((slotd effective-virtual-slot-definition))
  (with-slots (getter setter boundp) slotd
    (unless (slot-boundp slotd 'reader-function)
      (setf 
       (slot-value slotd 'reader-function)
       (etypecase getter
	 (function getter)
	 (null #'(lambda (object)
		   (declare (ignore object))
		   (error "Can't read slot: ~A" (slot-definition-name slotd))))
	 (symbol #'(lambda (object)
		     (funcall getter object)))
	 (string (let ((reader (mkbinding-late getter 
				(slot-definition-type slotd) 'pointer)))
		   (setf (slot-value slotd 'reader-function)
			 #'(lambda (object)
			     (funcall reader (proxy-location object)))))))))

    (unless (slot-boundp slotd 'writer-function)
      (setf 
       (slot-value slotd 'writer-function)
       (etypecase setter
	 (function setter)
	 (null #'(lambda (object)
		   (declare (ignore object))
		   (error "Can't set slot: ~A" (slot-definition-name slotd))))
	 ((or symbol cons) #'(lambda (value object)
			       (funcall (fdefinition setter) value object)))
	 (string
	  (let ((writer (mkbinding-late setter 'nil 'pointer 
			 (slot-definition-type slotd))))
	    (setf (slot-value slotd 'writer-function)
		  #'(lambda (value object)
		      (funcall writer (proxy-location object) value))))))))

    (unless (slot-boundp slotd 'boundp-function)
      (setf 
       (slot-value slotd 'boundp-function)
       (etypecase boundp
	 (function boundp)
	 (null #'(lambda (object)
		   (declare (ignore object))
		   t))
	 (symbol #'(lambda (object)
		     (funcall boundp object)))))))
  (initialize-internal-slot-gfs (slot-definition-name slotd)))



(defmethod compute-slot-accessor-info ((slotd effective-virtual-slot-definition)
					    type gf)
  nil)

(defmethod compute-effective-slot-definition-initargs ((class virtual-slot-class) direct-slotds)
  (if (eq (most-specific-slot-value direct-slotds 'allocation) :virtual)
      (nconc 
       (list :getter (most-specific-slot-value direct-slotds 'getter)
	     :setter (most-specific-slot-value direct-slotds 'setter)
	     :boundp (most-specific-slot-value direct-slotds 'boundp))
       (call-next-method))
    (call-next-method)))


(defmethod slot-value-using-class
    ((class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (if (funcall (slot-value slotd 'boundp-function) object)
      (funcall (slot-value slotd 'reader-function) object)
    (slot-unbound class object (slot-definition-name slotd))))

(defmethod slot-boundp-using-class
    ((class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'boundp-function) object))
  
(defmethod (setf slot-value-using-class) 
    (value (class virtual-slot-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'writer-function) value object))

  
(defmethod validate-superclass
    ((class virtual-slot-class) (super standard-class))
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
  (let ((class (class-of instance))
	(type (type-of instance))
	(location (proxy-location instance)))
    (declare (type symbol type) (type system-area-pointer location))
    (let ((free (proxy-class-free class)))
      #'(lambda ()
	  (funcall free type location)
	  (remove-cached-instance location)))))


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
      (let ((copy (proxy-class-copy (find-class type-spec)))) 
	(if (symbolp copy)
	    `(,copy ',type-spec (proxy-location ,instance))    
	`(funcall ',copy ',type-spec (proxy-location ,instance))))))

(deftype-method unreference-alien proxy (type-spec location)
  (let ((free (proxy-class-free (find-class type-spec)))) 
    (if (symbolp free)
	`(,free ',type-spec ,location)
    `(funcall ',free ',type-spec ,location))))


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
    ((offset :reader slot-definition-offset :initarg :offset)))


  (defmethod most-specific-proxy-superclass ((class proxy-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'proxy))
     (cdr (compute-class-precedence-list class))))
  
  (defmethod direct-proxy-superclass ((class proxy-class))
    (find-if
     #'(lambda (class)
	 (subtypep (class-name class) 'proxy))
     (class-direct-superclasses class)))
  
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
  
  (defmethod shared-initialize :after ((class proxy-class) names &rest initargs)
    (let ((super (most-specific-proxy-superclass class)))
      (unless (or (not super) (eq super (find-class 'proxy)))
	(unless (or (slot-boundp class 'copy) (not (slot-boundp super 'copy)))
	  (setf (slot-value class 'copy) (proxy-class-copy super)))
	(unless (or (slot-boundp class 'free) (not (slot-boundp super 'free)))
	  (setf (slot-value class 'free) (proxy-class-free super))))))
  
  (defmethod direct-slot-definition-class ((class proxy-class) &rest initargs)
    (case (getf initargs :allocation)
      ((nil :alien) (find-class 'direct-alien-slot-definition))
      (t (call-next-method))))
  
  (defmethod effective-slot-definition-class ((class proxy-class) &rest initargs)
    (case (getf initargs :allocation)
      (:alien (find-class 'effective-alien-slot-definition))
      (t (call-next-method))))
  
  
  (defmethod compute-effective-slot-definition-initargs ((class proxy-class) direct-slotds)
    (if (eq (most-specific-slot-value direct-slotds 'allocation) :alien)
	(nconc 
	 (list :offset (most-specific-slot-value direct-slotds 'offset))
	 (call-next-method))
      (call-next-method)))
  

  (defmethod initialize-internal-slot-functions ((slotd effective-alien-slot-definition))
    (with-slots (offset) slotd
      (let* ((type (slot-definition-type slotd))
	     (reader (intern-reader-function type))
	     (writer (intern-writer-function type))
	     (destroy (intern-destroy-function type)))
	(unless (slot-boundp slotd 'reader-function)
	  (setf 
	   (slot-value slotd 'reader-function)
	   #'(lambda (object)
	       (funcall reader (proxy-location object) offset))))

	(unless (slot-boundp slotd 'writer-function)
	  (setf 
	   (slot-value slotd 'writer-function)
	   #'(lambda (value object)
	       (let ((location (proxy-location object)))
		 (funcall destroy location offset)
		 (funcall writer value location offset)))))

	(unless (slot-boundp slotd 'boundp-function)
	  (setf 
	   (slot-value slotd 'boundp-function)
	   #'(lambda (object)
	       (declare (ignore object))
	       t)))))
    (call-next-method))
  

  ;; TODO: call some C code to detect this a compile time
  (defconstant +struct-alignmen+ 4)

  (defmethod compute-slots ((class proxy-class))
    ;; This stuff should really go somewhere else
    (loop 
     with offset = (proxy-class-size (most-specific-proxy-superclass class))
     with size = offset
     for slotd in (class-direct-slots class)
     when (eq (slot-definition-allocation slotd) :alien)
     do (if (not (slot-boundp slotd 'offset))
	    (setf (slot-value slotd 'offset) offset)
	  (setq offset (slot-value slotd 'offset)))

        (incf offset (size-of (slot-definition-type slotd)))
	(incf offset (mod offset +struct-alignmen+))
	(setq size (max size offset))

     finally (unless (slot-boundp class 'size)
	       (setf (slot-value class 'size) size)))
    (call-next-method))

  
  (defmethod validate-superclass ((class proxy-class) (super standard-class))
    (subtypep (class-name super) 'proxy))
  
  (defmethod proxy-class-size (class)
    (declare (ignore class))
    0)
)
  
(defgeneric make-proxy-instance (class location weak-ref
				       &rest initargs &key));)

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

(defmethod initialize-instance ((structure struct) &rest initargs)
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


;(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass static (struct)
    ()
    (:metaclass proxy-class)
    (:copy %copy-static)
    (:free %free-static));)

(defun %copy-static (type location)
  (declare (ignore type))
  location)

(defun %free-static (type location)
  (declare (ignore type location))
  nil)
