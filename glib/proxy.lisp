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

;; $Id: proxy.lisp,v 1.16 2004-12-19 23:33:57 espen Exp $

(in-package "GLIB")

;;;; Superclass for all metaclasses implementing some sort of virtual slots

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass virtual-slots-class (standard-class) 
    ())

  (defclass direct-virtual-slot-definition (standard-direct-slot-definition)
    ((setter :reader slot-definition-setter :initarg :setter)
     (getter :reader slot-definition-getter :initarg :getter)
     (unbound :reader slot-definition-unbound :initarg :unbound)
     (boundp :reader slot-definition-boundp :initarg :boundp)))
  
  (defclass effective-virtual-slot-definition (standard-effective-slot-definition)
    ((setter :reader slot-definition-setter :initarg :setter)
     (getter :reader slot-definition-getter :initarg :getter)
     (unbound :reader slot-definition-unbound :initarg :unbound)
     (boundp :reader slot-definition-boundp :initarg :boundp)))
  
  (defvar *unbound-marker* (gensym "UNBOUND-MARKER-"))

  (defun most-specific-slot-value (instances slot &optional 
				   (default *unbound-marker*))
    (let ((object (find-if
		   #'(lambda (ob)
		       (and (slot-exists-p ob slot) (slot-boundp ob slot)))
		   instances)))
      (if object
	  (slot-value object slot)
	  default))))

  

(defmethod direct-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'direct-virtual-slot-definition)
    (call-next-method)))

(defmethod effective-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'effective-virtual-slot-definition)
    (call-next-method)))


(defmethod initialize-internal-slot-functions ((slotd effective-virtual-slot-definition))
  (if (not (slot-boundp slotd 'getter))
      (setf
       (slot-value slotd 'reader-function)
       #'(lambda (object)
	   (declare (ignore object))
	   (error "Can't read slot: ~A" (slot-definition-name slotd)))
       (slot-value slotd 'boundp-function)
       #'(lambda (object) (declare (ignore object)) nil))

    (let ((getter-function
	   (let ((getter (slot-value slotd 'getter)))
	     (etypecase getter
	       (function getter)
	       (symbol 
		#'(lambda (object)
		    (funcall getter object)))
	       (string 
		(let ((reader nil))
		  (setf (slot-value slotd 'reader-function)
			#'(lambda (object)
			    (unless reader
			    (setq reader
			     (mkbinding getter 
			      (slot-definition-type slotd) 'pointer)))
			    (funcall reader (proxy-location object))))))))))

      (setf 
       (slot-value slotd 'boundp-function)
       (cond
	((and 
	  (not (slot-boundp slotd 'unbound))
	  (not (slot-boundp slotd 'boundp)))
	 #'(lambda (object) (declare (ignore object)) t))  
	((slot-boundp slotd 'unbound)
	 (let ((unbound-value (slot-value slotd 'unbound)))
	   (lambda (object)
	     (not (eq (funcall getter-function object) unbound-value)))))
	((let ((boundp (slot-value slotd 'boundp)))
	   (etypecase boundp
	     (function boundp)
	     (symbol #'(lambda (object)
			 (funcall boundp object)))
	     (string (let ((reader ()))
		       #'(lambda (object)
			   (unless reader
			     (setq reader
			      (mkbinding boundp
			       (slot-definition-type slotd) 'pointer)))
			   (funcall reader (proxy-location object))))))))))

      (setf
       (slot-value slotd 'reader-function)
       (cond
	((slot-boundp slotd 'unbound)
	 (let ((unbound (slot-value slotd 'unbound))
	       (slot-name (slot-definition-name slotd)))
	   (lambda (object)
	     (let ((value (funcall getter-function object)))
	       (if (eq value unbound)
		   (slot-unbound (class-of object) object slot-name)
		 value)))))
	((slot-boundp slotd 'boundp)
	 (let ((boundp-function (slot-value slotd 'boundp-function)))
	   (lambda (object)
	     (and
	      (funcall boundp-function object)
	      (funcall getter-function object)))))
	(getter-function)))))

  (setf 
   (slot-value slotd 'writer-function)
   (if (not (slot-boundp slotd 'setter))
       #'(lambda (object)
	   (declare (ignore object))
	   (error "Can't set slot: ~A" (slot-definition-name slotd)))
     (with-slots (setter) slotd
       (etypecase setter
	 (function setter)
	 ((or symbol cons) 
	  #'(lambda (value object)
	      (funcall (fdefinition setter) value object)))
	 (string
	  (let ((writer ()))
	    (setf
	     (slot-value slotd 'writer-function)
	     #'(lambda (value object)
		 (unless writer
		   (setq writer
		    (mkbinding setter 'nil 'pointer 
		     (slot-definition-type slotd))))
		 (funcall writer (proxy-location object) value)))))))))

  (initialize-internal-slot-gfs (slot-definition-name slotd)))



(defmethod compute-slot-accessor-info ((slotd effective-virtual-slot-definition) type gf)
  nil)

(defmethod compute-effective-slot-definition-initargs ((class virtual-slots-class) direct-slotds)
  (if (typep (first direct-slotds) 'direct-virtual-slot-definition)
      (let ((initargs ()))
	(let ((getter (most-specific-slot-value direct-slotds 'getter)))
	  (unless (eq getter *unbound-marker*)
	    (setf (getf initargs :getter) getter)))
	(let ((setter (most-specific-slot-value direct-slotds 'setter)))
	  (unless (eq setter *unbound-marker*)
	    (setf (getf initargs :setter) setter)))
	(let ((unbound (most-specific-slot-value direct-slotds 'unbound)))
	  (unless (eq unbound *unbound-marker*)
	    (setf (getf initargs :unbound) unbound)))
	(let ((boundp (most-specific-slot-value direct-slotds 'boundp)))
	  (unless (eq boundp *unbound-marker*)
	    (setf (getf initargs :boundp) boundp)))
	(nconc initargs (call-next-method)))
    (call-next-method)))


(defmethod slot-value-using-class
    ((class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (if (funcall (slot-value slotd 'boundp-function) object)
      (funcall (slot-value slotd 'reader-function) object)
    (slot-unbound class object (slot-definition-name slotd))))

(defmethod slot-boundp-using-class
    ((class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'boundp-function) object))
  
(defmethod (setf slot-value-using-class) 
    (value (class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'writer-function) value object))

  
(defmethod validate-superclass
    ((class virtual-slots-class) (super standard-class))
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

(defun instance-cached-p (location)
  (gethash (system:sap-int location) *instance-cache*))

(defun remove-cached-instance (location)
  (remhash (system:sap-int location) *instance-cache*))

;; For debuging
(defun cached-instances ()
  (let ((instances ()))
    (maphash #'(lambda (location ref)
		 (declare (ignore location))
		 (push (ext:weak-pointer-value ref) instances))
	     *instance-cache*)
    instances))
			


;;;; Proxy for alien instances

(defclass proxy ()
  ((location :reader proxy-location :type system-area-pointer)))

(defgeneric initialize-proxy (object &rest initargs))
(defgeneric instance-finalizer (object))
(defgeneric reference-foreign (class location))
(defgeneric unreference-foreign (class location))

(defmethod reference-foreign ((name symbol) location)
  (reference-foreign (find-class name) location))

(defmethod unreference-foreign ((name symbol) location)
  (unreference-foreign (find-class name) location))

(defmethod unreference-foreign :around ((class class) location)
  (unless (null-pointer-p location)
;;     (format t "Unreferencing ~A at ~A" (class-name class) location)
;;     (finish-output *standard-output*)
    (call-next-method)
;;     (write-line " done")
;;     (finish-output *standard-output*)
    ))

(defmethod print-object ((instance proxy) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (when (slot-boundp instance 'location)
      (format stream "at 0x~X" (sap-int (proxy-location instance))))))

(defmethod initialize-instance :around ((instance proxy) &key location)
  (if location
      (setf (slot-value instance 'location) location)      
    (call-next-method))
  (cache-instance instance)
  (ext:finalize instance (instance-finalizer instance))
  instance)

(defmethod instance-finalizer ((instance proxy))
  (let ((location (proxy-location instance))
	(class (class-of instance)))    
;;     (unless (find-method #'unreference-foreign nil (list (class-of class) t) nil)
;;       (error "No matching method for UNREFERENCE-INSTANCE when called with class ~A" class))
    #'(lambda ()
	(remove-cached-instance location)
	(unreference-foreign class location))))


;;;; Metaclass used for subclasses of proxy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy-class (virtual-slots-class)
    ((size :reader proxy-instance-size)))

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
  
  (defmethod shared-initialize ((class proxy-class) names &key size)
    (call-next-method)
    (cond
      (size (setf (slot-value class 'size) (first size)))
      ((slot-boundp class 'size) (slot-makunbound class 'size))))
  
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
      (let ((type (slot-definition-type slotd)))
	(unless (slot-boundp slotd 'getter)
	  (let ((reader (reader-function type)))
	    (setf 
	     (slot-value slotd 'getter)
	     #'(lambda (object)
		 (funcall reader (proxy-location object) offset)))))

	(unless (slot-boundp slotd 'setter)
	  (let ((writer (writer-function type))
		(destroy (destroy-function type)))
	    (setf 
	     (slot-value slotd 'setter)
	     #'(lambda (value object)
		 (let ((location (proxy-location object)))
		   (funcall destroy location offset) ; destroy old value
		   (funcall writer value location offset))))))))

    (call-next-method))
  

  ;; TODO: call some C code to detect this a compile time
  (defconstant +struct-alignmen+ 4)

  (defmethod compute-slots ((class proxy-class))
    (loop 
     with offset = (let ((size-of-super-classes
			  (proxy-instance-size 
			   (most-specific-proxy-superclass class))))
		     (+ size-of-super-classes 
			(mod size-of-super-classes +struct-alignmen+)))
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
  
  (defmethod proxy-instance-size (class)
    (declare (ignore class))
    0)
)
  
(defmethod alien-type ((class proxy-class) &rest args)
  (declare (ignore class args))
  (alien-type 'pointer))

(defmethod size-of ((class proxy-class) &rest args)
  (declare (ignore class args))
  (size-of 'pointer))

(defmethod from-alien-form (location (class proxy-class) &rest args)
  (declare (ignore args))
  `(ensure-proxy-instance ',(class-name class) ,location))

(defmethod from-alien-function ((class proxy-class) &rest args)
  (declare (ignore args))  
  #'(lambda (location)
      (ensure-proxy-instance class location)))

(defmethod to-alien-form (instance (class proxy-class) &rest args)
  (declare (ignore class args))
  `(proxy-location ,instance))

(defmethod to-alien-function ((class proxy-class) &rest args)
  (declare (ignore class args))
  #'proxy-location)

(defmethod copy-from-alien-form (location (class proxy-class) &rest args)
  (declare (ignore args))
  (let ((class-name (class-name class)))
    `(ensure-proxy-instance ',class-name
      (reference-foreign ',class-name ,location))))

(defmethod copy-from-alien-function ((class proxy-class) &rest args)
  (declare (ignore args))  
  #'(lambda (location)
      (ensure-proxy-instance class (reference-foreign class location))))

(defmethod copy-to-alien-form (instance (class proxy-class) &rest args)
  (declare (ignore args))
  `(reference-foreign ',(class-name class) (proxy-location ,instance)))

(defmethod copy-to-alien-function ((class proxy-class) &rest args)
  (declare (ignore class args))
  #'(lambda (instance)
      (reference-foreign class (proxy-location instance))))

(defmethod writer-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (instance location &optional (offset 0))
      (assert (null-pointer-p (sap-ref-sap location offset)))
      (setf 
       (sap-ref-sap location offset)
       (reference-foreign class (proxy-location instance)))))

(defmethod reader-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0))
      (let ((instance (sap-ref-sap location offset)))
	(unless (null-pointer-p instance)
	  (ensure-proxy-instance class (reference-foreign class instance))))))

(defmethod destroy-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0))
      (unreference-foreign class (sap-ref-sap location offset))))


(defgeneric ensure-proxy-instance (class location)
  (:documentation "Returns a proxy object representing the foreign object at the give location."))

(defmethod ensure-proxy-instance :around (class location)
  (unless (null-pointer-p location)
    (or 
     (find-cached-instance location)
     (call-next-method))))
  
(defmethod ensure-proxy-instance ((class symbol) location)
  (ensure-proxy-instance (find-class class) location))

(defmethod ensure-proxy-instance ((class proxy-class) location)
  (make-instance class :location location))


;;;; Superclasses for wrapping of C structures

(defclass struct (proxy)
  ()
  (:metaclass proxy-class))

(defmethod initialize-instance ((struct struct) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp struct 'location)
    (let ((size (proxy-instance-size (class-of struct))))
      (if (zerop size)
	  (error "~A has zero size" (class-of struct))
	  (setf (slot-value struct 'location) (allocate-memory size)))))
  (call-next-method))


;;;; Metaclasses used for subclasses of struct

(defclass struct-class (proxy-class)
  ())

(defmethod reference-foreign ((class struct-class) location)
  (copy-memory location (proxy-instance-size class)))

(defmethod unreference-foreign ((class struct-class) location)
  (deallocate-memory location))


(defclass static-struct-class (struct-class)
  ())

(defmethod reference-foreign ((class static-struct-class) location)
  (declare (ignore class))
  location)

(defmethod unreference-foreign ((class static-struct-class) location)
  (declare (ignore class location))
  nil)
