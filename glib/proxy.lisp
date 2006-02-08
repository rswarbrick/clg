;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: proxy.lisp,v 1.31 2006-02-08 22:10:47 espen Exp $

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

  (defclass direct-special-slot-definition (standard-direct-slot-definition)
    ())
  
  (defclass effective-special-slot-definition (standard-effective-slot-definition)
    ()))

(defvar *unbound-marker* (gensym "UNBOUND-MARKER-"))

(defun most-specific-slot-value (instances slot &optional (default *unbound-marker*))
  (let ((object (find-if
		 #'(lambda (ob)
		     (and (slot-exists-p ob slot) (slot-boundp ob slot)))
		 instances)))
    (if object
	(slot-value object slot)
      default)))

(defmethod initialize-instance ((slotd effective-special-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value slotd 'allocation) :instance))


(defmethod direct-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (case (getf initargs :allocation)
    (:virtual (find-class 'direct-virtual-slot-definition))
    (:special (find-class 'direct-special-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (case (getf initargs :allocation)
    (:virtual (find-class 'effective-virtual-slot-definition))
    (:special (find-class 'effective-special-slot-definition))
    (t (call-next-method))))


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
			    (funcall reader (foreign-location object))))))))))

      (setf 
       (slot-value slotd 'boundp-function)
       (cond
	((slot-boundp slotd 'unbound)
	 (let ((unbound-value (slot-value slotd 'unbound)))
	   #'(lambda (object)
	       (not (eq (funcall getter-function object) unbound-value)))))
	((slot-boundp slotd 'boundp)
	 (let ((boundp (slot-value slotd 'boundp)))
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
			   (funcall reader (foreign-location object))))))))
	((multiple-value-bind (unbound-p unbound-value)
	     (unbound-value (slot-definition-type slotd))
	   (when unbound-p
	     #'(lambda (object)
		 (not (eq (funcall getter-function object) unbound-value))))))
	(#'(lambda (object) (declare (ignore object)) t))))

      (setf
       (slot-value slotd 'reader-function)
       (cond
	((slot-boundp slotd 'unbound)
	 (let ((unbound (slot-value slotd 'unbound))
	       (slot-name (slot-definition-name slotd)))
	   #'(lambda (object)
	       (let ((value (funcall getter-function object)))
		 (if (eq value unbound)
		     (slot-unbound (class-of object) object slot-name)
		   value)))))
	((slot-boundp slotd 'boundp)
	 (let ((boundp-function (slot-value slotd 'boundp-function)))
	   #'(lambda (object)
	       (and
		(funcall boundp-function object)
		(funcall getter-function object)))))
	((multiple-value-bind (unbound-p unbound-value)
	     (unbound-value (slot-definition-type slotd))
	   (let ((slot-name (slot-definition-name slotd)))
	     (when unbound-p
	       #'(lambda (object)
		   (let ((value (funcall getter-function object)))
		     (if (eq value unbound-value)
			 (slot-unbound (class-of object) object slot-name)
			 value)))))))
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
		 (funcall writer (foreign-location object) value)))))))))

  #-sbcl>=0.9.8(initialize-internal-slot-gfs (slot-definition-name slotd)))



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

(defvar *instance-cache* (make-hash-table :test #'eql))

(defun cache-instance (instance &optional (weak-ref t))
  (setf
   (gethash (sap-int (foreign-location instance)) *instance-cache*)
   (if weak-ref
       (make-weak-pointer instance)
     instance)))

(defun find-cached-instance (location)
  (let ((ref (gethash (sap-int location) *instance-cache*)))
    (when ref
      (if (weak-pointer-p ref)
	  (weak-pointer-value ref)
	ref))))

(defun instance-cached-p (location)
  (gethash (sap-int location) *instance-cache*))

(defun remove-cached-instance (location)
  (remhash (sap-int location) *instance-cache*))

;; For debuging
(defun list-cached-instances ()
  (let ((instances ()))
    (maphash #'(lambda (location ref)
		 (declare (ignore location))
		 (push ref instances))
	     *instance-cache*)
    instances))
			
;; Instances that gets invalidated tend to be short lived, but created
;; in large numbers. So we're keeping them in a hash table to be able
;; to reuse them (and thus reduce consing)
(defvar *invalidated-instance-cache* (make-hash-table :test #'eql))

(defun cache-invalidated-instance (instance)
  (push instance
   (gethash (class-of instance) *invalidated-instance-cache*)))

(defun find-invalidated-instance (class)
  (when (gethash class *invalidated-instance-cache*)
    (pop (gethash class *invalidated-instance-cache*))))

(defun list-invalidated-instances ()
  (let ((instances ()))
    (maphash #'(lambda (location ref)
		 (declare (ignore location))
		 (push ref instances))
	     *invalidated-instance-cache*)
    instances))



;;;; Proxy for alien instances

;; TODO: add a ref-counted-proxy subclass
(defclass proxy ()
  ((location :allocation :special :type pointer))
  (:metaclass virtual-slots-class))

(defgeneric instance-finalizer (object))
(defgeneric reference-foreign (class location))
(defgeneric unreference-foreign (class location))
(defgeneric invalidate-instance (object))

(defun foreign-location (instance)
  (slot-value instance 'location))

(defun (setf foreign-location) (location instance)
  (setf (slot-value instance 'location) location))

(defun proxy-valid-p (instance)
  (slot-boundp instance 'location))

(defmethod reference-foreign ((name symbol) location)
  (reference-foreign (find-class name) location))

(defmethod unreference-foreign ((name symbol) location)
  (unreference-foreign (find-class name) location))

(defmethod unreference-foreign :around ((class class) location)
  (unless (null-pointer-p location)
    (call-next-method)))

(defmethod print-object ((instance proxy) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (if (slot-boundp instance 'location)
	(format stream "at 0x~X" (sap-int (foreign-location instance)))
      (write-string "at \"unbound\"" stream))))

(defmethod initialize-instance :around ((instance proxy) &rest initargs)
  (declare (ignore initargs))
  (prog1
      (call-next-method)
    (cache-instance instance)
    (finalize instance (instance-finalizer instance))))

(defmethod instance-finalizer ((instance proxy))
  (let ((location (foreign-location instance))
	(class (class-of instance)))    
;;     (unless (find-method #'unreference-foreign nil (list (class-of class) t) nil)
;;       (error "No matching method for UNREFERENCE-INSTANCE when called with class ~A" class))
    #'(lambda ()
	(remove-cached-instance location)
	(unreference-foreign class location))))

;; Any reference to the foreign object the instance may have held
;; should be released before this method is invoked
(defmethod invalidate-instance ((instance proxy))
  (remove-cached-instance (foreign-location instance))
  (slot-makunbound instance 'location)
  (cancel-finalization instance)
  (cache-invalidated-instance instance))


;;;; Metaclass used for subclasses of proxy

(defgeneric most-specific-proxy-superclass (class))
(defgeneric direct-proxy-superclass (class))
  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy-class (virtual-slots-class)
    ((size :reader foreign-size)))

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
      (:alien (find-class 'direct-alien-slot-definition))
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
		 (funcall reader (foreign-location object) offset)))))

	(unless (slot-boundp slotd 'setter)
	  (let ((writer (writer-function type))
		(destroy (destroy-function type)))
	    (setf 
	     (slot-value slotd 'setter)
	     #'(lambda (value object)
		 (let ((location (foreign-location object)))
		   (funcall destroy location offset) ; destroy old value
		   (funcall writer value location offset))))))))

    (call-next-method))
  
  ;; TODO: call some C code to detect this a compile time
  (defconstant +struct-alignmen+ 4)

  (defun align-offset (size)
    (if (zerop (mod size +struct-alignmen+))
	size
      (+ size (- +struct-alignmen+ (mod size +struct-alignmen+)))))

  (defmethod compute-slots ((class proxy-class))
    (let ((alien-slots 
	   (remove-if-not
	    #'(lambda (slotd)
		(eq (slot-definition-allocation slotd) :alien))
	    (class-direct-slots class))))      
      (when alien-slots
	(loop 
	 as offset = (align-offset (foreign-size 
				    (most-specific-proxy-superclass class)))
	             then (align-offset 
			   (+ 
			    (slot-definition-offset slotd)
			    (size-of (slot-definition-type slotd))))
       for slotd in alien-slots
       unless (slot-boundp slotd 'offset)
       do (setf (slot-value slotd 'offset) offset))))
    (call-next-method))

  (defmethod validate-superclass ((class proxy-class) (super standard-class))
    (subtypep (class-name super) 'proxy))
  
  (defmethod foreign-size ((class-name symbol))
    (foreign-size (find-class class-name))))

(defmethod foreign-size ((object proxy))
  (foreign-size (class-of object)))
  

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
  `(foreign-location ,instance))

(defmethod to-alien-function ((class proxy-class) &rest args)
  (declare (ignore class args))
  #'foreign-location)

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
  `(reference-foreign ',(class-name class) (foreign-location ,instance)))

(defmethod copy-to-alien-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (instance)
      (reference-foreign class (foreign-location instance))))

(defmethod writer-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (instance location &optional (offset 0))
      (assert (null-pointer-p (sap-ref-sap location offset)))
      (setf 
       (sap-ref-sap location offset)
       (reference-foreign class (foreign-location instance)))))

(defmethod reader-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (let ((instance (sap-ref-sap location offset)))
	(unless (null-pointer-p instance)
	  (ensure-proxy-instance class (reference-foreign class instance))))))

(defmethod destroy-function ((class proxy-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0))
      (unreference-foreign class (sap-ref-sap location offset))))

(defmethod unbound-value ((class proxy-class) &rest args)
  (declare (ignore args))
  (values t nil))

(defun ensure-proxy-instance (class location &rest initargs)
  "Returns a proxy object representing the foreign object at the give
location. If an existing object is not found in the cache
MAKE-PROXY-INSTANCE is called to create one."
  (unless (null-pointer-p location)
    (or 
     #-debug-ref-counting(find-cached-instance location)
     #+debug-ref-counting
     (let ((instance (find-cached-instance location)))
       (when instance
	 (format t "Object found in cache: ~A~%" instance)
	 instance))
     (let ((instance (apply #'make-proxy-instance class location initargs)))
       (cache-instance instance)
       instance))))

(defgeneric make-proxy-instance (class location &key weak)
  (:documentation "Creates a new proxy object representing the foreign
object at the give location. If WEAK is non NIL the foreign memory
will not be released when the proxy is garbage collected."))

(defmethod make-proxy-instance ((class symbol) location &rest initargs)
  (apply #'make-proxy-instance (find-class class) location initargs))

(defmethod make-proxy-instance ((class proxy-class) location &key weak)
  (let ((instance
	 (or
	  (find-invalidated-instance class)
	  (allocate-instance class))))
    (setf (foreign-location instance) location)
    (unless weak
      (finalize instance (instance-finalizer instance)))
    instance))


;;;; Superclasses for wrapping of C structures

(defclass struct (proxy)
  ()
  (:metaclass proxy-class)
  (:size 0))

(defmethod initialize-instance ((struct struct) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp struct 'location)
    (let ((size (foreign-size (class-of struct))))
      (if (zerop size)
	  (error "~A has zero size" (class-of struct))
	(setf (slot-value struct 'location) (allocate-memory size)))))
  (call-next-method))


;;;; Metaclasses used for subclasses of struct

(defclass struct-class (proxy-class)
  ())

(defmethod direct-slot-definition-class ((class struct-class) &rest initargs)
  (if (not (getf initargs :allocation))
      (find-class 'direct-alien-slot-definition)
    (call-next-method)))

(defmethod reference-foreign ((class struct-class) location)
  (copy-memory location (foreign-size class)))

(defmethod unreference-foreign ((class struct-class) location)
  (deallocate-memory location))

(defmethod compute-slots :around ((class struct-class))
    (let ((slots (call-next-method)))
      (when (and 
	     #-sbcl>=0.9.8(class-finalized-p class) #+sbc098 t
	     (not (slot-boundp class 'size)))
        (let ((size (loop
		     for slotd in slots
		     when (eq (slot-definition-allocation slotd) :alien)
		     maximize (+ 
			       (slot-definition-offset slotd)
			       (size-of (slot-definition-type slotd))))))
	  (setf (slot-value class 'size) (+ size (mod size +struct-alignmen+)))))
      slots))

(defmethod reader-function ((class struct-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0) weak-p)
      (let ((instance (sap-ref-sap location offset)))
	(unless (null-pointer-p instance)
	  (if weak-p
	      (ensure-proxy-instance class instance :weak t)
	    (ensure-proxy-instance class (reference-foreign class instance)))))))


(defclass static-struct-class (struct-class)
  ())

(defmethod reference-foreign ((class static-struct-class) location)
  (declare (ignore class))
  location)

(defmethod unreference-foreign ((class static-struct-class) location)
  (declare (ignore class location))
  nil)

(defmethod reader-function ((class struct-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (let ((instance (sap-ref-sap location offset)))
	(unless (null-pointer-p instance)
	  (ensure-proxy-instance class instance :weak t)))))


;;; Pseudo type for structs which are inlined in other objects

(defmethod size-of ((type (eql 'inlined)) &rest args)
  (declare (ignore type))
  (foreign-size (first args)))

(defmethod reader-function ((type (eql 'inlined)) &rest args)
  (declare (ignore type))
  (destructuring-bind (class) args
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(ensure-proxy-instance class 
	 (reference-foreign class (sap+ location offset))))))

(defmethod writer-function ((type (eql 'inlined)) &rest args)
  (declare (ignore type))
  (destructuring-bind (class) args
    #'(lambda (instance location &optional (offset 0))
	(copy-memory (foreign-location instance) (foreign-size class) (sap+ location offset)))))

(defmethod destroy-function ((type (eql 'inlined)) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0))
      (declare (ignore location offset))))

(export 'inlined)
