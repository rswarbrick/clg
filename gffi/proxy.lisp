;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: proxy.lisp,v 1.5 2006-09-29 13:14:19 espen Exp $

(in-package "GFFI")


;;;; Proxy cache

(defvar *instance-cache* (make-hash-table :test #'eql))

(defun cache-instance (instance &optional (weak-ref t))
  (setf
   (gethash (pointer-address (foreign-location instance)) *instance-cache*)
   (if weak-ref
       (make-weak-pointer instance)
     instance)))

(defun find-cached-instance (location)
  (let ((ref (gethash (pointer-address location) *instance-cache*)))
    (when ref
      (if (weak-pointer-p ref)
	  (weak-pointer-value ref)
	ref))))

(defun instance-cached-p (location)
  (gethash (pointer-address location) *instance-cache*))

(defun remove-cached-instance (location)
  (remhash (pointer-address location) *instance-cache*))

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

#?(or (sbcl>= 0 9 17) (featurep :clisp))
(defvar *foreign-instance-locations* 
  (make-hash-table #+clisp :weak #+sbcl :weakness :key))

;; TODO: add a ref-counted-proxy subclass
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy (virtual-slots-object)
    (#?-(or (sbcl>= 0 9 17) (featurep :clisp))(%location :special t :type pointer))
    (:metaclass virtual-slots-class)))

(defgeneric instance-finalizer (instance))
(defgeneric reference-function (class))
(defgeneric unreference-function (class))
(defgeneric invalidate-instance (instance &optional finalize-p))
(defgeneric allocate-foreign (object &key &allow-other-keys))

#?-(or (sbcl>= 0 9 17) (featurep :clisp))
(progn
  (defun foreign-location (instance)
    (slot-value instance '%location))

  (defun (setf foreign-location) (location instance)
    (setf (slot-value instance '%location) location))
  
  (defun proxy-valid-p (instance)
    (slot-boundp instance '%location)))
  
#?(or (sbcl>= 0 9 17) (featurep :clisp))
(progn
  (defun foreign-location (instance)
    (gethash instance *foreign-instance-locations*))

  (defun (setf foreign-location) (location instance)
    (setf (gethash instance *foreign-instance-locations*) location))

  (defun proxy-valid-p (instance)
    (and (gethash instance *foreign-instance-locations*) t)))


(defmethod reference-function ((name symbol))
  (reference-function (find-class name)))

(defmethod unreference-function ((name symbol))
  (unreference-function (find-class name)))

(defmethod print-object ((instance proxy) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (if (proxy-valid-p instance)
	(format stream "at 0x~X" (pointer-address (foreign-location instance)))
      (write-string "at \"unbound\"" stream))))


(defmethod initialize-instance :around ((instance proxy) &rest initargs &key &allow-other-keys) 
  (setf  
   (foreign-location instance)
   (apply #'allocate-foreign instance initargs))
  (prog1
      (call-next-method)
    (cache-instance instance)
    (finalize instance (instance-finalizer instance))))

(defmethod instance-finalizer :around ((instance proxy))
  (let ((finalizer (call-next-method)))
    (let ((location (foreign-location instance)))
      #+(or cmu sbcl)
      #'(lambda ()
	  (remove-cached-instance location)
	  (funcall finalizer))
      #+clisp
      #'(lambda (instance)
	  (declare (ignore instance))
	  (remove-cached-instance location)
	  (funcall finalizer)))))

(defmethod instance-finalizer ((instance proxy))
  (let ((location (foreign-location instance))
	(unref (unreference-function (class-of instance))))
    #'(lambda ()
	(funcall unref location))))

;; FINALIZE-P should always be the same as the keyword argument
;; :FINALZIE given to MAKE-PROXY-INSTANCE or non NIL if the proxy was
;; created with MAKE-INSTANCE
(defmethod invalidate-instance ((instance proxy) &optional finalize-p)
  (remove-cached-instance (foreign-location instance))
  #+(or sbcl cmu)
  (progn
    (when finalize-p
      (funcall (instance-finalizer instance)))
    #?-(sbcl>= 0 9 17)(slot-makunbound instance '%location)
    #?(sbcl>= 0 9 17)(remhash instance *foreign-instance-locations*)
    (cancel-finalization instance))
  ;; We can't cache invalidated instances in CLISP beacuse it is
  ;; not possible to cancel finalization
  #-clisp(cache-invalidated-instance instance))


;;;; Metaclass used for subclasses of proxy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass proxy-class (virtual-slots-class)
    ((size :accessor foreign-size)
     (packed :reader foreign-slots-packed-p)
     (ref :reader reference-function)
     (unref :reader unreference-function)))

  (defclass direct-alien-slot-definition (direct-virtual-slot-definition)
    ((offset :reader slot-definition-offset :initarg :offset))
    (:default-initargs :allocation :alien))
  
  (defclass effective-alien-slot-definition (effective-virtual-slot-definition)
    ((offset :reader slot-definition-offset :initarg :offset)))

  (defclass direct-virtual-alien-slot-definition (direct-virtual-slot-definition)
    ())
  
  (defclass effective-virtual-alien-slot-definition (effective-virtual-slot-definition)
    ())

  (defgeneric foreign-size-p (class))
  (defgeneric most-specific-proxy-superclass (class))
  (defgeneric direct-proxy-superclass (class))
  
  (defmethod foreign-size-p ((class proxy-class))
    (slot-boundp class 'size))

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
				&key size packed ref unref)
    (declare (ignore names))
    (cond
     (size (setf (slot-value class 'size) (first size)))
     ((slot-boundp class 'size) (slot-makunbound class 'size)))
    (setf (slot-value class 'packed) (first packed))
    (when ref
      (setf (slot-value class 'ref) (first ref)))
    (when unref
      (setf (slot-value class 'unref) (first unref)))
    (call-next-method))

  (defmethod direct-slot-definition-class ((class proxy-class) &rest initargs)
    (case (getf initargs :allocation)
      (:alien (find-class 'direct-alien-slot-definition))
      (:virtual (find-class 'direct-virtual-alien-slot-definition))
      (t (call-next-method))))
  
  (defmethod effective-slot-definition-class ((class proxy-class) &rest initargs)
    (case (getf initargs :allocation)
      (:alien (find-class 'effective-alien-slot-definition))
      (:virtual (find-class 'effective-virtual-alien-slot-definition))
      (t (call-next-method))))

  
  (defmethod compute-effective-slot-definition-initargs ((class proxy-class) direct-slotds)
    (if (eq (slot-definition-allocation (first direct-slotds)) :alien)
	(nconc 
	 (list :offset (most-specific-slot-value direct-slotds 'offset))
	 (call-next-method))
      (call-next-method)))
  
  (defmethod slot-readable-p ((slotd effective-alien-slot-definition))
    (declare (ignore slotd))
    t)

  (defmethod compute-slot-reader-function ((slotd effective-alien-slot-definition) &optional signal-unbound-p)
    (declare (ignore signal-unbound-p))
    (let* ((type (slot-definition-type slotd))
	   (offset (slot-definition-offset slotd))
	   (reader (reader-function type)))
      #'(lambda (object)
	  (funcall reader (foreign-location object) offset))))

  (defmethod slot-writable-p ((slotd effective-alien-slot-definition))
    (declare (ignore slotd))
    t)

  (defmethod compute-slot-writer-function ((slotd effective-alien-slot-definition))
    (let* ((type (slot-definition-type slotd))
	   (offset (slot-definition-offset slotd))
	   (writer (writer-function type))
	   (destroy (destroy-function type)))
      #'(lambda (value object)
	  (let ((location (foreign-location object)))
	    (funcall destroy location offset) ; destroy old value
	    (funcall writer value location offset))
	  value)))
  
  (defmethod compute-slot-reader-function ((slotd effective-virtual-alien-slot-definition) &optional signal-unbound-p)
    (declare (ignore signal-unbound-p))
    (if (and (slot-boundp slotd 'getter) (stringp (slot-definition-getter slotd)))
	(let ((getter (slot-definition-getter slotd))
	      (type (slot-definition-type slotd))
	      (reader nil))
	  #'(lambda (object)
	      (unless reader
		(setq reader (mkbinding getter type 'pointer)))
	      (funcall reader (foreign-location object))))
      (call-next-method)))

  (defmethod compute-slot-writer-function ((slotd effective-virtual-alien-slot-definition))
    (if (and (slot-boundp slotd 'setter) (stringp (slot-definition-setter slotd)))
	(let ((setter (slot-definition-setter slotd))
	      (type (slot-definition-type slotd))
	      (writer nil))
	  #'(lambda (value object)
 	      (unless writer
 		(setq writer (mkbinding setter nil 'pointer type)))
	      (funcall writer (foreign-location object) value)))
      (call-next-method)))
  
  (defun adjust-offset (offset type &optional packed-p)
    (let ((alignment (type-alignment type)))
      (if (or packed-p (zerop (mod offset alignment)))
	  offset
	(+ offset (- alignment (mod offset alignment))))))

  (defmethod compute-slots ((class proxy-class))
    (let ((alien-slots (remove-if-not 
			#'(lambda (allocation) (eq allocation :alien))
			(class-direct-slots class)
			:key #'slot-definition-allocation)))
      (when alien-slots
	(loop 
	 with packed-p = (foreign-slots-packed-p class)
         for slotd in alien-slots
	 as offset = (adjust-offset 
		      (foreign-size (most-specific-proxy-superclass class))
		      (slot-definition-type slotd)
		      packed-p)
	             then (adjust-offset offset (slot-definition-type slotd) packed-p)
	 do (if (slot-boundp slotd 'offset)
		(setf offset (slot-value slotd 'offset))
	      (setf (slot-value slotd 'offset) offset))
	    (incf offset (size-of (slot-definition-type slotd))))))
    (call-next-method))

  (defmethod validate-superclass ((class proxy-class) (super standard-class))
    (subtypep (class-name super) 'proxy))
  
  (defmethod foreign-size ((class-name symbol))
    (foreign-size (find-class class-name))))

(defmethod foreign-size ((object proxy))
  (foreign-size (class-of object)))

(define-type-method alien-type ((type proxy))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type proxy) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type proxy) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method from-alien-form ((type proxy) form &key (ref :free))
  (let ((class (type-expand type)))
    (ecase ref
      (:free `(ensure-proxy-instance ',class ,form :reference nil))
      (:copy `(ensure-proxy-instance ',class ,form))
      ((:static :temp) `(ensure-proxy-instance ',class ,form 
			 :reference nil :finalize nil)))))

(define-type-method from-alien-function ((type proxy) &key (ref :free))
  (let ((class (type-expand type)))
    (ecase ref
      (:free 
       #'(lambda (location)
	   (ensure-proxy-instance class location :reference nil)))
      (:copy 
       #'(lambda (location)
	   (ensure-proxy-instance class location)))
      ((:static :temp)
       #'(lambda (location)
	   (ensure-proxy-instance class location :reference nil :finalize nil))))))

(define-type-method to-alien-form ((type proxy) instance &optional copy-p)
  (if copy-p
      (let* ((class (type-expand type))
	     (ref (reference-function class)))
	(if (symbolp ref)
	    `(,ref (foreign-location ,instance))
	  `(funcall (reference-function ',class) 
	    (foreign-location ,instance))))
    `(foreign-location ,instance)))

(define-type-method to-alien-function ((type proxy) &optional copy-p)
  (if copy-p
      (let ((ref (reference-function (type-expand type))))
	#'(lambda (instance)
	    (funcall ref (foreign-location instance))))
    #'foreign-location))

(define-type-method writer-function ((type proxy) &key temp inlined)
  (assert-not-inlined type inlined)
  (if temp
      #'(lambda (instance location &optional (offset 0))
	  (assert (null-pointer-p (ref-pointer location offset)))
	  (setf (ref-pointer location offset) (foreign-location instance)))
    (let ((ref (reference-function (type-expand type))))
      #'(lambda (instance location &optional (offset 0))
	  (assert (null-pointer-p (ref-pointer location offset)))
	  (setf 
	   (ref-pointer location offset)
	   (funcall ref (foreign-location instance)))))))

(define-type-method reader-function ((type proxy) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (let ((class (type-expand type)))
    (ecase ref
      (:read
       #'(lambda (location &optional (offset 0))
	   (let ((instance (ref-pointer location offset)))
	     (unless (null-pointer-p instance)
	       (ensure-proxy-instance class instance)))))
      (:peek
       #'(lambda (location &optional (offset 0))
	   (let ((instance (ref-pointer location offset)))
	     (unless (null-pointer-p instance)
	       (ensure-proxy-instance class instance 
		:reference nil :finalize nil)))))
      (:get
       #'(lambda (location &optional (offset 0))
	   (let ((instance (ref-pointer location offset)))
	     (unless (null-pointer-p instance)
	       (prog1
		   (ensure-proxy-instance class instance :reference nil)
		 (setf (ref-pointer location offset) (make-pointer 0))))))))))

(define-type-method destroy-function ((type proxy) &key temp inlined)
  (assert-not-inlined type inlined)
  (if temp
      #'(lambda (location &optional (offset 0))
	  (setf (ref-pointer location offset) (make-pointer 0)))
    (let ((unref (unreference-function (type-expand type))))
      #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer location offset))
	    (funcall unref (ref-pointer location offset))
	    (setf (ref-pointer location offset) (make-pointer 0)))))))

(define-type-method copy-function ((type proxy) &key inlined)
  (assert-not-inlined type inlined)
  (let ((ref (reference-function (type-expand type))))
    #'(lambda (from to &optional (offset 0))
	(let ((instance (ref-pointer from offset)))
	  (unless (null-pointer-p instance)
	    (funcall ref instance))
	  (setf (ref-pointer to offset) instance)))))

(define-type-method unbound-value ((type proxy))
  (declare (ignore type))
  nil)

(defun ensure-proxy-instance (class location &rest initargs)
  "Returns a proxy object representing the foreign object at the give
location. If an existing proxy object is not found,
MAKE-PROXY-INSTANCE is called to create a new one. A second return
value indicates whether a new proxy was created or not."
  (unless (null-pointer-p location)
    (or 
     #-debug-ref-counting(find-cached-instance location)
     #+debug-ref-counting
     (let ((instance (find-cached-instance location)))
       (when instance
	 (format t "Object found in cache: ~A~%" instance)
	 instance))
     (values
      (apply #'make-proxy-instance class location initargs)
      t))))

(defgeneric make-proxy-instance (class location &key reference finalize)
  (:documentation "Creates a new proxy object representing the foreign
object at the give location."))

(defmethod make-proxy-instance ((class symbol) location &rest initargs)
  (apply #'make-proxy-instance (find-class class) location initargs))

(defmethod make-proxy-instance ((class proxy-class) location 
				&key (reference t) (finalize t))
  (let ((instance
	 (or
	  (find-invalidated-instance class)
	  (allocate-instance class))))    
    (setf (foreign-location instance) 
     (if reference
	 (funcall (reference-function class) location)
       location))
    (finalize instance 
     (if finalize
	 (instance-finalizer instance)
       ;; We still need to remove the instance from the cache even if we 
       ;; don't do normal finalization
       (let ((location (foreign-location instance)))
	 #+(or cmu sbcl)
	 #'(lambda ()
	     (remove-cached-instance location))
	 #+clisp
       	 #'(lambda (instance)
	     (declare (ignore instance))
	     (remove-cached-instance location)))))
    (cache-instance instance)
    instance))


;;;; Superclasses for wrapping of C structures

(defclass struct (proxy)
  ()
  (:metaclass proxy-class)
  (:size 0))

(defmethod allocate-foreign ((struct struct) &rest initargs)
  (declare (ignore initargs))
  (let ((size (foreign-size (class-of struct))))
    (if (zerop size)
	(error "~A has zero size" (class-of struct))
      (allocate-memory size))))


;;;; Metaclasses used for subclasses of struct

(defclass struct-class (proxy-class)
  ())

(defmethod shared-initialize ((class struct-class) names &rest initargs)
  (declare (ignore names initargs))
  (call-next-method)
  (let ((offsets nil) (copy-functions nil) (destroy-functions nil))
    (flet ((initialize-functions ()
	     (loop
	      for slotd in (class-slots class)
	      as type = (slot-definition-type slotd)
	      when (eq (slot-definition-allocation slotd) :alien)
	      do (push (slot-definition-offset slotd) offsets)
	         (push (copy-function type) copy-functions)
		 (push (destroy-function type) destroy-functions))))
      (unless (slot-boundp class 'ref)
	(setf 
	 (slot-value class 'ref)
	 #'(lambda (from &optional (to (allocate-memory (foreign-size class))))
	     (assert (not (null-pointer-p from)))
	     (unless offsets 
	       (initialize-functions))
	     (loop
	      for offset in offsets
	      for copy in copy-functions
	      do (funcall copy from to offset))
	     to)))
      (unless (slot-boundp class 'unref)
	(setf (slot-value class 'unref) 
	 #'(lambda (location &optional inlined-p)
	     (assert (not (null-pointer-p location)))
	     (unless offsets 
	       (initialize-functions))
	     (loop
	      for offset in offsets
	      for destroy in destroy-functions
	      do (funcall destroy location offset))
	     (unless inlined-p
	       (deallocate-memory location))))))))


(defmethod direct-slot-definition-class ((class struct-class) &rest initargs)
  (if (not (getf initargs :allocation))
      (find-class 'direct-alien-slot-definition)
    (call-next-method)))


(defmethod compute-slots :around ((class struct-class))  
  (let ((slots (call-next-method)))
    (when (and
	   #?-(or (sbcl>= 0 9 8) (featurep :clisp))(class-finalized-p class)
	   (not (slot-boundp class 'size)))
      (setf (slot-value class 'size)
       (or
	(loop
	 for slotd in slots
	 when (eq (slot-definition-allocation slotd) :alien)
	 maximize (+ 
		   (slot-definition-offset slotd)
		   (size-of (slot-definition-type slotd))))
	0)))
    slots))

(define-type-method callback-wrapper ((type struct) var arg form)
  (let ((class (type-expand type)))
    `(let ((,var (ensure-proxy-instance ',class ,arg :finalize nil)))
       (unwind-protect
	   ,form
	 (invalidate-instance ,var)))))

(define-type-method size-of ((type struct) &key inlined)
  (if inlined
      (foreign-size type)
    (size-of 'pointer)))

(define-type-method type-alignment ((type struct) &key inlined)
  (if inlined
      (let ((slot1 (find-if
		    #'(lambda (slotd)
			(eq (slot-definition-allocation slotd) :alien))
		    (class-slots (find-class type)))))
	(type-alignment (slot-definition-type slot1)))
    (type-alignment 'pointer)))

(define-type-method writer-function ((type struct) &key temp inlined)
  (if inlined
      (if temp
	  (let ((size (size-of type :inlined t)))
	    #'(lambda (instance location &optional (offset 0))
		(copy-memory 
		 (foreign-location instance) size
		 (pointer+ location offset))))
	(let ((ref (reference-function  (type-expand type))))
	  #'(lambda (instance location &optional (offset 0))
	      (funcall ref 
	       (foreign-location instance) 
	       (pointer+ location offset)))))
    (call-next-method)))

(define-type-method reader-function ((type struct) &key (ref :read) inlined)
  (if inlined
      (let ((class (type-expand type))
	    (size (size-of type :inlined t)))
	(ecase ref
	  (:read
	   #'(lambda (location &optional (offset 0))
	       (ensure-proxy-instance class (pointer+ location offset))))
	  (:peek
	   #'(lambda (location &optional (offset 0))	       
	       (ensure-proxy-instance class (pointer+ location offset) 
		:reference nil :finalize nil)))
	  (:get
	   #'(lambda (location &optional (offset 0))
	       (prog1
		   (ensure-proxy-instance class
		    (copy-memory (pointer+ location offset) size)
		    :reference nil)
		 (clear-memory (pointer+ location offset) size))))))
    (call-next-method)))

(define-type-method destroy-function ((type struct) &key temp inlined)
  (if inlined
      (let ((size (size-of type :inlined t)))
	(if temp
	    #'(lambda (location &optional (offset 0))
		(clear-memory (pointer+ location offset) size))
	  (let ((unref (unreference-function  (type-expand type))))
	    #'(lambda (location &optional (offset 0))
		(funcall unref (pointer+ location offset) t)))))
    (call-next-method)))

(define-type-method copy-function ((type struct) &key inlined)
  (if inlined
	(let ((ref (reference-function  (type-expand type))))
	  #'(lambda (from to &optional (offset 0))
	      (funcall ref (pointer+ from offset) (pointer+ to offset))))
    (call-next-method)))
