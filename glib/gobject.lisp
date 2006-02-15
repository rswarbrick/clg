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

;; $Id: gobject.lisp,v 1.47 2006-02-15 09:45:41 espen Exp $

(in-package "GLIB")


;;;; Metaclass used for subclasses of gobject

(eval-when (:compile-toplevel :load-toplevel :execute)
;;   (push :debug-ref-counting *features*)
  (defclass gobject-class (ginstance-class)
    ((instance-slots-p :initform nil
      :documentation "Non NIL if the class has slots with instance allocation")))

  (defmethod validate-superclass ((class gobject-class) (super standard-class))
;  (subtypep (class-name super) 'gobject)
    t))

(defclass direct-property-slot-definition (direct-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :reader slot-readable-p :initarg :readable)
   (writable :reader slot-writable-p :initarg :writable)
   (construct-only :initarg :construct-only :reader construct-only-property-p)))

(defclass effective-property-slot-definition (effective-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :reader slot-readable-p :initarg :readable)
   (writable :reader slot-writable-p :initarg :writable)
   (construct-only :initarg :construct-only :reader construct-only-property-p)))

(defclass direct-user-data-slot-definition (direct-virtual-slot-definition)
  ())

(defclass effective-user-data-slot-definition (effective-virtual-slot-definition)
  ())


(defbinding %object-ref () pointer
  (location pointer))

(defbinding %object-unref () nil
  (location pointer))

#+glib2.8
(progn
  (defcallback toggle-ref-callback (nil (data pointer) (location pointer) (last-ref-p boolean))
    #+debug-ref-counting
    (if last-ref-p
	(format t "Object at 0x~8,'0X has no foreign references~%" (sap-int location))
      (format t "Foreign reference added to object at 0x~8,'0X~%" (sap-int location)))
    (if last-ref-p
	(cache-instance (find-cached-instance location) t)
      (cache-instance (find-cached-instance location) nil)))

  (defbinding %object-add-toggle-ref () pointer
    (location pointer)
    ((callback toggle-ref-callback) pointer)
    (nil null))

  (defbinding %object-remove-toggle-ref () pointer
    (location pointer)
    ((callback toggle-ref-callback) pointer)
    (nil null)))

(defmethod reference-foreign ((class gobject-class) location)
  (declare (ignore class))
  (%object-ref location))

(defmethod unreference-foreign ((class gobject-class) location)
  (declare (ignore class))
  (%object-unref location))

#+debug-ref-counting
(progn
  (defcallback weak-ref-callback (nil (data pointer) (location pointer))
    (format t "Object at 0x~8,'0X being finalized~%" (sap-int location)))
  
  (defbinding %object-weak-ref () pointer
    (location pointer)
    ((callback weak-ref-callback) pointer)
    (nil null)))


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
    (:user-data (find-class 'direct-user-data-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-property-slot-definition))
    (:user-data (find-class 'effective-user-data-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class gobject-class) direct-slotds)
  (if (eq (slot-definition-allocation (first direct-slotds)) :property)
      (nconc 
       (list :pname (signal-name-to-string 
		     (most-specific-slot-value direct-slotds 'pname
		      (slot-definition-name (first direct-slotds))))
	     :readable (most-specific-slot-value direct-slotds 'readable t)
	     :writable (most-specific-slot-value direct-slotds 'writable t)
	     :construct-only (most-specific-slot-value direct-slotds 
                              'construct-only nil))
       (call-next-method))
    (call-next-method)))


(defvar *ignore-setting-construct-only-property* nil)
(declaim (special *ignore-setting-construct-only-property*))

(defmethod initialize-internal-slot-functions ((slotd effective-property-slot-definition))
  (let ((type (slot-definition-type slotd))
	(pname (slot-definition-pname slotd)))
    (when (and (not (slot-boundp slotd 'getter)) (slot-readable-p slotd))
      (setf 
       (slot-value slotd 'getter)
       (let ((reader nil))
	 #'(lambda (object)
	     (unless reader
	       (setq reader (reader-function type)))
	     (let ((gvalue (gvalue-new type)))
	       (%object-get-property object pname gvalue)
	       (unwind-protect
		 (funcall reader  gvalue +gvalue-value-offset+)
		 (gvalue-free gvalue t)))))))
    
    (when (not (slot-boundp slotd 'setter))
      (cond
       ((slot-writable-p slotd)
	(setf 
	 (slot-value slotd 'setter)
	 (let ((writer nil))
	   #'(lambda (value object)
	       (unless writer
		 (setq writer (writer-function type)))
	       (let ((gvalue (gvalue-new type)))
		 (funcall writer value gvalue +gvalue-value-offset+)
		 (%object-set-property object pname gvalue)
		 (gvalue-free gvalue t)
		 value)))))

       ((construct-only-property-p slotd)
	(setf 
	 (slot-value slotd 'setter)
	 #'(lambda (value object)
	     (declare (ignore value object))
	     (unless *ignore-setting-construct-only-property*
	       (error "Slot is not writable: ~A" (slot-definition-name slotd)))))))))

  (call-next-method))

(defmethod initialize-internal-slot-functions ((slotd effective-user-data-slot-definition))
  (let ((slot-name (slot-definition-name slotd)))
    (unless (slot-boundp slotd 'getter)
      (setf 
       (slot-value slotd 'getter)
       #'(lambda (object)
	   (prog1 (user-data object slot-name)))))
    (unless (slot-boundp slotd 'setter)
      (setf 
       (slot-value slotd 'setter)
       #'(lambda (value object)
	   (setf (user-data object slot-name) value))))
    (unless (slot-boundp slotd 'boundp)
      (setf 
       (slot-value slotd 'boundp)
       #'(lambda (object)
	   (user-data-p object slot-name)))))
  (call-next-method))

(defmethod shared-initialize :after ((class gobject-class) names &rest initargs)
  (declare (ignore initargs))
  (when (some #'(lambda (slotd)
		  (and
		   (eq (slot-definition-allocation slotd) :instance)
		   (not (typep slotd 'effective-special-slot-definition))))
	      (class-slots class))
    (setf (slot-value class 'instance-slots-p) t)))



;;;; Super class for all classes in the GObject type hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    (#+debug-ref-counting
     (ref-count :allocation :alien :type int :reader ref-count))
    (:metaclass gobject-class)
    (:gtype "GObject")))

#+debug-ref-counting
(defmethod print-object ((instance gobject) stream)
  (print-unreadable-object (instance stream :type t :identity nil)
    (if (proxy-valid-p instance)
	(format stream "at 0x~X (~D)" (sap-int (foreign-location instance)) (ref-count instance))
      (write-string "at \"unbound\"" stream))))


(defun initial-add (object function initargs key pkey)
  (loop 
   as (initarg value . rest) = initargs then rest
   do (cond
       ((eq initarg key) (funcall function object value))
       ((eq initarg pkey) (mapc #'(lambda (value)
				    (funcall function object value))
				value)))
       while rest))

(defun initial-apply-add (object function initargs key pkey)
  (initial-add object #'(lambda (object value)
			  (apply function object (mklist value)))
	       initargs key pkey))


(defmethod make-proxy-instance ((class gobject-class) location &rest initargs)
  (declare (ignore location initargs))
  (if (slot-value class 'instance-slots-p)
      (error "An object of class ~A has instance slots and should only be created with MAKE-INSTANCE" class)
    (call-next-method)))


(defmethod allocate-foreign ((object gobject) &rest initargs)
  (let ((init-slots ())) 
    (flet ((value-from-initargs (slotd)
	     (loop
	      with slot-initargs = (slot-definition-initargs slotd)
	      for (initarg value) on initargs by #'cddr
	      when (find initarg slot-initargs)
	      do (return (values value t)))))

    (loop 
     for slotd in (class-slots (class-of object))
     when (and 
	   (eq (slot-definition-allocation slotd) :property)
	   (construct-only-property-p slotd))
     do (multiple-value-bind (value initarg-p) (value-from-initargs slotd)
	  (cond
	   (initarg-p (push (cons slotd value) init-slots))
	   ((slot-definition-initfunction slotd)
	    (push 
	     (cons slotd (funcall (slot-definition-initfunction slotd)))
	     init-slots))))))

    (cond
     (init-slots
      (let ((element-size (+ +gvalue-size+ +size-of-pointer+))
	    (num-slots (length init-slots)))
	(with-allocated-memory (params (* num-slots element-size))
          (loop
	   with string-writer = (writer-function 'string)
	   for (slotd . value) in init-slots
	   as offset = params then (sap+ offset element-size)
	   as type = (slot-definition-type slotd)
	   as pname = (slot-definition-pname slotd)
	   do (funcall string-writer pname offset)
              (gvalue-init (sap+ offset +size-of-pointer+) type value))

	  (unwind-protect
	      (%gobject-newv (type-number-of object) num-slots params)
	
	    (loop
	     with string-destroy = (destroy-function 'string)
	     repeat num-slots
	     as offset = params then (sap+ offset element-size)
	     do (funcall string-destroy offset)
	        (gvalue-unset (sap+ offset +size-of-pointer+)))))))

     (t (%gobject-new (type-number-of object))))))


(defmethod shared-initialize ((object gobject) names &rest initargs)
  (declare (ignore names initargs))
  (let ((*ignore-setting-construct-only-property* t))
    (call-next-method)))

(defmethod initialize-instance :around ((object gobject) &rest initargs)
  (declare (ignore initargs))
  (prog1
      (call-next-method)
    #+debug-ref-counting(%object-weak-ref (foreign-location object))
    #+glib2.8
    (when (slot-value (class-of object) 'instance-slots-p)
      (with-slots (location) object
        (%object-add-toggle-ref location)
	(%object-unref location)))))


(defmethod instance-finalizer ((instance gobject))
  (let ((location (foreign-location instance)))
    #+glib2.8
    (if (slot-value (class-of instance) 'instance-slots-p)
	#'(lambda ()
	    #+debug-ref-counting
	    (format t "Finalizing proxy for 0x~8,'0X~%" (sap-int location))
	    (remove-cached-instance location)
	    (%object-remove-toggle-ref location))
      #'(lambda ()
	  #+debug-ref-counting
	  (format t "Finalizing proxy for 0x~8,'0X~%" (sap-int location))
	  (remove-cached-instance location)
	  (%object-unref location)))
    #-glib2.8
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


;;;; User data

(defbinding %object-set-qdata-full () nil
  (object gobject)
  (id quark)
  (data unsigned-long)
  (destroy-marshal pointer))

(defcallback user-data-destroy-func (nil (id unsigned-int))
  (destroy-user-data id))

(export 'user-data-destroy-func)

(defun (setf user-data) (data object key)
  (%object-set-qdata-full object (quark-intern key)
   (register-user-data data) (callback user-data-destroy-func))
  data)

;; deprecated
(defun (setf object-data) (data object key &key (test #'eq))
  (assert (eq test #'eq))
  (setf (user-data object key) data))

(defbinding %object-get-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun user-data (object key)
  (find-user-data (%object-get-qdata object (quark-intern key))))

;; deprecated
(defun object-data (object key &key (test #'eq))
  (assert (eq test #'eq))
  (user-data object key))

(defun user-data-p (object key)
  (user-data-exists-p (%object-get-qdata object (quark-intern key))))

(defbinding %object-steal-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun unset-user-data (object key)
  (destroy-user-data (%object-steal-qdata object (quark-intern key))))


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
  (let* ((type-number (find-type-number type t))
	 (class (type-class-ref type-number)))
    (unwind-protect
	 (multiple-value-bind (array length)
	     (%object-class-list-properties class)
	   (unless (null-pointer-p array)
	     (unwind-protect
		 (%map-params array length type-number inherited-p)
	       (deallocate-memory array))))
;      (type-class-unref type-number)
      )))


(defun default-slot-name (name)
  (intern (substitute #\- #\_ (string-upcase (string-upcase name)))))

(defun default-slot-accessor (class-name slot-name type)
  (intern
   (format
    nil "~A-~A~A" class-name slot-name
    (if (eq type 'boolean) "-P" ""))))


(defun slot-definition-from-property (class property &optional slot-name args)
  (with-slots (name flags value-type documentation) property
    (let* ((slot-name (or slot-name (default-slot-name name)))
	   (slot-type (or (getf args :type) (type-from-number value-type) 'pointer))
	   (accessor (default-slot-accessor class slot-name slot-type)))
      
      `(,slot-name
	:allocation :property :pname ,name

	,@(when (find :unbound args) (list :unbound (getf args :unbound)))
	,@(when (find :getter args) (list :getter (getf args :getter)))
	,@(when (find :setter args) (list :setter (getf args :setter)))
	
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
	,@(when (member :construct-only flags)
	    '(:construct-only t))
	
	;; initargs
	,@(if (find :initarg args)
	      (let ((initarg (getf args :initarg)))
		(etypecase initarg
		  (null ())
		  (symbol `(:initarg ,initarg))))
	    (when (or (member :construct flags)
		      (member :construct-only flags)
		      (member :writable flags))
	      (list :initarg (intern (string slot-name) "KEYWORD"))))
	
	:type ,slot-type
	:documentation ,documentation))))


(defun slot-definitions (class properties slots)
  (loop 
   for property in properties
   as slot = (or
	      (find (param-name property) slots 
	       :key #'(lambda (slot) (getf (rest slot) :pname)) 
	       :test #'string=)
	      (find (param-name property) slots 
	       :key #'first :test #'string-equal))
   do (cond
       ((not slot) 
	(push (slot-definition-from-property class property) slots))
       ((getf (rest slot) :merge)
	(setf 
	 (rest slot) 
	 (rest (slot-definition-from-property class property (first slot) (rest slot)))))))
  (delete-if #'(lambda (slot) (getf (rest slot) :ignore)) slots))


(defun expand-gobject-type (type forward-p options &optional (metaclass 'gobject-class))
  (let ((supers (cons (supertype type) (implements type)))
	(class  (type-from-number type))
	(slots (getf options :slots)))    
    `(defclass ,class ,supers
	 ,(unless forward-p
	    (slot-definitions class (query-object-class-properties type) slots))
	 (:metaclass ,metaclass)
	 (:gtype ,(register-type-as type)))))

(defun gobject-dependencies (type)
  (delete-duplicates 
   (cons
    (supertype type)
    (append 
     (type-interfaces type)
     (mapcar #'param-value-type (query-object-class-properties type))))))


(register-derivable-type 'gobject "GObject" 'expand-gobject-type 'gobject-dependencies)


;;; Pseudo type for gobject instances which have their reference count
;;; increased by the returning function

(defmethod alien-type ((type (eql 'referenced)) &rest args)
  (declare (ignore type args))
  (alien-type 'gobject))

(defmethod from-alien-form (form (type (eql 'referenced)) &rest args)
  (declare (ignore type))
  (destructuring-bind (type) args
    (if (subtypep type 'gobject)
	(let ((instance (make-symbol "INSTANCE")))
	  `(let ((,instance ,(from-alien-form form type)))
	     (when ,instance
	       (%object-unref (foreign-location ,instance)))
	     ,instance))
      (error "~A is not a subclass of GOBJECT" type))))

(export 'referenced)
