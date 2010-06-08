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

;; $Id: gobject.lisp,v 1.59 2008-11-04 03:22:23 espen Exp $

(in-package "GLIB")


;;;; Metaclass used for subclasses of gobject

(eval-when (:compile-toplevel :load-toplevel :execute)
;;   (push :debug-ref-counting *features*)
  (defclass gobject-class (ginstance-class)
    ((instance-slots-p :initform nil :reader instance-slots-p
      :documentation "Non NIL if the class has slots with instance allocation")))

  (defmethod validate-superclass ((class gobject-class) (super standard-class))
;  (subtypep (class-name super) 'gobject)
    t))

(defmethod slot-unbound (metaclass (class gobject-class) (slot (eql 'ref)))
  (assert (class-direct-superclasses class))
  (setf (slot-value class 'ref) 
   #?-(pkg-exists-p "glib-2.0" :atleast-version "2.10.0") '%object-ref
   #?(pkg-exists-p "glib-2.0" :atleast-version "2.10.0")
   ;; We do this hack instead of creating a new metaclass to avoid
   ;; breaking backward compatibility
   (if (subtypep (class-name class) 'initially-unowned)
       '%object-ref-sink
     '%object-ref)))

(defmethod slot-unbound (metaclass (class gobject-class) (slot (eql 'unref)))
  (setf (slot-value class 'unref) '%object-unref))


(defclass direct-property-slot-definition (direct-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :reader slot-readable-p :initarg :readable)
   (writable :reader slot-writable-p :initarg :writable)
   (construct-only :initarg :construct-only :reader construct-only-property-p)))

(defclass effective-property-slot-definition (effective-virtual-slot-definition)
  ((pname :reader slot-definition-pname :initarg :pname)
   (readable :initform t :reader slot-readable-p :initarg :readable)
   (writable :initform t :reader slot-writable-p :initarg :writable)
   (construct-only :initform nil :initarg :construct-only :reader construct-only-property-p)))

(defclass direct-user-data-slot-definition (direct-virtual-slot-definition)
  ())

(defclass effective-user-data-slot-definition (effective-virtual-slot-definition)
  ())


(defbinding %object-ref () pointer
  (location pointer))

(defbinding %object-unref () nil
  (location pointer))

#?(pkg-exists-p "glib-2.0" :atleast-version "2.8.0")
(progn
  (define-callback toggle-ref-callback nil
      ((data pointer) (location pointer) (last-ref-p boolean))
    (declare (ignore data))
    #+debug-ref-counting
    (if last-ref-p
	(format t "Object at 0x~8,'0X has no foreign references~%" (pointer-address location))
      (format t "Foreign reference added to object at 0x~8,'0X~%" (pointer-address location)))
    (if last-ref-p
	(cache-instance (find-cached-instance location) t)
      (cache-instance (find-cached-instance location) nil)))

  (defbinding %object-add-toggle-ref (location) pointer
    (location pointer)
    (toggle-ref-callback callback)
    (nil null))

  (defbinding %object-remove-toggle-ref (location) pointer
    (location pointer)
    (toggle-ref-callback callback)
    (nil null)))

#+debug-ref-counting
(progn
  (define-callback weak-ref-callback nil ((data pointer) (location pointer))
    (format t "Object at 0x~8,'0X (~A) being finalized~%" (pointer-address location) (type-from-number (%type-number-of-ginstance location))))
  
  (defbinding %object-weak-ref (location) pointer
    (location pointer)
    (weak-ref-callback callback)
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
       (compute-most-specific-initargs direct-slotds
        '(pname construct-only readable writable))
       (call-next-method))
    (call-next-method)))


(defvar *ignore-setting-construct-only-property* nil)
(declaim (special *ignore-setting-construct-only-property*))

(defmethod compute-slot-reader-function ((slotd effective-property-slot-definition) &optional signal-unbound-p)
  (declare (ignore signal-unbound-p))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (get-reader (reader-function type :ref :get))
	 (peek-reader (reader-function type :ref :peek)))
    #'(lambda (object)
	(with-memory (gvalue +gvalue-size+)
	  (%gvalue-init gvalue (find-type-number type))
	  (%object-get-property object pname gvalue)
	  (if (gvalue-static-p gvalue)
	      (funcall peek-reader gvalue +gvalue-value-offset+)
	    (funcall get-reader gvalue +gvalue-value-offset+))))))

(defmethod compute-slot-writer-function :around ((slotd effective-property-slot-definition))
  (if (construct-only-property-p slotd)
      #'(lambda (value object)
	  (declare (ignore value))
	  (unless *ignore-setting-construct-only-property*
	    (error 'unwritable-slot :name (slot-definition-name slotd) :instance object)))
    (call-next-method)))

(defmethod compute-slot-writer-function ((slotd effective-property-slot-definition))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (writer (writer-function type :temp t))
	 (destroy (destroy-function type :temp t)))
    #'(lambda (value object)
	(with-memory (gvalue +gvalue-size+)
	  (%gvalue-init gvalue (find-type-number type))
	  (funcall writer value gvalue +gvalue-value-offset+)
	  (%object-set-property object pname gvalue)
	  (funcall destroy gvalue +gvalue-value-offset+))
	value)))

(defmethod slot-readable-p ((slotd effective-user-data-slot-definition))
  (declare (ignore slotd))
  t)

(defmethod compute-slot-reader-function ((slotd effective-user-data-slot-definition) &optional signal-unbound-p)
  (declare (ignore signal-unbound-p))
  (let ((slot-name (slot-definition-name slotd)))
    #'(lambda (object)
	(user-data object slot-name))))

(defmethod compute-slot-boundp-function ((slotd effective-user-data-slot-definition))
  (let ((slot-name (slot-definition-name slotd)))
    #'(lambda (object)
	(user-data-p object slot-name))))

(defmethod slot-writable-p ((slotd effective-user-data-slot-definition))
  (declare (ignore slotd))
  t)

(defmethod compute-slot-writer-function ((slotd effective-user-data-slot-definition))
  (let ((slot-name (slot-definition-name slotd)))
    #'(lambda (value object)
	(setf (user-data object slot-name) value))))

(defmethod compute-slot-makunbound-function ((slotd effective-user-data-slot-definition))
  (let ((slot-name (slot-definition-name slotd)))
    #'(lambda (object)
	(unset-user-data object slot-name))))

(defmethod compute-slots :around ((class gobject-class))
  (let ((slots (call-next-method)))
    (when (some #'(lambda (slotd)
		    (and
		     (eq (slot-definition-allocation slotd) :instance)
		     (not (typep slotd 'effective-special-slot-definition))))
		slots)
      (setf (slot-value class 'instance-slots-p) t))
    slots))


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
	(format stream "at 0x~X (~D)" (pointer-address (foreign-location instance)) (ref-count instance))
      (write-string "at \"unbound\"" stream))))


(define-type-method reader-function ((type gobject) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (ecase ref
    ((:read :peek) (call-next-method type :ref :read))
    (:get
     #'(lambda (location &optional (offset 0))
	 (let ((instance (ref-pointer location offset)))
	   (unless (null-pointer-p instance)
	     (multiple-value-bind (gobject new-p)
		 (ensure-proxy-instance 'gobject instance :reference nil)
	       (unless new-p
		 (%object-unref instance))
	       (setf (ref-pointer location offset) (make-pointer 0))
	       gobject)))))))

(define-type-method callback-wrapper ((type gobject) var arg form)
  (let ((class (type-expand type)))
    `(let ((,var (ensure-proxy-instance ',class ,arg)))
       ,form)))

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
      (error "Objects of class ~A has instance slots and should only be created with MAKE-INSTANCE" class)
    (call-next-method)))

(defparameter +gparameter-gvalue-offset+
  (max (size-of 'pointer) (type-alignment '(unsigned-byte 64))))
(defparameter +gparameter-size+
  (+ +gparameter-gvalue-offset+ +gvalue-size+))

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
      (let* ((num-slots (length init-slots)))
	(with-memory (params (* num-slots +gparameter-size+))
          (loop
	   with string-writer = (writer-function 'string)
	   for (slotd . value) in init-slots
	   as param = params then (pointer+ param +gparameter-size+)
	   as type = (slot-definition-type slotd)
	   as pname = (slot-definition-pname slotd)
	   do (funcall string-writer pname param)
	      (gvalue-init 
	       (pointer+ param +gparameter-gvalue-offset+) type value))

	  (unwind-protect
	      (%gobject-newv (type-number-of object) num-slots params)
	
	    (loop
	     with string-destroy = (destroy-function 'string)
	     repeat num-slots
	     as param = params then (pointer+ param +gparameter-size+)
	     do (funcall string-destroy param)
	        (gvalue-unset (pointer+ param +gparameter-gvalue-offset+)))))))
     
     (t (%gobject-new (type-number-of object))))))


(defmethod shared-initialize ((object gobject) names &rest initargs)
  (declare (ignore names initargs))
  (let ((*ignore-setting-construct-only-property* t))
    (call-next-method)))

(defmethod initialize-instance :around ((object gobject) &rest initargs)
  (declare (ignore initargs))
  (prog1
      (call-next-method)
    (let ((location (foreign-location object)))
      #+debug-ref-counting(%object-weak-ref location)
      #?(pkg-exists-p "glib-2.0" :atleast-version "2.8.0")
      (when (slot-value (class-of object) 'instance-slots-p)
	(%object-add-toggle-ref location)
	(%object-unref location)))))


(defmethod instance-finalizer ((instance gobject))
  (let ((location (foreign-location instance)))
    #?(pkg-exists-p "glib-2.0" :atleast-version "2.8.0")
    (if (slot-value (class-of instance) 'instance-slots-p)
	#'(lambda ()
	    #+debug-ref-counting
	    (format t "Finalizing proxy for 0x~8,'0X (~A)~%" 
	     (pointer-address location) 
	     (find-foreign-type-name (%type-number-of-ginstance location)))
	    (%object-remove-toggle-ref location))
      #'(lambda ()
	  #+debug-ref-counting
	  (format t "Finalizing proxy for 0x~8,'0X (~A)~%" 
	   (pointer-address location)
	   (find-foreign-type-name (%type-number-of-ginstance location)))
	  (%object-unref location)))
    #?-(pkg-exists-p "glib-2.0" :atleast-version "2.8.0")
    #'(lambda ()
	(%object-unref location))))


(defbinding (%gobject-new "g_object_new") () pointer
  (type type-number)
  (nil null))

(defbinding (%gobject-newv "g_object_newv") () pointer
  (type type-number)
  (n-parameters unsigned-int)
  (params pointer))


;;;; Floating references

#?(pkg-exists-p "glib-2.0" :atleast-version "2.10.0")
(progn
  (defclass initially-unowned (gobject)
    ()
    (:metaclass gobject-class)
    (:gtype "GInitiallyUnowned"))

  (defbinding %object-ref-sink () pointer
    (location pointer))

  (defbinding %object-is-floating () boolean
    (location pointer))

  (defmethod initialize-instance :before ((object initially-unowned) &rest initargs)
    (declare (ignore initargs))
    (%object-ref-sink (foreign-location object))))


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

(defgeneric (setf user-data) (data object key))
(defgeneric user-data (object key))
(defgeneric user-data-p (object key))
(defgeneric unset-user-data (object key))

(defbinding %object-set-qdata-full () nil
  (object gobject)
  (id quark)
  (data pointer-data)
  (destroy-marshal callback))

(define-callback user-data-destroy-callback nil ((id pointer-data))
  (destroy-user-data id))

(defmethod (setf user-data) (data (object gobject) key)
  (%object-set-qdata-full object (quark-intern key)
   (register-user-data data) user-data-destroy-callback)
  data)

(defbinding %object-get-qdata () pointer-data
  (object gobject)		 
  (id quark))

(defmethod user-data ((object gobject) key)
  (find-user-data (%object-get-qdata object (quark-intern key))))

(defmethod user-data-p ((object gobject) key)
  (user-data-exists-p (%object-get-qdata object (quark-intern key))))

(defbinding %object-steal-qdata () pointer-data
  (object gobject)		 
  (id quark))

(defmethod unset-user-data ((object gobject) key)
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
  (let ((prefix-len (length (package-prefix))))
    (intern (substitute #\- #\_
	     (string-upcase
	      (if (and
		   (string-prefix-p (package-prefix) name)
		   (char= #\- (char name prefix-len)))
		  (subseq name (1+ prefix-len))
		name))))))

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
    (when (member nil supers)
      (error "Got NIL as a supertype for ~A (supertype name: ~A).~%~
              This shouldn't happen - is the parent type correctly registered?"
             (find-foreign-type-name type)
             (find-foreign-type-name (type-parent type))))
    (unless class
      (error "It seems that the class for ~A hasn't been correctly registered.~
              This might mean that you've forgotten to shadow an import in~
              your defpackage?"
             (find-foreign-type-name type)))
    `(defclass ,class ,supers
	 ,(unless forward-p
	    (slot-definitions class (query-object-class-properties type) slots))
	 (:metaclass ,metaclass)
	 (:gtype ,(register-type-as type)))))

(defun gobject-dependencies (type options)
  (delete-duplicates 
   (cons
    (supertype type)
    (append 
     (type-interfaces type)
     (mapcar #'param-value-type (query-object-class-properties type))
     (getf options :dependencies)
     (loop
      for slot in (getf options :slots)
      as type = (getf (rest slot) :type)
      when (and type (symbolp type) (find-type-number type))
      collect (find-type-number type))))))


(register-derivable-type 'gobject "GObject" 'expand-gobject-type 'gobject-dependencies)


;;; Pseudo type for gobject instances which have their reference count
;;; increased by the returning function

(deftype referenced (type) type)

(define-type-method from-alien-form ((type referenced) form &key (ref :free))
  (cond
   ((not (eq ref :free))
    (error "Keyword arg :REF to FROM-ALIEN-FORM should be :FREE for type ~A. It was give ~A" type ref))
   ((subtypep type 'gobject)
    (from-alien-form (second (type-expand-to 'referenced type)) form :ref ref))))

(define-type-method from-alien-function ((type referenced) &key (ref :free))
  (cond
   ((not (eq ref :free))
    (error "Keyword arg :REF to FROM-ALIEN-FUNCTION should be :FREE for type ~A. It was give ~A" type ref))
;   ((subtypep type 'gobject) (call-next-method type ref :free))))
   ((subtypep type 'gobject) 
    (from-alien-function (second (type-expand-to 'referenced type)) :ref ref))))
