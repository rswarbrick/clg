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

;; $Id: gobject.lisp,v 1.14 2004-10-28 09:34:35 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass ginstance-class)
    (:alien-name "GObject")
    (:copy %object-ref)
    (:free %object-unref)))


(defmethod initialize-instance ((object gobject) &rest initargs)
  (let ((slotds (class-slots (class-of object)))
	(names (make-array 0 :adjustable t :fill-pointer t))
	(values (make-array 0 :adjustable t :fill-pointer t)))

    (loop 
     as tmp = initargs then (cddr tmp) while tmp
     as key = (first tmp)
     as value = (second tmp)
     as slotd = (find-if
		 #'(lambda (slotd)
		     (member key (slot-definition-initargs slotd)))
		 slotds)
     when (and (typep slotd 'effective-gobject-slot-definition)
	       (slot-value slotd 'construct))
     do (let ((type (find-type-number (slot-definition-type slotd))))
	  (vector-push-extend (slot-definition-pname slotd) names)
	  (vector-push-extend (gvalue-new type value) values)
	  (remf initargs key)))

    (setf  
     (slot-value object 'location) 
     (if (zerop (length names))
	 (%gobject-new (type-number-of object))
       (%gobject-newvv (type-number-of object) (length names) names values)))
    
    (mapc #'gvalue-free values))
  
  (apply #'call-next-method object initargs))



(defbinding (%gobject-new "g_object_new") () pointer
  (type type-number)
  (nil null))

(defbinding (%gobject-newvv "g_object_newvv") () pointer
  (type type-number)
  (n-parameters unsigned-int)
  (names (vector string))
  (values (vector gvalue)))


(defbinding %object-ref (type location) pointer
  (location pointer))

 (defbinding %object-unref (type location) nil
   (location pointer))

(defun object-ref (object)
  (%object-ref nil (proxy-location object)))

(defun object-unref (object)
  (%object-unref nil (proxy-location object)))



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
   (register-user-data data) *destroy-notify*)
  data)

(defbinding %object-get-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun object-data (object key &key (test #'eq))
  (find-user-data
   (%object-get-qdata object (quark-from-object key :test test))))



;;;; Metaclass used for subclasses of gobject

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject-class (ginstance-class)
    ())

  (defclass direct-gobject-slot-definition (direct-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname)
     (readable :initform t :reader slot-readable-p :initarg :readable)
     (writable :initform t :reader slot-writable-p :initarg :writable)
     (construct :initform nil :initarg :construct)))

  (defclass effective-gobject-slot-definition (effective-virtual-slot-definition)
    ((pname :reader slot-definition-pname :initarg :pname)
     (readable :reader slot-readable-p :initarg :readable)
     (writable :reader slot-writable-p :initarg :writable)
     (construct :initarg :construct))))



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
    (:property (find-class 'direct-gobject-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-gobject-slot-definition))
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


(defmethod initialize-internal-slot-functions ((slotd effective-gobject-slot-definition))
  (let* ((type (slot-definition-type slotd))
	 (pname (slot-definition-pname slotd))
	 (type-number (find-type-number type)))
    (unless (slot-boundp slotd 'reader-function)
      (setf 
       (slot-value slotd 'reader-function)
       (if (slot-readable-p slotd)
	   #'(lambda (object)
	       (with-gc-disabled
		   (let ((gvalue (gvalue-new type-number)))
		     (%object-get-property object pname gvalue)
		     (unwind-protect
			  (funcall
			   (intern-reader-function (type-from-number type-number)) gvalue +gvalue-value-offset+) ; temporary workaround for wrong topological sorting of types
		       (gvalue-free gvalue t)))))
	   #'(lambda (value object)
	       (error "Slot is not readable: ~A" (slot-definition-name slotd))))))
    
    (unless (slot-boundp slotd 'writer-function)
      (setf 
       (slot-value slotd 'writer-function)
       (if (slot-writable-p slotd)
	   #'(lambda (value object)
	       (with-gc-disabled
		   (let ((gvalue (gvalue-new type-number)))
		     (funcall
		      (intern-writer-function (type-from-number type-number)) ; temporary
		      value gvalue +gvalue-value-offset+)
		     (%object-set-property object pname gvalue)
		     (funcall
		      (intern-destroy-function (type-from-number type-number)) ; temporary
		      gvalue +gvalue-value-offset+)
		     (gvalue-free gvalue nil)
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


(defmethod validate-superclass ((class gobject-class)
				(super pcl::standard-class))
;  (subtypep (class-name super) 'gobject)
  t)



;;;;

(defbinding %object-class-list-properties () pointer
  (class pointer)
  (n-properties unsigned-int :out))

(defun query-object-class-properties (type-number &optional
				      inherited-properties)
  (let ((class (type-class-ref type-number)))
    (multiple-value-bind (array length)
	(%object-class-list-properties class)
      (unwind-protect
	  (let ((all-properties
		 (map-c-array 'list #'identity array 'param length)))
	    (if (not inherited-properties)
		(delete-if
		 #'(lambda (param)
		     (not (eql type-number (param-owner-type param))))
		 all-properties)
	      all-properties))
	(deallocate-memory array)))))


(defun default-slot-name (name)
  (intern (substitute #\- #\_ (string-upcase (string-upcase name)))))

(defun default-slot-accessor (class-name slot-name type)
  (intern
   (format
    nil "~A-~A~A" class-name slot-name
    (if (eq type 'boolean) "-P" ""))))

(defun expand-gobject-type (type-number &optional options
			    (metaclass 'gobject-class))
  (let* ((supers (cons (supertype type-number) (implements type-number)))
	 (class  (type-from-number type-number))
	 (manual-slots (getf options :slots))
	 (expanded-slots
	  (mapcar
	   #'(lambda (param)
	       (with-slots (name flags value-type documentation) param
	         (let* ((slot-name (default-slot-name name))
;			(slot-type value-type) ;(type-from-number value-type t))
			(slot-type (or (type-from-number value-type) value-type))
			(accessor
			 (default-slot-accessor class slot-name slot-type)));(type-from-number slot-type)))) ; temporary workaround for wrong topological sorting of types

		   `(,slot-name
		     :allocation :property
		     :pname ,name
		     ,@(cond
			((and
			  (member :writable flags)
			  (member :readable flags)
			  (not (member :construct-only flags)))
			 (list :accessor accessor))
			((and (member :writable flags)
			      (not (member :construct-only flags)))
			 (list :writer `(setf ,accessor)))
			((member :readable flags)
			 (list :reader accessor)))
		     ,@(when (or
			      (not (member :writable flags))
			      (member :construct-only flags))
		         (list :writable nil))
		     ,@(when (not (member :readable flags))
		         (list :readable nil))
		     ,@(when (or 
			      (member :construct flags)
			      (member :construct-only flags))
		         (list :construct t))
		     ,@(when (or
			      (member :construct flags)
			      (member :construct-only flags)
			      (member :writable flags))
			 (list :initarg (intern (string slot-name) "KEYWORD")))
		     :type ,slot-type
		     ,@(when documentation
			 (list :documentation documentation))))))
	   (query-object-class-properties type-number))))

    (dolist (slot-def (reverse manual-slots))
      (let ((name (car slot-def))
	    (pname (getf (cdr slot-def) :pname)))
	(setq
	 expanded-slots
	 (delete-if
	  #'(lambda (expanded-slot-def)
	      (or
	       (eq name (car expanded-slot-def))
	       (and
		pname
		(string= pname (getf (cdr expanded-slot-def) :pname)))))
	  expanded-slots))

	(unless (getf (cdr slot-def) :ignore)
	  (push slot-def expanded-slots))))
    
    `(progn
       (defclass ,class ,supers
	 ,expanded-slots
	 (:metaclass ,metaclass)
	 (:alien-name ,(find-type-name type-number))))))


(register-derivable-type 'gobject "GObject" 'expand-gobject-type)

