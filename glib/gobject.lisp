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

;; $Id: gobject.lisp,v 1.12 2002-04-02 14:57:19 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass ginstance-class)
    (:alien-name "GObject")
    (:copy %object-ref)
    (:free %object-unref)))

(defmethod initialize-instance ((object gobject) &rest initargs)
  (declare (ignore initargs))
  (setf  (slot-value object 'location) (%gobject-new (type-number-of object)))
  (call-next-method))

(defbinding (%gobject-new "g_object_new") () pointer
  (type type-number)
  (nil null))


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
  (defclass gobject-class (ginstance-class))

  (defclass direct-gobject-slot-definition (direct-virtual-slot-definition)
    ((pname :reader slot-definition-pname)))

  (defclass effective-gobject-slot-definition
    (effective-virtual-slot-definition)))

  

; (defbinding object-class-install-param () nil
;   (class pointer)
;   (id unsigned-int)
;   (parameter parameter))

; (defbinding object-class-find-param-spec () parameter
;   (class pointer)
;   (name string))

(defun signal-name-to-string (name)
  (substitute #\_ #\- (string-downcase (string name))))

(defmethod initialize-instance :after ((slotd direct-gobject-slot-definition)
				       &rest initargs &key pname)
  (declare (ignore initargs))
  (when pname
    (setf
     (slot-value slotd 'pname)
     (signal-name-to-string (slot-definition-name slotd)))))

(defmethod direct-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-gobject-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-gobject-slot-definition))
    (t (call-next-method))))

(defmethod compute-virtual-slot-accessors
    ((class gobject-class) (slotd effective-gobject-slot-definition)
     direct-slotds)
  (with-slots (type) slotd
    (let ((pname (slot-definition-pname (first direct-slotds)))
	  (type-number (find-type-number type)))
      (list
       #'(lambda (object)
	   (with-gc-disabled
	     (let ((gvalue (gvalue-new type-number)))
	       (%object-get-property object pname gvalue)
	       (unwind-protect
		   (funcall
		    (intern-reader-function (type-from-number type-number)) gvalue +gvalue-value-offset+) ; temporary workaround for wrong topological sorting of types
		 (gvalue-free gvalue t)))))
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
	       value)))))))

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
    (if (eq 'boolean type) "-P" ""))))

(defun expand-gobject-type (type-number &optional options
			    (metaclass 'gobject-class))
  (let* ((supers (cons (supertype type-number) (implements type-number)))
	 (class  (type-from-number type-number))
	 (override-slots (getf options :slots))
	 (expanded-slots
	  (mapcar
	   #'(lambda (param)
	       (with-slots (name flags value-type documentation) param
	         (let* ((slot-name (default-slot-name name))
			(slot-type value-type) ;(type-from-number value-type t))
			(accessor
			 (default-slot-accessor class slot-name (type-from-number slot-type)))) ; temporary workaround for wrong topological sorting of types
		   `(,slot-name
		     :allocation :property
		     :pname ,name
		     ,@(cond
			((and
			  (member :writable flags)
			  (member :readable flags))
			 (list :accessor accessor))
			((member :writable flags)
			 (list :writer `(setf ,accessor)))
			((member :readable flags)
			 (list :reader accessor)))
		     ,@(when (or
			      (member :construct flags)
			      (member :writable flags))
			 (list :initarg (intern (string slot-name) "KEYWORD")))
		     :type ,slot-type
		     ,@(when documentation
			 (list :documentation documentation))))))
	   (query-object-class-properties type-number))))

    (dolist (slot-def override-slots)
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

