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

;; $Id: gobject.lisp,v 1.7 2001-05-11 16:08:08 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass ginstance-class)
    (:alien-name "GObject")
    (:ref "g_object_ref")
    (:unref "g_object_unref")))

(defmethod initialize-instance ((object gobject) &rest initargs)
  (declare (ignore initargs))
  (setf 
   (slot-value object 'location)
   (%gobject-new (type-number-of object)))
  (call-next-method))

(defbinding (%gobject-new "g_object_new") () pointer
  (type type-number)
  (nil null))



;;;; Parameter stuff

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
    ((param :reader slot-definition-param)))

  (defclass effective-gobject-slot-definition
    (effective-virtual-slot-definition)))


; (defbinding object-class-install-param () nil
;   (class pointer)
;   (id unsigned-int)
;   (parameter parameter))

; (defbinding object-class-find-param-spec () parameter
;   (class pointer)
;   (name string))


(defmethod initialize-instance :after ((slotd direct-gobject-slot-definition)
				       &rest initargs &key param)
  (declare (ignore initargs))
  (when param
    (setf
     (slot-value slotd 'param)
     (signal-name-to-string (slot-definition-name slotd)))))

(defmethod direct-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:param (find-class 'direct-gobject-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:param (find-class 'effective-gobject-slot-definition))
    (t (call-next-method))))

(defmethod compute-virtual-slot-accessors
    ((class gobject-class) (slotd effective-gobject-slot-definition)
     direct-slotds)
  (with-slots (type) slotd
    (let ((param-name (slot-definition-param (first direct-slotds)))
	  (type-number (find-type-number type))
	  (getter (intern-reader-function type))
	  (setter (intern-writer-function type))
	  (destroy (intern-destroy-function type)))
      (list
       #'(lambda (object)
	   (with-gc-disabled
	     (let ((gvalue (gvalue-new type-number)))
	       (%object-get-property object param-name gvalue)
	       (prog1
		   (funcall getter gvalue +gvalue-value-offset+)
		 (gvalue-free gvalue t)))))
       #'(lambda (value object)
	   (with-gc-disabled
  	     (let ((gvalue (gvalue-new type-number)))
	       (funcall setter value gvalue +gvalue-value-offset+)
	       (%object-set-property object param-name gvalue)
	       (funcall destroy gvalue +gvalue-value-offset+)
	       (gvalue-free gvalue nil)
	       value)))))))

(defmethod validate-superclass ((class gobject-class)
				(super pcl::standard-class))
  (subtypep (class-name super) 'gobject))



;;;;

(defbinding %object-class-properties () pointer
  (class pointer)
  (n-properties unsigned-int :out))

(defun query-object-class-properties (type)
  (let ((class (type-class-ref type)))
    (multiple-value-bind (array length)
	(%object-class-properties class)
      (map-c-array 'list #'identity array 'param length))))

(defun query-object-class-dependencies (class)
  (delete-duplicates
   (reduce
    #'nconc
    (mapcar
     #'(lambda (param)
	 ;; A gobject does not depend on it's supertypes due to forward
	 ;; referenced superclasses
	 (delete-if
	  #'(lambda (type)
	      (type-is-p class type))
	  (type-hierarchy (param-type param))))
     (query-object-class-properties class)))))


(defun default-slot-name (name)
  (intern (substitute #\- #\_ (string-upcase (string-upcase name)))))

(defun default-slot-accessor (class-name slot-name type)
  (intern
   (format
    nil "~A-~A~A" class-name slot-name
    (if (eq 'boolean type) "-p" ""))))

(defun expand-gobject-type (type-number &optional slots)
  (let* ((super (supertype type-number))
	 (class  (type-from-number type-number))
	 (expanded-slots
	  (mapcar
	   #'(lambda (param)
	       (with-slots (name flags type documentation) param
	         (let* ((slot-name (default-slot-name name))
			(slot-type (type-from-number type))
			(accessor
			 (default-slot-accessor class slot-name slot-type)))
		   `(,slot-name
		     :allocation :param
		     :param ,name
		     ,@(when (member :writable flags)
			 (list :writer `(setf ,accessor)))
		     ,@(when (member :readable flags)
			 (list :reader accessor))
		     ,@(when (member :construct flags)
			 (list :initarg (intern (string slot-name) "KEYWORD")))
		     :type ,slot-type
		     ,@(when documentation
			 (list :documentation documentation))))))
	   (query-object-class-properties type-number))))

    `(defclass ,class (,super)
       ,expanded-slots
       (:metaclass gobject-class)
       (:alien-name ,(find-type-name type-number)))))
    
(register-derivable-type
 'gobject "GObject"
 :query 'query-object-class-dependencies
 :expand 'expand-gobject-type)

