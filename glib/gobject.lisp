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

;; $Id: gobject.lisp,v 1.4 2001-01-28 14:17:12 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass ginstance-class)
    (:alien-name "GObject")))


;;;; Object construction

(define-foreign ("g_object_new" %gobject-new) () gobject
  (type type-number)
  (nil null))


;;;; Reference counting for gobject

;; Specializing reference-instance and unreference-instance on gobject
;; is not really necessary but done for efficiency

(defmethod reference-instance ((object gobject))
  (%object-ref object)
  object)

(defmethod unreference-instance ((object gobject))
  (%object-unref object))

(deftype-method alien-ref gobject (type-spec)
  (declare (ignore type-spec))
  '%object-ref)

(deftype-method alien-unref gobject (type-spec)
  (declare (ignore type-spec))
  '%object-unref)

(define-foreign %object-ref () pointer
  (object (or gobject pointer)))

(define-foreign %object-unref () nil
  (object (or gobject pointer)))


;;;; Parameter stuff

(define-foreign %object-set-property () nil
  (object gobject)
  (name string)
  (value gvalue))

(define-foreign %object-get-property () nil
  (object gobject)
  (name string)
  (value gvalue))

(define-foreign %object-notify () nil
  (object gobject)
  (name string))

(define-foreign object-freeze-notify () nil
  (object gobject))

(define-foreign object-thaw-notify () nil
  (object gobject))

(define-foreign %object-set-qdata-full () nil
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

(define-foreign %object-get-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun object-data (object key &key (test #'eq))
  (find-user-data
   (%object-get-qdata object (quark-from-object key :test test))))



;;;; Metaclass used for subclasses of gobject

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject-class (ginstance-class))

  (defclass direct-gobject-slot-definition (direct-virtual-slot-definition))

  (defclass effective-gobject-slot-definition
    (effective-virtual-slot-definition)))

(defmethod allocate-alien-storage ((class gobject-class))
  (alien-instance-location (%gobject-new (find-type-number class))))

(defmethod shared-initialize ((class gobject-class) names &rest initargs
			      &key type-init name)
  (declare (ignore initargs names))
  (let ((alien
	 (alien::%heap-alien
	  (alien::make-heap-alien-info
	   :type (alien::parse-alien-type '(function (unsigned 32)))
	   :sap-form (system:foreign-symbol-address
		      (or
		       (first type-init)
		       (default-alien-func-name
			 (format
			  nil "~A_get_type" (or name (class-name class))))))))))
    (alien:alien-funcall alien))
  (call-next-method))


; (define-foreign object-class-install-param () nil
;   (class pointer)
;   (id unsigned-int)
;   (parameter parameter))

; (define-foreign object-class-find-param-spec () parameter
;   (class pointer)
;   (name string))


(defmethod initialize-instance :after ((slotd direct-gobject-slot-definition)
				       &rest initargs &key)
  (declare (ignore initargs))
  (unless (slot-boundp slotd 'location)
    ;; Find parameter name from slot name
    (with-slots (pcl::name location) slotd
      (setf location (signal-name-to-string pcl::name)))))

(defmethod direct-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:param (find-class 'direct-gobject-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class gobject-class) initargs)
  (case (getf initargs :allocation)
    (:param (find-class 'effective-gobject-slot-definition))
    (t (call-next-method))))

(defmethod compute-virtual-slot-location
    ((class gobject-class) (slotd effective-gobject-slot-definition)
     direct-slotds)
  (with-slots (type) slotd
    (let ((param-name (slot-definition-location (first direct-slotds)))
	  (type-number (find-type-number type))
	  (reader (get-reader-function type))
	  (writer (get-writer-function type))
	  (destroy (get-destroy-function type)))
      (list
       #'(lambda (object)
	   (with-gc-disabled
	     (let ((gvalue (gvalue-new type-number)))
	       (%object-get-property object param-name gvalue)
	       (prog1
		   (funcall reader gvalue +gvalue-value-offset+)
		 (gvalue-free gvalue t)))))
       #'(lambda (value object)
	   (with-gc-disabled
  	     (let ((gvalue (gvalue-new type-number)))
	       (funcall writer value gvalue +gvalue-value-offset+)
	       (%object-set-property object param-name gvalue)
	       (funcall destroy gvalue +gvalue-value-offset+)
	       (gvalue-free gvalue nil)
	       value)))))))


(defmethod validate-superclass ((class gobject-class)
				(super pcl::standard-class))
  (subtypep (class-name super) 'gobject))
