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

;; $Id: gobject.lisp,v 1.6 2001-04-29 20:34:18 espen Exp $

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

(defbinding ("g_object_new" %gobject-new) () gobject
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

  (defclass direct-gobject-slot-definition (direct-virtual-slot-definition))

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
	  (reader (intern-reader-function type))
	  (writer (intern-writer-function type))
	  (destroy (intern-destroy-function type)))
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
    