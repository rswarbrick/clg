;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2001 Espen S. Johnsen <espen@users.sourceforge.net>
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

;; $Id: ginterface.lisp,v 1.3 2004-10-31 00:56:29 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;;;; 

(defclass ginterface ()
  ())

(deftype-method translate-type-spec ginterface (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'gobject))

(deftype-method size-of ginterface (type-spec)
  (declare (ignore type-spec))
  (size-of 'gobject))

(deftype-method translate-from-alien
    ginterface (type-spec location &optional weak-ref)
  (declare (ignore type-spec))
  (translate-from-alien 'gobject location weak-ref))

(deftype-method translate-to-alien
    ginterface (type-spec instance &optional weak-ref)
  (declare (ignore type-spec))
  (translate-to-alien 'gobject instance weak-ref))



;;;; Metaclass for interfaces

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginterface-class (virtual-slot-class)
    ()))

(defmethod direct-slot-definition-class ((class ginterface-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-property-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class ginterface-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-property-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class ginterface-class) direct-slotds)
  (if (eq (most-specific-slot-value direct-slotds 'allocation) :property)
      (nconc 
       (list :pname (signal-name-to-string 
		     (most-specific-slot-value direct-slotds 'pname))
	     :readable (most-specific-slot-value direct-slotds 'readable)
	     :writable (most-specific-slot-value direct-slotds 'writable)
	     :construct (most-specific-slot-value direct-slotds 'construct))
       (call-next-method))
    (call-next-method)))


(defmethod shared-initialize ((class ginterface-class) names
			      &rest initargs &key name alien-name)
  (declare (ignore initargs names))
  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (find-type-number
	   (or (first alien-name) (default-alien-type-name class-name)) t)))
    (register-type class-name type-number))
  (call-next-method))


(defmethod validate-superclass
    ((class ginterface-class) (super pcl::standard-class))
  (subtypep (class-name super) 'ginterface))


;;;;


(defbinding type-default-interface-ref (type) pointer
  ((find-type-number type t) type-number))

(defbinding type-default-interface-unref (type) nil
  ((find-type-number type t) type-number))

(defbinding type-default-interface-peek (type) pointer
  ((find-type-number type t) type-number))

(defbinding %object-interface-list-properties () pointer
  (iface pointer)
  (n-properties unsigned-int :out))

(defun query-object-interface-properties (type &optional inherited-p)
  (let* ((type-number (find-type-number type))
	 (iface (type-default-interface-ref type-number)))
    (unwind-protect
	 (multiple-value-bind (array length)
	     (%object-interface-list-properties iface)
	   (unwind-protect
		(%map-params array length type-number inherited-p)
	     (deallocate-memory array)))
;      (type-default-interface-unref type-number)
      )))


(defun expand-ginterface-type (type options &rest args)
  (declare (ignore args))
  (let ((class (type-from-number type))
	(slots (getf options :slots)))    
    `(defclass ,class (,(supertype type))
      ,(slot-definitions class (query-object-interface-properties type) slots)
      (:metaclass ginterface-class)
      (:alien-name ,(find-type-name type)))))


(register-derivable-type 'ginterface "GInterface" 'expand-ginterface-type)
