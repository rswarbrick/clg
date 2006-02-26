;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2001-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: ginterface.lisp,v 1.15 2006-02-26 15:30:01 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;;;; 

(defclass ginterface ()
  ())

;;;; Metaclass for interfaces

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginterface-class (virtual-slots-class)
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
  (if (eq (slot-definition-allocation (first direct-slotds)) :property)
      (nconc 
       (list :pname (signal-name-to-string 
		     (most-specific-slot-value direct-slotds 'pname))
	     :readable (most-specific-slot-value direct-slotds 'readable)
	     :writable (most-specific-slot-value direct-slotds 'writable)
	     :construct-only (most-specific-slot-value direct-slotds 'construct))
       (call-next-method))
    (call-next-method)))


(defmethod shared-initialize ((class ginterface-class) names &key name gtype)
  (declare (ignore names))
  (let* ((class-name (or name (class-name class)))
;; 	 (type-number
;; 	  (or
;; 	   (find-type-number class-name)
;; 	   (register-type class-name 
;; 	    (or (first gtype) (default-type-init-name class-name)))))
	 )
;    (type-default-interface-ref type-number)
    )
  (call-next-method))


(defmethod validate-superclass ((class ginterface-class) (super standard-class))
  (subtypep (class-name super) 'ginterface))


(define-type-method alien-type ((type ginterface))
  (declare (ignore type))
  (alien-type 'gobject))

(define-type-method size-of ((type ginterface))
  (declare (ignore type))
  (size-of 'gobject))

(define-type-method from-alien-form ((type ginterface) location)
  (declare (ignore type))
  (from-alien-form 'gobject location))

(define-type-method from-alien-function ((type ginterface))
  (declare (ignore type))
  (from-alien-function 'gobject))

(define-type-method to-alien-form ((type ginterface) instance)
  (declare (ignore type))
  (to-alien-form 'gobject instance))

(define-type-method to-alien-function ((type ginterface))
  (declare (ignore type))
  (to-alien-function 'gobject))

(define-type-method reader-function ((type ginterface))
  (declare (ignore type))
  (reader-function 'gobject))

(define-type-method writer-function ((type ginterface))
  (declare (ignore type))
  (writer-function 'gobject))

(define-type-method destroy-function ((type ginterface))
  (declare (ignore type))
  (destroy-function 'gobject))


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
	   (unless (null-pointer-p array)
	     (unwind-protect
		 (%map-params array length type-number inherited-p)
	       (deallocate-memory array))))
;      (type-default-interface-unref type-number)
      )))


(defun expand-ginterface-type (type forward-p options &rest args)
  (declare (ignore args))
  (let ((class (type-from-number type))
	(slots (getf options :slots))) 
    `(defclass ,class (,(supertype type))
       ,(unless forward-p
	  (slot-definitions class (query-object-interface-properties type) slots))
      (:metaclass ginterface-class)
      (:gtype ,(register-type-as type)))))

(defun ginterface-dependencies (type)
  (delete-duplicates 
   (cons
    (supertype type)
    (mapcar #'param-value-type (query-object-interface-properties type)))))

(register-derivable-type 'ginterface "GInterface" 'expand-ginterface-type 'ginterface-dependencies)
