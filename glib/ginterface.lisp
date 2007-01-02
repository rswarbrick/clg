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

;; $Id: ginterface.lisp,v 1.19 2007-01-02 16:06:15 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;;;; Superclass for interfaces

(defclass interface ()
  ())


;;;; Metaclass for interfaces

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass interface-class (virtual-slots-class)
    ()))

(defmethod direct-slot-definition-class ((class interface-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'direct-property-slot-definition))
    (:virtual (find-class 'direct-virtual-alien-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class ((class interface-class) &rest initargs)
  (case (getf initargs :allocation)
    (:property (find-class 'effective-property-slot-definition))
    (:virtual (find-class 'effective-virtual-alien-slot-definition))
    (t (call-next-method))))

(defmethod compute-effective-slot-definition-initargs ((class interface-class) direct-slotds)
  (if (eq (slot-definition-allocation (first direct-slotds)) :property)
      (nconc 
       (list :pname (signal-name-to-string 
		     (most-specific-slot-value direct-slotds 'pname))
	     :readable (most-specific-slot-value direct-slotds 'readable)
	     :writable (most-specific-slot-value direct-slotds 'writable)
	     :construct-only (most-specific-slot-value direct-slotds 'construct))
       (call-next-method))
    (call-next-method)))


(defmethod shared-initialize ((class interface-class) names &key name gtype)
  (declare (ignore names))
  (let* ((class-name (or name (class-name class)))	 
	 (type-number
	  (or
	   (find-type-number class-name)
	   (register-type class-name 
	    (or (first gtype) (default-type-init-name class-name))))))
    (type-default-interface-ref type-number))
  (call-next-method))


(defmethod validate-superclass ((class interface-class) (super standard-class))
  (subtypep (class-name super) 'interface))


(define-type-method alien-type ((type interface))
  (declare (ignore type))
  (alien-type 'gobject))

(define-type-method size-of ((type interface) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'gobject))

(define-type-method from-alien-form ((type interface) location &key (ref :copy))
  (declare (ignore type))
  (from-alien-form 'gobject location :ref ref))

(define-type-method from-alien-function ((type interface) &key (ref :copy))
  (declare (ignore type))
  (from-alien-function 'gobject :ref ref))

(define-type-method to-alien-form ((type interface) instance &optional copy-p)
  (declare (ignore type))
  (to-alien-form 'gobject instance copy-p))

(define-type-method to-alien-function ((type interface) &optional copy-p)
  (declare (ignore type))
  (to-alien-function 'gobject copy-p))

(define-type-method reader-function ((type interface) &key ref inlined)
  (assert-not-inlined type inlined)
  (reader-function 'gobject :ref ref))

(define-type-method writer-function ((type interface) &key temp inlined)
  (assert-not-inlined type inlined)
  (writer-function 'gobject :temp temp))

(define-type-method destroy-function ((type interface) &key temp inlined)
  (assert-not-inlined type inlined)
  (destroy-function 'gobject :temp temp))


;;;;


(defbinding type-default-interface-ref (type) pointer
  ((find-type-number type t) type-number))

(defbinding type-default-interface-unref () nil
  (iface pointer))

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
      (type-default-interface-unref iface))))


(defun expand-interface-type (type forward-p options &rest args)
  (declare (ignore args))
  (let ((class (type-from-number type))
	(slots (getf options :slots))) 
    `(defclass ,class (,(supertype type))
       ,(unless forward-p
	  (slot-definitions class (query-object-interface-properties type) slots))
      (:metaclass interface-class)
      (:gtype ,(register-type-as type)))))

(defun interface-dependencies (type options)
  (delete-duplicates 
   (cons
    (supertype type)
    (append
     (mapcar #'param-value-type (query-object-interface-properties type))
     (loop
      for slot in (getf options :slots)
      as type = (getf (rest slot) :type)
      when (and type (symbolp type) (find-type-number type))
      collect (find-type-number type))))))


(register-derivable-type 'interface "GInterface" 'expand-interface-type 'interface-dependencies)
