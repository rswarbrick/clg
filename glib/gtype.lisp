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

;; $Id: gtype.lisp,v 1.8 2001-04-29 20:17:07 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; 

(deftype type-number () '(unsigned 32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass type-query (alien-structure)
    ((type-number :allocation :alien :type type-number)
     (name :allocation :alien :type string)
     (class-size :allocation :alien :type unsigned-int)
     (instance-size :allocation :alien :type unsigned-int))
    (:metaclass proxy-class)))


(defbinding ("g_type_name" alien-type-name) (type) (static string)
  ((find-type-number type) type-number))

(defbinding %type-from-name () type-number
  (name string))

(defbinding type-parent () type-number
  (type type-number))

(defbinding %type-query () nil
  (type type-number)
  (query type-query))

(defun type-query (type)
  (let ((query (make-instance 'type-query)))
    (%type-query (find-type-number type) query)
    query))

(defun type-instance-size (type)
  (slot-value (type-query type) 'instance-size))

(defun type-class-size (type)
  (slot-value (type-query type) 'class-size))

(defbinding type-class-ref () pointer
  (type type-number))

(defbinding type-class-unref () nil
  (type type-number))

(defbinding type-class-peek () pointer
  (type type-number))

(defbinding type-create-instance (type) pointer
  ((find-type-number type) type-number))

(defbinding type-free-instance () nil
  (instance pointer))


(defvar *type-to-number-hash* (make-hash-table))
(defvar *number-to-type-hash* (make-hash-table))

(defun type-number-from-alien-name (name &optional (error t))
  (if (string= name "invalid")
      0
    (let ((type-number (%type-from-name name)))
      (cond
       ((and (zerop type-number) error)
	(error "Invalid alien type name: ~A" name))
       ((zerop type-number) nil)
       (t type-number)))))

(defun (setf alien-type-name) (alien-name type)
  (let ((type-name (ensure-type-name type))
	(type-number (type-number-from-alien-name alien-name)))
    (setf (gethash type-number *number-to-type-hash*) type-name)
    (setf (gethash type-name *type-to-number-hash*) type-number)))

(defun (setf find-type-number) (type-number type)
  (setf (gethash (ensure-type-name type) *type-to-number-hash*) type-number))

(defun find-type-number (type)
  (etypecase type
    (integer type)
    (symbol (gethash type *type-to-number-hash*))
    (pcl::class (gethash (class-name type) *type-to-number-hash*))))
 
(defun type-from-number (type-number)
  (gethash type-number *number-to-type-hash*))

(defun type-number-of (object)
  (find-type-number (type-of object)))

(defun alien-function (name return-type &rest arg-types)
  (let ((alien
	 (alien::%heap-alien
	  (alien::make-heap-alien-info
	   :type (alien::parse-alien-type
		  `(function ,@(cons return-type arg-types)))
	   :sap-form (system:foreign-symbol-address name)))))
    #'(lambda (&rest args)
	(apply #'alien:alien-funcall alien args))))


(defun type-init (name &optional init-fname)
  (funcall
   (alien-function
    (or
     init-fname
     (default-alien-fname (format nil "~A_get_type" name)))
    '(unsigned 32))))


;;;; Superclass for wrapping types in the glib type system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance (proxy)
    ()
    (:metaclass proxy-class)
    (:size 4 #|(size-of 'pointer|#)))

(defmethod initialize-proxy ((instance ginstance) &rest initargs &key location)
  (declare (ignore initargs))
  (setf 
   (slot-value instance 'location)
   (funcall (ginstance-class-ref (class-of instance)) location))
  (call-next-method))

(defmethod instance-finalizer ((instance ginstance))
  (let ((location (proxy-location instance))
	(unref (ginstance-class-unref (class-of instance))))
    (declare (type system-area-pointer location))
    #'(lambda ()
	(funcall unref location)
	(remove-cached-instance location))))

(defun %type-of-ginstance (location)
  (let ((class (sap-ref-sap location 0)))
    (type-from-number (sap-ref-unsigned class 0))))

(deftype-method translate-from-alien
    ginstance (type-spec location &optional weak-ref)
  (declare (ignore type-spec))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-proxy-instance
	(%type-of-ginstance location) location ,weak-ref))))

(deftype-method translate-to-alien
    ginstance (type-spec object &optional weak-ref)
  (declare (ignore type-spec))
  (if weak-ref
      `(proxy-location ,object)
    `(let ((object ,object))
       (funcall
	(ginstance-class-ref (class-of object)) (proxy-location object)))))

(deftype-method unreference-alien ginstance (type-spec location)
  (declare (ignore type-spec))
  `(let* ((location ,location)
	  (class (find-class (%type-of-ginstance location))))
     (funcall (ginstance-class-unref class) location)))



;;;; Metaclass for subclasses of ginstance

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance-class (proxy-class)
    ((ref :reader ginstance-class-ref)
     (unref :reader ginstance-class-unref))))


(defmethod shared-initialize ((class ginstance-class) names
			      &rest initargs
			      &key name alien-name size
			      ref unref type-init)
  (declare (ignore initargs names))
  (call-next-method)

  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (cond
	   ((and alien-name type-init)
	    (error
	     "Specify either :type-init or :alien-name for class ~A"
	     class-name))
	   (alien-name (type-number-from-alien-name (first alien-name)))
	   (type-init (type-init class-name (first type-init)))
	   (t
	    (or
	     (type-number-from-alien-name
	      (default-alien-type-name class-name) nil)
	     (type-init class-name))))))
    (setf (find-type-number class) type-number)
    (unless size
      (setf
       (slot-value class 'size)
       (type-instance-size (find-type-number class-name))))
    (when ref
      (setf
       (slot-value class 'ref)
       (alien-function (first ref) 'system-area-pointer 'system-area-pointer)))
    (when unref
      (setf
       (slot-value class 'unref)
       (alien-function (first unref) 'void 'system-area-pointer)))))

(defmethod shared-initialize :after ((class ginstance-class) names
				     &rest initargs)
  (declare (ignore names initargs))
  (unless (slot-boundp class 'ref)
    (setf
     (slot-value class 'ref)
     (ginstance-class-ref (most-specific-proxy-superclass class))))
  (unless (slot-boundp class 'unref)
    (setf
     (slot-value class 'unref)
     (ginstance-class-unref (most-specific-proxy-superclass class)))))


(defmethod validate-superclass
    ((class ginstance-class) (super pcl::standard-class))
  (subtypep (class-name super) 'ginstance))


;;;; Initializing type numbers

(setf (alien-type-name 'invalid) "invalid")
(setf (alien-type-name 'char) "gchar")
(setf (alien-type-name 'unsigned-char) "guchar")
(setf (alien-type-name 'boolean) "gboolean")
(setf (alien-type-name 'int) "gint")
(setf (alien-type-name 'unsigned-int) "guint")
(setf (alien-type-name 'long) "glong")
(setf (alien-type-name 'unsigned-long) "gulong")
(setf (alien-type-name 'single-float) "gfloat")
(setf (alien-type-name 'double-float) "gdouble")
(setf (alien-type-name 'string) "GString")
(setf (find-type-number 'fixnum) (find-type-number 'int))
