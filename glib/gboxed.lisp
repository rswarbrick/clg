;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gboxed.lisp,v 1.2 2001-04-30 11:25:25 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed (proxy)
    ()
    (:metaclass proxy-class)))

(defmethod initialize-proxy ((boxed boxed) &rest initargs
			     &key location weak-ref)
  (declare (ignore initargs))
  (setf
   (slot-value boxed 'location)
   (if weak-ref
       (%boxed-copy (find-type-number (class-of boxed)) location)
     location))
  (call-next-method))

(defmethod instance-finalizer ((boxed boxed))
  (let ((location (proxy-location boxed))
	(type-number (find-type-number (class-of boxed))))
    (declare (type system-area-pointer location))
    #'(lambda ()
	(%boxed-free type-number location)
	(remove-cached-instance location))))


(deftype-method translate-to-alien boxed (type-spec boxed &optional weak-ref)
  (if weak-ref
      `(proxy-location ,boxed)
    `(let ((boxed ,boxed))
       (%boxed-copy
	(find-type-number type-spec)
	(proxy-location boxed)))))

(deftype-method unreference-alien boxed (type-spec c-struct)
  `(%boxed-free ,(find-type-number type-spec) ,c-struct))


(defbinding %boxed-copy () pointer
  (type type-number)
  (location pointer))

(defbinding %boxed-free () nil
  (type type-number)
  (location pointer))


;;;; Metaclass for boxed classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed-class (proxy-class)))


(defmethod shared-initialize ((class boxed-class) names
			      &rest initargs
			      &key name alien-name type-init)
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
	   (type-init (funcall (mkbinding (first type-init) 'type-number)))
	   (t
	    (or
	     (type-number-from-alien-name
	      (default-alien-type-name class-name) nil)
	     (funcall
	      (mkbinding
	       (default-alien-fname (format nil "~A_get_type" class-name))
	       'type-number)))))))
    (setf (find-type-number class) type-number)))


(defmethod validate-superclass
    ((class boxed-class) (super pcl::standard-class))
  (subtypep (class-name super) 'boxed))


;;;; Initializing type numbers

(setf (alien-type-name 'boxed) "GBoxed")
