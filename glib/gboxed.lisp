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

;; $Id: gboxed.lisp,v 1.11 2004-11-06 21:39:58 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string
			  (pkg-config:pkg-variable "glib-2.0" "libdir")
			  "/libgobject-2.0.so")))

(defclass boxed (proxy)
  ()
  (:metaclass struct-class))


;;;; Metaclass for boxed classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed-class (struct-class)
    ())

  (defmethod validate-superclass ((class boxed-class) (super standard-class))
    (subtypep (class-name super) 'boxed)))


(defmethod shared-initialize ((class boxed-class) names
			      &rest initargs &key name alien-name)
  (declare (ignore initargs names))
  (call-next-method)
  
  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (find-type-number
	   (or (first alien-name) (default-alien-type-name class-name)))))
    (register-type class-name type-number)))


(defbinding %boxed-copy (type location) pointer
  ((find-type-number type) type-number)
  (location pointer))

(defbinding %boxed-free (type location) nil
  ((find-type-number type) type-number)
  (location pointer))

(defmethod reference-foreign ((class boxed-class) location)
  (%boxed-copy (class-name class) location))

(defmethod unreference-foreign ((class boxed-class) location)
  (%boxed-free (class-name class) location))


;;;; 

(defun expand-boxed-type (type-number &optional slots)
  `(defclass ,(type-from-number type-number) (boxed)
     ,slots
     (:metaclass boxed-class)
     (:alien-name ,(find-type-name type-number))))

(register-derivable-type 'boxed "GBoxed" 'expand-boxed-type)

;;;; Special boxed types

;; (defclass gstring (boxed)
;;   ()
;;   (:metaclass boxed-class)
;;   (:alien-name "GString"))

;; (deftype-method translate-from-alien
;;     gstring (type-spec location &optional weak-ref)
;;   `(let ((location ,location))
;;      (unless (null-pointer-p location)
;;        (prog1
;; 	   (c-call::%naturalize-c-string location)
;; 	 ,(unless weak-ref
;; 	    (unreference-alien type-spec location))))))

;; (deftype-method translate-to-alien
;;     gstring (type-spec string &optional weak-ref)
;;   (declare (ignore weak-ref))
;;   `(let ((string ,string))
;;      ;; Always copy strings to prevent seg fault due to GC
;;      (funcall
;;       ',(proxy-class-copy (find-class type-spec))
;;       ',type-spec
;;       (make-pointer (1+ (kernel:get-lisp-obj-address string))))))

;; (deftype-method cleanup-alien gstring (type-spec c-string &optional weak-ref)
;;   (when weak-ref
;;     (unreference-alien type-spec c-string)))

