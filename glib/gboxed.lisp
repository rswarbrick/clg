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

;; $Id: gboxed.lisp,v 1.17 2005-03-06 17:26:23 espen Exp $

(in-package "GLIB")


(defclass boxed (struct)
  ()
  (:metaclass struct-class))

(defmethod instance-finalizer ((instance boxed))
  (let ((location (proxy-location instance))
	(type-number (type-number-of instance)))
    #'(lambda ()
	(remove-cached-instance location)
	(%boxed-free type-number location))))


;;;; Metaclass for boxed classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed-class (struct-class)
    ())

  (defmethod validate-superclass ((class boxed-class) (super standard-class))
    (subtypep (class-name super) 'boxed)))


(defmethod shared-initialize ((class boxed-class) names &key name gtype)
  (declare (ignore names))
  (call-next-method)
  (let ((class-name (or name (class-name class))))
    (unless (find-type-number class-name)
      (register-type class-name 
       (or (first gtype) (default-type-init-name class-name))))))

(defbinding %boxed-copy () pointer
  (type-number type-number)
  (location pointer))

(defbinding %boxed-free () nil
  (type-number type-number)
  (location pointer))

(defmethod reference-foreign ((class boxed-class) location)
  (%boxed-copy (find-type-number class) location))

(defmethod unreference-foreign ((class boxed-class) location)
  (%boxed-free (find-type-number class) location))


;;;; 

(defun expand-boxed-type (type-number forward-p slots)
  `(defclass ,(type-from-number type-number) (boxed)
     ,(unless forward-p
	slots)
     (:metaclass boxed-class)
     (:gtype ,(find-type-init-function type-number))))

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



;;;; NULL terminated vector of strings

(deftype strings () '(null-terminated-vector string))
(register-type 'strings '|g_strv_get_type|)
