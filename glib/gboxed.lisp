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

;; $Id: gboxed.lisp,v 1.3 2001-05-11 16:04:33 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed (proxy)
    ()
    (:metaclass proxy-class)
    (:copy %boxed-copy)
    (:free %boxed-free)))

(defbinding %boxed-copy (type location) pointer
  ((find-type-number type) type-number)
  (location pointer))

(defbinding %boxed-free (type location) nil
  ((find-type-number type) type-number)
  (location pointer))


;;;; Metaclass for boxed classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed-class (proxy-class)))


(defmethod shared-initialize ((class boxed-class) names
			      &rest initargs &key name alien-name)
  (declare (ignore initargs names))
  (call-next-method)

  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (find-type-number
	   (or (first alien-name) (default-alien-type-name class-name)))))
    (register-type class-name type-number)))


(defmethod validate-superclass
    ((class boxed-class) (super pcl::standard-class))
  (subtypep (class-name super) 'boxed))


;;;; 

(defun expand-boxed-type (type-number &optional slots)
  `(defclass ,(type-from-number type-number) (boxed)
     ,slots
     (:metaclass boxed-class)
     (:alien-name ,(find-type-name type-number))))

(register-derivable-type 'boxed "GBoxed" :expand 'expand-boxed-type)
