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

;; $Id: ginterface.lisp,v 1.2 2004-10-27 14:58:59 espen Exp $

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

(defun expand-ginterface-type (type-number options &rest args)
  (declare (ignore args))
  `(defclass ,(type-from-number type-number) (ginterface)
     ,(getf options :slots)
     (:metaclass ginterface-class)
     (:alien-name ,(find-type-name type-number))))


(register-derivable-type 'ginterface "GInterface" 'expand-ginterface-type)
