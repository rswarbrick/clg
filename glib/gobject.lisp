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

;; $Id: gobject.lisp,v 1.3 2000-11-09 20:29:19 espen Exp $

(in-package "GLIB")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass gobject (ginstance)
    ()
    (:metaclass ginstance-class)
    (:alien-name "GObject"))

  (defclass gobject-class (ginstance-class)))


;;;; Reference counting for gobject

;; Specializing reference-instance and unreference-instance on gobject
;; is not really necessary but done for efficiency

(defmethod reference-instance ((object gobject))
  (%object-ref object)
  object)

(defmethod unreference-instance ((object gobject))
  (%object-unref object))

(deftype-method alien-ref gobject (type-spec)
  (declare (ignore type-spec))
  '%object-ref)

(deftype-method alien-unref gobject (type-spec)
  (declare (ignore type-spec))
  '%object-unref)

(define-foreign %object-ref () pointer
  (object (or gobject pointer)))

(define-foreign %object-unref () nil
  (object (or gobject pointer)))


;;;; Parameter stuff

(define-foreign %object-set-param () nil
  (object gobject)
  (name string)
  (value gvalue))

(define-foreign %object-get-param () nil
  (object gobject)
  (name string)
  (value gvalue :out))

(define-foreign object-queue-param-changed () nil
  (object gobject)
  (name string))




(define-foreign %object-set-qdata-full () nil
  (object gobject)
  (id quark)
  (data unsigned-long)
  (destroy-marshal pointer))

(defun (setf object-data) (data object key &key (test #'eq))
  (%object-set-qdata-full
   object (quark-from-object key :test test)
   (register-user-data data) *destroy-notify*)
  data)

(define-foreign %object-get-qdata () unsigned-long
  (object gobject)		 
  (id quark))

(defun object-data (object key &key (test #'eq))
  (find-user-data
   (%object-get-qdata object (quark-from-object key :test test))))






;;;; Methods for gobject-class

(defmethod shared-initialize ((class gobject-class) names &rest initargs
			      &key type-init name)
  (declare (ignore initargs names))
  (let ((alien
	 (alien::%heap-alien
	  (alien::make-heap-alien-info
	   :type (alien::parse-alien-type '(function (unsigned 32)))
	   :sap-form (system:foreign-symbol-address
		      (or
		       (first type-init)
		       (default-alien-func-name
			 (format
			  nil "~A_get_type" (or name (class-name class))))))))))
    (alien:alien-funcall alien))
  (call-next-method))


; (define-foreign object-class-install-param () nil
;   (class pointer)
;   (id unsigned-int)
;   (parameter parameter))

; (define-foreign object-class-find-param-spec () parameter
;   (class pointer)
;   (name string))

