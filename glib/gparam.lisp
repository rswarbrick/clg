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

;; $Id: gparam.lisp,v 1.10 2004-11-12 15:01:42 espen Exp $

(in-package "GLIB")

(deftype gvalue () 'pointer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defbinding (size-of-gvalue "size_of_gvalue") () unsigned-int))

;(defconstant +gvalue-size+ (+ (size-of 'type-number) (* 2 (size-of 'double-float))))
(defconstant +gvalue-size+ #.(size-of-gvalue))

(defconstant +gvalue-value-offset+ (size-of 'type-number))

(defbinding (%gvalue-init "g_value_init") () nil
  (value gvalue)
  (type type-number))

(defbinding (gvalue-unset "g_value_unset") () nil
  (value gvalue))

(defun gvalue-init (gvalue type &optional (value nil value-p))
  (%gvalue-init gvalue (find-type-number type))
  (when value-p
    (funcall (writer-function type) value gvalue +gvalue-value-offset+)))

(defun gvalue-new (type &optional (value nil value-p))
  (let ((gvalue (allocate-memory +gvalue-size+)))
    (if value-p
	(gvalue-init gvalue type value)
      (gvalue-init gvalue type))
    gvalue))

(defun gvalue-free (gvalue &optional (unset-p t))
  (unless (null-pointer-p gvalue)
    (when unset-p
      (gvalue-unset gvalue))
    (deallocate-memory gvalue)))

(defun gvalue-type (gvalue)
  (type-from-number (system:sap-ref-32 gvalue 0)))

(defun gvalue-get (gvalue)
  (funcall (reader-function (gvalue-type gvalue))
   gvalue +gvalue-value-offset+))

(defun gvalue-set (gvalue value)
  (funcall (writer-function (gvalue-type gvalue))
   value gvalue +gvalue-value-offset+)
  value)

(defmacro with-gvalue ((gvalue type &optional (value nil value-p)) &body body)
  `(let ((,gvalue ,(if value-p
		       `(gvalue-new ',type ,value)
		     `(gvalue-new ',type ,value))))
    (unwind-protect
	 (progn
	   ,@body
	   ,(unless value-p `(gvalue-get ,gvalue)))
      (gvalue-free ,gvalue))))


(deftype param-flag-type ()
  '(flags
    (:readable 1)
    (:writable 2)
    (:construct 4)
    (:construct-only 8)
    (:lax-validation 16)
    (:private 32)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass param-spec-class (ginstance-class)
    ())

  (defmethod validate-superclass 
      ((class param-spec-class) (super pcl::standard-class))
    t ;(subtypep (class-name super) 'param)
))


(defbinding %param-spec-ref () pointer
  (location pointer))
  
(defbinding %param-spec-unref () nil
  (location pointer))

(defmethod reference-foreign ((class param-spec-class) location)
  (declare (ignore class))
  (%param-spec-ref location))

(defmethod unreference-foreign ((class param-spec-class) location)
  (declare (ignore class))
  (%param-spec-unref location))



;; TODO: rename to param-spec
(defclass param (ginstance)
  ((name
    :allocation :alien
    :reader param-name
    :type string)
   (flags
    :allocation :alien
    :reader param-flags
    :type param-flag-type)
   (value-type
    :allocation :alien
    :reader param-value-type
    :type type-number)
   (owner-type
    :allocation :alien
    :reader param-owner-type
    :type type-number)
   (nickname
    :allocation :virtual
    :getter "g_param_spec_get_nick"
    :reader param-nickname
    :type string)
   (documentation
    :allocation :virtual
    :getter "g_param_spec_get_blurb"
    :reader param-documentation
    :type string))
  (:metaclass param-spec-class))


(defclass param-char (param)
  ((minimum
    :allocation :alien
    :reader param-char-minimum
    :type char)
   (maximum
    :allocation :alien
    :reader param-char-maximum
    :type char)
   (default-value
    :allocation :alien
    :reader param-char-default-value
    :type char))
  (:metaclass param-spec-class))

(defclass param-unsigned-char (param)
  (
; (minimum
;     :allocation :alien
;     :reader param-unsigned-char-minimum
;     :type unsigned-char)
;    (maximum
;     :allocation :alien
;     :reader param-unsigned-char-maximum
;     :type unsigned-char)
;    (default-value
;     :allocation :alien
;     :reader param-unsigned-char-default-value
;     :type unsigned-char)
   )
  (:metaclass param-spec-class)
  (:alien-name "GParamUChar"))

(defclass param-boolean (param)
  ((default-value
     :allocation :alien
     :reader param-boolean-default-value
     :type boolean))
  (:metaclass param-spec-class))

(defclass param-int (param)
  ((minimum
    :allocation :alien
    :reader param-int-minimum
    :type int)
   (maximum
    :allocation :alien
    :reader param-int-maximum
    :type int)
   (default-value
    :allocation :alien
    :reader param-int-default-value
    :type int))
  (:metaclass param-spec-class))

(defclass param-unsigned-int (param)
  ((minimum
    :allocation :alien
    :reader param-unsigned-int-minimum
    :type unsigned-int)
   (maximum
    :allocation :alien
    :reader param-unsigned-int-maximum
    :type unsigned-int)
   (default-value
    :allocation :alien
    :reader param-unsigned-int-default-value
    :type unsigned-int))
  (:metaclass param-spec-class)
  (:alien-name "GParamUInt"))

(defclass param-long (param)
  ((minimum
    :allocation :alien
    :reader param-long-minimum
    :type long)
   (maximum
    :allocation :alien
    :reader param-long-maximum
    :type long)
   (default-value
    :allocation :alien
    :reader param-long-default-value
    :type long))
  (:metaclass param-spec-class))

(defclass param-unsigned-long (param)
  ((minimum
    :allocation :alien
    :reader param-unsigned-long-minimum
    :type unsigned-long)
   (maximum
    :allocation :alien
    :reader param-unsigned-long-maximum
    :type unsigned-long)
   (default-value
    :allocation :alien
    :reader param-unsigned-long-default-value
    :type unsigned-long))
  (:metaclass param-spec-class)
  (:alien-name "GParamULong"))

(defclass param-unichar (param)
  ()
  (:metaclass param-spec-class))

(defclass param-enum (param)
  ((class
    :allocation :alien
    :reader param-enum-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-enum-default-value
    :type long))
  (:metaclass param-spec-class))

(defclass param-flags (param)
  ((class
    :allocation :alien
    :reader param-flags-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-flags-default-value
    :type long))
  (:metaclass param-spec-class))

(defclass param-single-float (param)
  ((minimum
    :allocation :alien
    :reader param-single-float-minimum
    :type single-float)
   (maximum
    :allocation :alien
    :reader param-single-float-maximum
    :type single-float)
   (default-value
    :allocation :alien
    :reader param-single-float-default-value
    :type single-float)
   (epsilon
    :allocation :alien
    :reader param-single-float-epsilon
    :type single-float))
  (:metaclass param-spec-class)
  (:alien-name "GParamFloat"))

(defclass param-double-float (param)
  ((minimum
    :allocation :alien
    :reader param-double-float-minimum
    :type double-float)
   (maximum
    :allocation :alien
    :reader param-double-float-maximum
    :type double-float)
   (default-value
    :allocation :alien
    :reader param-double-float-default-value
    :type double-float)
   (epsilon
    :allocation :alien
    :reader param-double-float-epsilon
    :type double-float))
  (:metaclass param-spec-class)
  (:alien-name "GParamDouble"))

(defclass param-string (param)
  ((default-value
    :allocation :alien
    :reader param-string-default-value
    :type string))
  (:metaclass param-spec-class))

(defclass param-param (param)
  ()
  (:metaclass param-spec-class))

(defclass param-boxed (param)
  ()
  (:metaclass param-spec-class))

(defclass param-pointer (param)
  ()
  (:metaclass param-spec-class))

(defclass param-value-array (param)
  ((element-spec
    :allocation :alien
    :reader param-value-array-element-spec
    :type param)
   (length
    :allocation :alien
    :reader param-value-array-length
    :type unsigned-int))
  (:metaclass param-spec-class))

;; (defclass param-closure (param)
;;   ()
;;   (:metaclass param-spec-class))

(defclass param-object (param)
  ()
  (:metaclass param-spec-class))
