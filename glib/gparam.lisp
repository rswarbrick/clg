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

;; $Id: gparam.lisp,v 1.6 2002-03-19 17:01:42 espen Exp $

(in-package "GLIB")

(deftype gvalue () 'pointer)

(defconstant +gvalue-size+ (+ (size-of 'type-number) (* 2 (size-of 'double-float))))
(defconstant +gvalue-value-offset+ (size-of 'type-number))

(defbinding (gvalue-init "g_value_init") () nil
  (type type-number))

(defun gvalue-new (type)
  (let ((gvalue (allocate-memory +gvalue-size+)))
    (setf (system:sap-ref-32 gvalue 0) type)
;    (gvalue-init (type-number-of type))
    gvalue))

(defun gvalue-free (gvalue free-content)
  (unless (null-pointer-p gvalue)
    (when free-content
      (funcall
       (intern-destroy-function (gvalue-type gvalue))
       gvalue +gvalue-value-offset+))
    (deallocate-memory gvalue)))

(defun gvalue-type (gvalue)
  (type-from-number (system:sap-ref-32 gvalue 0)))

(defun gvalue-get (gvalue)
  (funcall
   (intern-reader-function (gvalue-type gvalue))
   gvalue +gvalue-value-offset+))

(defun gvalue-set (gvalue value)
  (funcall
   (intern-writer-function (gvalue-type gvalue))
   value gvalue +gvalue-value-offset+)
  value)


(deftype param-flag-type ()
  '(flags
    (:readable 1)
    (:writable 2)
    (:construct 4)
    (:construct-only 8)
    (:lax-validation 16)
    (:private 32)))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
    (:metaclass ginstance-class)
    (:ref "g_param_spec_ref")
    (:unref "g_param_spec_unref")))


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
  (:metaclass ginstance-class))

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
  (:metaclass ginstance-class)
  (:alien-name "GParamUChar"))

(defclass param-boolean (param)
  ((default-value
     :allocation :alien
     :reader param-boolean-default-value
     :type boolean))
  (:metaclass ginstance-class))

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
  (:metaclass ginstance-class))

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
  (:metaclass ginstance-class)
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
  (:metaclass ginstance-class))

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
  (:metaclass ginstance-class)
  (:alien-name "GParamULong"))

(defclass param-unichar (param)
  ()
  (:metaclass ginstance-class))

(defclass param-enum (param)
  ((class
    :allocation :alien
    :reader param-enum-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-enum-default-value
    :type long))
  (:metaclass ginstance-class))

(defclass param-flags (param)
  ((class
    :allocation :alien
    :reader param-flags-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-flags-default-value
    :type long))
  (:metaclass ginstance-class))

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
  (:metaclass ginstance-class)
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
  (:metaclass ginstance-class)
  (:alien-name "GParamDouble"))

(defclass param-string (param)
  ((default-value
    :allocation :alien
    :reader param-string-default-value
    :type string))
  (:metaclass ginstance-class))

(defclass param-param (param)
  ()
  (:metaclass ginstance-class))

(defclass param-boxed (param)
  ()
  (:metaclass ginstance-class))

(defclass param-pointer (param)
  ()
  (:metaclass ginstance-class))

(defclass param-value-array (param)
  ((element-spec
    :allocation :alien
    :reader param-value-array-element-spec
    :type param)
   (length
    :allocation :alien
    :reader param-value-array-length
    :type unsigned-int))
  (:metaclass ginstance-class))

;; (defclass param-closure (param)
;;   ()
;;   (:metaclass ginstance-class))

(defclass param-object (param)
  ()
  (:metaclass ginstance-class))



