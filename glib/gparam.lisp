;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gparam.lisp,v 1.20 2006-02-15 09:55:50 espen Exp $

(in-package "GLIB")

(deftype gvalue () 'pointer)

(register-type 'gvalue '|g_value_get_type|)

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

(defun gvalue-new (&optional type (value nil value-p))
  (let ((gvalue (allocate-memory +gvalue-size+)))
    (cond
     (value-p (gvalue-init gvalue type value))
     (type (gvalue-init gvalue type)))
    gvalue))

(defun gvalue-free (gvalue &optional (unset-p t))
  (unless (null-pointer-p gvalue)
    (when unset-p
      (gvalue-unset gvalue))
    (deallocate-memory gvalue)))

(defun gvalue-type (gvalue)
  (type-from-number (sap-ref-32 gvalue 0)))

(defun gvalue-get (gvalue &optional weak-p)
  (funcall (reader-function (gvalue-type gvalue))
   gvalue +gvalue-value-offset+ weak-p))

(defun gvalue-set (gvalue value)
  (funcall (writer-function (gvalue-type gvalue))
   value gvalue +gvalue-value-offset+)
  value)

(defbinding (gvalue-p "g_type_check_value") () boolean
  (location pointer))

(defmacro with-gvalue ((gvalue &optional type (value nil value-p)) &body body)
  `(with-allocated-memory (,gvalue +gvalue-size+)
     ,(cond
       ((and type value-p) `(gvalue-init ,gvalue ,type ,value))
       (type `(gvalue-init ,gvalue ,type)))
     ,@body
     ,(unless value-p `(gvalue-get ,gvalue))))


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

  (defmethod validate-superclass  ((class param-spec-class) (super standard-class))
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
    :type (copy-of string))
   (documentation
    :allocation :virtual
    :getter "g_param_spec_get_blurb"
    :reader param-documentation
    :type (copy-of string)))
  (:metaclass param-spec-class)
  (:gtype "GParam"))


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
  (:metaclass param-spec-class)
  (:gtype "GParamChar"))

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
  (:gtype "GParamUChar"))

(defclass param-boolean (param)
  ((default-value
     :allocation :alien
     :reader param-boolean-default-value
     :type boolean))
  (:metaclass param-spec-class)
  (:gtype "GParamBoolean"))

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
  (:metaclass param-spec-class)
  (:gtype "GParamInt"))

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
  (:gtype "GParamUInt"))

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
  (:metaclass param-spec-class)
  (:gtype "GParam"))

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
  (:gtype "GParamULong"))

(defclass param-unichar (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamUnichar"))

(defclass param-enum (param)
  ((class
    :allocation :alien
    :reader param-enum-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-enum-default-value
    :type long))
  (:metaclass param-spec-class)
  (:gtype "GParamEnum"))

(defclass param-flags (param)
  ((class
    :allocation :alien
    :reader param-flags-class
    :type pointer)
   (default-value
    :allocation :alien
    :reader param-flags-default-value
    :type long))
  (:metaclass param-spec-class)
  (:gtype "GParamFlags"))

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
  (:gtype "GParamFloat"))

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
  (:gtype "GParamDouble"))

(defclass param-string (param)
  ((default-value
    :allocation :alien
    :reader param-string-default-value
    :type string))
  (:metaclass param-spec-class)
  (:gtype "GParamString"))

(defclass param-param (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamParam"))

(defclass param-boxed (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamBoxed"))

(defclass param-pointer (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamPointer"))

(defclass param-value-array (param)
  ((element-spec
    :allocation :alien
    :reader param-value-array-element-spec
    :type param)
   (length
    :allocation :alien
    :reader param-value-array-length
    :type unsigned-int))
  (:metaclass param-spec-class)
  (:gtype "GParamValueArray"))

(defclass param-object (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamObject"))

(defclass param-overrride (param)
  ()
  (:metaclass param-spec-class)
  (:gtype "GParamOverride"))
