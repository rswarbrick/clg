;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2001-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gboxed.lisp,v 1.21 2006-04-25 21:55:42 espen Exp $

(in-package "GLIB")


(defclass boxed (struct)
  ()
  (:metaclass struct-class))

(defmethod instance-finalizer ((instance boxed))
  (let ((location (foreign-location instance))
	(type-number (type-number-of instance)))
    #'(lambda ()
	(%boxed-free type-number location))))


;;;; Metaclass for boxed classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass boxed-class (struct-class)
    ())

  (defmethod validate-superclass ((class boxed-class) (super standard-class))
    (subtypep (class-name super) 'boxed)))


(defbinding %boxed-copy () pointer
  (type-number type-number)
  (location pointer))

(defbinding %boxed-free () nil
  (type-number type-number)
  (location pointer))

(defmethod shared-initialize ((class boxed-class) names 
			      &key name gtype ref unref)
  (declare (ignore names))
  (let* ((class-name (or name (class-name class)))
	 (type-number 
	  (register-type class-name 
	  (or 
	   (first gtype) 
	   (default-type-init-name class-name)))))
    (unless (or ref (slot-boundp class 'ref))
      (setf 
       (slot-value class 'ref)
       #'(lambda (location)
	   (%boxed-copy type-number location))))
    (unless (or unref (slot-boundp class 'unref))
      (setf 
       (slot-value class 'unref)
       #'(lambda (location)
	   (%boxed-free type-number location)))))
  (call-next-method))


(defun expand-boxed-type (type-number forward-p slots)
  `(defclass ,(type-from-number type-number) (boxed)
     ,(unless forward-p
	slots)
     (:metaclass boxed-class)
     (:gtype ,(register-type-as type-number))))

(register-derivable-type 'boxed "GBoxed" 'expand-boxed-type)


;;;; NULL terminated vector of strings

(deftype strings () '(null-terminated-vector string))
(register-type 'strings '|g_strv_get_type|)
