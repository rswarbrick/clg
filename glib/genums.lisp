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

;; $Id: genums.lisp,v 1.19 2006-02-26 15:30:01 espen Exp $

(in-package "GLIB")
  
;;;; Generic enum type

(defun %map-enum (mappings op)
  (loop
   as value = 0 then (1+ value)
   for mapping in mappings
   collect (let ((symbol (if (atom mapping) mapping (first mapping))))
	     (unless (atom mapping)
	       (setq value (second mapping)))
	     (ecase op
	       (:symbol-int `(,symbol ,value))
	       (:int-symbol `(,value ,symbol))
	       (:int-quoted-symbol `(,value ',symbol))
	       (:symbols symbol)))))

(deftype enum (&rest args)
  `(member ,@(%map-enum args :symbols)))

(define-type-method alien-type ((type enum))
  (declare (ignore type))
  (alien-type 'signed))

(define-type-method size-of ((type enum))
  (declare (ignore type))
  (size-of 'signed))

(define-type-method to-alien-form ((type enum) form )
  `(case ,form
    ,@(%map-enum (rest (type-expand-to 'enum type)) :symbol-int)
    (t (error 'type-error :datum ,form :expected-type ',type))))

(define-type-method from-alien-form ((type enum) form)
  `(case ,form
    ,@(%map-enum (rest (type-expand-to 'enum type)) :int-quoted-symbol)))

(define-type-method to-alien-function ((type enum))
  (let ((mappings (%map-enum (rest (type-expand-to 'enum type)) :symbol-int)))
    #'(lambda (enum)
	(or
	 (second (assoc enum mappings))
	 (error 'type-error :datum enum :expected-type type)))))

(define-type-method from-alien-function ((type enum))
  (let ((mappings (%map-enum (rest (type-expand-to 'enum type)) :int-symbol)))
    #'(lambda (int)
	(second (assoc int mappings)))))

(define-type-method writer-function ((type enum))
  (let ((writer (writer-function 'signed))
	(function (to-alien-function (type-expand-to 'enum type))))
    #'(lambda (enum location &optional (offset 0))
	(funcall writer (funcall function enum) location offset))))
    
(define-type-method reader-function ((type enum))
  (let ((reader (reader-function 'signed))
	(function (from-alien-function (type-expand-to 'enum type))))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(funcall function (funcall reader location offset)))))

(defun enum-int (enum type)
  (funcall (to-alien-function type) enum))

(defun int-enum (int type)
  (funcall (from-alien-function type) int))

(defun enum-mapping (type)
  (rest (type-expand-to 'enum type)))


;;;; Named enum types

(defmacro define-enum-type (name &rest args)
  (let ((enum-int (intern (format nil "~A-TO-INT" name)))
	(int-enum (intern (format nil "INT-TO-~A" name))))
    `(progn
       (deftype ,name () '(enum ,@args))
       (defun ,enum-int (enum)
	 (case enum
	   ,@(%map-enum args :symbol-int)
	   (t (error 'type-error :datum enum :expected-type ',name))))
       (defun ,int-enum (value)
	 (case value
	   ,@(%map-enum args :int-quoted-symbol)))
       (define-type-method to-alien-form ((type ,name) form)
	 (declare (ignore type))
	 (list ',enum-int form))
       (define-type-method from-alien-form ((type ,name) form)
	 (declare (ignore type))
	 (list ',int-enum form))
       (define-type-method to-alien-function ((type ,name))
	 (declare (ignore type))
	 #',enum-int)
       (define-type-method from-alien-function ((type ,name))
	 (declare (ignore type))
	 #',int-enum)
       (define-type-method writer-function ((type ,name))
	 (declare (ignore type))
	 (let ((writer (writer-function 'signed)))
	   #'(lambda (enum location &optional (offset 0))
	       (funcall writer (,enum-int enum) location offset))))    
       (define-type-method reader-function ((type ,name))
	 (declare (ignore type))
	 (let ((reader (reader-function 'signed)))
	   #'(lambda (location &optional (offset 0) weak-p)
	       (declare (ignore weak-p))
	       (,int-enum (funcall reader location offset))))))))


;;;;  Generic flags type

(defun %map-flags (mappings op)
  (loop
   as value = 1 then (ash value 1)
   for mapping in mappings
   collect (let ((symbol (if (atom mapping) mapping (first mapping))))
	     (unless (atom mapping)
	       (setq value (second mapping)))
	     (ecase op
	       (:symbol-int `(,symbol ,value))
	       (:int-symbol `(,value ,symbol))
	       (:symbols symbol)))))

(deftype flags (&rest args)
  `(or (member ,@(%map-flags args :symbols)) list))

(define-type-method alien-type ((type flags))
  (declare (ignore type))
  (alien-type 'unsigned))

(define-type-method size-of ((type flags))
  (declare (ignore type))
  (size-of 'unsigned))

(define-type-method to-alien-form ((type flags) flags)
  `(reduce #'logior (mklist ,flags)
    :key #'(lambda (flag)
	     (case flag
	       ,@(%map-flags (rest (type-expand-to 'flags type)) :symbol-int)
	       (t (error 'type-error :datum ,flags :expected-type ',type))))))

(define-type-method from-alien-form ((type flags) value)
  `(loop
    for (int symbol) in ',(%map-flags (rest (type-expand-to 'flags type)) :int-symbol)
    when (= (logand ,value int) int)
    collect symbol))

(define-type-method to-alien-function ((type flags))
  (let ((mappings (%map-flags (rest (type-expand-to 'flags type)) :symbol-int)))
    #'(lambda (flags)
	(reduce #'logior (mklist flags)
	 :key #'(lambda (flag)
		  (or
		   (second (assoc flag mappings))
		   (error 'type-error :datum flags :expected-type type)))))))

(define-type-method from-alien-function ((type flags))
  (let ((mappings (%map-flags (rest (type-expand-to 'flags type)) :int-symbol)))
    #'(lambda (value)
	(loop
	 for (int symbol) in mappings
	 when (= (logand value int) int)
	 collect symbol))))

(define-type-method writer-function ((type flags))
  (let ((writer (writer-function 'unsigned))
	(function (to-alien-function (type-expand-to 'flags type))))
    #'(lambda (flags location &optional (offset 0))
	(funcall writer (funcall function flags) location offset))))
    
(define-type-method reader-function ((type flags))
  (let ((reader (reader-function 'unsigned))
	(function (from-alien-function (type-expand-to 'flags type))))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(funcall function (funcall reader location offset)))))


;;;; Named flags types

(defmacro define-flags-type (name &rest args)
  (let ((flags-int (intern (format nil "~A-TO-INT" name)))
	(int-flags (intern (format nil "INT-TO-~A" name)))
	(satisfies  (intern (format nil "~A-P" name))))
    `(progn
       (deftype ,name () '(satisfies ,satisfies))
       (defun ,satisfies (object)
	 (flet ((valid-p (ob)
		  (find ob ',(%map-flags args :symbols))))
	   (typecase object
	     (symbol (valid-p object))
	     (list (every #'valid-p object)))))
       (defun ,flags-int (flags)
	 (reduce #'logior (mklist flags)
	  :key #'(lambda (flag)
		   (case flag
		     ,@(%map-flags args :symbol-int)
		     (t (error 'type-error :datum flags 
		         :expected-type ',name))))))
       (defun ,int-flags (value)
	 (loop
	  for (int symbol) in ',(%map-flags args :int-symbol)
	  when(= (logand value int) int)
	  collect symbol))
       (define-type-method alien-type ((type ,name))
	 (declare (ignore type))
	 (alien-type 'flags))
       (define-type-method size-of ((type ,name))
	 (declare (ignore type))
	 (size-of 'flags))
       (define-type-method to-alien-form ((type ,name) form)
	 (declare (ignore type))
	 (list ',flags-int form))
       (define-type-method from-alien-form ((type ,name) form)
	 (declare (ignore type))
	 (list ',int-flags form))
       (define-type-method to-alien-function ((type ,name))
	 (declare (ignore type))
	 #',flags-int)
       (define-type-method from-alien-function ((type ,name))
	 (declare (ignore type))
	 #',int-flags)
       (define-type-method writer-function ((type ,name))
	 (declare (ignore type))
	 (let ((writer (writer-function 'signed)))
	   #'(lambda (flags location &optional (offset 0))
	       (funcall writer (,flags-int flags) location offset))))
       (define-type-method reader-function ((type ,name))
	 (declare (ignore type))
	 (let ((reader (reader-function 'signed)))
	   #'(lambda (location &optional (offset 0) weak-p)
	       (declare (ignore weak-p))
	       (,int-flags (funcall reader location offset))))))))



;;;; Type definition by introspection

(defun %query-enum-or-flags-values (query-function class type)
  (multiple-value-bind (sap length)
      (funcall query-function (type-class-ref type))
    (let ((values nil)
	  (size (foreign-size (find-class class)))
	  (proxy (ensure-proxy-instance class sap)))
      (dotimes (i length)
	(with-slots (location nickname value) proxy
	  (setf location sap)
	  (setq sap (sap+ sap size))
	  (push
	   (list
	    (intern (substitute #\- #\_ (string-upcase nickname)) "KEYWORD")
	    value)
	   values)))
      values)))


(defclass %enum-value (struct)
  ((value :allocation :alien :type int)
   (name :allocation :alien :type string)
   (nickname :allocation :alien :type string))
  (:metaclass static-struct-class))

(defbinding %enum-class-values () pointer
  (class pointer)
  (n-values unsigned-int :out))

(defun query-enum-values (type)
  (%query-enum-or-flags-values #'%enum-class-values '%enum-value type))


(defclass %flags-value (struct)
  ((value :allocation :alien :type unsigned-int)
   (name :allocation :alien :type string)
   (nickname :allocation :alien :type string))
  (:metaclass static-struct-class))

(defbinding %flags-class-values () pointer
  (class pointer)
  (n-values unsigned-int :out))

(defun query-flags-values (type)
  (%query-enum-or-flags-values #'%flags-class-values '%flags-value type))


(defun expand-enum-type (type-number forward-p options)
  (declare (ignore forward-p))
  (let* ((super (supertype type-number))
	 (type (type-from-number type-number))
	 (mappings (getf options :mappings))
	 (expanded-mappings
	  (append
	   (delete-if
	    #'(lambda (mapping)
		(or
		 (assoc (first mapping) mappings)
		 (rassoc (cdr mapping) mappings :test #'equal)))
	    (if (eq super 'enum)
		(query-enum-values type-number)
	      (query-flags-values type-number)))
	   (remove-if
	    #'(lambda (mapping) (eq (second mapping) nil)) mappings))))
    `(progn
       (register-type ',type ',(find-type-init-function type-number))
       ,(ecase super
	  (enum `(define-enum-type ,type ,@expanded-mappings))
	  (flags `(define-flags-type ,type ,@expanded-mappings))))))


(register-derivable-type 'enum "GEnum" 'expand-enum-type)
(register-derivable-type 'flags "GFlags" 'expand-enum-type)

