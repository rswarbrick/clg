;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: enums.lisp,v 1.2 2006-06-08 13:24:25 espen Exp $

(in-package "GFFI")
  
;;;; Generic enum type

(defun %map-enum (mappings op)
  (delete-duplicates 
   (loop
    as value = 0 then (1+ value)
    for mapping in mappings
    collect (let ((symbol (if (atom mapping) mapping (first mapping))))
	      (unless (atom mapping)
		(setq value (second mapping)))
	      (ecase op
		(:symbol-int `(,symbol ,value))
		(:int-symbol `(,value ,symbol))
		(:int-quoted-symbol `(,value ',symbol)))))
   :key #'first :from-end t))

(defun %map-symbols (mappings)
  (mapcar #'(lambda (mapping)
	      (first (mklist mapping)))
	  mappings))

(deftype enum (&rest args)
  `(member ,@(%map-symbols args)))

(define-type-method alien-type ((type enum))
  (declare (ignore type))
  (alien-type 'signed))

(define-type-method size-of ((type enum) &key (inlined t))
  (assert-inlined type inlined)
  (size-of 'signed))

(define-type-method type-alignment ((type enum) &key (inlined t))
  (assert-inlined type inlined)
  (type-alignment 'signed))

(define-type-method to-alien-form ((type enum) form &optional copy-p)
  (declare (ignore copy-p))
  `(case ,form
    ,@(%map-enum (rest (type-expand-to 'enum type)) :symbol-int)
    (t (error 'type-error :datum ,form :expected-type ',type))))

(define-type-method from-alien-form ((type enum) form &key ref)
  (declare (ignore ref))
  `(case ,form
    ,@(%map-enum (rest (type-expand-to 'enum type)) :int-quoted-symbol)))

(define-type-method to-alien-function ((type enum) &optional copy-p)
  (declare (ignore copy-p))
  (let ((mappings (%map-enum (rest (type-expand-to 'enum type)) :symbol-int)))
    #'(lambda (enum)
	(or
	 (second (assoc enum mappings))
	 (error 'type-error :datum enum :expected-type type)))))

(define-type-method from-alien-function ((type enum) &key ref)
  (declare (ignore ref))
  (let ((mappings (%map-enum (rest (type-expand-to 'enum type)) :int-symbol)))
    #'(lambda (int)
	(second (assoc int mappings)))))

(define-type-method writer-function ((type enum) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (let ((writer (writer-function 'signed))
	(function (to-alien-function (type-expand-to 'enum type))))
    #'(lambda (enum location &optional (offset 0))
	(funcall writer (funcall function enum) location offset))))
    
(define-type-method reader-function ((type enum) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (let ((reader (reader-function 'signed))
	(function (from-alien-function (type-expand-to 'enum type))))
    #'(lambda (location &optional (offset 0))
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
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (define-type-method to-alien-form ((type ,name) form &optional copy-p)
	   (declare (ignore type copy-p))
	   (list ',enum-int form))
	 (define-type-method from-alien-form ((type ,name) form &key ref)
	   (declare (ignore type ref))
	   (list ',int-enum form))
	 (define-type-method to-alien-function ((type ,name) &optional copy-p)
	   (declare (ignore type copy-p))
	   #',enum-int)
	 (define-type-method from-alien-function ((type ,name) &key ref)
	   (declare (ignore type ref))
	   #',int-enum)
	 (define-type-method writer-function ((type ,name) &key temp (inlined t))
	   (declare (ignore temp))
	   (assert-inlined type inlined)
	   (let ((writer (writer-function 'signed)))
	     #'(lambda (enum location &optional (offset 0))
		 (funcall writer (,enum-int enum) location offset))))
	 (define-type-method reader-function ((type ,name) &key ref (inlined t))
	   (declare (ignore ref))
	   (assert-inlined type inlined)
	   (let ((reader (reader-function 'signed)))
	     #'(lambda (location &optional (offset 0))
		 (,int-enum (funcall reader location offset)))))))))


;;;;  Generic flags type

(defun %map-flags (mappings op)
  (delete-duplicates
   (loop
    as value = 1 then (ash value 1)
    for mapping in mappings
    collect (let ((symbol (if (atom mapping) mapping (first mapping))))
	      (unless (atom mapping)
	       (setq value (second mapping)))
	      (case op
		(:symbol-int `(,symbol ,value))
		(:int-symbol `(,value ,symbol)))))
   :key #'first :from-end t))

(deftype flags (&rest args)
  `(or (member ,@(%map-symbols args)) list))

(define-type-method alien-type ((type flags))
  (declare (ignore type))
  (alien-type 'unsigned))

(define-type-method size-of ((type flags) &key (inlined t))
  (assert-inlined type inlined)
  (size-of 'unsigned))

(define-type-method type-alignment ((type flags) &key (inlined t))
  (assert-inlined type inlined)
  (type-alignment 'unsigned))

(define-type-method to-alien-form ((type flags) flags &optional copy-p)
  (declare (ignore copy-p))
  `(reduce #'logior (mklist ,flags)
    :key #'(lambda (flag)
	     (case flag
	       ,@(%map-flags (rest (type-expand-to 'flags type)) :symbol-int)
	       (t (error 'type-error :datum ,flags :expected-type ',type))))))

(define-type-method from-alien-form ((type flags) value &key ref)
  (declare (ignore ref))
  `(loop
    for (int symbol) in ',(%map-flags (rest (type-expand-to 'flags type)) :int-symbol)
    when (= (logand ,value int) int)
    collect symbol))

(define-type-method to-alien-function ((type flags) &optional copy-p)
  (declare (ignore copy-p))
  (let ((mappings (%map-flags (rest (type-expand-to 'flags type)) :symbol-int)))
    #'(lambda (flags)
	(reduce #'logior (mklist flags)
	 :key #'(lambda (flag)
		  (or
		   (second (assoc flag mappings))
		   (error 'type-error :datum flags :expected-type type)))))))

(define-type-method from-alien-function ((type flags) &key ref)
  (declare (ignore ref))
  (let ((mappings (%map-flags (rest (type-expand-to 'flags type)) :int-symbol)))
    #'(lambda (value)
	(loop
	 for (int symbol) in mappings
	 when (= (logand value int) int)
	 collect symbol))))

(define-type-method writer-function ((type flags) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (let ((writer (writer-function 'unsigned))
	(function (to-alien-function (type-expand-to 'flags type))))
    #'(lambda (flags location &optional (offset 0))
	(funcall writer (funcall function flags) location offset))))
    
(define-type-method reader-function ((type flags) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (let ((reader (reader-function 'unsigned))
	(function (from-alien-function (type-expand-to 'flags type))))
    #'(lambda (location &optional (offset 0))
	(funcall function (funcall reader location offset)))))


;;;; Named flags types

(defmacro define-flags-type (name &rest args)
  (let ((flags-int (intern (format nil "~A-TO-INT" name)))
	(int-flags (intern (format nil "INT-TO-~A" name)))
	(satisfies  (intern (format nil "~A-P" name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (deftype ,name () '(satisfies ,satisfies))
       (defun ,satisfies (object)
	 (flet ((valid-p (ob)
		  (find ob ',(%map-symbols args))))
	   (typecase object
	     (symbol (valid-p object))
	     (list (every #'valid-p object)))))
       (defun ,flags-int (flags)
	 (reduce #'logior (mklist flags)
	  :key #'(lambda (flag)
		   (case flag
		     ,@(%map-flags args :symbol-int)
		     (t (error 'type-error :datum flags :expected-type ',name))))))
       (defun ,int-flags (value)
	 (loop
	  for (int symbol) in ',(%map-flags args :int-symbol)
	  when(= (logand value int) int)
	  collect symbol))
       (define-type-method alien-type ((type ,name))
	 (declare (ignore type))
	 (alien-type 'flags))
       (define-type-method size-of ((type ,name) &key (inlined t))
	 (assert-inlined type inlined)
	 (size-of 'flags))
       (define-type-method type-alignment ((type ,name) &key (inlined t))
	 (assert-inlined type inlined)
	 (type-alignment 'flags))
       (define-type-method to-alien-form ((type ,name) form &optional copy-p)
	 (declare (ignore type copy-p))
	 (list ',flags-int form))
       (define-type-method from-alien-form ((type ,name) form &key ref)
	 (declare (ignore type ref))
	 (list ',int-flags form))
       (define-type-method to-alien-function ((type ,name) &optional copy-p)
	 (declare (ignore type copy-p))
	 #',flags-int)
       (define-type-method from-alien-function ((type ,name) &key ref)
	 (declare (ignore type ref))
	 #',int-flags)
       (define-type-method writer-function ((type ,name) &key temp (inlined t))
	 (declare (ignore temp))
	 (assert-inlined type inlined)
	 (let ((writer (writer-function 'signed)))
	   #'(lambda (flags location &optional (offset 0))
	       (funcall writer (,flags-int flags) location offset))))
       (define-type-method reader-function ((type ,name) &key ref (inlined t))
	 (declare (ignore ref))
	 (assert-inlined type inlined)
	 (let ((reader (reader-function 'signed)))
	   #'(lambda (location &optional (offset 0))
	       (,int-flags (funcall reader location offset))))))))


(defexport define-enum-type (name &rest args)
  (declare (ignore args))
  name)

(defexport define-flags-type (name &rest args)
  (declare (ignore args))
  name)
