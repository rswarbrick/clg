;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: genums.lisp,v 1.11 2005-02-25 17:20:25 espen Exp $

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

(defmethod alien-type ((type (eql 'enum)) &rest args)
  (declare (ignore type args))
  (alien-type 'signed))

(defmethod size-of ((type (eql 'enum)) &rest args)
  (declare (ignore type args))
  (size-of 'signed))

(defmethod to-alien-form (form (type (eql 'enum)) &rest args)
  (declare (ignore type))
  `(case ,form
    ,@(%map-enum args :symbol-int)
    (t (error 'type-error :datum ,form :expected-type '(enum ,@args)))))


(defmethod from-alien-form (form (type (eql 'enum)) &rest args)
  (declare (ignore type))
  `(case ,form
    ,@(%map-enum args :int-quoted-symbol)))

(defmethod to-alien-function ((type (eql 'enum)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-enum args :symbol-int)))
    #'(lambda (enum)
	(or
	 (second (assoc enum mappings))
	 (error 'type-error :datum enum :expected-type (cons 'enum args))))))

(defmethod from-alien-function ((type (eql 'enum)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-enum args :int-symbol)))
    #'(lambda (int)
	(second (assoc int mappings)))))

(defmethod writer-function ((type (eql 'enum)) &rest args)
  (declare (ignore type))
  (let ((writer (writer-function 'signed))
	(function (apply #'to-alien-function 'enum args)))
    #'(lambda (enum location &optional (offset 0))
	(funcall writer (funcall function enum) location offset))))
    
(defmethod reader-function ((type (eql 'enum)) &rest args)
  (declare (ignore type))
  (let ((reader (reader-function 'signed))
	(function (apply #'from-alien-function 'enum args)))
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
       (defmethod to-alien-form (form (type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (list ',enum-int form))
       (defmethod from-alien-form (form (type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (list ',int-enum form))
       (defmethod to-alien-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 #',enum-int)
       (defmethod from-alien-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 #',int-enum)
       (defmethod writer-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (let ((writer (writer-function 'signed)))
	   #'(lambda (enum location &optional (offset 0))
	       (funcall writer (,enum-int enum) location offset))))    
       (defmethod reader-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (let ((reader (reader-function 'signed)))
	   #'(lambda (location &optional (offset 0))
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

(defmethod alien-type ((type (eql 'flags)) &rest args)
  (declare (ignore type args))
  (alien-type 'unsigned))

(defmethod size-of ((type (eql 'flags)) &rest args)
  (declare (ignore type args))
  (size-of 'unsigned))

(defmethod to-alien-form (flags (type (eql 'flags)) &rest args)
  `(reduce #'logior (mklist ,flags)
    :key #'(lambda (flag)
	     (case flag
	       ,@(%map-flags args :symbol-int)
	       (t (error 'type-error :datum ,flags 
		   :expected-type '(,type ,@args)))))))

(defmethod from-alien-form (int (type (eql 'flags)) &rest args)
  (declare (ignore type))
  `(loop
    for mapping in ',(%map-flags args :int-symbol)
    unless (zerop (logand ,int (first mapping)))
    collect (second mapping)))

(defmethod to-alien-function ((type (eql 'flags)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-flags args :symbol-int)))
    #'(lambda (flags)
	(reduce #'logior (mklist flags)
	 :key #'(lambda (flag)
		  (or
		   (second (assoc flag mappings))
		   (error 'type-error :datum flags 
		    :expected-type (cons 'flags args))))))))

(defmethod from-alien-function ((type (eql 'flags)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-flags args :int-symbol)))
    #'(lambda (int)
	(loop
	 for mapping in mappings
	 unless (zerop (logand int (first mapping)))
	 collect (second mapping)))))

(defmethod writer-function ((type (eql 'flags)) &rest args)
  (declare (ignore type))
  (let ((writer (writer-function 'unsigned))
	(function (apply #'to-alien-function 'flags args)))
    #'(lambda (flags location &optional (offset 0))
	(funcall writer (funcall function flags) location offset))))
    
(defmethod reader-function ((type (eql 'flags)) &rest args)
  (declare (ignore type))
  (let ((reader (reader-function 'unsigned))
	(function (apply #'from-alien-function 'flags args)))
    #'(lambda (location &optional (offset 0))
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
	  for mapping in ',(%map-flags args :int-symbol)
	  unless (zerop (logand value (first mapping)))
	  collect (second mapping)))
       (defmethod alien-type ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (alien-type 'flags))
       (defmethod size-of ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (size-of 'flags))
       (defmethod to-alien-form (form (type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (list ',flags-int form))
       (defmethod from-alien-form (form (type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (list ',int-flags form))
       (defmethod to-alien-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 #',flags-int)
       (defmethod from-alien-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 #',int-flags)
       (defmethod writer-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (let ((writer (writer-function 'signed)))
	   #'(lambda (flags location &optional (offset 0))
	       (funcall writer (,flags-int flags) location offset))))    
       (defmethod reader-function ((type (eql ',name)) &rest args)
	 (declare (ignore type args))
	 (let ((reader (reader-function 'signed)))
	   #'(lambda (location &optional (offset 0))
	       (,int-flags (funcall reader location offset))))))))



;;;; Type definition by introspection

(defun %query-enum-or-flags-values (query-function class type)
  (multiple-value-bind (sap length)
      (funcall query-function (type-class-ref type))
    (let ((values nil)
	  (size (proxy-instance-size (find-class class)))
	  (proxy (make-instance class :location sap)))
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
       (register-type ',type ,(find-type-name type-number))
       ,(ecase super
	  (enum `(define-enum-type ,type ,@expanded-mappings))
	  (flags `(define-flags-type ,type ,@expanded-mappings))))))


(register-derivable-type 'enum "GEnum" 'expand-enum-type)
(register-derivable-type 'flags "GFlags" 'expand-enum-type)

