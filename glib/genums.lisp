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

;; $Id: genums.lisp,v 1.4 2004-11-06 21:39:58 espen Exp $

(in-package "GLIB")


(defun %map-enum (args op)
  (let ((current-value 0))
    (mapcar
     #'(lambda (mapping)
	 (destructuring-bind (symbol &optional (value current-value))
	     (mklist mapping)
	   (setf current-value (1+ value))
	   (case op
	     (:enum-int (list symbol value))
	     (:flags-int (list symbol value))
	     (:int-enum (list value symbol))
	     (:int-flags (list value symbol))
	     (:symbols symbol))))
     args)))

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
   
  
;;;; Generic enum type

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
  `(ecase ,form
    ,@(%map-enum args :enum-int)))

(defmethod from-alien-form (form (type (eql 'enum)) &rest args)
  (declare (ignore type))
  `(ecase ,form
    ,@(%map-enum args :int-enum)))

(defmethod to-alien-function ((type (eql 'enum)) &rest args)
  (let ((mappings (%map-enum args :enum-int)))
    #'(lambda (enum)
	(or
	 (second (assoc enum mappings))
	 (error "~S is not of type ~S" enum (cons type args))))))

(defmethod from-alien-function ((type (eql 'enum)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-enum args :int-enum)))
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



;;;;  Generic flags type

(deftype flags (&rest args)
  `(or null (cons (member ,@(%map-enum args :symbols)) list)))

(defmethod alien-type ((type (eql 'flags)) &rest args)
  (declare (ignore type args))
  (alien-type 'unsigned))

(defmethod size-of ((type (eql 'flags)) &rest args)
  (declare (ignore type args))
  (size-of 'unsigned))

(defmethod to-alien-form (flags (type (eql 'flags)) &rest args)
  `(loop
    with value = 0
    with flags = ,flags
    for flag in (mklist flags)
    do (let ((flagval
	      (or
	       (second (assoc flag ',(%map-enum args :flags-int)))
	       (error "~S is not of type ~S" flags '(,type ,@args)))))
	 (setq value (logior value flagval)))
    finally (return value)))

(defmethod from-alien-form (int (type (eql 'flags)) &rest args)
  (declare (ignore type))
  `(loop
    for mapping in ',(%map-enum args :int-flags)
    unless (zerop (logand int (first mapping)))
    collect (second mapping)))

(defmethod to-alien-function ((type (eql 'flags)) &rest args)
  (let ((mappings (%map-enum args :flags-int)))
    #'(lambda (flags)	
	(loop
	 with value = 0
	 for flag in (mklist flags)
	 do (let ((flagval (or
		    (second (assoc flag mappings))
		    (error "~S is not of type ~S" flags (cons type args)))))
	      (setq value (logior value flagval)))
	 finally (return value)))))

(defmethod from-alien-function ((type (eql 'flags)) &rest args)
  (declare (ignore type))
  (let ((mappings (%map-enum args :int-flags)))
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



;;;;

(defun expand-enum-type (type-number &optional options)
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
       (deftype ,type () '(,super ,@expanded-mappings)))))


(register-derivable-type 'enum "GEnum" 'expand-enum-type)
(register-derivable-type 'flags "GFlags" 'expand-enum-type)

