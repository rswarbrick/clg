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

;; $Id: genums.lisp,v 1.2 2001-05-11 16:04:33 espen Exp $

(in-package "GLIB")


(defun %map-mappings (args op)
  (let ((current-value 0))
    (map
     'list 
     #'(lambda (mapping)
	 (destructuring-bind (symbol &optional (value current-value))
	     (mklist mapping)
	   (setf current-value (1+ value))
	   (case op
	     (:enum-int (list symbol value))
	     (:flags-int (list symbol value #|(ash 1 value)|#))
	     (:int-enum (list value symbol))
	     (:int-flags (list value #|(ash 1 value)|# symbol))
	     (:symbols symbol))))
     (if (integerp (first args))
	 (rest args)
       args))))

(defun %query-enum-or-flags-values (query-function class type)
  (multiple-value-bind (sap length)
      (funcall query-function (type-class-ref type))
    (let ((values nil)
	  (size (proxy-class-size (find-class class)))
	  (proxy (make-proxy-instance class sap nil)))
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
   
  
;;;; Enum type

(deftype enum (&rest args)
  `(member ,@(%map-mappings args :symbols)))

(deftype-method translate-type-spec enum (type-spec)
  (let ((args (cdr (type-expand-to 'enum type-spec))))
    (if (integerp (first args))
	(translate-type-spec `(signed ,(first args)))
      (translate-type-spec 'signed))))

(deftype-method size-of enum (type-spec)
  (let ((args (cdr (type-expand-to 'enum type-spec))))
    (if (integerp (first args))
	(size-of `(signed ,(first args)))
      (size-of 'signed))))

(deftype-method translate-to-alien enum (type-spec expr &optional weak-ref)
  (declare (ignore weak-ref))
  (let ((args (cdr (type-expand-to 'enum type-spec))))
    `(ecase ,expr
       ,@(%map-mappings args :enum-int))))

(deftype-method translate-from-alien enum (type-spec expr &optional weak-ref)
  (declare (ignore weak-ref))
  (destructuring-bind (name &rest args) (type-expand-to 'enum type-spec)
    (declare (ignore name))
    `(ecase ,expr
       ,@(%map-mappings args :int-enum))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass %enum-value (static)
    ((value :allocation :alien :type int)
     (name :allocation :alien :type string)
     (nickname :allocation :alien :type string))
    (:metaclass proxy-class)))

(defbinding %enum-class-values () pointer
  (class pointer)
  (n-values unsigned-int :out))

(defun query-enum-values (type)
  (%query-enum-or-flags-values #'%enum-class-values '%enum-value type))



;;;;  Flags type

(deftype flags (&rest args)
  `(or
    null
    (cons
     (member ,@(%map-mappings args :symbols))
     list)))

(deftype-method translate-type-spec flags (type-spec)
  (let ((args (cdr (type-expand-to 'flags type-spec))))
    (if (integerp (first args))
	(translate-type-spec `(unsigned ,(first args)))
      (translate-type-spec 'unsigned))))

(deftype-method size-of flags (type-spec)
  (let ((args (cdr (type-expand-to 'flags type-spec))))
    (if (integerp (first args))
	(size-of `(unsigned ,(first args)))
      (size-of 'unsigned))))

(deftype-method translate-to-alien flags (type-spec expr &optional weak-ref)
  (declare (ignore weak-ref))
  (destructuring-bind (name &rest args) (type-expand-to 'flags type-spec)
    (declare (ignore name))
    (let ((mappings (%map-mappings args :flags-int))
	  (value (make-symbol "VALUE")))
      `(let ((,value 0))
	 (dolist (flag ,expr ,value)
	   (setq ,value (logior ,value (second (assoc flag ',mappings)))))))))

(deftype-method translate-from-alien flags (type-spec expr &optional weak-ref)
  (declare (ignore weak-ref))
  (destructuring-bind (name &rest args) (type-expand-to 'flags type-spec)
    (declare (ignore name))
    (let ((mappings (%map-mappings args :int-flags))
	  (result (make-symbol "RESULT")))
      `(let ((,result nil))
	 (dolist (mapping ',mappings ,result)
	   (unless (zerop (logand ,expr (first mapping)))
	     (push (second mapping) ,result)))))))



;(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass %flags-value (static)
    ((value :allocation :alien :type unsigned-int)
     (name :allocation :alien :type string)
     (nickname :allocation :alien :type string))
    (:metaclass proxy-class));)

(defbinding %flags-class-values () pointer
  (class pointer)
  (n-values unsigned-int :out))

(defun query-flags-values (type)
  (%query-enum-or-flags-values #'%flags-class-values '%flags-value type))



;;;;

(defun expand-enum-type (type-number &optional mappings)
  (let* ((super (supertype type-number))
	 (type (type-from-number type-number))
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


(register-derivable-type 'enum "GEnum" :expand 'expand-enum-type)
(register-derivable-type 'flags "GFlags" :expand 'expand-enum-type)

