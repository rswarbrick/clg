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

;; $Id: genums.lisp,v 1.1 2001-04-29 20:19:25 espen Exp $

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

(setf (alien-type-name 'enum) "GEnum")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass %enum-value (alien-structure)
    ((value :allocation :alien :type int)
     (name :allocation :alien :type string)
     (nickname :allocation :alien :type string))
    (:metaclass proxy-class)))

(defbinding %enum-class-values () (glist %enum-value)
  (class pointer))

(defun %query-enum-values (type-number)
  (mapcar
   #'(lambda (enum-value)
       (list
	(intern
	 (substitute
	  #\- #\_ (string-upcase (slot-value enum-value 'nickname))) "KEYWORD")
	(slot-value enum-value 'value)))
   (%enum-class-values (type-class-peek type-number))))

(defun define-enum-by-query (init-fname &optional name)
  (let ((type-number (type-init name init-fname)))
    (unless (= (type-parent type-number) (find-type-number 'enum))
      (error "~A is not an enum type" (alien-type-name type-number)))
    
    (type-class-ref type-number)
    (setf (find-type-number name) type-number)
    (let ((expanded (cons 'enum (%query-enum-values type-number)))
      	  (name (or name (default-type-name (alien-type-name type-number)))))
      (lisp::%deftype
       name
       #'(lambda (whole)
	   (unless (zerop (length (cdr whole)))
	     (lisp::do-arg-count-error 'deftype name (cdr whole) nil 0 0))
	   expanded)))))


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

(setf (alien-type-name 'flags) "GFlags")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass %flags-value (alien-structure)
    ((value :allocation :alien :type unsigned-int)
     (name :allocation :alien :type string)
     (nickname :allocation :alien :type string))
    (:metaclass proxy-class)))

(defbinding %flags-class-values () (glist %flags-value)
  (class pointer))

(defun %query-flags-values (type-number)
  (mapcar
   #'(lambda (flags-value)
       (list
	(intern
	 (substitute
	  #\- #\_ (string-upcase (slot-value flags-value 'nickname))) "KEYWORD")
	(slot-value flags-value 'value)))
   (%flags-class-values (type-class-peek type-number))))

(defun define-flags-by-query (init-fname &optional name)
  (let ((type-number (type-init nil init-fname)))
    (unless (= (type-parent type-number) (find-type-number 'flags))
      (error "~A is not a flags type" (alien-type-name type-number)))
    
    (type-class-ref type-number)
    (setf (find-type-number name) type-number)
    (let ((expanded (cons 'flags (%query-flags-values type-number)))
	  (name (or name (default-type-name (alien-type-name type-number)))))
      (lisp::%deftype
       name
       #'(lambda (whole)
	   (unless (zerop (length (cdr whole)))
	     (lisp::do-arg-count-error 'deftype name (cdr whole) nil 0 0))
	   expanded)))))
