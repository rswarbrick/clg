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

;; $Id: gtype.lisp,v 1.10 2001-05-11 16:04:33 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;;;; 

(deftype type-number () '(unsigned 32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass type-query (struct)
    ((type-number :allocation :alien :type type-number)
     (name :allocation :alien :type string)
     (class-size :allocation :alien :type unsigned-int)
     (instance-size :allocation :alien :type unsigned-int))
    (:metaclass proxy-class)))


(defbinding %type-query () nil
  (type type-number)
  (query type-query))

(defun type-query (type)
  (let ((query (make-instance 'type-query)))
    (%type-query (find-type-number type t) query)
    query))

(defun type-instance-size (type)
  (slot-value (type-query type) 'instance-size))

(defun type-class-size (type)
  (slot-value (type-query type) 'class-size))

(defbinding type-class-ref (type) pointer
  ((find-type-number type t) type-number))

(defbinding type-class-unref (type) nil
  ((find-type-number type t) type-number))

(defbinding type-class-peek (type) pointer
  ((find-type-number type t) type-number))


;;;; Mapping between lisp types and glib types

(defvar *type-to-number-hash* (make-hash-table))
(defvar *number-to-type-hash* (make-hash-table))

(defun register-type (type id)
  (let ((type-number
	 (etypecase id
	   (integer id)
	   (string (find-type-number id t)))))
    (setf (gethash type *type-to-number-hash*) type-number)
    (setf (gethash type-number *number-to-type-hash*) type)
    type-number))

(defbinding %type-from-name () type-number
  (name string))

(defun find-type-number (type &optional error)
  (etypecase type
    (integer type)
    (string
     (let ((type-number (%type-from-name type)))
       (cond
	((and (zerop type-number) error)
	 (error "Invalid alien type name: ~A" type))
	((zerop type-number) nil)
	(t type-number))))
    (symbol
     (let ((type-number (gethash type *type-to-number-hash*)))
       (or
	type-number
	(and error (error "Type not registered: ~A" type)))))
    (pcl::class (find-type-number (class-name type) error))))
 
(defun type-from-number (type-number)
  (gethash type-number *number-to-type-hash*))

(defun type-from-name (name)
  (etypecase name
    (string (type-from-number (find-type-number name t)))))

(defbinding (find-type-name "g_type_name") (type) string
  ((find-type-number type t) type-number))

(defun type-number-of (object)
  (find-type-number (type-of object) t))

(defun init-type (init)
  (mapc
   #'(lambda (fname)
       (funcall (mkbinding fname 'type-number)))
   (mklist init)))

(defmacro init-types-in-library (pathname)
  (let ((process (ext:run-program
		  "nm" (list (namestring (truename pathname)))
		  :output :stream :wait nil))
	(fnames ()))
    (labels ((read-symbols ()
	       (let ((line (read-line (ext:process-output process) nil)))
		 (when line
		   (when (search "_get_type" line)
		     (push (subseq line 11) fnames))
		   (read-symbols)))))
      (read-symbols)
      (ext:process-close process)
      `(init-type ',fnames))))


;;;; Superclass for wrapping types in the glib type system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance (proxy)
    ((class :allocation :alien :type pointer))
    (:metaclass proxy-class)))

(defun %type-of-ginstance (location)
  (let ((class (sap-ref-sap location 0)))
    (type-from-number (sap-ref-unsigned class 0))))

(deftype-method translate-from-alien
    ginstance (type-spec location &optional weak-ref)
  (declare (ignore type-spec))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-proxy-instance
	(%type-of-ginstance location) location ,weak-ref))))



;;;; Metaclass for subclasses of ginstance

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance-class (proxy-class)))


(defmethod shared-initialize ((class ginstance-class) names
			      &rest initargs &key name alien-name
			      size ref unref)
  (declare (ignore initargs names))
  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (find-type-number
	   (or (first alien-name) (default-alien-type-name class-name)))))
    (register-type class-name type-number)
    (let ((size (or size (type-instance-size type-number))))
      (call-next-method)))

  (when ref
    (let ((ref (mkbinding (first ref) 'pointer 'pointer)))
      (setf
       (slot-value class 'copy)
       #'(lambda (type location)
	   (declare (ignore type))
	   (funcall ref location)))))     
  (when unref
    (let ((unref (mkbinding (first unref) 'nil 'pointer)))
      (setf
       (slot-value class 'free)
       #'(lambda (type location)
	   (declare (ignore type))
	   (funcall unref location))))))


(defmethod validate-superclass
    ((class ginstance-class) (super pcl::standard-class))
  (subtypep (class-name super) 'ginstance))


;;;; Registering fundamental types

(register-type 'pointer "gpointer")
(register-type 'char "gchar")
(register-type 'unsigned-char "guchar")
(register-type 'boolean "gboolean")
(register-type 'fixnum "gint")
(register-type 'int "gint")
(register-type 'unsigned-int "guint")
(register-type 'long "glong")
(register-type 'unsigned-long "gulong")
(register-type 'single-float "gfloat")
(register-type 'double-float "gdouble")
(register-type 'string "GString")


;;;; 

(defvar *derivable-type-info* ())

(defun register-derivable-type (type id &key query expand)
  (register-type type id)
  (let* ((type-number (register-type type id))
	 (info (assoc type-number *derivable-type-info*)))
    (if info
	(setf (cdr info) (list query expand))
      (push
       (list type-number query expand)
       *derivable-type-info*))))

(defun type-dependencies (type)
  (let ((query (second (assoc (car (last (type-hierarchy type)))
			      *derivable-type-info*))))
    (when query
      (funcall query (find-type-number type t)))))

(defun expand-type-definition (type)
  (let ((expander (third (assoc (car (last (type-hierarchy type)))
				*derivable-type-info*))))
    (funcall expander (find-type-number type t))))


(defbinding type-parent (type) type-number
  ((find-type-number type t) type-number))

(defun supertype (type)
  (type-from-number (type-parent type)))

(defun type-hierarchy (type)
  (let ((type-number (find-type-number type t)))
    (unless (= type-number 0)
      (cons type-number (type-hierarchy (type-parent type-number))))))
  
(defbinding (type-is-p "g_type_is_a") (type super) boolean
  ((find-type-number type) type-number)
  ((find-type-number super) type-number))

(defbinding %type-children () pointer
  (type-number type-number)
  (num-children unsigned-int :out))

(defun map-subtypes (function type &optional prefix)
  (let ((type-number (find-type-number type t)))
    (multiple-value-bind (array length) (%type-children type-number)
      (unwind-protect
	  (map-c-array
	   'nil
	   #'(lambda (type-number)
	       (when (or
		      (not prefix)
		      (string-prefix-p prefix (find-type-name type-number)))
		 (funcall function type-number))
	       (map-subtypes function type-number prefix))
	   array 'type-number length)
	(deallocate-memory array)))))

(defun find-types (prefix)
  (let ((type-list nil))
    (dolist (type-info *derivable-type-info*)
      (map-subtypes
       #'(lambda (type-number)
	   (push type-number type-list))
       (first type-info) prefix))
    type-list))

(defun %sort-types-topologicaly (unsorted)
  (let ((sorted ()))
    (loop while unsorted do
      (dolist (type unsorted)
	(let ((dependencies (type-dependencies type)))
	  (cond
	   ((null dependencies)
	    (push type sorted)
	    (setq unsorted (delete type unsorted)))
	   (t
	    (unless (dolist (dep dependencies)
		      (when (find type (type-dependencies dep))
			(error "Cyclic type dependencies not yet supported"))
		      (return-if (find dep unsorted)))
	      (push type sorted)
	      (setq unsorted (delete type unsorted))))))))
    (nreverse sorted)))


(defun expand-type-definitions (prefix &optional args)
  (flet ((type-options (type-number)
	   (let ((name (find-type-name type-number)))
	     (cdr (assoc name argss :test #'string=)))))

    (let ((type-list
	   (delete-if
	    #'(lambda (type-number)
		(getf (type-options type-number) :ignore nil))
	    (find-types prefix))))
             
      (dolist (type-number type-list)
	(let ((name (find-type-name type-number)))
	  (register-type
	   (getf (type-options type-number) :type (default-type-name name))
	   type-number)))

      `(progn
	 ,@(mapcar
	    #'expand-type-definition
	    (%sort-types-topologicaly type-list))))))
	    
(defmacro define-types-by-introspection (prefix &rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(expand-type-definitions prefix args)))