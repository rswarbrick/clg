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

;; $Id: gtype.lisp,v 1.18 2004-10-31 11:41:06 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;; Initialize the glib type system
(defbinding type-init () nil)
(type-init)

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
	   (string (find-type-number id t))
	   (symbol (gethash id *type-to-number-hash*)))))
    (setf (gethash type *type-to-number-hash*) type-number)
    (unless (symbolp id)
      (setf (gethash type-number *number-to-type-hash*) type))
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
 
(defun type-from-number (type-number &optional error)
  (multiple-value-bind (type found)
      (gethash type-number *number-to-type-hash*)
    (when (and error (not found))
      (let ((name (find-type-name type-number)))
	(if name
	    (error "Type number not registered: ~A (~A)" type-number name)
	  (error "Invalid type number: ~A" type-number))))
    type))

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

(defun %init-types-in-library (pathname ignore)
  (let ((process (ext:run-program
		  "nm" (list "-D" (namestring (truename pathname)))
		  :output :stream :wait nil))
	(fnames ()))
    (labels ((read-symbols ()
	       (let ((line (read-line (ext:process-output process) nil)))
		 (when line
		   (let ((symbol (subseq line 11)))
		     (when (and
			    (search "_get_type" symbol)
			    (not (member symbol ignore :test #'string=)))
		       (push symbol fnames)))
		   (read-symbols)))))
      (read-symbols)
      (ext:process-close process)
      `(init-type ',fnames))))

(defmacro init-types-in-library (filename &key ignore)
  (%init-types-in-library
   (format nil "~A/~A" *gtk-library-path* filename) ignore))



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
  (defclass ginstance-class (proxy-class)
    ()))


(defmethod shared-initialize ((class ginstance-class) names
			      &rest initargs &key name alien-name
			      ref unref)
  (declare (ignore initargs names))
  (let* ((class-name (or name (class-name class)))
	 (type-number
	  (find-type-number
	   (or (first alien-name) (default-alien-type-name class-name)) t)))
    (register-type class-name type-number)
    (if (getf initargs :size)
	(call-next-method)
      (let ((size (type-instance-size type-number)))
	(apply #'call-next-method class names :size (list size) initargs))))

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
(register-type 'string "gchararray")


;;;; 

(defvar *derivable-type-info* (make-hash-table))

(defun register-derivable-type (type id expander)
  (register-type type id)
  (let ((type-number (register-type type id)))
    (setf (gethash type-number *derivable-type-info*) expander)))

(defun find-type-info (type)
  (dolist (super (cdr (type-hierarchy type)))
    (let ((info (gethash super *derivable-type-info*)))
      (return-if info))))

(defun expand-type-definition (type options)
  (let ((expander (find-type-info type)))
    (funcall expander (find-type-number type t) options)))

(defbinding type-parent (type) type-number
  ((find-type-number type t) type-number))

(defun supertype (type)
  (type-from-number (type-parent type)))

(defbinding %type-interfaces (type) pointer
  ((find-type-number type t) type-number)
  (n-interfaces unsigned-int :out))

(defun type-interfaces (type)
  (multiple-value-bind (array length) (%type-interfaces type)
    (unwind-protect
	(map-c-array 'list #'identity array 'type-number length)
      (deallocate-memory array))))

(defun implements (type)
  (mapcar #'type-from-number (type-interfaces type)))

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
    (maphash
     #'(lambda (type-number expander)
	 (declare (ignore expander))
	 (map-subtypes
	  #'(lambda (type-number)
	      (pushnew type-number type-list))
	  type-number prefix))
     *derivable-type-info*)
    type-list))

(defun %sort-types-topologicaly (unsorted)
  (let ((sorted ()))
    (loop while unsorted do
      (dolist (type unsorted)
	(let ((dependencies
	       (append (rest (type-hierarchy type)) (type-interfaces type))))
	  (cond
	   ((null dependencies)
	    (push type sorted)
	    (setq unsorted (delete type unsorted)))
	   (t
	    (unless (dolist (dep dependencies)
		      (when (find type (rest (type-hierarchy dep)))
			(error "Cyclic type dependencie"))
		      (return-if (find dep unsorted)))
	      (push type sorted)
	      (setq unsorted (delete type unsorted))))))))
    (nreverse sorted)))


(defun expand-type-definitions (prefix &optional args)
 (flet ((type-options (type-number)
	   (let ((name (find-type-name type-number)))
	     (cdr (assoc name args :test #'string=)))))

   (let ((type-list
	  (delete-if
	   #'(lambda (type-number)
	       (let ((name (find-type-name type-number)))
		 (or
		  (getf (type-options type-number) :ignore)
		  (find-if
		   #'(lambda (options)
		       (and
			(string-prefix-p (first options) name)
			(getf (cdr options) :ignore-prefix)
			(not (some
			      #'(lambda (exception)
				  (string= name exception))
			      (getf (cdr options) :except)))))
		   args))))
	   (find-types prefix))))
             
     (dolist (type-number type-list)
       (let ((name (find-type-name type-number)))
	 (register-type
	  (getf (type-options type-number) :type (default-type-name name))
	  type-number)))
    
     `(progn
	,@(mapcar
	   #'(lambda (type)
	       (expand-type-definition type (type-options type)))
	   (%sort-types-topologicaly type-list))))))

(defmacro define-types-by-introspection (prefix &rest args)
  (expand-type-definitions prefix args))



