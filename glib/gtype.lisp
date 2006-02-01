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

;; $Id: gtype.lisp,v 1.36 2006-02-01 22:48:39 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;; Initialize the glib type system
(defbinding type-init () nil)
(type-init)

(deftype type-number () '(unsigned 32))

(deftype gtype () 'symbol)

(defmethod alien-type ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  (alien-type 'type-number))

(defmethod size-of ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  (size-of 'type-number))

(defmethod to-alien-form (gtype (type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  `(find-type-number ,gtype t)) 

(defmethod to-alien-function ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  #'(lambda (gtype)
      (find-type-number gtype t)))

(defmethod from-alien-form (type-number (type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  `(type-from-number ,type-number)) 

(defmethod from-alien-function ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  #'(lambda (type-number)
      (type-from-number type-number)))

(defmethod writer-function ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  (let ((writer (writer-function 'type-number)))
    #'(lambda (gtype location &optional (offset 0))
	(funcall writer (find-type-number gtype t) location offset))))

(defmethod reader-function ((type (eql 'gtype)) &rest args)
  (declare (ignore type args))
  (let ((reader (reader-function 'type-number)))
    #'(lambda (location &optional (offset 0))
	(type-from-number (funcall reader location offset)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass type-query (struct)
    ((type-number :allocation :alien :type type-number)
     (name :allocation :alien :type string)
     (class-size :allocation :alien :type unsigned-int)
     (instance-size :allocation :alien :type unsigned-int))
    (:metaclass struct-class)))


(defbinding type-query (type) nil
  ((find-type-number type t) type-number)
  ((make-instance 'type-query) type-query :return))

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

(defvar *registered-types* ())
(defvar *registered-type-aliases* ())
(defvar *lisp-type-to-type-number* (make-hash-table))
(defvar *type-number-to-lisp-type* (make-hash-table))

(defbinding %type-from-name () type-number
  (name string))

(defun type-number-from-glib-name (name &optional (error-p t))
  (let ((type-number (%type-from-name name)))
    (cond
     ((not (zerop type-number)) type-number)
     (error-p (error "Invalid gtype name: ~A" name)))))

(defun register-type (type id)
  (pushnew (cons type id) *registered-types* :key #'car)
  (let ((type-number 
	 (typecase id
	   (string (type-number-from-glib-name id))
	   (symbol (funcall id)))))
       (setf (gethash type *lisp-type-to-type-number*) type-number)
       (setf (gethash type-number *type-number-to-lisp-type*) type)
       type-number))

(defun register-type-alias (type alias)
  (pushnew (cons type alias) *registered-type-aliases* :key #'car)
  (setf 
   (gethash type *lisp-type-to-type-number*)
   (find-type-number alias t)))

(defun reinitialize-all-types ()
  (clrhash *lisp-type-to-type-number*)
  (clrhash *type-number-to-lisp-type*)
  (type-init) ; initialize the glib type system
  (mapc #'(lambda (type) 
	    (register-type (car type) (cdr type)))
	*registered-types*)
  (mapc #'(lambda (type) 
	    (register-type-alias (car type) (cdr type)))
	*registered-type-aliases*))

(pushnew 'reinitialize-all-types 
  #+cmu *after-save-initializations*
  #+sbcl *init-hooks*)

#+cmu
(pushnew 'system::reinitialize-global-table ; we shouldn't have to do this?
 *after-save-initializations*)


(defun find-type-number (type &optional error-p)
  (etypecase type
    (integer type)
    (string (type-number-from-glib-name type error-p))
    (symbol
     (or
      (gethash type *lisp-type-to-type-number*)
      (and error-p (error "Type not registered: ~A" type))))
    (class (find-type-number (class-name type) error-p))))
 
(defun type-from-number (type-number &optional error)
  (multiple-value-bind (type found)
      (gethash type-number *type-number-to-lisp-type*)
    (if found
	type
      (let ((name (find-foreign-type-name type-number)))
	(cond
	 ((and name (not (= (type-number-from-glib-name name nil) type-number)))
	  ;; This is a hack because GdkEvent seems to be registered
	  ;; multiple times
	  (type-from-number (type-number-from-glib-name name)))
	 ((and error name)
	  (error "Type number not registered: ~A (~A)" type-number name))
	 ((and error)
	  (error "Invalid type number: ~A" type-number)))))))

(defbinding (find-foreign-type-name "g_type_name") (type) (copy-of string)
  ((find-type-number type t) type-number))

(defun type-number-of (object)
  (find-type-number (type-of object) t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *type-initializers* ())
  (defun %find-types-in-library (pathname prefixes ignore)
    (let ((process (run-program
		    "/usr/bin/nm" (list "--defined-only" "-D" (namestring (truename pathname)))
		    :output :stream :wait nil)))
      (unwind-protect
	  (loop 
	   as symbol = (let ((line (read-line (process-output process) nil)))
			 (when line (subseq line 11)))			  
	   while symbol
	   when (and
		 (> (length symbol) 9)
		 (or 
		  (not prefixes)
		  (some #'(lambda (prefix)
			    (and
			     (> (length symbol) (length prefix))
			     (string= prefix symbol :end2 (length prefix))))
			(mklist prefixes)))
		 (string= "_get_type" symbol :start2 (- (length symbol) 9))
		 (not (member symbol ignore :test #'string=)))
	   collect symbol)
	(process-close process)))))


(defmacro init-types-in-library (filename &key prefix ignore)
  (let ((names (%find-types-in-library filename prefix ignore)))
    `(progn
       ,@(mapcar #'(lambda (name)
		     `(progn
			(defbinding (,(intern name) ,name) () type-number)
			(,(intern name))
			(pushnew ',(intern name) *type-initializers*)))
		 names))))

(defun find-type-init-function (type-number)
  (loop
   for type-init in *type-initializers*
   when (= type-number (funcall type-init))
   do (return type-init)))

(defun register-type-as (type-number)
  (or 
   (find-type-init-function type-number)
   (find-foreign-type-name type-number)
   (error "Unknown type-number: ~A" type-number)))

(defun default-type-init-name (type)
  (find-symbol (format nil "~A_~A_get_type" 
		(package-prefix *package*)
		(substitute #\_ #\- (string-downcase type)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass type-info (struct)
    ((class-size :allocation :alien :type (unsigned 16) :initarg :class-size)
     (base-init :allocation :alien :type pointer)
     (base-finalize :allocation :alien :type pointer)
     (class-init :allocation :alien :type pointer)
     (class-finalize :allocation :alien :type pointer)
     (class-data :allocation :alien :type pointer)
     (instance-size :allocation :alien :type (unsigned 16) 
		    :initarg :instance-size)
     (n-preallocs :allocation :alien :type (unsigned 16))
     (instance-init :allocation :alien :type pointer)
     (value-table :allocation :alien :type pointer))
    (:metaclass struct-class)))

(defbinding %type-register-static () type-number
  (parent-type type-number)
  (name string)
  (info type-info)
  (0 unsigned-int))

(defun register-new-type (type parent &optional foreign-name)
  (let ((parent-info (type-query parent)))
    (with-slots ((parent-number type-number) class-size instance-size) parent-info
      (let ((type-number 
	     (%type-register-static 
	      parent-number
	      (or foreign-name (default-alien-type-name type))
	      (make-instance 'type-info :class-size class-size :instance-size instance-size))))
       (setf (gethash type *lisp-type-to-type-number*) type-number)
       (setf (gethash type-number *type-number-to-lisp-type*) type)
       type-number))))



;;;; Metaclass for subclasses of ginstance

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance-class (proxy-class)
    ((foreign-init))))


(defmethod shared-initialize ((class ginstance-class) names &key name gtype)
  (call-next-method)
  (setf 
   (slot-value class 'foreign-init) 
   (or (first gtype) (default-type-init-name (or name (class-name class))))))


(defmethod finalize-inheritance ((class ginstance-class))
  (call-next-method)
  (let* ((class-name (class-name class))
	 (super (most-specific-proxy-superclass class))
	 (foreign-init (slot-value class 'foreign-init))
	 (type-number
	  (or 
	   (find-type-number class-name)
	   (if (or 
		(symbolp foreign-init)
		(type-number-from-glib-name foreign-init nil))
	       (register-type class-name foreign-init)
	     (register-new-type class-name (class-name super) foreign-init)))))
    (unless (eq (class-name super) (supertype type-number))
      (warn "~A is the super type for ~A in the gobject type system."
       (supertype type-number) class-name))

    (unless (slot-boundp class 'size)
      (setf (slot-value class 'size) (type-instance-size type-number)))))


(defmethod validate-superclass ((class ginstance-class) (super standard-class))
  (subtypep (class-name super) 'ginstance))


;;;; Superclass for wrapping types in the glib type system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance (proxy)
    ((class :allocation :alien :type pointer))
    (:metaclass proxy-class)))

(defun %type-number-of-ginstance (location)
  (let ((class (sap-ref-sap location 0)))
    (sap-ref-32 class 0)))

(defmethod ensure-proxy-instance ((class ginstance-class) location)
  (declare (ignore class))
  (let ((class (labels ((find-known-class (type-number)
		          (or
			   (find-class (type-from-number type-number) nil)
			   (unless (zerop type-number)
			     (find-known-class (type-parent type-number))))))
		 (find-known-class (%type-number-of-ginstance location)))))
    (if class
	(make-instance class :location (reference-foreign class location))
      (error "Object at ~A has an unkown type number: ~A" 
        location (%type-number-of-ginstance location)))))

(defmethod copy-from-alien-form (location (class ginstance-class) &rest args)
  (declare (ignore location class args))
  (error "Doing copy-from-alien on a ref. counted class is most certainly an error, but if it really is what you want you should use REFERENCE-FOREIGN on the returned instance instead."))

(defmethod copy-from-alien-function ((class ginstance-class) &rest args)
  (declare (ignore class args))  
  (error "Doing copy-from-alien on a ref. counted class is most certainly an error, but if it really is what you want you should use REFERENCE-FOREIGN on the returned instance instead."))

(defmethod reader-function ((class ginstance-class) &rest args)
  (declare (ignore args))
  #'(lambda (location &optional (offset 0))
      (ensure-proxy-instance class (sap-ref-sap location offset))))


;;;; Registering fundamental types

(register-type 'nil "void")
(register-type 'pointer "gpointer")
(register-type 'char "gchar")
(register-type 'unsigned-char "guchar")
(register-type 'boolean "gboolean")
(register-type 'int "gint")
(register-type-alias 'integer 'int)
(register-type-alias 'fixnum 'int)
(register-type 'unsigned-int "guint")
(register-type 'long "glong")
(register-type 'unsigned-long "gulong")
(register-type 'single-float "gfloat")
(register-type 'double-float "gdouble")
(register-type 'pathname "gchararray")
(register-type 'string "gchararray")


;;;; Introspection of type information

(defvar *derivable-type-info* (make-hash-table))

(defun register-derivable-type (type id expander &optional dependencies)
  (register-type type id)
  (let ((type-number (register-type type id)))
    (setf 
     (gethash type-number *derivable-type-info*) 
     (list expander dependencies))))

(defun find-type-info (type)
  (dolist (super (cdr (type-hierarchy type)))
    (let ((info (gethash super *derivable-type-info*)))
      (return-if info))))

(defun expand-type-definition (type forward-p options)
  (let ((expander (first (find-type-info type))))
    (funcall expander (find-type-number type t) forward-p options)))

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
	(map-c-vector 'list #'identity array 'type-number length)
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
	  (map-c-vector
	   'nil
	   #'(lambda (type-number)
	       (when (or
		      (not prefix)
		      (string-prefix-p prefix (find-foreign-type-name type-number)))
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

(defun find-type-dependencies (type)
  (let ((list-dependencies (second (find-type-info type))))
    (when list-dependencies
      (funcall list-dependencies (find-type-number type t)))))

(defun %sort-types-topologicaly (types)
  (let ((partial-sorted
	 (sort
	  (mapcar 
	   #'(lambda (type)
	       (cons type (remove-if #'(lambda (dep)
					 (not (find dep types)))
				     (find-type-dependencies type))))
	   types)
	  #'(lambda (type1 type2) (type-is-p type2 type1)) :key #'car))
	(sorted ()))

    (loop
     as tmp = partial-sorted then (or (rest tmp) partial-sorted)
     while tmp
     do (destructuring-bind (type . dependencies) (first tmp)
	  (cond
	   ((every #'(lambda (dep)
		       (assoc dep sorted))
		   dependencies)
	    (push (cons type nil) sorted) ; no forward definition needed
	    (setq partial-sorted (delete type partial-sorted :key #'first)))
	   ((some #'(lambda (dep)
		      (find type (find-type-dependencies dep)))
		  dependencies)
	    (push (cons type t) sorted) ; forward definition needed
	    (setq partial-sorted (delete type partial-sorted :key #'first))))))
    (nreverse sorted)))


(defun expand-type-definitions (prefix &optional args)
  (flet ((type-options (type-number)
	   (let ((name (find-foreign-type-name type-number)))
	     (cdr (assoc name args :test #'string=)))))

   (let ((type-list
	  (delete-if
	   #'(lambda (type-number)
	       (let ((name (find-foreign-type-name type-number)))
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
       (let ((name (find-foreign-type-name type-number)))
	 (register-type
	  (getf (type-options type-number) :type (default-type-name name))
	  (register-type-as type-number))))

     (let ((sorted-type-list (%sort-types-topologicaly type-list)))
       `(progn
	  ,@(mapcar
	     #'(lambda (pair)
		 (destructuring-bind (type . forward-p) pair
		   (expand-type-definition type forward-p (type-options type))))
	     sorted-type-list)
	  ,@(mapcar
	     #'(lambda (pair)
		 (destructuring-bind (type . forward-p) pair
		   (when forward-p
		     (expand-type-definition type nil (type-options type)))))
	     sorted-type-list))))))

(defmacro define-types-by-introspection (prefix &rest args)
  (expand-type-definitions prefix args))


;;;; Initialize all non static types in GObject

(init-types-in-library #.(concatenate 'string (pkg-config:pkg-variable "glib-2.0" "libdir") "/libgobject-2.0.so"))
