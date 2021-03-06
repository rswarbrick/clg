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

;; $Id: gtype.lisp,v 1.69 2009-02-10 15:16:34 espen Exp $

(in-package "GLIB")

(use-prefix "g")

;; Initialize the glib type system
(defbinding type-init () nil)
(type-init)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defbinding (bitsize-of-gtype "bitsize_of_gtype") () unsigned-int))

(deftype type-number () `(unsigned-byte ,(bitsize-of-gtype)))

(deftype gtype () 'symbol)

(define-type-method alien-type ((type gtype))
  (declare (ignore type))
  (alien-type 'type-number))

(define-type-method size-of ((type gtype) &key (inlined t))
  (assert-inlined type inlined)
  (size-of 'type-number))

(define-type-method to-alien-form ((type gtype) gtype &optional copy-p)
  (declare (ignore type copy-p))
  `(find-type-number ,gtype t)) 

(define-type-method to-alien-function ((type gtype) &optional copy-p)
  (declare (ignore type copy-p))
  #'(lambda (gtype)
      (find-type-number gtype t)))

(define-type-method from-alien-form ((type gtype) form &key ref)
  (declare (ignore type ref))
  `(type-from-number ,form))

(define-type-method from-alien-function ((type gtype) &key ref)
  (declare (ignore type ref))
  #'(lambda (type-number)
      (type-from-number type-number)))

(define-type-method writer-function ((type gtype) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (let ((writer (writer-function 'type-number)))
    #'(lambda (gtype location &optional (offset 0))
	(funcall writer (find-type-number gtype t) location offset))))

(define-type-method reader-function ((type gtype) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (let ((reader (reader-function 'type-number)))
    #'(lambda (location &optional (offset 0))
	(type-from-number (funcall reader location offset)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass type-query (struct)
    ((type-number :allocation :alien :type type-number)
     (name :allocation :alien :type (copy-of string))
     (class-size :allocation :alien :type unsigned-int)
     (instance-size :allocation :alien :type unsigned-int))
    (:metaclass struct-class)))


(defbinding type-query (type) nil
  ((find-type-number type t) type-number)
  ((make-instance 'type-query) type-query :in/return))

(defun type-instance-size (type)
  (slot-value (type-query type) 'instance-size))

(defun type-class-size (type)
  (slot-value (type-query type) 'class-size))

(defbinding type-class-ref (type) pointer
  ((find-type-number type t) type-number))

(defbinding type-class-unref () nil
  (class pointer))

(defbinding type-class-peek (type) pointer
  ((find-type-number type t) type-number))



;;;; Mapping between lisp types and glib types

(defvar *registered-types* ())
(defvar *registered-type-aliases* ())
(defvar *registered-static-types* ())
(defvar *lisp-type-to-type-number* (make-hash-table))
(defvar *type-number-to-lisp-type* (make-hash-table))

(defbinding %type-from-name () type-number
  (name string))

(defun type-number-from-glib-name (name &optional (error-p t))
  (let ((type-number (%type-from-name name)))
    (cond
     ((not (zerop type-number)) type-number)
     (error-p (error "Invalid gtype name: ~A" name)))))

(defun type-from-glib-name (name)
  (type-from-number (type-number-from-glib-name name) t))

(defun type-registered-p (type)
  (nth-value 1 (gethash type *lisp-type-to-type-number*)))

(defun register-type (type id &optional (error-p t))
  (cond
   ((type-registered-p type) (find-type-number type))
   ((not id) (warn "Can't register type with no foreign id: ~A" type))
   (t    
    (pushnew (cons type id) *registered-types* :key #'car)
    (let ((type-number 
	   (typecase id
	     (string (type-number-from-glib-name id error-p))
	     (symbol (funcall id)))))
      (setf (gethash type *lisp-type-to-type-number*) type-number)
      (setf (gethash type-number *type-number-to-lisp-type*) type)
      type-number))))

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
	    (register-type (car type) (cdr type) nil))
	*registered-types*)
  (mapc #'(lambda (type) 
	    (apply #'register-new-type type))
	(reverse *registered-static-types*))
  (mapc #'(lambda (type) 
	    (register-type-alias (car type) (cdr type)))
	*registered-type-aliases*))

#+cmu
(asdf:install-init-hook 'system::reinitialize-global-table 
 *after-save-initializations*) ; we shouldn't need to do this?
(asdf:install-init-hook 'reinitialize-all-types)



(defun find-type-number (type &optional error-p)
  (etypecase type
    (integer type)
    (string (type-number-from-glib-name type error-p))
    (symbol
     (or
      (gethash type *lisp-type-to-type-number*)
      (let ((class (find-class type nil)))
	(when (and class (not (class-finalized-p class)))
	  (finalize-inheritance class)
	  (gethash type *lisp-type-to-type-number*)))
      (and error-p (error "Type not registered: ~A" type))))
    (class 
     (find-type-number (class-name type) error-p))))
 
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

;; For #+(SBCL WIN32):
;;   The first 2 lines of the output from "pexports" are:
;;   LIBRARY XXX.dll
;;   EXPORTS
;;   We don't do anything to skip these 2 lines because they won't pass the
;;   WHEN (AND ...) in the LOOP
;;   - cph 19-May-2007

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *type-initializers* ())

  (defun library-filename (system library)
  (let ((component (asdf:find-component (asdf:find-system system) library)))
    (etypecase component
      (asdf:shared-object 
       (first (asdf:output-files (make-instance 'asdf:compile-op) component)))
      (asdf:library (asdf:component-pathname component)))))

  (defun %find-types-in-library (pathname prefixes ignore)
    (let ((outname (tmpname "types")))
      (unwind-protect
	  (let ((asdf::*verbose-out* nil))
	    #-win32
	    (asdf:run-shell-command "nm ~A ~A > ~A"
	     #-darwin "--defined-only --dynamic --extern-only"
	     #+darwin "-f -s __TEXT __text"
	     (namestring (truename pathname)) outname)
	    ;; Note about win32 port:
	    ;;   1. (TRUENAME PATHNAME) will bomb.
	    ;;   2. either
	    ;;        pexports "d:\\whatever\\bin\\zlib1.dll"
	    ;;      or
	    ;;        pexports d:/whatever/bin/zlib1.dll
	    ;;      anything else will bomb.  this is why ~S is used below.
	    #+win32
	    (asdf:run-shell-command "pexports ~S > ~A" 
	     (namestring pathname) outname)

	    (with-open-file (output outname)
	      (loop 
	       as line = (read-line output nil)
	       as symbol = (when line
			     #-win32
			     (let ((pos (position #\space line :from-end t)))
			       #-darwin(subseq line (1+ pos))
			       #+darwin
			       (when (char= (char line (1- pos)) #\T)
				 (subseq line (+ pos 2))))
			     #+win32
			     (subseq line 0 (1- (length line))))
	       while line
	       when (and
		     symbol (> (length symbol) 9)
		     (not (char= (char symbol 0) #\_))
		     (or 
		      (not prefixes)
		      (some #'(lambda (prefix)
				(and
				 (> (length symbol) (length prefix))
				 (string= prefix symbol :end2 (length prefix))))
		       (mklist prefixes)))
		     (string= "_get_type" symbol :start2 (- (length symbol) 9))
		     (not (member symbol ignore :test #'string=)))
	       collect symbol)))
	(delete-file outname)))))


(defun car-eq-p (ob1 ob2)
  (eq (car ob1) (car ob2)))

(defmacro init-types-in-library (system library &key prefix ignore)
  (let* ((filename (library-filename system library))
	 (names (%find-types-in-library filename prefix ignore)))
    `(progn
       ,@(mapcar 
	  #'(lambda (name)	      
	      `(progn
		 (defbinding (,(intern name) ,name) () type-number)
		 (,(intern name))
		 (pushnew (cons ',(intern name) ,filename) *type-initializers* 
		  :test #'car-eq-p)))
	  names))))

(defun find-type-init-function (type-number)
  (loop
   for (type-init) in *type-initializers*
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
	(pushnew (list type parent foreign-name) *registered-static-types* :key #'car)
	(setf (gethash type *lisp-type-to-type-number*) type-number)
	(setf (gethash type-number *type-number-to-lisp-type*) type)
	type-number))))



;;;; Metaclass for subclasses of ginstance

(defvar *referenced-ginstance-classes* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance-class (proxy-class)
    ((gtype :initarg :gtype :initform nil :reader ginstance-class-gtype))))


(defun update-size (class)
  (let ((type-number (find-type-number class)))
    (cond
     ((not (foreign-size-p class))
      (setf (foreign-size class) (type-instance-size type-number)))
     ((and 
       (foreign-size-p class)
       (not (= (type-instance-size type-number) (foreign-size class))))
      (warn "Size mismatch for class ~A" class)))))


(defmethod finalize-inheritance ((class ginstance-class))
  (prog1
      #+clisp(call-next-method)
    (let* ((class-name (class-name class))
	   (super (most-specific-proxy-superclass class))
	   (gtype (or 
		   (first (ginstance-class-gtype class))
		   (default-alien-type-name class-name))))
      (unless (type-registered-p class-name)
	(type-class-ref
	 (if (or (symbolp gtype) (type-number-from-glib-name gtype nil))
	     (register-type class-name gtype)
	   (register-new-type class-name (class-name super) gtype)))
	(push class-name *referenced-ginstance-classes*))
      #+nil
      (when (and
	     (supertype (find-type-number class))
	     (not (eq (class-name super) (supertype (find-type-number class)))))
	(warn "Super class mismatch between CLOS and GObject for ~A" 
	      class-name)))
    (update-size class))
  #-clisp(call-next-method))

(defun reinitialize-ginstance-classes ()
  (mapc #'type-class-ref *referenced-ginstance-classes*))

(asdf:install-init-hook 'reinitialize-ginstance-classes)



(defmethod shared-initialize ((class ginstance-class) names &rest initargs)
  (declare (ignore names initargs))
  (call-next-method)
  (when (class-finalized-p class)
    (update-size class)))


(defmethod validate-superclass ((class ginstance-class) (super standard-class))
  (subtypep (class-name super) 'ginstance))


;;;; Superclass for wrapping types in the glib type system

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ginstance (ref-counted-object)
    (;(class :allocation :alien :type pointer :offset 0)
     )
    (:metaclass proxy-class)
    (:size #.(size-of 'pointer))))

(defun ref-type-number (location &optional offset)
  (declare (ignore location offset)))

(setf (symbol-function 'ref-type-number) (reader-function 'type-number))

(defun %type-number-of-ginstance (location)
  (let ((class (ref-pointer location)))
    (ref-type-number class)))

(defmethod make-proxy-instance :around ((class ginstance-class) location 
					&rest initargs)
  (declare (ignore class))
  (let ((class (labels ((find-known-class (type-number)
		          (or
			   (find-class (type-from-number type-number) nil)
			   (unless (zerop type-number)
			     (find-known-class (type-parent type-number))))))
		 (find-known-class (%type-number-of-ginstance location)))))
    ;; Note that changing the class argument must not alter "the
    ;; ordered set of applicable methods" as specified in the
    ;; Hyperspec
    (if class
	(apply #'call-next-method class location initargs)
      (error "Object at ~A has an unkown type number: ~A"
       location (%type-number-of-ginstance location)))))


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
(register-type 'string "gchararray")
(register-type-alias 'pathname 'string)


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

(defun find-type-dependencies (type &optional options)
  (let ((find-dependencies (second (find-type-info type))))
    (when find-dependencies
      (remove-duplicates
       (mapcar #'find-type-number
        (funcall find-dependencies (find-type-number type t) options))))))


;; The argument is a list where each elements is on the form 
;; (type . dependencies). This function will not handle indirect
;; dependencies and types depending on them selves.
(defun sort-types-topologicaly (unsorted)
  (flet ((depend-p (type1)
           (find-if #'(lambda (type2)
			(and
			 ;; If a type depends a subtype it has to be
			 ;; forward defined
			 (not (type-is-p (car type2) (car type1)))
			 (find (car type2) (cdr type1))))
		    unsorted)))
    (let ((sorted
	   (loop
	    while unsorted
	    nconc (multiple-value-bind (sorted remaining)
		      (delete-collect-if 
		       #'(lambda (type)
			   (or (not (cdr type)) (not (depend-p type))))
		       unsorted)
		    (cond
		     ((not sorted)
		      ;; We have a circular dependency which have to
		      ;; be resolved
		      (let ((selected
			     (find-if 
			      #'(lambda (type)			
				  (every 
				   #'(lambda (dep)
				       (or
					(not (type-is-p (car type) dep))
					(not (find dep unsorted :key #'car))))
				   (cdr type)))
			      unsorted)))
			(unless selected
			  (error "Couldn't resolve circular dependency"))
			(setq unsorted (delete selected unsorted))
			(list selected)))
		     (t
		      (setq unsorted remaining)
		      sorted))))))

      ;; Mark types which have to be forward defined
      (loop
       for tmp on sorted
       as (type . dependencies) = (first tmp)
       collect (cons type (and
			   dependencies
			   (find-if #'(lambda (type)
					(find (car type) dependencies))
				    (rest tmp))
			   t))))))


(defun expand-type-definitions (type-list &optional args)
  (flet ((type-options (type-number)
	   (let ((name (find-foreign-type-name type-number)))
	     (cdr (assoc name args :test #'string=)))))

    (setq type-list
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
      type-list))

    (dolist (type-number type-list)
      (let ((name (find-foreign-type-name type-number)))
	(register-type
	 (getf (type-options type-number) :type (default-type-name name))
	 (register-type-as type-number))))

    ;; This is needed for some unknown reason to get type numbers right
    (mapc #'find-type-dependencies type-list)

    (let ((sorted-type-list 
	   #+clisp (mapcar #'list type-list)
	   #-clisp
	   (sort-types-topologicaly 
	    (mapcar 
	     #'(lambda (type)
		 (cons type (find-type-dependencies type (type-options type))))
	     type-list))))
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
	    sorted-type-list)))))

(defun expand-types-with-prefix (prefix args)
  (expand-type-definitions (find-types prefix) args))

(defun expand-types-in-library (system library args)
  (let* ((filename (library-filename system library))
	 (types (loop 
	         for (type-init . %filename) in *type-initializers*
		 when (equal filename %filename)
		 collect (funcall type-init))))
    (expand-type-definitions types args)))

(defun list-types-in-library (system library)
  (let ((filename (library-filename system library)))
    (loop 
     for (type-init . %filename) in *type-initializers*
     when (equal filename %filename)
     collect type-init)))

(defmacro define-types-by-introspection (prefix &rest args)
  (expand-types-with-prefix prefix args))

(defexport define-types-by-introspection (prefix &rest args)
  (list-autoexported-symbols (expand-types-with-prefix prefix args)))

(defmacro define-types-in-library (system library &rest args)
  (expand-types-in-library system library args))

(defexport define-types-in-library (system library &rest args)
  (list-autoexported-symbols (expand-types-in-library system library args)))


;;;; Initialize all non static types in GObject

(init-types-in-library glib "libgobject-2.0")
