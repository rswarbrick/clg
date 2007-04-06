;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: interface.lisp,v 1.5 2007-04-06 16:06:24 espen Exp $

(in-package "GFFI")


;;;; Foreign function call interface

(defvar *package-prefix* nil)

(defun set-package-prefix (prefix &optional (package *package*))
  (let ((package (find-package package)))
    (setq *package-prefix* (delete package *package-prefix* :key #'car))
    (push (cons package prefix) *package-prefix*))
  prefix)

(defun package-prefix (&optional (package *package*))
  (let ((package (find-package package)))
    (or
     (cdr (assoc package *package-prefix*))
     (substitute #\_ #\- (string-downcase (package-name package))))))

(defun find-prefix-package (prefix)
  (or
   (car (rassoc (string-downcase prefix) *package-prefix* :test #'string=))
   (find-package (string-upcase prefix))))

(defmacro use-prefix (prefix &optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-package-prefix ,prefix ,package)))


(defun default-alien-fname (lisp-name)
  (let* ((name (substitute #\_ #\- (string-downcase lisp-name)))
	 (start (position-if-not #'(lambda (char) (char= char #\%)) name))
	 (end (if (string= "_p" name :start2 (- (length name) 2))
		  (- (length name) 2)
		(length name)))
	 (stripped-name (subseq name start end))
	 (prefix (package-prefix *package*)))
    (if (or (not prefix) (string= prefix ""))
	stripped-name
      (format nil "~A_~A" prefix stripped-name))))

(defun default-alien-type-name (type-name)
  (let ((prefix (package-prefix *package*)))
    (apply
     #'concatenate
     'string
     (mapcar
      #'string-capitalize    
      (cons prefix (split-string (symbol-name type-name) :delimiter #\-))))))

(defun default-type-name (alien-name)
  (let ((parts
	 (mapcar
	  #'string-upcase
	  (split-string-if alien-name #'upper-case-p))))
    (intern
     (concatenate-strings (rest parts) #\-)
     (find-prefix-package (first parts)))))


(defun in-arg-p (style)
  (find style '(:in :in/out :in/return :in-out :return)))

(defun out-arg-p (style)
  (find style '(:out :in/out :in-out)))

(defun return-arg-p (style)
  (find style '(:in/return :return)))

(defmacro defbinding (name lambda-list return-type &rest args)
  (multiple-value-bind (lisp-name c-name)
      (if (atom name)
 	  (values name (default-alien-fname name))
 	(values-list name))
		       
    (let* ((lambda-list-supplied-p lambda-list)
	   (lambda-list (unless (equal lambda-list '(nil)) lambda-list))
	   (aux-vars ())
	   (doc-string (when (stringp (first args)) (pop args)))
	   (parsed-args	         
	    (mapcar 
	     #'(lambda (arg)
		 (destructuring-bind 
		     (expr type &optional (style :in) (out-type type)) arg
		   (cond
		    ((find style '(:in-out :return))
		     (warn "Deprecated argument style: ~S" style))
		    ((not (find style '(:in :out :in/out :in/return)))
		     (error "Bogus argument style: ~S" style)))
		   (when (and 
			  (not lambda-list-supplied-p) 
			  (namep expr) (in-arg-p style))
		     (push expr lambda-list))
		   (let ((aux (unless (or (not (in-arg-p style)) (namep expr))
				(gensym))))
		     (when aux
		       (push `(,aux ,expr) aux-vars))
		     (list 
		      (cond 
		       ((and (namep expr) (not (in-arg-p style))) expr)
		       ((namep expr) (make-symbol (string expr)))
		       ((gensym)))
		      (or aux expr) type style out-type))))
	     args)))
  
      (%defbinding c-name lisp-name
       (if lambda-list-supplied-p lambda-list (nreverse lambda-list))
       aux-vars return-type doc-string parsed-args))))


#+(or cmu sbcl)
(defun foreign-funcall (cname args return-type)
  (let ((fparams (loop
		  for (var expr type style out-type) in args
		  collect (if (out-arg-p style)
			      `(addr ,var)
			    var)))
	(ftypes (loop
		 for (var expr type style out-type) in args
		 collect (if (out-arg-p style)
			     `(* ,(alien-type out-type))
			   (alien-type out-type))))
	(fname (make-symbol cname)))
    `(with-alien ((,fname (function ,(alien-type return-type) ,@ftypes) :extern ,cname))
      (alien-funcall ,fname ,@fparams))))

#+clisp
(defun foreign-funcall (cname args return-type)
  (let* ((fparams (loop
		   for (var expr type style out-type) in args
		   collect (if (out-arg-p style)
			       `(ffi:c-var-address ,var)
			     var)))
	 (fargs (loop
		 for (var expr type style out-type) in args
		 collect (list var (if (out-arg-p style)
				       'ffi:c-pointer
				     (alien-type out-type)))))
	 (c-function `(ffi:c-function 
		       (:arguments ,@fargs)
		       (:return-type ,(alien-type return-type))
		       (:language :stdc))))
    `(funcall
      (load-time-value
       (ffi::foreign-library-function 
	,cname (ffi::foreign-library :default) #?(clisp>= 2 40)nil
	nil (ffi:parse-c-type ',c-function)))
      ,@fparams)))


;; TODO: check if in and out types (if different) translates to same
;; alien type
(defun %defbinding (cname lisp-name lambda-list aux-vars return-type doc args)
  (let ((out (loop
	      for (var expr type style out-type) in args
	      when (or (out-arg-p style) (return-arg-p style))
	      collect (from-alien-form out-type var)))
	(fcall (from-alien-form return-type 
		(foreign-funcall cname args return-type))))

    (labels ((create-wrapper (args body)
	       (if args
		   (destructuring-bind (var expr type style out-type) (first args)
		     (declare (ignore out-type))
		     (alien-arg-wrapper type var expr style
		      (create-wrapper (rest args) body)))
		 body)))
       `(defun ,lisp-name ,lambda-list
	  ,doc
	  (let ,aux-vars
	    ,(if return-type
		 (create-wrapper args `(values ,fcall ,@out))
	       (create-wrapper args `(progn ,fcall (values ,@out)))))))))



;;;; Dynamic (runtime) bindings

(defun mkbinding (name return-type &rest arg-types)
  #+cmu(declare (optimize (inhibit-warnings 3)))
  #+sbcl(declare (muffle-conditions compiler-note))
  (let* ((c-function
	  #+(or cmu sbcl)
	  `(function ,@(mapcar #'alien-type (cons return-type arg-types)))
	  #+clisp
	  `(ffi:c-function 
	    (:arguments ,@(mapcar 
			   #'(lambda (type)
			       (list (gensym) (alien-type type)))
			   arg-types)) 
	    (:return-type ,(alien-type return-type))
	    (:language :stdc)))
	 (foreign
	  #+(or cmu sbcl)
	  (handler-bind (#+sbcl(compiler-note #'(lambda (condition)
						  (declare (ignore condition))
						  (muffle-warning))))
	    (%heap-alien
	     (make-heap-alien-info
	      :type (parse-alien-type c-function #+sbcl nil)
	      :sap-form (let ((address (foreign-symbol-address name)))
			  (etypecase address
			    (integer (int-sap address))
			    (system-area-pointer address))))))
	  #+clisp
	  (ffi::foreign-library-function name 
	   (ffi::foreign-library :default) #?(clisp>= 2 40)nil
	   nil (ffi:parse-c-type c-function)))
	 (return-value-translator (from-alien-function return-type)))
    (multiple-value-bind (arg-translators cleanup-funcs)
	(let ((translator/cleanup-pairs
	       (mapcar 
		#'(lambda (type)
		    (multiple-value-list (to-alien-function type)))
		arg-types)))
	  (values 
	   (mapcar #'first translator/cleanup-pairs)
	   (mapcar #'second translator/cleanup-pairs)))
      #'(lambda (&rest args)
	  (let ((translated-args (mapcar #'funcall arg-translators args)))
	    (prog1
		(funcall return-value-translator 
		 #+(or cmu sbcl)(apply #'alien-funcall foreign translated-args)
		 #+clisp(apply foreign translated-args))
	      (mapc 
	       #'(lambda (cleanup arg translated-arg)
		   (when cleanup
		     (funcall cleanup arg translated-arg)))
	       cleanup-funcs args translated-args)))))))



;;;; C Callbacks

(defun callback-body (args return-type body)
  (labels ((create-wrappers (args body)
	     (if args
		 (destructuring-bind (var type) (first args)
		   (callback-wrapper type var var
		    (create-wrappers (rest args) body)))
	       body))
	   (create-body (args body)
	     (to-alien-form return-type 
	      (create-wrappers args `(progn ,@body)))))
    (if (and (consp (first body)) (eq (caar body) 'declare))
	(let ((ignored (loop
			for declaration in (cdar body)
			when (eq (first declaration) 'ignore)
			nconc (rest declaration))))
	  `(,(first body)
	    ,(create-body 
	      (remove-if #'(lambda (arg)
			     (find (first arg) ignored))
			 args)
	      (rest body))))
      (list (create-body args body)))))


#+(or cmu sbcl)
(defmacro define-callback (name return-type args &body body)
  (let ((define-callback 
	  #+cmu'alien:def-callback 	              
	  #+(and sbcl alien-callbacks)'sb-alien::define-alien-callback
	  #+(and sbcl (not alien-callbacks))'sb-alien:define-alien-function))
    `(progn
       #+cmu(defparameter ,name nil)
       (,define-callback ,name 
	   #+(and sbcl alien-callbacks) ,(alien-type return-type) 
	   (#+(or cmu (and sbcl (not alien-callbacks))),(alien-type return-type)
	    ,@(loop
	       for (name type) in args
	       collect `(,name ,(alien-type type))))
	 ,@(callback-body args return-type body)))))

#+(or cmu sbcl)	   
(defun callback-address (callback)
  #+cmu(alien::callback-trampoline callback)
  #+(and sbcl (not alien-callbacks))(sb-alien:alien-function-sap callback)
  #+(and sbcl alien-callbacks)(sb-alien:alien-sap callback))

#+sbcl
(deftype callback () 
  #-alien-callbacks'sb-alien:alien-function
  #+alien-callbacks'sb-alien:alien)


;;; The callback code for CLISP is based on code from CFFI
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
;;;           (C) 2005, Joerg Hoehle  <hoehle@users.sourceforge.net>


;;; *CALLBACKS* contains the callbacks defined by the %DEFCALLBACK
;;; macro.  The symbol naming the callback is the key, and the value
;;; is a list containing a Lisp function, the parsed CLISP FFI type of
;;; the callback, and a saved pointer that should not persist across
;;; saved images.
#+clisp
(progn
  (defvar *callbacks* (make-hash-table))

  ;;; Return a CLISP FFI function type for a CFFI callback function
  ;;; given a return type and list of argument names and types.
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun callback-type (return-type arg-names arg-types)
      (ffi:parse-c-type
       `(ffi:c-function
	 (:arguments ,@(mapcar (lambda (sym type)
				 (list sym (alien-type type)))
			       arg-names arg-types))
	 (:return-type ,(alien-type return-type))
	 (:language :stdc)))))
  
  ;;; Register and create a callback function.
  (defun register-callback (name function parsed-type)
    (setf (gethash name *callbacks*)
	  (list function parsed-type
		(ffi:with-foreign-object (ptr 'ffi:c-pointer)
                 ;; Create callback by converting Lisp function to foreign
		 (setf (ffi:memory-as ptr parsed-type) function)
                 (ffi:foreign-value ptr)))))

  ;;; Restore all saved callback pointers when restarting the Lisp
  ;;; image.  This is pushed onto CUSTOM:*INIT-HOOKS*.
  ;;; Needs clisp > 2.35, bugfix 2005-09-29
  (defun restore-callback-pointers ()
    (maphash
     (lambda (name list)
       (register-callback name (first list) (second list)))
     *callbacks*))

  ;;; Add RESTORE-CALLBACK-POINTERS to the lists of functions to run
  ;;; when an image is restarted.
  (eval-when (:load-toplevel :execute)
    (pushnew 'restore-callback-pointers custom:*init-hooks*))

  ;;; Define a callback function NAME to run BODY with arguments
  ;;; ARG-NAMES translated according to ARG-TYPES and the return type
  ;;; translated according to RETTYPE.  Obtain a pointer that can be
  ;;; passed to C code for this callback by calling %CALLBACK.
  (defmacro define-callback (name return-type args &body body)
    (let ((arg-names (mapcar #'first args))
	  (arg-types (mapcar #'second args)))
      `(progn
	 (defvar ,name ',name)
	 (register-callback ',name 
	  (lambda ,arg-names ,@(callback-body args return-type body))
	  ,(callback-type return-type arg-names arg-types)))))

  ;;; Look up the name of a callback and return a pointer that can be
  ;;; passed to a C function.  Signals an error if no callback is
  ;;; defined called NAME.
  (defun callback-address (name)
    (multiple-value-bind (list winp) (gethash name *callbacks*)
      (unless winp
	(error "Undefined callback: ~S" name))
      (third list)))

  (deftype callback () 'symbol))



;;;; Type expansion

(defun type-expand-1 (form)
  #+(or cmu sbcl)
  (let ((def (cond ((symbolp form)
		    #+cmu(kernel::info type expander form)
		    #+sbcl(sb-impl::info :type :expander form))
		   ((and (consp form) (symbolp (car form)))
		    #+cmu(kernel::info type expander (car form))
		    #+sbcl(sb-impl::info :type :expander (car form)))
		   (t nil))))
    (if def
	(values (funcall def (if (consp form) form (list form))) t)
      (values form nil)))
  #+clisp(ext:type-expand form t))

(defun type-expand-to (type form)
  (labels ((expand (form0)
             (if (eq (first (mklist form0)) type)
		 form0
	       (multiple-value-bind (expanded-form expanded-p)
		   (type-expand-1 form0)
		 (if expanded-p
		     (expand expanded-form)
		   (error "~A can not be expanded to ~A" form type))))))
    (expand form)))



;;;; Type methods

(defun find-next-type-method (name type-spec &optional (error-p t))
  (let ((type-methods (get name 'type-methods)))
    (labels ((search-method-in-cpl-order (classes)
	       (when classes
		 (or 
		  (gethash (class-name (first classes)) type-methods)
		  (search-method-in-cpl-order (rest classes)))))
	     (lookup-method (type-spec)
	       (if (and (symbolp type-spec) (find-class type-spec nil))
		   (let ((class (find-class type-spec)))
		     #?(or (sbcl>= 0 9 15) (featurep :clisp))
		     (unless (class-finalized-p class)
		       (finalize-inheritance class))
		     (search-method-in-cpl-order 
		      (rest (class-precedence-list class))))
		 (multiple-value-bind (expanded-type expanded-p) 
		      (type-expand-1 type-spec)
		   (when expanded-p
		     (or 
		      (let ((specifier (etypecase expanded-type
					 (symbol expanded-type)
					 (list (first expanded-type)))))
			(gethash specifier type-methods))
		      (lookup-method expanded-type))))))
	     (search-built-in-type-hierarchy (sub-tree)
               (when (subtypep type-spec (first sub-tree))
		 (or
		  (search-nodes (cddr sub-tree))
		  (second sub-tree))))
	     (search-nodes (nodes)
	       (loop
		for node in nodes
		as method = (search-built-in-type-hierarchy node)
		until method
		finally (return method))))
      (or 
       (lookup-method type-spec)
       ;; This is to handle unexpandable types whichs doesn't name a
       ;; class.  It may cause infinite loops with illegal
       ;; call-next-method calls
       (unless (or 
		(null type-spec)
		(and (symbolp type-spec) (find-class type-spec nil)))
	 (search-nodes (get name 'built-in-type-hierarchy)))
       (when error-p
	 (error "No next type method ~A for type specifier ~A"
	  name type-spec))))))

(defun find-applicable-type-method (name type-spec &optional (error-p t))
  (let ((type-methods (get name 'type-methods))
	(specifier (if (atom type-spec)
		       type-spec
		     (first type-spec))))
    (or
     (gethash specifier type-methods)
     (find-next-type-method name type-spec nil)
     (when error-p 
       (error 
	"No applicable type method for ~A when call width type specifier ~A"
	name type-spec)))))

(defun insert-type-in-hierarchy (specifier function nodes)
  (cond
   ((let ((node (find specifier nodes :key #'first)))
      (when node
	(setf (second node) function)
	nodes)))
   ((let ((node
	   (find-if 
	    #'(lambda (node)
		(subtypep specifier (first node)))
	    nodes)))
      (when node
	(setf (cddr node) 
	      (insert-type-in-hierarchy specifier function (cddr node)))
	nodes)))
   ((let ((sub-nodes (remove-if-not 
		      #'(lambda (node)
			  (subtypep (first node) specifier))
		      nodes)))
      (cons
       (list* specifier function sub-nodes)
       (nset-difference nodes sub-nodes))))))

(defun add-type-method (name specifier function)
  (setf (gethash specifier (get name 'type-methods)) function)
  (when (typep (find-class specifier nil) 'built-in-class)
    (setf (get name 'built-in-type-hierarchy)
     (insert-type-in-hierarchy specifier function 
      (get name 'built-in-type-hierarchy)))))
  

(defmacro define-type-generic (name lambda-list &optional documentation)
  (let ((type-spec (first lambda-list)))
    (if (or 
	 (not lambda-list) 
	 (find type-spec '(&optional &key &rest &allow-other-keys)))
	(error "A type generic needs at least one required argument")
      `(progn 
	 (unless (get ',name 'type-methods)
	   (setf (get ',name 'type-methods) (make-hash-table))	 
	   (setf (get ',name 'built-in-type-hierarchy) ()))
	 ,(if (intersection '(&optional &key &rest &allow-other-keys) lambda-list)
	      (let ((args (make-symbol "ARGS")))
		`(defun ,name (,type-spec &rest ,args)
		   ,documentation
		   (apply
		    (find-applicable-type-method ',name ,type-spec)
		    ,type-spec ,args)))
	    `(defun ,name ,lambda-list
	       ,documentation
	       (funcall 
		(find-applicable-type-method ',name ,type-spec)
		,@lambda-list)))))))


(defmacro define-type-method (name lambda-list &body body)
  (let ((specifier (cadar lambda-list))	
	(args (make-symbol "ARGS")))
    `(progn
       (add-type-method ',name ',specifier 
	#'(lambda (&rest ,args)
	    (flet ((call-next-method (&rest args)
		     (let ((next-method (find-next-type-method ',name ',specifier)))
		       (apply next-method (or args ,args)))))
	      (destructuring-bind (,(caar lambda-list) ,@(rest lambda-list)) ,args
	        ,@body))))
       ',name)))


;;; Rules for auto-exporting symbols

(defexport defbinding (name &rest args)
  (declare (ignore args))
  (if (symbolp name)
      name
    (first name)))

(defexport define-type-generic (name &rest args)
  (declare (ignore args))
  name)
