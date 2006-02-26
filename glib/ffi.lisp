;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: ffi.lisp,v 1.28 2006-02-26 16:12:25 espen Exp $

(in-package "GLIB")


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
	 (stripped-name
	  (cond
	   ((and 
	     (char= (char name 0) #\%)
	     (string= "_p" name :start2 (- (length name) 2)))
	    (subseq name 1 (- (length name) 2)))
	   ((char= (char name 0) #\%)
	    (subseq name 1))
	   ((string= "_p" name :start2 (- (length name) 2))
	    (subseq name 0 (- (length name) 2)))
	   (name)))
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
      (cons prefix (split-string (symbol-name type-name) #\-))))))

(defun default-type-name (alien-name)
  (let ((parts
	 (mapcar
	  #'string-upcase
	  (split-string-if alien-name #'upper-case-p))))
    (intern
     (concatenate-strings
      (rest parts) #\-) (find-prefix-package (first parts)))))
    
	 
(defmacro defbinding (name lambda-list return-type &rest docs/args)
  (multiple-value-bind (lisp-name c-name)
      (if (atom name)
 	  (values name (default-alien-fname name))
 	(values-list name))
		       
    (let ((supplied-lambda-list lambda-list)
	  (docs nil)
	  (args nil))
      (dolist (doc/arg docs/args)
	(if (stringp doc/arg)
	    (push doc/arg docs)
	  (progn
	    (destructuring-bind (expr type &optional (style :in)) doc/arg
	      (unless (member style '(:in :out :in-out :return))
		(error "Bogus argument style ~S in ~S." style doc/arg))
	      (when (and
		     (not supplied-lambda-list)
		     (namep expr) (member style '(:in :in-out :return)))
		(push expr lambda-list))
	      (push (list (cond 
			   ((and (namep expr) (eq style :out)) expr)
			   ((namep expr) (make-symbol (string expr)))
			   ((gensym)))
			  expr type style) args)))))
      
      (%defbinding
       c-name lisp-name (or supplied-lambda-list (nreverse lambda-list))
       return-type (reverse docs) (reverse args)))))

#+(or cmu sbcl)
(defun %defbinding (foreign-name lisp-name lambda-list return-type docs args)
  (collect ((alien-types) (alien-bindings) (alien-parameters) 
	    (return-values) (cleanup-forms))
    (dolist (arg args)
      (destructuring-bind (var expr type style) arg
	(let ((declaration (alien-type type))
	      (cleanup (cleanup-form type var)))

	  (cond
	    ((member style '(:out :in-out))
	     (alien-types `(* ,declaration))
	     (alien-parameters `(addr ,var))
	     (alien-bindings
	      `(,var ,declaration
		,@(cond 
		   ((eq style :in-out) (list (to-alien-form type expr)))
		   ((eq declaration 'system-area-pointer) 
		    (list '(make-pointer 0))))))
	     (return-values (from-alien-form type var)))
	    ((eq style :return)
	     (alien-types declaration)
	     (alien-bindings
	      `(,var ,declaration ,(to-alien-form type expr)))
	     (alien-parameters var)
	     (return-values (from-alien-form type var)))
	    (cleanup
	     (alien-types declaration)
	     (alien-bindings
	      `(,var ,declaration ,(to-alien-form type expr)))
	     (alien-parameters var)
	     (cleanup-forms cleanup))
	    (t
	     (alien-types declaration)
	     (alien-parameters (to-alien-form type expr)))))))

    (let* ((alien-name (make-symbol (string lisp-name)))
	   (alien-funcall `(alien-funcall ,alien-name ,@(alien-parameters))))
      `(defun ,lisp-name ,lambda-list
	 ,@docs
	 #+cmu(declare (optimize (inhibit-warnings 3)))
	 #+sbcl(declare (muffle-conditions compiler-note))
	 (with-alien ((,alien-name
		       (function
			,(alien-type return-type)
			,@(alien-types))
		       :extern ,foreign-name)
		      ,@(alien-bindings))
	   ,(if return-type
		`(values
		  (unwind-protect 
		      ,(from-alien-form return-type alien-funcall)
		    ,@(cleanup-forms))
		  ,@(return-values))
	      `(progn
		(unwind-protect 
		     ,alien-funcall
		  ,@(cleanup-forms))
		(values ,@(return-values)))))))))


;;; Creates bindings at runtime
(defun mkbinding (name return-type &rest arg-types)
  #+cmu(declare (optimize (inhibit-warnings 3)))
  #+sbcl(declare (muffle-conditions compiler-note))
  (let* ((ftype 
	  `(function ,@(mapcar #'alien-type (cons return-type arg-types))))
	 (alien
	  (%heap-alien
	   (make-heap-alien-info
	    :type (parse-alien-type ftype #+sbcl nil)
	    :sap-form (let ((address (foreign-symbol-address name)))
			(etypecase address
			  (integer (int-sap address))
			  (system-area-pointer address))))))
	 (translate-arguments (mapcar #'to-alien-function arg-types))
	 (translate-return-value (from-alien-function return-type))
	 (cleanup-arguments (mapcar #'cleanup-function arg-types)))
        
    #'(lambda (&rest args)
	(map-into args #'funcall translate-arguments args)
	(prog1
	    (funcall translate-return-value 
	     (apply #'alien-funcall alien args))
	  (mapc #'funcall cleanup-arguments args)))))



;;;; C callbacks

(defmacro define-callback (name return-type args &body body)
  (let ((define-callback 
	  #+cmu'alien:def-callback 	              
	  #+(and sbcl alien-callbacks)'sb-alien::define-alien-callback
	  #+(and sbcl (not alien-callbacks))'sb-alien:define-alien-function))
    (multiple-value-bind (doc declaration body)
	(cond
	 ((and (stringp (first body)) (eq (cadr body) 'declare))
	  (values (first body) (second body) (cddr body)))
	 ((stringp (first body))
	  (values (first body) nil (rest body)))
	 ((eq (caar body) 'declare)
	  (values nil (first body) (rest body)))
	 (t (values nil nil body)))
      `(progn
	 #+cmu(defparameter ,name nil)
	 (,define-callback ,name 
	   #+(and sbcl alien-callbacks),(alien-type return-type) 
	   (#+(or cmu (and sbcl (not alien-callbacks))),(alien-type return-type)
	   ,@(mapcar #'(lambda (arg)
			 (destructuring-bind (name type) arg
			   `(,name ,(alien-type type))))
		     args))
	   ,@(when doc (list doc))
	   ,(to-alien-form return-type
	     `(let (,@(loop
		       for (name type) in args
		       as from-alien-form = (callback-from-alien-form type name)
		       collect `(,name ,from-alien-form)))
		,@(when declaration (list declaration))
		(unwind-protect
		    (progn ,@body)
		  ,@(loop 
		     for (name type) in args
		     do (callback-cleanup-form type name))))))))))

(defun callback-address (callback)
  #+cmu(alien::callback-trampoline callback)
  #+(and sbcl (not alien-callbacks))(sb-alien:alien-function-sap callback)
  #+(and sbcl alien-callbacks)(sb-alien:alien-sap callback))

#+sbcl
(deftype callback () 
  #-alien-callbacks'sb-alien:alien-function
  #+alien-callbacks'sb-alien:alien)


;;; These are for backward compatibility

(defmacro defcallback (name (return-type &rest args) &body body)
  `(define-callback ,name ,return-type ,args ,@body))

#-cmu
(defun callback (callback)
  (callback-address callback))



;;;; The "type method" system

(defun find-applicable-type-method (name type-spec &optional (error-p t))
  (let ((type-methods (get name 'type-methods)))
    (labels ((search-method-in-cpl-order (classes)
	       (when classes
		 (or 
		  (gethash (class-name (first classes)) type-methods)
		  (search-method-in-cpl-order (rest classes)))))
	     (lookup-method (type-spec)
	       (if (and (symbolp type-spec) (find-class type-spec nil))
		   (search-method-in-cpl-order
		    (class-precedence-list (find-class type-spec)))
		 (or 
		  (let ((specifier (etypecase type-spec
				     (symbol type-spec)
				     (list (first type-spec)))))
		    (gethash specifier type-methods))
		  (multiple-value-bind (expanded-type expanded-p) 
		      (type-expand-1 type-spec)
		    (when expanded-p
		      (lookup-method expanded-type))))))
	     (search-built-in-type-hierarchy (sub-tree)
               (when (subtypep type-spec (first sub-tree))
		 (or
		  (search-nodes (cddr sub-tree))
		  (second sub-tree))))
	     (search-nodes (nodes)
	       (loop
		for node in nodes
		as function = (search-built-in-type-hierarchy node)
		until function
		finally (return function))))
    (or 
     (lookup-method type-spec)
     ;; This is to handle unexpandable types whichs doesn't name a class
     (unless (and (symbolp type-spec) (find-class type-spec nil))
       (search-nodes (get name 'built-in-type-hierarchy)))
     (and 
      error-p
      (error "No applicable type method for ~A when call width type specifier ~A" name type-spec))))))


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


;; TODO: handle optional, key and rest arguments
(defmacro define-type-generic (name lambda-list &optional documentation)
  (if (or 
       (not lambda-list) 
       (find (first lambda-list) '(&optional &key &rest &allow-other-keys)))
      (error "A type generic needs at least one required argument")
    `(progn 
       (setf (get ',name 'type-methods) (make-hash-table))
       (setf (get ',name 'built-in-type-hierarchy) ())
       (defun ,name ,lambda-list
	 ,documentation
	 (funcall 
	  (find-applicable-type-method ',name ,(first lambda-list))
	  ,@lambda-list)))))


(defmacro define-type-method (name lambda-list &body body)
  (let ((specifier (cadar lambda-list))
	(args (cons (caar lambda-list) (rest lambda-list))))
    `(progn
       (add-type-method ',name ',specifier #'(lambda ,args ,@body))
       ',name)))



;;;; Definitons and translations of fundamental types    

(define-type-generic alien-type (type-spec))
(define-type-generic size-of (type-spec))
(define-type-generic to-alien-form (type-spec form))
(define-type-generic from-alien-form (type-spec form))
(define-type-generic cleanup-form (type-spec form)
  "Creates a form to clean up after the alien call has finished.")
(define-type-generic callback-from-alien-form (type-spec form))
(define-type-generic callback-cleanup-form (type-spec form))

(define-type-generic to-alien-function (type-spec))
(define-type-generic from-alien-function (type-spec))
(define-type-generic cleanup-function (type-spec))

(define-type-generic copy-to-alien-form (type-spec form))
(define-type-generic copy-to-alien-function (type-spec))
(define-type-generic copy-from-alien-form (type-spec form))
(define-type-generic copy-from-alien-function (type-spec))
(define-type-generic writer-function (type-spec))
(define-type-generic reader-function (type-spec))
(define-type-generic destroy-function (type-spec))

(define-type-generic unbound-value (type-spec)
  "Returns a value which should be intepreted as unbound for slots with virtual allocation")


#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sb-sizeof-bits (type)
    (sb-alien-internals:alien-type-bits
     (sb-alien-internals:parse-alien-type type nil)))

  (defun sb-sizeof (type)
    (/ (sb-sizeof-bits type) 8)))


;; Sizes of fundamental C types in bytes (8 bits)
(defconstant +size-of-short+
  #+sbcl (sb-sizeof 'sb-alien:short)
  #-sbcl 2)
(defconstant +size-of-int+
  #+sbcl (sb-sizeof 'sb-alien:int)
  #-sbcl 4)
(defconstant +size-of-long+
  #+sbcl (sb-sizeof 'sb-alien:long)
  #-sbcl 4)
(defconstant +size-of-pointer+
  #+sbcl (sb-sizeof 'sb-alien:system-area-pointer)
  #-sbcl 4)
(defconstant +size-of-float+
  #+sbcl (sb-sizeof 'sb-alien:float)
  #-sbcl 4)
(defconstant +size-of-double+
  #+sbcl (sb-sizeof 'sb-alien:double)
  #-sbcl 8)


;; Sizes of fundamental C types in bits
(defconstant +bits-of-byte+ 8)
(defconstant +bits-of-short+
  #+sbcl (sb-sizeof-bits 'sb-alien:short)
  #-sbcl 16)
(defconstant +bits-of-int+
  #+sbcl (sb-sizeof-bits 'sb-alien:int)
  #-sbcl 32)
(defconstant +bits-of-long+
  #+sbcl (sb-sizeof-bits 'sb-alien:long)
  #-sbcl 32)


(deftype int () '(signed-byte #.+bits-of-int+))
(deftype unsigned-int () '(unsigned-byte #.+bits-of-int+))
(deftype long () '(signed-byte #.+bits-of-long+))
(deftype unsigned-long () '(unsigned-byte #.+bits-of-long+))
(deftype short () '(signed-byte #.+bits-of-short+))
(deftype unsigned-short () '(unsigned-byte #.+bits-of-short+))
(deftype signed (&optional (size '*)) `(signed-byte ,size))
(deftype unsigned (&optional (size '*)) `(unsigned-byte ,size))
(deftype char () 'base-char)
(deftype pointer () 'system-area-pointer)
(deftype boolean (&optional (size '*)) (declare (ignore size)) t)
(deftype copy-of (type) type)

(define-type-method alien-type ((type t))
  (error "No alien type corresponding to the type specifier ~A" type))

(define-type-method to-alien-form ((type t) form)
  (declare (ignore form))
  (error "Not a valid type specifier for arguments: ~A" type))

(define-type-method to-alien-function ((type t))
  (error "Not a valid type specifier for arguments: ~A" type))

(define-type-method from-alien-form ((type t) form)
  (declare (ignore form))
  (error "Not a valid type specifier for return values: ~A" type))

(define-type-method from-alien-function ((type t))
  (error "Not a valid type specifier for return values: ~A" type))
 
(define-type-method cleanup-form ((type t) form)
  (declare (ignore form type))
  nil)

(define-type-method cleanup-function ((type t))
  (declare (ignore type))
  #'identity)

(define-type-method callback-from-alien-form ((type t) form)
  (copy-from-alien-form type form))

(define-type-method callback-cleanup-form ((type t) form)
  (declare (ignore form type))
  nil)

(define-type-method destroy-function ((type t))
  (declare (ignore type))
  #'(lambda (location &optional offset)
      (declare (ignore location offset))))

(define-type-method copy-to-alien-form ((type t) form)
  (to-alien-form type form))

(define-type-method copy-to-alien-function ((type t))
  (to-alien-function type))

(define-type-method copy-from-alien-form ((type t) form)
  (from-alien-form type  form))

(define-type-method copy-from-alien-function ((type t))
  (from-alien-function type))


(define-type-method to-alien-form ((type real) form)
  (declare (ignore type))
  form)

(define-type-method to-alien-function ((type real))
  (declare (ignore type))
  #'identity)

(define-type-method from-alien-form ((type real) form)
  (declare (ignore type))
  form)

(define-type-method from-alien-function ((type real))
  (declare (ignore type))
  #'identity)


(define-type-method alien-type ((type integer))
  (declare (ignore type))
  (alien-type 'signed-byte))

(define-type-method size-of ((type integer))
  (declare (ignore type))
  (size-of 'signed-byte))

(define-type-method writer-function ((type integer))
  (declare (ignore type))
  (writer-function 'signed-byte))

(define-type-method reader-function ((type integer))
  (declare (ignore type))
  (reader-function 'signed-byte))

  
(define-type-method alien-type ((type signed-byte))
  (destructuring-bind (&optional (size '*)) 
      (rest (mklist (type-expand-to 'signed-byte type)))
    (ecase size
      (#.+bits-of-byte+ #+cmu'(alien:signed 8) #+sbcl'(sb-alien:signed 8))
      (#.+bits-of-short+ #+cmu 'c-call:short #+sbcl 'sb-alien:short)
      ((* #.+bits-of-int+) #+cmu 'c-call:int #+sbcl 'sb-alien:int)
      (#.+bits-of-long+ #+cmu 'c-call:long #+sbcl 'sb-alien:long))))

(define-type-method size-of ((type signed-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (ecase size
      (#.+bits-of-byte+ 1)
      (#.+bits-of-short+ +size-of-short+)
      ((* #.+bits-of-int+) +size-of-int+)
      (#.+bits-of-long+ +size-of-long+))))

(define-type-method writer-function ((type signed-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (value location &optional (offset 0))
	       (setf (signed-sap-ref-8 location offset) value)))
	(16 #'(lambda (value location &optional (offset 0))
		(setf (signed-sap-ref-16 location offset) value)))
	(32 #'(lambda (value location &optional (offset 0))
		(setf (signed-sap-ref-32 location offset) value)))
	(64 #'(lambda (value location &optional (offset 0))
		(setf (signed-sap-ref-64 location offset) value)))))))
  
(define-type-method reader-function ((type signed-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (sap &optional (offset 0) weak-p) 
	       (declare (ignore weak-p))
	       (signed-sap-ref-8 sap offset)))
	(16 #'(lambda (sap &optional (offset 0) weak-p)
		(declare (ignore weak-p))
		(signed-sap-ref-16 sap offset)))
	(32 #'(lambda (sap &optional (offset 0) weak-p) 
		(declare (ignore weak-p)) 
		(signed-sap-ref-32 sap offset)))
	(64 #'(lambda (sap &optional (offset 0) weak-p) 
		(declare (ignore weak-p))
		(signed-sap-ref-64 sap offset)))))))


(define-type-method alien-type ((type unsigned-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (ecase size
      (#.+bits-of-byte+ #+cmu'(alien:unsigned 8) #+sbcl'(sb-alien:unsigned 8))
      (#.+bits-of-short+ #+cmu 'c-call:unsigned-short 
			 #+sbcl 'sb-alien:unsigned-short)
      ((* #.+bits-of-int+) #+cmu 'c-call:unsigned-int 
                           #+sbcl 'sb-alien:unsigned-int)
      (#.+bits-of-long+ #+cmu 'c-call:unsigned-long 
			#+sbcl 'sb-alien:unsigned-long))))


(define-type-method size-of ((type unsigned-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
  (size-of `(signed ,size))))

(define-type-method writer-function ((type unsigned-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (value location &optional (offset 0))
	       (setf (sap-ref-8 location offset) value)))
	(16 #'(lambda (value location &optional (offset 0))
		(setf (sap-ref-16 location offset) value)))
	(32 #'(lambda (value location &optional (offset 0))
		(setf (sap-ref-32 location offset) value)))
	(64 #'(lambda (value location &optional (offset 0))
		(setf (sap-ref-64 location offset) value)))))))
      
(define-type-method reader-function ((type unsigned-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (sap &optional (offset 0) weak-p)
	       (declare (ignore weak-p))
	       (sap-ref-8 sap offset)))
	(16 #'(lambda (sap &optional (offset 0) weak-p)
		(declare (ignore weak-p)) 
		(sap-ref-16 sap offset)))
	(32 #'(lambda (sap &optional (offset 0) weak-p)
		(declare (ignore weak-p)) 
		(sap-ref-32 sap offset)))
	(64 #'(lambda (sap &optional (offset 0) weak-p)
		(declare (ignore weak-p))
		(sap-ref-64 sap offset)))))))

(define-type-method alien-type ((type single-float))
  (declare (ignore type))
  #+cmu 'alien:single-float #+sbcl 'sb-alien:single-float)

(define-type-method size-of ((type single-float))
  (declare (ignore type))
  +size-of-float+)

(define-type-method to-alien-form ((type single-float) form)
  (declare (ignore type))
  `(coerce ,form 'single-float))

(define-type-method to-alien-function ((type single-float))
  (declare (ignore type))
  #'(lambda (number)
      (coerce number 'single-float)))

(define-type-method writer-function ((type single-float))
  (declare (ignore type))
  #'(lambda (value location &optional (offset 0))
      (setf (sap-ref-single location offset) (coerce value 'single-float))))

(define-type-method reader-function ((type single-float))
  (declare (ignore type))
  #'(lambda (sap &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (sap-ref-single sap offset)))


(define-type-method alien-type ((type double-float))
  (declare (ignore type))
  #+cmu 'alien:double-float #+sbcl 'sb-alien:double-float)

(define-type-method size-of ((type double-float))
  (declare (ignore type))
  +size-of-double+)

(define-type-method to-alien-form ((type double-float) form)
  (declare (ignore type))
  `(coerce ,form 'double-float))

(define-type-method to-alien-function ((type double-float))
  (declare (ignore type))
  #'(lambda (number)
      (coerce number 'double-float)))

(define-type-method writer-function ((type double-float))
  (declare (ignore type))
  #'(lambda (value location &optional (offset 0))
      (setf (sap-ref-double location offset) (coerce value 'double-float))))

(define-type-method reader-function ((type double-float))
  (declare (ignore type))
  #'(lambda (sap &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (sap-ref-double sap offset)))


(define-type-method alien-type ((type base-char))
  (declare (ignore type))
  #+cmu 'c-call:char #+sbcl 'sb-alien:char)

(define-type-method size-of ((type base-char))
  (declare (ignore type))
  1)

(define-type-method to-alien-form ((type base-char) form)
  (declare (ignore type))
  form)

(define-type-method to-alien-function ((type base-char))
  (declare (ignore type))
  #'identity)

(define-type-method from-alien-form ((type base-char) form)
  (declare (ignore type))
  form)

(define-type-method from-alien-function ((type base-char))
  (declare (ignore type))
  #'identity)

(define-type-method writer-function ((type base-char))
  (declare (ignore type))
  #'(lambda (char location &optional (offset 0))
      (setf (sap-ref-8 location offset) (char-code char))))

(define-type-method reader-function ((type base-char))
  (declare (ignore type))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (code-char (sap-ref-8 location offset))))


(define-type-method alien-type ((type string))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type string))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type string) string)
  (declare (ignore type))
  `(let ((string ,string))
     ;; Always copy strings to prevent seg fault due to GC
     #+cmu
     (copy-memory
      (vector-sap (coerce string 'simple-base-string))
      (1+ (length string)))
     #+sbcl
     (let ((utf8 (%deport-utf8-string string)))
       (copy-memory (vector-sap utf8) (length utf8)))))
  
(define-type-method to-alien-function ((type string))
  (declare (ignore type))
  #'(lambda (string)
      #+cmu
      (copy-memory
       (vector-sap (coerce string 'simple-base-string))
       (1+ (length string)))
      #+sbcl
      (let ((utf8 (%deport-utf8-string string)))
	(copy-memory (vector-sap utf8) (length utf8)))))

(define-type-method from-alien-form ((type string) string)
  (declare (ignore type))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      (prog1
	  #+cmu(%naturalize-c-string string)
	  #+sbcl(%naturalize-utf8-string string)
	(deallocate-memory string)))))

(define-type-method from-alien-function ((type string))
  (declare (ignore type))
  #'(lambda (string)
      (unless (null-pointer-p string)
	(prog1
	    #+cmu(%naturalize-c-string string)
	    #+sbcl(%naturalize-utf8-string string)
	  (deallocate-memory string)))))

(define-type-method cleanup-form ((type string) string)
  (declare (ignore type))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      (deallocate-memory string))))

(define-type-method cleanup-function ((type string))
  (declare (ignore type))
  #'(lambda (string)
      (unless (null-pointer-p string)
	(deallocate-memory string))))

(define-type-method copy-from-alien-form ((type string) string)
  (declare (ignore type))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      #+cmu(%naturalize-c-string string)
      #+sbcl(%naturalize-utf8-string string))))

(define-type-method copy-from-alien-function ((type string))
  (declare (ignore type))
  #'(lambda (string)
      (unless (null-pointer-p string)
	#+cmu(%naturalize-c-string string)
	#+sbcl(%naturalize-utf8-string string))))

(define-type-method writer-function ((type string))
  (declare (ignore type))
  #'(lambda (string location &optional (offset 0))
      (assert (null-pointer-p (sap-ref-sap location offset)))
      (setf (sap-ref-sap location offset)
       #+cmu
       (copy-memory
	(vector-sap (coerce string 'simple-base-string))
	(1+ (length string)))
       #+sbcl
       (let ((utf8 (%deport-utf8-string string)))
	 (copy-memory (vector-sap utf8) (length utf8))))))

(define-type-method reader-function ((type string))
  (declare (ignore type))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (unless (null-pointer-p (sap-ref-sap location offset))
	#+cmu(%naturalize-c-string (sap-ref-sap location offset))
	#+sbcl(%naturalize-utf8-string (sap-ref-sap location offset)))))

(define-type-method destroy-function ((type string))
  (declare (ignore type))
  #'(lambda (location &optional (offset 0))
      (unless (null-pointer-p (sap-ref-sap location offset))
	(deallocate-memory (sap-ref-sap location offset))
	(setf (sap-ref-sap location offset) (make-pointer 0)))))

(define-type-method unbound-value ((type string))
  (declare (ignore type))
  nil)


(define-type-method alien-type ((type pathname))
  (declare (ignore type))
  (alien-type 'string))

(define-type-method size-of ((type pathname))
  (declare (ignore type))
  (size-of 'string))

(define-type-method to-alien-form ((type pathname) path)
  (declare (ignore type))
  (to-alien-form 'string `(namestring (translate-logical-pathname ,path))))

(define-type-method to-alien-function ((type pathname))
  (declare (ignore type))
  (let ((string-function (to-alien-function 'string)))
    #'(lambda (path)
	(funcall string-function (namestring path)))))

(define-type-method from-alien-form ((type pathname) string)
  (declare (ignore type))
  `(parse-namestring ,(from-alien-form 'string string)))

(define-type-method from-alien-function ((type pathname))
  (declare (ignore type))
  (let ((string-function (from-alien-function 'string)))
    #'(lambda (string)
	(parse-namestring (funcall string-function string)))))

(define-type-method cleanup-form ((type pathnanme) string)
  (declare (ignore type))
  (cleanup-form 'string string))

(define-type-method cleanup-function ((type pathnanme))
  (declare (ignore type))
  (cleanup-function 'string))

(define-type-method writer-function ((type pathname))
  (declare (ignore type))
  (let ((string-writer (writer-function 'string)))
    #'(lambda (path location &optional (offset 0))
	(funcall string-writer (namestring path) location offset))))

(define-type-method reader-function ((type pathname))
  (declare (ignore type))
  (let ((string-reader (reader-function 'string)))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (let ((string (funcall string-reader location offset)))
	(when string
	  (parse-namestring string))))))

(define-type-method destroy-function ((type pathname))
  (declare (ignore type))
  (destroy-function 'string))

(define-type-method unbound-value ((type pathname))
  (declare (ignore type))
  (unbound-value 'string))


(define-type-method alien-type ((type boolean))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'boolean type)))
    (alien-type `(signed-byte ,size))))

(define-type-method size-of ((type boolean))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'boolean type)))
    (size-of `(signed-byte ,size))))

(define-type-method to-alien-form ((type boolean) boolean)
  (declare (ignore type))
  `(if ,boolean 1 0))

(define-type-method to-alien-function ((type boolean))
  (declare (ignore type))
  #'(lambda (boolean)
      (if boolean 1 0)))

(define-type-method from-alien-form ((type boolean) boolean)
  (declare (ignore type))
  `(not (zerop ,boolean)))

(define-type-method from-alien-function ((type boolean))
  (declare (ignore type))
  #'(lambda (boolean)
      (not (zerop boolean))))

(define-type-method writer-function ((type boolean))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'boolean type)))
    (let ((writer (writer-function `(signed-byte ,size))))
      #'(lambda (boolean location &optional (offset 0))
	  (funcall writer (if boolean 1 0) location offset)))))

(define-type-method reader-function ((type boolean))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'boolean type)))
    (let ((reader (reader-function `(signed-byte ,size))))
      #'(lambda (location &optional (offset 0) weak-p)
	  (declare (ignore weak-p))
	  (not (zerop (funcall reader location offset)))))))


(define-type-method alien-type ((type or))
  (let* ((expanded-type (type-expand-to 'or type))
	 (alien-type (alien-type (second expanded-type))))
    (unless (every #'(lambda (type)
		       (eq alien-type (alien-type type)))
		   (cddr expanded-type))
      (error "No common alien type specifier for union type: ~A" type))
    alien-type))

(define-type-method size-of ((type or))
  (size-of (second (type-expand-to 'or type))))

(define-type-method to-alien-form ((type or) form)
  `(let ((value ,form))
     (etypecase value
       ,@(mapcar	 
	  #'(lambda (type)
	      `(,type ,(to-alien-form type 'value)))
	  (rest (type-expand-to 'or type))))))

(define-type-method to-alien-function ((type or))
  (let* ((expanded-type (type-expand-to 'or type))
	 (functions (mapcar #'to-alien-function (rest expanded-type))))
    #'(lambda (value)
	(loop
	 for function in functions
	 for alt-type in (rest expanded-type)
	 when (typep value alt-type)
	 do (return (funcall function value))
	 finally (error "~S is not of type ~A" value type)))))


(define-type-method alien-type ((type pointer))
  (declare (ignore type))
  'system-area-pointer)

(define-type-method size-of ((type pointer))
  (declare (ignore type))
  +size-of-pointer+)

(define-type-method to-alien-form ((type pointer) form)
  (declare (ignore type))
  form)

(define-type-method to-alien-function ((type pointer))
  (declare (ignore type))
  #'identity)

(define-type-method from-alien-form ((type pointer) form)
  (declare (ignore type))
  form)

(define-type-method from-alien-function ((type pointer))
  (declare (ignore type))
  #'identity)

(define-type-method writer-function ((type pointer))
  (declare (ignore type))
  #'(lambda (sap location &optional (offset 0))
      (setf (sap-ref-sap location offset) sap)))

(define-type-method reader-function ((type pointer))
  (declare (ignore type))
  #'(lambda (location &optional (offset 0) weak-p)
      (declare (ignore weak-p))
      (sap-ref-sap location offset)))


(define-type-method alien-type ((type null))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type null))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type null) null)
  (declare (ignore null type))
  `(make-pointer 0))

(define-type-method to-alien-function ((type null))
  (declare (ignore type))
  #'(lambda (null)
      (declare (ignore null))
      (make-pointer 0)))


(define-type-method alien-type ((type nil))
  (declare (ignore type))
  'void)

(define-type-method from-alien-function ((type nil))
  (declare (ignore type))
  #'(lambda (value)
      (declare (ignore value))
      (values)))

(define-type-method to-alien-form ((type nil) form)
  (declare (ignore type))
  form)


(define-type-method to-alien-form ((type copy-of) form)
  (copy-to-alien-form (second (type-expand-to 'copy-of type)) form))

(define-type-method to-alien-function ((type copy-of))
  (copy-to-alien-function (second (type-expand-to 'copy-of type))))

(define-type-method from-alien-form ((type copy-of) form)
  (copy-from-alien-form (second (type-expand-to 'copy-of type)) form))

(define-type-method from-alien-function ((type copy-of))
  (copy-from-alien-function (second (type-expand-to 'copy-of type))))


(define-type-method alien-type ((type callback))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method to-alien-form ((type callback) callback)
  (declare (ignore type ))
  `(callback-address ,callback))
