;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: ffi.lisp,v 1.7 2004-12-04 00:28:47 espen Exp $

(in-package "GLIB")


;;;; Foreign function call interface

(defvar *package-prefix* nil)

(defun set-package-prefix (prefix &optional (package *package*))
  (let ((package (find-package package)))
    (delete-if #'(lambda (assoc) (eq (car assoc) package)) *package-prefix*)
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
  (let* ((lisp-name-string
	  (if (char= (char (the simple-string (string lisp-name)) 0) #\%)
	      (subseq (the simple-string (string lisp-name)) 1)
	    (string lisp-name)))
	 (prefix (package-prefix *package*))
	 (name (substitute #\_ #\- (string-downcase lisp-name-string))))
    (if (or (not prefix) (string= prefix ""))
	name
      (format nil "~A_~A" prefix name))))

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
	      (push
	       (list (if (namep expr) 
			 (make-symbol (string expr))
		       (gensym))
		     expr (mklist type) style) args)))))
      
      (%defbinding
       c-name lisp-name (or supplied-lambda-list (nreverse lambda-list))
       return-type (reverse docs) (reverse args)))))

#+cmu
(defun %defbinding (foreign-name lisp-name lambda-list return-type docs args)
  (ext:collect ((alien-types) (alien-bindings) (alien-parameters) 
		(return-values) (cleanup-forms))
    (dolist (arg args)
      (destructuring-bind (var expr type style) arg
	(let ((declaration (alien-type type))
	      (cleanup (cleanup-form var type)))

	  (cond
	    ((member style '(:out :in-out))
	     (alien-types `(* ,declaration))
	     (alien-parameters `(addr ,var))
	     (alien-bindings
	      `(,var ,declaration
		,@(when (eq style :in-out)
		    (list (to-alien-form expr type)))))
	     (return-values (from-alien-form var type)))
	    ((eq style :return)
	     (alien-types declaration)
	     (alien-bindings
	      `(,var ,declaration ,(to-alien-form expr type)))
	     (alien-parameters var)
	     (return-values (from-alien-form var type)))
	    (cleanup
	     (alien-types declaration)
	     (alien-bindings
	      `(,var ,declaration ,(to-alien-form expr type)))
	     (alien-parameters var)
	     (cleanup-forms cleanup))
	    (t
	     (alien-types declaration)
	     (alien-parameters (to-alien-form expr type)))))))

    (let* ((alien-name (make-symbol (string lisp-name)))
	   (alien-funcall `(alien-funcall ,alien-name ,@(alien-parameters))))
      `(defun ,lisp-name ,lambda-list
	 ,@docs
	 (declare (optimize (ext:inhibit-warnings 3)))
	 (with-alien ((,alien-name
		       (function
			,(alien-type return-type)
			,@(alien-types))
		       :extern ,foreign-name)
		      ,@(alien-bindings))
	   ,(if return-type
		`(values
		  (unwind-protect 
		      ,(from-alien-form alien-funcall return-type)
		    ,@(cleanup-forms))
		  ,@(return-values))
	      `(progn
		(unwind-protect 
		     ,alien-funcall
		  ,@(cleanup-forms))
		(values ,@(return-values)))))))))


;;; Creates bindings at runtime
(defun mkbinding (name return-type &rest arg-types)
  (declare (optimize (ext:inhibit-warnings 3)))
  (let* ((ftype 
	  `(function ,@(mapcar #'alien-type (cons return-type arg-types))))
	 (alien
	  (alien::%heap-alien
	   (alien::make-heap-alien-info
	    :type (alien::parse-alien-type ftype)
	    :sap-form (system:foreign-symbol-address name :flavor :code))))
	 (translate-arguments (mapcar #'to-alien-function arg-types))
	 (translate-return-value (from-alien-function return-type))
	 (cleanup-arguments (mapcar #'cleanup-function arg-types)))
        
    #'(lambda (&rest args)
	(map-into args #'funcall translate-arguments args)
	(prog1
	    (funcall translate-return-value 
	     (apply #'alien:alien-funcall alien args))
	  (mapc #'funcall cleanup-arguments args)))))


(defmacro defcallback (name (return-type &rest args) &body body)
  `(def-callback ,name 
       (,(alien-type return-type) 
	,@(mapcar #'(lambda (arg)
		      (destructuring-bind (name type) arg
			`(,name ,(alien-type type))))
		  args))
    ,(to-alien-form 
      `(let (,@(mapcar #'(lambda (arg)
			   (destructuring-bind (name type) arg
			     `(,name ,(from-alien-form name type))))
		       args))
	,@body)
      return-type)))



;;;; Definitons and translations of fundamental types

(defmacro def-type-method (name args &optional documentation)
  `(progn
    (defgeneric ,name (,@args type &rest args)
      ,@(when documentation `((:documentation ,documentation))))
    (defmethod ,name (,@args (type symbol) &rest args)
      (let ((class (find-class type nil)))
	(if class 
	    (apply #',name ,@args class args)
	  (multiple-value-bind (super-type expanded-p)
	      (type-expand-1 (cons type args))
	    (if expanded-p
		(,name ,@args super-type)
	      (call-next-method))))))
    (defmethod ,name (,@args (type cons) &rest args)
      (declare (ignore args))
      (apply #',name ,@args (first type) (rest type)))))
    

(def-type-method alien-type ())
(def-type-method size-of ())
(def-type-method to-alien-form (form))
(def-type-method from-alien-form (form))
(def-type-method cleanup-form (form)
  "Creates a form to clean up after the alien call has finished.")

(def-type-method to-alien-function ())
(def-type-method from-alien-function ())
(def-type-method cleanup-function ())

(def-type-method copy-to-alien-form (form))
(def-type-method copy-to-alien-function ())
(def-type-method copy-from-alien-form (form))
(def-type-method copy-from-alien-function ())

(def-type-method writer-function ())
(def-type-method reader-function ())
(def-type-method destroy-function ())


;; Sizes of fundamental C types in bytes (8 bits)
(defconstant +size-of-short+ 2)
(defconstant +size-of-int+ 4)
(defconstant +size-of-long+ 4)
(defconstant +size-of-pointer+ 4)
(defconstant +size-of-float+ 4)
(defconstant +size-of-double+ 8)

;; Sizes of fundamental C types in bits
(defconstant +bits-of-byte+ 8)
(defconstant +bits-of-short+ 16)
(defconstant +bits-of-int+ 32)
(defconstant +bits-of-long+ 32)


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
(deftype boolean (&optional (size '*)) (declare (ignore size)) `(member t nil))
;(deftype invalid () nil)


(defmethod to-alien-form (form (type t) &rest args)
  (declare (ignore type args))
  form)

(defmethod to-alien-function ((type t) &rest args)
  (declare (ignore type args))
  #'identity)

(defmethod from-alien-form (form (type t) &rest args)
  (declare (ignore type args))
  form)

(defmethod from-alien-function ((type t) &rest args)
  (declare (ignore type args))
  #'identity)
 
(defmethod cleanup-form (form (type t) &rest args)
  (declare (ignore form type args))
  nil)

(defmethod cleanup-function ((type t) &rest args)
  (declare (ignore type args))
  #'identity)

(defmethod destroy-function ((type t) &rest args)
  (declare (ignore type args))
  #'(lambda (location &optional offset)
      (declare (ignore location offset))))

(defmethod copy-to-alien-form  (form (type t) &rest args)
  (apply #'to-alien-form form type args))

(defmethod copy-to-alien-function  ((type t) &rest args)
  (apply #'to-alien-function type args))

(defmethod copy-from-alien-form  (form (type t) &rest args)
  (apply #'from-alien-form form type args))

(defmethod copy-from-alien-function  ((type t) &rest args)
  (apply #'from-alien-function type args))


(defmethod alien-type ((type (eql 'signed-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
    (ecase size
      (#.+bits-of-byte+ '(signed-byte 8))
      (#.+bits-of-short+ 'c-call:short)
      ((* #.+bits-of-int+) 'c-call:int)
      (#.+bits-of-long+ 'c-call:long))))

(defmethod size-of ((type (eql 'signed-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
    (ecase size
      (#.+bits-of-byte+ 1)
      (#.+bits-of-short+ +size-of-short+)
      ((* #.+bits-of-int+) +size-of-int+)
      (#.+bits-of-long+ +size-of-long+))))

(defmethod writer-function ((type (eql 'signed-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
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
  
(defmethod reader-function ((type (eql 'signed-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (sap &optional (offset 0)) 
	       (signed-sap-ref-8 sap offset)))
	(16 #'(lambda (sap &optional (offset 0)) 
		(signed-sap-ref-16 sap offset)))
	(32 #'(lambda (sap &optional (offset 0)) 
		(signed-sap-ref-32 sap offset)))
	(64 #'(lambda (sap &optional (offset 0))
		(signed-sap-ref-64 sap offset)))))))

(defmethod alien-type ((type (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (&optional (size '*)) args
    (ecase size
      (#.+bits-of-byte+ '(unsigned-byte 8))
      (#.+bits-of-short+ 'c-call:unsigned-short)
      ((* #.+bits-of-int+) 'c-call:unsigned-int)
      (#.+bits-of-long+ 'c-call:unsigned-long))))

(defmethod size-of ((type (eql 'unsigned-byte)) &rest args)
  (apply #'size-of 'signed args))

(defmethod writer-function ((type (eql 'unsigned-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
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
      
(defmethod reader-function ((type (eql 'unsigned-byte)) &rest args)
  (declare (ignore type))
  (destructuring-bind (&optional (size '*)) args
    (let ((size (if (eq size '*) +bits-of-int+ size)))
      (ecase size
	(8 #'(lambda (sap &optional (offset 0)) 
	       (sap-ref-8 sap offset)))
	(16 #'(lambda (sap &optional (offset 0)) 
		(sap-ref-16 sap offset)))
	(32 #'(lambda (sap &optional (offset 0)) 
		(sap-ref-32 sap offset)))
	(64 #'(lambda (sap &optional (offset 0))
		(sap-ref-64 sap offset)))))))
  
  
(defmethod alien-type ((type (eql 'integer)) &rest args)
  (declare (ignore type args))
  (alien-type 'signed-byte))

(defmethod size-of ((type (eql 'integer)) &rest args)
  (declare (ignore type args))
  (size-of 'signed-byte))

(defmethod writer-function ((type (eql 'integer)) &rest args)
  (declare (ignore type args))
  (writer-function 'signed-byte))

(defmethod reader-function ((type (eql 'integer)) &rest args)
  (declare (ignore type args))
  (reader-function 'signed-byte))


(defmethod alien-type ((type (eql 'fixnum)) &rest args)
  (declare (ignore type args))
  (alien-type 'signed-byte))

(defmethod size-of ((type (eql 'fixnum)) &rest args)
  (declare (ignore type args))
  (size-of 'signed-byte))


(defmethod alien-type ((type (eql 'single-float)) &rest args)
  (declare (ignore type args))
  'alien:single-float)

(defmethod size-of ((type (eql 'single-float)) &rest args)
  (declare (ignore type args))
  +size-of-float+)

(defmethod writer-function ((type (eql 'single-float)) &rest args)
  (declare (ignore type args))
  #'(lambda (value location &optional (offset 0))
      (setf (sap-ref-single location offset) (coerce value 'single-float))))

(defmethod reader-function ((type (eql 'single-float)) &rest args)
  (declare (ignore type args))
  #'(lambda (sap &optional (offset 0)) 
      (sap-ref-single sap offset)))


(defmethod alien-type ((type (eql 'double-float)) &rest args)
  (declare (ignore type args))
  'alien:double-float)

(defmethod size-of ((type (eql 'double-float)) &rest args)
  (declare (ignore type args))
  +size-of-float+)

(defmethod writer-function ((type (eql 'double-float)) &rest args)
  (declare (ignore type args))
  #'(lambda (value location &optional (offset 0))
      (setf (sap-ref-double location offset) (coerce value 'double-float))))

(defmethod reader-function ((type (eql 'double-float)) &rest args)
  (declare (ignore type args))
  #'(lambda (sap &optional (offset 0)) 
      (sap-ref-double sap offset)))


(defmethod alien-type ((type (eql 'base-char)) &rest args)
  (declare (ignore type args))
  'c-call:char)

(defmethod size-of ((type (eql 'base-char)) &rest args)
  (declare (ignore type args))
  1)

(defmethod writer-function ((type (eql 'base-char)) &rest args)
  (declare (ignore type args))
  #'(lambda (char location &optional (offset 0))
      (setf (sap-ref-8 location offset) (char-code char))))

(defmethod reader-function ((type (eql 'base-char)) &rest args)
  (declare (ignore type args))
  #'(lambda (location &optional (offset 0))
      (code-char (sap-ref-8 location offset))))


(defmethod alien-type ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (string (type (eql 'string)) &rest args)
  (declare (ignore type args))
  `(let ((string ,string))
     ;; Always copy strings to prevent seg fault due to GC
     (copy-memory
      (make-pointer (1+ (kernel:get-lisp-obj-address string)))
      (1+ (length string)))))
  
(defmethod to-alien-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (string)
      (copy-memory
       (make-pointer (1+ (kernel:get-lisp-obj-address string)))
       (1+ (length string)))))

(defmethod from-alien-form (string (type (eql 'string)) &rest args)
  (declare (ignore type args))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      (prog1
	  (c-call::%naturalize-c-string string)
	(deallocate-memory string)))))

(defmethod from-alien-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (string)
      (unless (null-pointer-p string)
	(prog1
	    (c-call::%naturalize-c-string string)
	  (deallocate-memory string)))))

(defmethod cleanup-form (string (type (eql 'string)) &rest args)
  (declare (ignore type args))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      (deallocate-memory string))))

(defmethod cleanup-function ((type (eql 'string)) &rest args)
  (declare (ignore args))
  #'(lambda (string)
      (unless (null-pointer-p string)
	(deallocate-memory string))))

(defmethod copy-from-alien-form (string (type (eql 'string)) &rest args)
  (declare (ignore type args))
  `(let ((string ,string))
    (unless (null-pointer-p string)
      (c-call::%naturalize-c-string string))))

(defmethod copy-from-alien-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (string)
      (unless (null-pointer-p string)
	(c-call::%naturalize-c-string string))))

(defmethod writer-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (string location &optional (offset 0))
      (assert (null-pointer-p (sap-ref-sap location offset)))
      (setf (sap-ref-sap location offset)
       (copy-memory
	(make-pointer (1+ (kernel:get-lisp-obj-address string)))
	(1+ (length string))))))

(defmethod reader-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (location &optional (offset 0))
      (unless (null-pointer-p (sap-ref-sap location offset))
	(c-call::%naturalize-c-string (sap-ref-sap location offset)))))

(defmethod destroy-function ((type (eql 'string)) &rest args)
  (declare (ignore type args))
  #'(lambda (location &optional (offset 0))
      (unless (null-pointer-p (sap-ref-sap location offset))
	(deallocate-memory (sap-ref-sap location offset))
	(setf (sap-ref-sap location offset) (make-pointer 0)))))


(defmethod alien-type ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (alien-type 'string))

(defmethod size-of ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (size-of 'string))

(defmethod to-alien-form (path (type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (to-alien-form `(namestring (translate-logical-pathname ,path)) 'string))

(defmethod to-alien-function ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (let ((string-function (to-alien-function 'string)))
    #'(lambda (path)
	(funcall string-function (namestring path)))))

(defmethod from-alien-form (string (type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  `(parse-namestring ,(from-alien-form string 'string)))

(defmethod from-alien-function ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (let ((string-function (from-alien-function 'string)))
    #'(lambda (string)
	(parse-namestring (funcall string-function string)))))

(defmethod cleanup-form (string (type (eql 'pathnanme)) &rest args)
  (declare (ignore type args))
  (cleanup-form string 'string))

(defmethod cleanup-function ((type (eql 'pathnanme)) &rest args)
  (declare (ignore type args))
  (cleanup-function 'string))

(defmethod writer-function ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (let ((string-writer (writer-function 'string)))
    #'(lambda (path location &optional (offset 0))
	(funcall string-writer (namestring path) location offset))))

(defmethod reader-function ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (let ((string-reader (reader-function 'string)))
  #'(lambda (location &optional (offset 0))
      (let ((string (funcall string-reader location offset)))
	(when string
	  (parse-namestring string))))))

(defmethod destroy-function ((type (eql 'pathname)) &rest args)
  (declare (ignore type args))
  (destroy-function 'string))


(defmethod alien-type ((type (eql 'boolean)) &rest args)
  (apply #'alien-type 'signed-byte args))

(defmethod size-of ((type (eql 'boolean)) &rest args)
  (apply #'size-of 'signed-byte args))

(defmethod to-alien-form (boolean (type (eql 'boolean)) &rest args)
  (declare (ignore type args))
  `(if ,boolean 1 0))

(defmethod to-alien-function ((type (eql 'boolean)) &rest args)
  (declare (ignore type args))
  #'(lambda (boolean)
      (if boolean 1 0)))

(defmethod from-alien-form (boolean (type (eql 'boolean)) &rest args)
  (declare (ignore type args))
  `(not (zerop ,boolean)))

(defmethod from-alien-function ((type (eql 'boolean)) &rest args)
  (declare (ignore type args))
  #'(lambda (boolean)
      (not (zerop boolean))))

(defmethod writer-function ((type (eql 'boolean)) &rest args)
  (declare (ignore type))
  (let ((writer (apply #'writer-function 'signed-byte args)))
    #'(lambda (boolean location &optional (offset 0))
	(funcall writer (if boolean 1 0) location offset))))

(defmethod reader-function ((type (eql 'boolean)) &rest args)
  (declare (ignore type))
  (let ((reader (apply #'reader-function 'signed-byte args)))
  #'(lambda (location &optional (offset 0))
      (not (zerop (funcall reader location offset))))))


(defmethod alien-type ((type (eql 'or)) &rest args)
  (let ((alien-type (alien-type (first args))))
    (unless (every #'(lambda (type)
		       (eq alien-type (alien-type type)))
		   (rest args))
      (error "No common alien type specifier for union type: ~A" 
       (cons type args)))
    alien-type))

(defmethod size-of ((type (eql 'or)) &rest args)
  (declare (ignore type))
  (size-of (first args)))

(defmethod to-alien-form (form (type (eql 'or)) &rest args)
  (declare (ignore type))
  `(let ((value ,form))
    (etypecase value
      ,@(mapcar	 
	 #'(lambda (type)
	     `(,type ,(to-alien-form 'value type)))
	 args))))

(defmethod to-alien-function ((type (eql 'or)) &rest types)
  (declare (ignore type))
  (let ((functions (mapcar #'to-alien-function types)))
    #'(lambda (value)
	(loop
	 for function in functions
	 for type in types
	 when (typep value type)
	 do (return (funcall function value))
	 finally (error "~S is not of type ~A" value `(or ,@types))))))

(defmethod alien-type ((type (eql 'system-area-pointer)) &rest args)
  (declare (ignore type args))
  'system-area-pointer)

(defmethod size-of ((type (eql 'system-area-pointer)) &rest args)
  (declare (ignore type args))
  +size-of-pointer+)

(defmethod writer-function ((type (eql 'system-area-pointer)) &rest args)
  (declare (ignore type args))
  #'(lambda (sap location &optional (offset 0))
      (setf (sap-ref-sap location offset) sap)))

(defmethod reader-function ((type (eql 'system-area-pointer)) &rest args)
  (declare (ignore type args))
  #'(lambda (location &optional (offset 0))
      (sap-ref-sap location offset)))


(defmethod alien-type ((type (eql 'null)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'null)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (null (type (eql 'null)) &rest args)
  (declare (ignore null type args))
  `(make-pointer 0))

(defmethod to-alien-function ((type (eql 'null)) &rest args)
  (declare (ignore type args))
  #'(lambda (null)
      (declare (ignore null))
      (make-pointer 0)))


(defmethod alien-type ((type (eql 'nil)) &rest args)
  (declare (ignore type args))
  'c-call:void)

(defmethod from-alien-function ((type (eql 'nil)) &rest args)
  (declare (ignore type args))
  #'(lambda (value)
      (declare (ignore value))
      (values)))


(defmethod alien-type ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (alien-type (first args)))

(defmethod size-of ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (size-of (first args)))

(defmethod to-alien-form (form (type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (copy-to-alien-form form (first args)))

(defmethod to-alien-function ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (copy-to-alien-function (first args)))

(defmethod from-alien-form (form (type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (copy-from-alien-form form (first args)))

(defmethod from-alien-function ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (copy-from-alien-function (first args)))

(defmethod reader-function ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (reader-function (first args)))

(defmethod writer-function ((type (eql 'copy-of)) &rest args)
  (declare (ignore type))
  (writer-function (first args)))

(export 'copy-of)
