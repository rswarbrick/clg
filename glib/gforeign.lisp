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

;; $Id: gforeign.lisp,v 1.10 2001-05-31 21:52:15 espen Exp $

(in-package "GLIB")

;;;; Type methods

(defvar *type-methods* (make-hash-table))

(defun ensure-type-method-fun (fname)
  (unless (fboundp fname)
    (setf
     (symbol-function fname)
     #'(lambda (type-spec &rest args)
	 (apply
	  (find-applicable-type-method type-spec fname) type-spec args)))))

(defmacro define-type-method-fun (fname lambda-list)
  (declare (ignore lambda-list))
  `(defun ,fname (type-spec &rest args)
     (apply
      (find-applicable-type-method type-spec ',fname) type-spec args)))


(defun ensure-type-name (type)
  (etypecase type
    (symbol type)
    (pcl::class (class-name type))))

(defun add-type-method (type fname function)
  (push
   (cons fname function)
   (gethash (ensure-type-name type) *type-methods*)))

(defun find-type-method (type fname)
  (cdr (assoc fname (gethash (ensure-type-name type) *type-methods*))))

(defun find-applicable-type-method (type-spec fname &optional (error t))
  (flet ((find-superclass-method (class)
	   (when class
  	     (unless (class-finalized-p class)		   
	       (finalize-inheritance class))
	     (dolist (super (cdr (pcl::class-precedence-list class)))
	       (return-if (find-type-method super fname)))))
	 (find-expanded-type-method (type-spec)
	   (multiple-value-bind (expanded-type-spec expanded-p)
	       (type-expand-1 type-spec)
	     (cond
	      (expanded-p 
	       (find-applicable-type-method expanded-type-spec fname nil))
	      ((neq type-spec t)
	       (find-applicable-type-method t fname nil))))))

    (or
     (typecase type-spec
       (pcl::class
	(or
	 (find-type-method type-spec fname)
	 (find-superclass-method type-spec)))
       (symbol
	(or
	 (find-type-method type-spec fname)
	 (find-expanded-type-method type-spec)
	 (find-superclass-method (find-class type-spec nil))))
       (cons
	(or
	 (find-type-method (first type-spec) fname)
	 (find-expanded-type-method type-spec)))
       (t
	(error "Invalid type specifier ~A" type-spec)))
     (and
      error
      (error
       "No applicable method for ~A when called with type specifier ~A"
       fname type-spec)))))

(defmacro deftype-method (fname type lambda-list &body body)
  `(progn
     (ensure-type-method-fun ',fname)
     (add-type-method ',type ',fname #'(lambda ,lambda-list ,@body))
     ',fname))
  
;; To make the compiler happy
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-type-method-fun translate-type-spec (type-spec))
  (define-type-method-fun size-of (type-spec))
  (define-type-method-fun translate-to-alien (type-spec expr &optional weak-ref))
  (define-type-method-fun translate-from-alien (type-spec expr &optional weak-ref))
  (define-type-method-fun cleanup-alien (type-spec sap &otional weak-ref))
  (define-type-method-fun unreference-alien (type-spec sap)))


;;;; 

(defvar *type-function-cache* (make-hash-table :test #'equal))

(defun get-cached-function (type-spec fname)
  (cdr (assoc fname (gethash type-spec *type-function-cache*))))

(defun set-cached-function (type-spec fname function)
  (push (cons fname function) (gethash type-spec *type-function-cache*))
  function)
  

(defun intern-argument-translator (type-spec)
  (or
   (get-cached-function type-spec 'argument-translator)
   (set-cached-function type-spec 'argument-translator
    (compile
     nil
     `(lambda (object)
	(declare (ignorable object))
	,(translate-to-alien type-spec 'object t))))))

(defun intern-return-value-translator (type-spec)
  (or
   (get-cached-function type-spec 'return-value-translator)
   (set-cached-function type-spec 'return-value-translator
    (compile
     nil
     `(lambda (alien)
	(declare (ignorable alien))
	,(translate-from-alien type-spec 'alien nil))))))

(defun intern-cleanup-function (type-spec)
  (or
   (get-cached-function type-spec 'cleanup-function)
   (set-cached-function type-spec 'cleanup-function
    (compile
     nil
     `(lambda (alien)
	(declare (ignorable alien))
	,(cleanup-alien type-spec 'alien t))))))



;; Returns a function to write an object of the specified type
;; to a memory location
(defun intern-writer-function (type-spec)
  (or
   (get-cached-function type-spec 'writer-function)
   (set-cached-function type-spec 'writer-function
    (compile
     nil
     `(lambda (value sap offset)
	(declare (ignorable value sap offset))
	(setf
	 (,(sap-ref-fname type-spec) sap offset)
	 ,(translate-to-alien type-spec 'value nil)))))))

;; Returns a function to read an object of the specified type
;; from a memory location
(defun intern-reader-function (type-spec)
  (or
   (get-cached-function type-spec 'reader-function)
   (set-cached-function type-spec 'reader-function
    (compile
     nil
     `(lambda (sap offset)	 
	(declare (ignorable sap offset))
	,(translate-from-alien
	  type-spec `(,(sap-ref-fname type-spec) sap offset) t))))))

(defun intern-destroy-function (type-spec)
  (if (atomic-type-p type-spec)
      #'(lambda (sap offset)	 
	  (declare (ignore sap offset)))
    (or
     (get-cached-function type-spec 'destroy-function)
     (set-cached-function type-spec 'destroy-function
       (compile
	nil
	`(lambda (sap offset)	 
	   (declare (ignorable sap offset))
	   ,(unreference-alien
	     type-spec `(,(sap-ref-fname type-spec) sap offset))))))))



;;;;

(defconstant +bits-per-unit+ 8
  "Number of bits in an addressable unit (byte)")

;; Sizes of fundamental C types in addressable units
(defconstant +size-of-short+ 2)
(defconstant +size-of-int+ 4)
(defconstant +size-of-long+ 4)
(defconstant +size-of-sap+ 4)
(defconstant +size-of-float+ 4)
(defconstant +size-of-double+ 8)

(defun sap-ref-unsigned (sap offset)
  (sap-ref-32 sap offset))

(defun sap-ref-signed (sap offset)
  (signed-sap-ref-32 sap offset))

(defun sap-ref-fname (type-spec)
  (let ((alien-type-spec (mklist (translate-type-spec type-spec))))
    (ecase (first alien-type-spec)
      (unsigned
       (ecase (second alien-type-spec)
	 (8 'sap-ref-8)
	 (16 'sap-ref-16)
	 (32 'sap-ref-32)
	 (64 'sap-ref-64)))
      (signed
       (ecase (second alien-type-spec)
	 (8 'signed-sap-ref-8)
	 (16 'signed-sap-ref-16)
	 (32 'signed-sap-ref-32)
	 (64 'signed-sap-ref-64)))
      (system-area-pointer 'sap-ref-sap)
      (single-float 'sap-ref-single)
      (double-float 'sap-ref-double))))


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
    
	 
(defmacro defbinding (name lambda-list return-type-spec &rest docs/args)
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
	      (unless (member style '(:in :out :in-out))
		(error "Bogus argument style ~S in ~S." style doc/arg))
	      (when (and
		     (not supplied-lambda-list)
		     (namep expr) (member style '(:in :in-out)))
		(push expr lambda-list))
	      (push
	       (list (if (namep expr) expr (gensym)) expr type style) args)))))
      
      (%defbinding
       c-name lisp-name (or supplied-lambda-list (nreverse lambda-list))
       return-type-spec (reverse docs) (reverse args)))))

#+cmu
(defun %defbinding (foreign-name lisp-name lambda-list
		    return-type-spec docs args)
  (ext:collect ((alien-types) (alien-bindings) (alien-parameters)
		(alien-values) (alien-deallocators))
    (dolist (arg args)
      (destructuring-bind (var expr type-spec style) arg
	(let ((declaration (translate-type-spec type-spec))
	      (deallocation (cleanup-alien type-spec var t)))
	  (cond
	   ((member style '(:out :in-out))
	    (alien-types `(* ,declaration))
	    (alien-parameters `(addr ,var))
	    (alien-bindings
	     `(,var ,declaration
	       ,@(when (eq style :in-out)
		   (list (translate-to-alien type-spec expr t)))))
	    (alien-values (translate-from-alien type-spec var nil)))
	  (deallocation
	   (alien-types declaration)
	   (alien-bindings
	    `(,var ,declaration ,(translate-to-alien type-spec expr t)))
	   (alien-parameters var)
	   (alien-deallocators deallocation))
	  (t
	   (alien-types declaration)
	   (alien-parameters (translate-to-alien type-spec expr t)))))))

    (let ((alien-funcall `(alien-funcall ,lisp-name ,@(alien-parameters))))
      `(defun ,lisp-name ,lambda-list
	 ,@docs
	 (with-alien ((,lisp-name
		       (function
			,(translate-type-spec return-type-spec)
			,@(alien-types))
		       :extern ,foreign-name)
		      ,@(alien-bindings))
	   ,(if return-type-spec
		`(let ((result
			,(translate-from-alien return-type-spec alien-funcall nil)))
		   ,@(alien-deallocators)
		   (values result ,@(alien-values)))
	      `(progn
		 ,alien-funcall
		 ,@(alien-deallocators)
		 (values ,@(alien-values)))))))))


(defun mkbinding (name rettype &rest types)
  (declare (optimize (ext:inhibit-warnings 3)))
  (let* ((ftype
	  `(function ,@(mapcar #'translate-type-spec (cons rettype types))))
	 (alien
	  (alien::%heap-alien
	   (alien::make-heap-alien-info
	    :type (alien::parse-alien-type ftype)
	    :sap-form (system:foreign-symbol-address name))))
	 (translate-arguments (mapcar #'intern-return-value-translator types))
	 (translate-return-value (intern-return-value-translator rettype))
	 (cleanup-arguments (mapcar #'intern-cleanup-function types)))
	 
    #'(lambda (&rest args)
	(map-into args #'funcall translate-arguments args)
	(prog1
	    (funcall
	     translate-return-value (apply #'alien:alien-funcall alien args))
	  (mapc #'funcall cleanup-arguments args)))))


;;;; Definitons and translations of fundamental types

(deftype long (&optional (min '*) (max '*)) `(integer ,min ,max))
(deftype unsigned-long (&optional (min '*) (max '*)) `(integer ,min ,max))
(deftype int (&optional (min '*) (max '*)) `(long ,min ,max))
(deftype unsigned-int (&optional (min '*) (max '*)) `(unsigned-long ,min ,max))
(deftype short (&optional (min '*) (max '*)) `(int ,min ,max))
(deftype unsigned-short (&optional (min '*) (max '*)) `(unsigned-int ,min ,max))
(deftype signed (&optional (size '*)) `(signed-byte ,size))
(deftype unsigned (&optional (size '*)) `(signed-byte ,size))
(deftype char () 'base-char)
(deftype pointer () 'system-area-pointer)
(deftype boolean (&optional (size '*))
  (declare (ignore size))
  `(member t nil))
(deftype invalid () nil)

(defun atomic-type-p (type-spec)
  (or
   (eq type-spec 'pointer)
   (not (eq (translate-type-spec type-spec) 'system-area-pointer))))


(deftype-method cleanup-alien t (type-spec sap &optional weak-ref)
  (declare (ignore type-spec sap weak-ref))
  nil)


(deftype-method translate-to-alien integer (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien integer (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec fixnum (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'signed))

(deftype-method size-of fixnum (type-spec)
  (declare (ignore type-spec))
  (size-of 'signed))

(deftype-method translate-to-alien fixnum (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien fixnum (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec long (type-spec)
  (declare (ignore type-spec))
  `(signed ,(* +bits-per-unit+ +size-of-long+)))

(deftype-method size-of long (type-spec)
  (declare (ignore type-spec))
  +size-of-long+)


(deftype-method translate-type-spec unsigned-long (type-spec)
  (declare (ignore type-spec))
  `(unsigned ,(* +bits-per-unit+ +size-of-long+)))

(deftype-method size-of unsigned-long (type-spec)
  (declare (ignore type-spec))
  +size-of-long+)


(deftype-method translate-type-spec int (type-spec)
  (declare (ignore type-spec))
  `(signed ,(* +bits-per-unit+ +size-of-int+)))

(deftype-method size-of int (type-spec)
  (declare (ignore type-spec))
  +size-of-int+)


(deftype-method translate-type-spec unsigned-int (type-spec)
  (declare (ignore type-spec))
  `(unsigned ,(* +bits-per-unit+ +size-of-int+)))

(deftype-method size-of unsigned-int (type-spec)
  (declare (ignore type-spec))
  +size-of-int+)


(deftype-method translate-type-spec short (type-spec)
  (declare (ignore type-spec))
  `(signed ,(* +bits-per-unit+ +size-of-short+)))

(deftype-method size-of short (type-spec)
  (declare (ignore type-spec))
  +size-of-short+)


(deftype-method translate-type-spec unsigned-short (type-spec)
  (declare (ignore type-spec))
  `(unsigned ,(* +bits-per-unit+ +size-of-short+)))

(deftype-method size-of unsigned-short (type-spec)
  (declare (ignore type-spec))
  +size-of-short+)


(deftype-method translate-type-spec signed-byte (type-spec)
  (let ((size (second (mklist (type-expand-to 'signed-byte type-spec)))))
    `(signed
      ,(cond
	((member size '(nil *)) (* +bits-per-unit+ +size-of-int+))
	(t size)))))

(deftype-method size-of signed-byte (type-spec)
  (let ((size (second (mklist (type-expand-to 'signed-byte type-spec)))))
    (cond
     ((member size '(nil *)) +size-of-int+)
     (t (/ size +bits-per-unit+)))))

(deftype-method translate-to-alien signed-byte (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien signed-byte
    (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec unsigned-byte (type-spec)
  (let ((size (second (mklist (type-expand-to 'unsigned-byte type-spec)))))
    `(signed
      ,(cond
	((member size '(nil *)) (* +bits-per-unit+ +size-of-int+))
	(t size)))))

(deftype-method size-of unsigned-byte (type-spec)
  (let ((size (second (mklist (type-expand-to 'unsigned-byte type-spec)))))
    (cond
     ((member size '(nil *)) +size-of-int+)
     (t (/ size +bits-per-unit+)))))

(deftype-method translate-to-alien unsigned-byte (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien unsigned-byte
    (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec single-float (type-spec)
  (declare (ignore type-spec))
  'single-float)

(deftype-method size-of single-float (type-spec)
  (declare (ignore type-spec))
  +size-of-float+)

(deftype-method translate-to-alien single-float (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien single-float
    (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec double-float (type-spec)
  (declare (ignore type-spec))
  'double-float)

(deftype-method size-of double-float (type-spec)
  (declare (ignore type-spec))
  +size-of-double+)

(deftype-method translate-to-alien double-float (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)

(deftype-method translate-from-alien double-float
    (type-spec number &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  number)


(deftype-method translate-type-spec base-char (type-spec)
  (declare (ignore type-spec))
  `(unsigned ,+bits-per-unit+))

(deftype-method size-of base-char (type-spec)
  (declare (ignore type-spec))
  1)

(deftype-method translate-to-alien base-char (type-spec char &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(char-code ,char))

(deftype-method translate-from-alien base-char (type-spec code &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(code-char ,code))


(deftype-method translate-type-spec string (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method size-of string (type-spec)
  (declare (ignore type-spec))
  +size-of-sap+)

(deftype-method translate-to-alien string (type-spec string &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(let ((string ,string))
     ;; Always copy strings to prevent seg fault due to GC
     (copy-memory
      (make-pointer (1+ (kernel:get-lisp-obj-address string)))
      (1+ (length string)))))

(deftype-method translate-from-alien string
    (type-spec c-string &optional weak-ref)
  (declare (ignore type-spec))
  `(let ((c-string ,c-string))
     (unless (null-pointer-p c-string)
       (prog1
	   (c-call::%naturalize-c-string c-string)
	 ;,(unless weak-ref `(deallocate-memory c-string))
	 ))))

(deftype-method cleanup-alien string (type-spec c-string &optional weak-ref)
  (when weak-ref
    (unreference-alien type-spec c-string)))

(deftype-method unreference-alien string (type-spec c-string)
  (declare (ignore type-spec))
  `(let ((c-string ,c-string))
     (unless (null-pointer-p c-string)
       (deallocate-memory c-string))))
  

(deftype-method translate-type-spec boolean (type-spec)
  (translate-type-spec
   (cons 'unsigned (cdr (mklist (type-expand-to 'boolean type-spec))))))

(deftype-method size-of boolean (type-spec)
  (size-of
   (cons 'unsigned (cdr (mklist (type-expand-to 'boolean type-spec))))))

(deftype-method translate-to-alien boolean (type-spec boolean &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(if ,boolean 1 0))

(deftype-method translate-from-alien boolean (type-spec int &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(not (zerop ,int)))


(deftype-method translate-type-spec or (union-type)
  (let* ((member-types (cdr (type-expand-to 'or union-type)))
	 (alien-type (translate-type-spec (first member-types))))
    (dolist (type (cdr member-types))
      (unless (eq alien-type (translate-type-spec type))
	(error "No common alien type specifier for union type: ~A" union-type)))
    alien-type))

(deftype-method size-of or (union-type)
  (size-of (first (cdr (type-expand-to 'or union-type)))))

(deftype-method translate-to-alien or (union-type-spec expr &optional weak-ref)
  (destructuring-bind (name &rest type-specs)
      (type-expand-to 'or union-type-spec)
    (declare (ignore name))
    `(let ((value ,expr))
       (etypecase value
	 ,@(map
	    'list
	      #'(lambda (type-spec)
		  (list type-spec (translate-to-alien type-spec 'value weak-ref)))
	      type-specs)))))


(deftype-method translate-type-spec system-area-pointer (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method size-of system-area-pointer (type-spec)
  (declare (ignore type-spec))
  +size-of-sap+)

(deftype-method translate-to-alien system-area-pointer (type-spec sap &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  sap)

(deftype-method translate-from-alien system-area-pointer
    (type-spec sap &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  sap)


(deftype-method translate-type-spec null (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method translate-to-alien null (type-spec expr &optional weak-ref)
  (declare (ignore type-spec expr weak-ref))
  `(make-pointer 0))


(deftype-method translate-type-spec nil (type-spec)
  (declare (ignore type-spec))
  'void)

(deftype-method translate-from-alien nil (type-spec expr &optional weak-ref)
  (declare (ignore type-spec weak-ref))
  `(progn
     ,expr
     (values)))
