;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: gforeign.lisp,v 1.1 2000-08-14 16:44:38 espen Exp $

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
  
(defmacro deftype (name parameters &body body)
  (destructuring-bind (lisp-name &optional alien-name) (mklist name)
    `(progn
       ,(when alien-name
	  `(setf (alien-type-name ',lisp-name) ,alien-name))
       (lisp:deftype ,lisp-name ,parameters ,@body))))

;; To make the compiler shut up
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-type-method-fun translate-type-spec (type-spec))
  (define-type-method-fun translate-to-alien (type-spec expr &optional copy))
  (define-type-method-fun translate-from-alien (type-spec expr &optional alloc))
  (define-type-method-fun cleanup-alien (type-spec expr &optional copied)))
  

;;;; 

(defvar *type-function-cache* (make-hash-table :test #'equal))

(defun get-cached-function (type-spec fname)
  (cdr (assoc fname (gethash type-spec *type-function-cache*))))

(defun set-cached-function (type-spec fname function)
  (push (cons fname function) (gethash type-spec *type-function-cache*))
  function)
  

;; Creates a function to translate an object of the specified type
;; from lisp to alien representation.
(defun get-to-alien-function (type-spec)
  (or
   (get-cached-function type-spec 'to-alien-function)
   (set-cached-function type-spec 'to-alien-function
    (compile
     nil
     `(lambda (object)
	(declare (ignorable object))
	,(translate-to-alien type-spec 'object))))))

;; and the opposite
(defun get-from-alien-function (type-spec)
  (or
   (get-cached-function type-spec 'from-alien-function)
   (set-cached-function type-spec 'from-alien-function
    (compile
     nil
     `(lambda (alien)
	(declare (ignorable alien))
	,(translate-from-alien type-spec 'alien))))))

;; and for cleaning up
(defun get-cleanup-function (type-spec)
  (or
   (get-cached-function type-spec 'cleanup-function)
   (set-cached-function type-spec 'cleanup-function
    (compile
     nil
     `(lambda (alien)
	(declare (ignorable alien))
	,(cleanup-alien type-spec 'alien))))))



;; Creates a function to write an object of the specified type
;; to the given memory location
(defun get-writer-function (type-spec)
  (or
   (get-cached-function type-spec 'writer-function)
   (set-cached-function type-spec 'writer-function
    (compile
     nil
     `(lambda (value sap offset)
	(declare (ignorable value sap offset))
	(setf
	 (,(sap-ref-fname type-spec) sap offset)
	 ,(translate-to-alien type-spec 'value :copy)))))))

;; Creates a function to read an object of the specified type
;; from the given memory location
(defun get-reader-function (type-spec)
  (or
   (get-cached-function type-spec 'reader-function)
   (set-cached-function type-spec 'reader-function
    (compile
     nil
     `(lambda (sap offset)	 
	(declare (ignorable sap offset))
	,(translate-from-alien
	  type-spec `(,(sap-ref-fname type-spec) sap offset) :copy))))))


(defun get-destroy-function (type-spec)
  (or
   (get-cached-function type-spec 'destroy-function)
   (set-cached-function type-spec 'destroy-function
    (compile
     nil
     `(lambda (sap offset)	 
	(declare (ignorable sap offset))
	,(cleanup-alien
	  type-spec `(,(sap-ref-fname type-spec) sap offset) :copied))))))



;;;;

(defconstant +size-of-int+ 4)
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


(defun signed (size)
  (if (eq size '*)
      `(signed ,(* 8 +size-of-int+))
    `(signed ,size)))

(defun unsigned (size)
  (if (eq size '*)
      `(unsigned ,(* 8 +size-of-int+))
    `(unsigned ,size)))

(defun size-of (type-spec)
  (let ((alien-type-spec (translate-type-spec type-spec)))
    (ecase (first (mklist alien-type-spec))
     ((signed unsigned) (/ (second alien-type-spec) 8))
     ((system-area-pointer single-float) +size-of-sap+)
     (single-float +size-of-float+)
     (double-float +size-of-double+))))


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

(defmacro use-prefix (prefix &optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-package-prefix ,prefix ,package)))


(defun default-alien-func-name (lisp-name)
  (let* ((lisp-name-string
	  (if (char= (char (the simple-string (string lisp-name)) 0) #\%)
	      (subseq (the simple-string (string lisp-name)) 1)
	    (string lisp-name)))
	 (prefix (package-prefix *package*))
	 (name (substitute #\_ #\- (string-downcase lisp-name-string))))
    (if (or (not prefix) (string= prefix ""))
	name
      (format nil "~A_~A" prefix name))))


(defmacro define-foreign (name lambda-list return-type-spec &rest docs/args)
  (multiple-value-bind (c-name lisp-name)
      (if (atom name)
	  (values (default-alien-func-name name) name)
	(values-list name))
    (let ((supplied-lambda-list lambda-list)
	  (docs nil)
	  (args nil))
      (dolist (doc/arg docs/args)
	(if (stringp doc/arg)
	    (push doc/arg docs)
	  (progn
	    (destructuring-bind (expr type &optional (style :in)) doc/arg
	      (unless (member style '(:in :out))
		(error "Bogus argument style ~S in ~S." style doc/arg))
	      (when (and (not supplied-lambda-list) (namep expr) (eq style :in))
		(push expr lambda-list))
	      (push
	       (list (if (namep expr) expr (gensym)) expr type style) args)))))
      
      (%define-foreign
       c-name lisp-name (or supplied-lambda-list (nreverse lambda-list))
       return-type-spec (reverse docs) (reverse args)))))


#+cmu
(defun %define-foreign (foreign-name lisp-name lambda-list
			return-type-spec docs args)
  (ext:collect ((alien-types) (alien-bindings) (alien-parameters)
		(alien-values) (alien-deallocatiors))
    (dolist (arg args)
      (destructuring-bind (var expr type-spec style) arg
	(let ((declaration (translate-type-spec type-spec))
	      (deallocation (cleanup-alien type-spec expr)))
	  (cond
	   ((eq style :out)
	    (alien-types `(* ,declaration))
	    (alien-parameters `(addr ,var))
	    (alien-bindings `(,var ,declaration))
	    (alien-values (translate-from-alien type-spec var)))
	  (deallocation
	   (alien-types declaration)
	   (alien-bindings
	    `(,var ,declaration ,(translate-to-alien type-spec expr)))
	   (alien-parameters var)
	   (alien-deallocatiors deallocation))
	  (t
	   (alien-types declaration)
	   (alien-parameters (translate-to-alien type-spec expr)))))))

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
			,(translate-from-alien return-type-spec alien-funcall)))
		   ,@(alien-deallocatiors)
		   (values result ,@(alien-values)))
	      `(progn
		 ,alien-funcall
		 ,@(alien-deallocatiors)
		 (values ,@(alien-values)))))))))

  


;;;; Translations for fundamental types

(lisp:deftype long (&optional (min '*) (max '*)) `(integer ,min ,max))
(lisp:deftype unsigned-long (&optional (min '*) (max '*)) `(integer ,min ,max))
(lisp:deftype int (&optional (min '*) (max '*)) `(long ,min ,max))
(lisp:deftype unsigned-int (&optional (min '*) (max '*)) `(unsigned-long ,min ,max))
(lisp:deftype short (&optional (min '*) (max '*)) `(int ,min ,max))
(lisp:deftype unsigned-short (&optional (min '*) (max '*)) `(unsigned-int ,min ,max))
(lisp:deftype signed (&optional (size '*)) `(signed-byte ,size))
(lisp:deftype unsigned (&optional (size '*)) `(signed-byte ,size))
(lisp:deftype char () 'base-char)
(lisp:deftype pointer () 'system-area-pointer)
(lisp:deftype boolean (&optional (size '*))
  (declare (ignore size))
  `(member t nil))
(lisp:deftype static (type) type)
(lisp:deftype invalid () nil)


(deftype-method cleanup-alien t (type-spec alien &optional copied)
  (declare (ignore type-spec alien copied))
  nil)


(deftype-method translate-to-alien integer (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method translate-from-alien integer (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec fixnum (type-spec)
  (declare (ignore type-spec))
  (signed '*))

(deftype-method translate-to-alien fixnum (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method translate-from-alien fixnum (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec long (type-spec)
  (declare (ignore type-spec))
  (signed '*))


(deftype-method translate-type-spec unsigned-long (type-spec)
  (declare (ignore type-spec))
  (unsigned '*))


(deftype-method translate-type-spec short (type-spec)
  (declare (ignore type-spec))
  '(signed 16))


(deftype-method translate-type-spec unsigned-short (type-spec)
  (declare (ignore type-spec))
  '(unsigned 16))


(deftype-method translate-type-spec signed-byte (type-spec)
  (destructuring-bind (name &optional (size '*))
      (type-expand-to 'signed-byte type-spec)
    (declare (ignore name))
    (signed size)))

(deftype-method translate-to-alien signed-byte (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method
    translate-from-alien signed-byte (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec unsigned-byte (type-spec)
  (destructuring-bind (name &optional (size '*))
      (type-expand-to 'unsigned-byte type-spec)
    (declare (ignore name))
    (unsigned size)))

(deftype-method
    translate-to-alien unsigned-byte (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method
    translate-from-alien unsigned-byte (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec single-float (type-spec)
  (declare (ignore type-spec))
  'single-float)

(deftype-method
    translate-to-alien single-float (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method
    translate-from-alien single-float (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec double-float (type-spec)
  (declare (ignore type-spec))
  'double-float)

(deftype-method
    translate-to-alien double-float (type-spec number &optional copy)
  (declare (ignore type-spec copy))
  number)

(deftype-method
    translate-from-alien double-float (type-spec number &optional alloc)
  (declare (ignore type-spec alloc))
  number)


(deftype-method translate-type-spec base-char (type-spec)
  (declare (ignore type-spec))
  '(unsigned 8))

(deftype-method translate-to-alien base-char (type-spec char &optional copy)
  (declare (ignore type-spec copy))
  `(char-code ,char))

(deftype-method translate-from-alien base-char (type-spec code &optional alloc)
  (declare (ignore type-spec alloc))
  `(code-char ,code))


(deftype-method translate-type-spec string (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method translate-to-alien string (type-spec string &optional copy)
  (declare (ignore type-spec))
  (if copy
      `(let ((string ,string))
	 (copy-memory
	  (make-pointer (1+ (kernel:get-lisp-obj-address string)))
	  (1+ (length string))))
    `(make-pointer (1+ (kernel:get-lisp-obj-address ,string)))))

(deftype-method
    translate-from-alien string (type-spec sap &optional (alloc :dynamic))
  (declare (ignore type-spec))
  `(let ((sap ,sap))
     (unless (null-pointer-p sap)
       (prog1
	   (c-call::%naturalize-c-string sap)
	 ,(when (eq alloc :dynamic) `(deallocate-memory ,sap))))))

(deftype-method cleanup-alien string (type-spec sap &optional copied)
  (declare (ignore type-spec))
  (when copied
    `(let ((sap ,sap))
       (unless (null-pointer-p sap)
	 (deallocate-memory sap)))))


(deftype-method translate-type-spec boolean (type-spec)
  (if (atom type-spec)
      (unsigned '*)
    (destructuring-bind (name &optional (size '*))
	(type-expand-to 'boolean type-spec)
      (declare (ignore name))
      (unsigned size))))

(deftype-method translate-to-alien boolean (type-spec boolean &optional copy)
  (declare (ignore type-spec copy))
  `(if ,boolean 1 0))

(deftype-method translate-from-alien boolean (type-spec int &optional alloc)
  (declare (ignore type-spec alloc))
  `(not (zerop ,int)))


(deftype-method translate-type-spec or (union-type-spec)
  (destructuring-bind (name &rest type-specs)
      (type-expand-to 'or union-type-spec)
    (declare (ignore name))
    (let ((type-spec-translations
	   (map 'list #'translate-type-spec type-specs)))
      (unless (apply #'all-equal type-spec-translations)
	(error
	 "No common alien type specifier for union type: ~A" union-type-spec))
      (first type-spec-translations))))

(deftype-method translate-to-alien or (union-type-spec expr &optional copy)
  (destructuring-bind (name &rest type-specs)
      (type-expand-to 'or union-type-spec)
    (declare (ignore name))
    `(let ((value ,expr))
       (etypecase value
	 ,@(map
	    'list
	    #'(lambda (type-spec)
		(list type-spec (translate-to-alien type-spec 'value copy)))
	    type-specs)))))



(deftype-method translate-type-spec system-area-pointer (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method
    translate-to-alien system-area-pointer (type-spec sap &optional copy)
  (declare (ignore type-spec copy))
  sap)

(deftype-method
  translate-from-alien system-area-pointer (type-spec sap &optional alloc)
  (declare (ignore type-spec alloc))
  sap)


(deftype-method translate-type-spec null (type-spec)
  (declare (ignore type-spec))
  'system-area-pointer)

(deftype-method translate-to-alien null (type-spec expr &optional copy)
  (declare (ignore type-spec copy))
  `(make-pointer 0))


(deftype-method translate-type-spec nil (type-spec)
  (declare (ignore type-spec))
  'void)


(deftype-method transalte-type-spec static (type-spec)
  (translate-type-spec (second type-spec)))
  
(deftype-method translate-to-alien static (type-spec expr &optional copy)
  (declare (ignore copy))
  (translate-to-alien (second type-spec) expr nil))

(deftype-method translate-from-alien static (type-spec alien &optional alloc)
  (declare (ignore alloc))
  (translate-from-alien (second type-spec) alien nil))

(deftype-method cleanup-alien static (type-spec alien &optional copied)
  (declare (ignore copied))
  (cleanup-alien type-spec alien nil))



;;;; Enum and flags type

(defun map-mappings (args op)
  (let ((current-value 0))
    (map
     'list 
     #'(lambda (mapping)
	 (destructuring-bind (symbol &optional (value current-value))
	     (mklist mapping)
	   (setf current-value (1+ value))
	   (case op
	     (:enum-int (list symbol value))
	     (:flags-int (list symbol (ash 1 value)))
	     (:int-enum (list value symbol))
	     (:int-flags (list (ash 1 value) symbol))
	     (:symbols symbol))))
     (if (integerp (first args))
	 (rest args)
       args))))

(lisp:deftype enum (&rest args)
  `(member ,@(map-mappings args :symbols)))

(deftype-method translate-type-spec enum (type-spec)
  (destructuring-bind (name &rest args) (type-expand-to 'enum type-spec)
    (declare (ignore name))
    (if (integerp (first args))
	`(signed ,(first args))
      '(signed 32))))

(deftype-method translate-to-alien enum (type-spec expr &optional copy)
  (declare (ignore copy))
  (destructuring-bind (name &rest args) (type-expand-to 'enum type-spec)
    (declare (ignore name))
    `(ecase ,expr
       ,@(map-mappings args :enum-int))))

(deftype-method translate-from-alien enum (type-spec expr &optional alloc)
  (declare (ignore alloc))
  (destructuring-bind (name &rest args) (type-expand-to 'enum type-spec)
    (declare (ignore name))
    `(ecase ,expr
       ,@(map-mappings args :int-enum))))


(lisp:deftype flags (&rest args)
  `(or
    null
    (cons
     (member ,@(map-mappings args :symbols))
     list)))

(deftype-method translate-type-spec flags (type-spec)
  (destructuring-bind (name &rest args) (type-expand-to 'flags type-spec)
    (declare (ignore name))
    (if (integerp (first args))
	`(signed ,(first args))
      '(signed 32))))

(deftype-method translate-to-alien flags (type-spec expr &optional copy)
  (declare (ignore copy))
  (destructuring-bind (name &rest args) (type-expand-to 'flags type-spec)
    (declare (ignore name))
    (let ((mappings (map-mappings args :flags-int)))
      `(let ((value 0))
	 (dolist (flag ,expr value)
	   (setq value (logior value (second (assoc flag ',mappings)))))))))

(deftype-method translate-from-alien flags (type-spec expr &optional alloc)
  (declare (ignore alloc))
  (destructuring-bind (name &rest args) (type-expand-to 'flags type-spec)
    (declare (ignore name))
    (let ((mappings (map-mappings args :int-flags)))
      `(let ((result nil))
	 (dolist (mapping ',mappings result)
	   (unless (zerop (logand ,expr (first mapping)))
	     (push (second mapping) result)))))))
