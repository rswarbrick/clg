;; Common Lisp bindings for GTK+ 2.x
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

;; $Id: glib.lisp,v 1.36 2006-02-26 15:30:01 espen Exp $


(in-package "GLIB")

(use-prefix "g")


;;;; Memory management

(defbinding (allocate-memory "g_malloc0") () pointer
  (size unsigned-long))

(defbinding (reallocate-memory "g_realloc") () pointer
  (address pointer)
  (size unsigned-long))

(defbinding (deallocate-memory "g_free") () nil
  (address pointer))
;; (defun deallocate-memory (address)
;;   (declare (ignore address)))

(defun copy-memory (from length &optional (to (allocate-memory length)))
  #+cmu(system-area-copy from 0 to 0 (* 8 length))
  #+sbcl(system-area-ub8-copy from 0 to 0 length)
  to)

(defun clear-memory (from length)
  #+cmu(vm::system-area-fill 0 from 0 (* 8 length))
  #+sbcl(system-area-ub8-fill 0 from 0 length))

(defmacro with-allocated-memory ((var size) &body body)
  (if (constantp size)
      (let ((alien (make-symbol "ALIEN"))
	    (size (eval size)))
	`(with-alien ((,alien (array #+sbcl(sb-alien:unsigned 8) #+cmu(alien:unsigned 8) ,size)))
	   (let ((,var (alien-sap ,alien)))
	     (clear-memory ,var ,size)
	     ,@body)))
    `(let ((,var (allocate-memory ,size)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-memory ,var)))))


;;;; User data mechanism

(internal *user-data* *user-data-count*)

(defvar *user-data* (make-hash-table))
(defvar *user-data-count* 0)

(defun register-user-data (object &optional destroy-function)
  (check-type destroy-function (or null symbol function))
  (incf *user-data-count*)
  (setf
   (gethash *user-data-count* *user-data*)
   (cons object destroy-function))
  *user-data-count*)

(defun find-user-data (id)
  (check-type id fixnum)
  (multiple-value-bind (user-data p) (gethash id *user-data*)
    (values (car user-data) p)))

(defun user-data-exists-p (id)
  (nth-value 1 (find-user-data id)))

(defun update-user-data (id object)
  (check-type id fixnum)
  (multiple-value-bind (user-data exists-p) (gethash id *user-data*)
    (cond
     ((not exists-p) (error "User data id ~A does not exist" id))
     (t 
      (when (cdr user-data)
	(funcall (cdr user-data) (car user-data)))
      (setf (car user-data) object)))))

(defun destroy-user-data (id)
  (check-type id fixnum)
  (let ((user-data (gethash id *user-data*)))
    (when (cdr user-data)
      (funcall (cdr user-data) (car user-data))))
  (remhash id *user-data*))


;;;; Quarks

(deftype quark () 'unsigned)

(defbinding %quark-from-string () quark
  (string string))

(defun quark-intern (object)
  (etypecase object
    (quark object)
    (string (%quark-from-string object))
    (symbol (%quark-from-string (format nil "clg-~A:~A" 
				 (package-name (symbol-package object)) 
				 object)))))

(defbinding quark-to-string () (copy-of string)
  (quark quark))


;;;; Linked list (GList)

(deftype glist (type) 
  `(or null (cons ,type list)))

(defbinding (%glist-append "g_list_append") () pointer
  (glist pointer)
  (nil null))

(defun make-glist (type list)
  (loop
   with writer = (writer-function type)
   for element in list
   as glist = (%glist-append (or glist (make-pointer 0)))
   do (funcall writer element glist)
   finally (return glist)))

(defun glist-next (glist)
  (unless (null-pointer-p glist)
    (sap-ref-sap glist +size-of-pointer+)))
  
;; Also used for gslists
(defun map-glist (seqtype function glist element-type)
  (let ((reader (reader-function element-type)))
    (case seqtype 
     ((nil)
      (loop
       as tmp = glist then (glist-next tmp)
       until (null-pointer-p tmp)
       do (funcall function (funcall reader tmp))))
     (list
      (loop
       as tmp = glist then (glist-next tmp)
       until (null-pointer-p tmp)
       collect (funcall function (funcall reader tmp))))
     (t
      (coerce 
       (loop
	as tmp = glist then (glist-next tmp)
	until (null-pointer-p tmp)
	collect (funcall function (funcall reader tmp)))
       seqtype)))))

(defbinding (glist-free "g_list_free") () nil
  (glist pointer))

(defun destroy-glist (glist element-type)
  (loop
   with destroy = (destroy-function element-type)
   as tmp = glist then (glist-next tmp)
   until (null-pointer-p tmp)
   do (funcall destroy tmp 0))
  (glist-free glist))

(define-type-method alien-type ((type glist))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type glist))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type glist) list)
  (let ((element-type (second (type-expand-to 'glist type))))
    `(make-glist ',element-type ,list)))

(define-type-method to-alien-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (list)
	(make-glist element-type list))))

(define-type-method from-alien-form ((type glist) glist)
  (let ((element-type (second (type-expand-to 'glist type))))
    `(let ((glist ,glist))
      (unwind-protect
	   (map-glist 'list #'identity glist ',element-type)
	(destroy-glist glist ',element-type)))))

(define-type-method from-alien-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (glist)
	(unwind-protect
	     (map-glist 'list #'identity glist element-type)
	  (destroy-glist glist element-type)))))

(define-type-method copy-from-alien-form ((type glist) glist)
  (let ((element-type (second (type-expand-to 'glist type))))
    `(map-glist 'list #'identity ,glist ',element-type)))

(define-type-method copy-from-alien-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (glist)
	(map-glist 'list #'identity glist element-type))))

(define-type-method cleanup-form ((type glist) glist)
  (let ((element-type (second (type-expand-to 'glist type))))
    `(destroy-glist ,glist ',element-type)))

(define-type-method cleanup-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (glist)
	(destroy-glist glist element-type))))

(define-type-method writer-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (list location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-glist element-type list)))))

(define-type-method reader-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-glist 'list #'identity (sap-ref-sap location offset) element-type)))))

(define-type-method destroy-function ((type glist))
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (destroy-glist (sap-ref-sap location offset) element-type)
	  (setf (sap-ref-sap location offset) (make-pointer 0))))))



;;;; Single linked list (GSList)

(deftype gslist (type) `(or null (cons ,type list)))

(defbinding (%gslist-prepend "g_slist_prepend") () pointer
  (gslist pointer)
  (nil null))

(defun make-gslist (type list)
  (loop
   with writer = (writer-function type)
   for element in (reverse list)
   as gslist = (%gslist-prepend (or gslist (make-pointer 0)))
   do (funcall writer element gslist)
   finally (return gslist)))

(defbinding (gslist-free "g_slist_free") () nil
  (gslist pointer))

(defun destroy-gslist (gslist element-type)
  (loop
   with destroy = (destroy-function element-type)
   as tmp = gslist then (glist-next tmp)
   until (null-pointer-p tmp)
   do (funcall destroy tmp 0))
  (gslist-free gslist))

(define-type-method alien-type ((type gslist))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type gslist))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type gslist) list)
  (let ((element-type (second (type-expand-to 'gslist type))))
    `(make-sglist ',element-type ,list)))

(define-type-method to-alien-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (list)
	(make-gslist element-type list))))

(define-type-method from-alien-form ((type gslist) gslist)
  (let ((element-type (second (type-expand-to 'gslist type))))
    `(let ((gslist ,gslist))
      (unwind-protect
	   (map-glist 'list #'identity gslist ',element-type)
	(destroy-gslist gslist ',element-type)))))

(define-type-method from-alien-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (gslist)
	(unwind-protect
	     (map-glist 'list #'identity gslist element-type)
	  (destroy-gslist gslist element-type)))))

(define-type-method copy-from-alien-form ((type gslist) gslist)
  (let ((element-type (second (type-expand-to 'gslist type))))
    `(map-glist 'list #'identity ,gslist ',element-type)))

(define-type-method copy-from-alien-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (gslist)
	(map-glist 'list #'identity gslist element-type))))

(define-type-method cleanup-form ((type gslist) gslist)
  (let ((element-type (second (type-expand-to 'gslist type))))
    `(destroy-gslist ,gslist ',element-type)))

(define-type-method cleanup-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (gslist)
	(destroy-gslist gslist element-type))))

(define-type-method writer-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (list location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-gslist element-type list)))))

(define-type-method reader-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-glist 'list #'identity (sap-ref-sap location offset) element-type)))))

(define-type-method destroy-function ((type gslist))
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (destroy-gslist (sap-ref-sap location offset) element-type)
	  (setf (sap-ref-sap location offset) (make-pointer 0))))))


;;; Vector

(defun make-c-vector (type length &optional content location)
  (let* ((size-of-type (size-of type))
	 (location (or location (allocate-memory (* size-of-type length))))
	 (writer (writer-function type)))
    (etypecase content
      (vector
       (loop
	for element across content
	for i from 0 below length
	as offset = 0 then (+ offset size-of-type)
	do (funcall writer element location offset)))
      (list
       (loop
	for element in content
	for i from 0 below length
	as offset = 0 then (+ offset size-of-type)
	do (funcall writer element location offset))))
    location))


(defun map-c-vector (seqtype function location element-type length)
  (let ((reader (reader-function element-type))
	(size-of-element (size-of element-type)))
    (case seqtype 
     ((nil)
      (loop
       for i from 0 below length
       as offset = 0 then (+ offset size-of-element)
       do (funcall function (funcall reader location offset))))
     (list
      (loop
       for i from 0 below length
       as offset = 0 then (+ offset size-of-element)
       collect (funcall function (funcall reader location offset))))
     (t
      (loop
       with sequence = (make-sequence seqtype length)
       for i from 0 below length
       as offset = 0 then (+ offset size-of-element)
       do (setf 
	   (elt sequence i)
	   (funcall function (funcall reader location offset)))
       finally (return sequence))))))


(defun destroy-c-vector (location element-type length)
  (loop
   with destroy = (destroy-function element-type)
   with element-size = (size-of element-type)
   for i from 0 below length
   as offset = 0 then (+ offset element-size)
   do (funcall destroy location offset))
  (deallocate-memory location))


(define-type-method alien-type ((type vector))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type vector))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type vector) vector)
  (destructuring-bind (element-type &optional (length '*)) 
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	`(let* ((vector ,vector)
		(location (sap+
			   (allocate-memory (+ ,+size-of-int+ 
					       (* ,(size-of element-type) 
						  (length vector))))
			   ,+size-of-int+)))
	  (make-c-vector ',element-type (length vector) vector location)
	  (setf (sap-ref-32 location ,(- +size-of-int+)) (length vector))
	  location)	  
      `(make-c-vector ',element-type ,length ,vector))))

(define-type-method from-alien-form ((type vector) c-vector)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(let ((c-vector ,c-vector))
	(prog1
	    (map-c-vector 'vector #'identity c-vector ',element-type ,length)
	  (destroy-c-vector c-vector ',element-type ,length))))))

(define-type-method copy-from-alien-form ((type vector) c-vector)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(map-c-vector 'vector #'identity ,c-vector ',element-type ,length))))

(define-type-method copy-from-alien-function ((type vector))
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      #'(lambda (c-vector)
	  (map-c-vector 'vector #'identity c-vector element-type length)))))

(define-type-method cleanup-form ((type vector) location)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    `(let* ((location ,location)
	    (length ,(if (eq length '*)
			 `(sap-ref-32 location ,(- +size-of-int+))
			 length)))
      (loop
       with destroy = (destroy-function ',element-type)
       for i from 0 below length
       as offset = 0 then (+ offset ,(size-of element-type))
       do (funcall destroy location offset))
      (deallocate-memory ,(if (eq length '*) 
			      `(sap+ location  ,(- +size-of-int+))
			    'location)))))

;; We need these so that we can specify vectors with length given as
;; a non constant in callbacks
(define-type-method callback-from-alien-form ((type vector) form)
  (copy-from-alien-form type form))
(define-type-method callback-cleanup-form ((type vector) form)
  (declare (ignore type form))
  nil)


(define-type-method writer-function ((type vector))
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-c-vector element-type length vector)))))

(define-type-method reader-function ((type vector))
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't create reader function for vector of variable size")
      #'(lambda (location &optional (offset 0) weak-p)
	  (declare (ignore weak-p))
	  (unless (null-pointer-p (sap-ref-sap location offset))
	    (map-c-vector 'vector #'identity (sap-ref-sap location offset) 
	     element-type length))))))

(define-type-method destroy-function ((type vector))
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't create destroy function for vector of variable size")
      #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (sap-ref-sap location offset))
	    (destroy-c-vector 
	     (sap-ref-sap location offset) element-type length)
	    (setf (sap-ref-sap location offset) (make-pointer 0)))))))


;;;; Null terminated vector

(defun make-0-vector (type content &optional location)
  (let* ((size-of-type (size-of type))
	 (location (or 
		    location 
		    (allocate-memory (* size-of-type (1+ (length content))))))
	 (writer (writer-function type)))
    (etypecase content
      (vector
       (loop
	for element across content
	as offset = 0 then (+ offset size-of-type)
	do (funcall writer element location offset)
	finally (setf (sap-ref-sap location offset) (make-pointer 0))))
      (list
       (loop
	for element in content
	as offset = 0 then (+ offset size-of-type)
	do (funcall writer element location offset)
	finally (setf (sap-ref-sap location (+ offset size-of-type)) (make-pointer 0)))))
    location))


(defun map-0-vector (seqtype function location element-type)
  (let ((reader (reader-function element-type))
	(size-of-element (size-of element-type)))
    (case seqtype 
     ((nil)
      (loop
       as offset = 0 then (+ offset size-of-element)
       until (null-pointer-p (sap-ref-sap location offset))
       do (funcall function (funcall reader location offset))))
     (list
      (loop
       as offset = 0 then (+ offset size-of-element)
       until (null-pointer-p (sap-ref-sap location offset))
       collect (funcall function (funcall reader location offset))))
     (t
      (coerce 
       (loop
	as offset = 0 then (+ offset size-of-element)
	until (null-pointer-p (sap-ref-sap location offset))
	collect (funcall function (funcall reader location offset)))
       seqtype)))))


(defun destroy-0-vector (location element-type)
  (loop
   with destroy = (destroy-function element-type)
   with element-size = (size-of element-type)
   as offset = 0 then (+ offset element-size)
   until (null-pointer-p (sap-ref-sap location offset))
   do (funcall destroy location offset))
  (deallocate-memory location))

(deftype null-terminated-vector (element-type) `(vector ,element-type))

(define-type-method alien-type ((type null-terminated-vector))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type null-terminated-vector))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type null-terminated-vector) vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    `(make-0-vector ',element-type ,vector)))

(define-type-method from-alien-form ((type null-terminated-vector) c-vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    `(let ((c-vector ,c-vector))
       (prog1
	   (map-0-vector 'vector #'identity c-vector ',element-type)
	 (destroy-0-vector c-vector ',element-type)))))

(define-type-method copy-from-alien-form ((type null-terminated-vector) c-vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    `(map-0-vector 'vector #'identity ,c-vector ',element-type)))

(define-type-method cleanup-form ((type null-terminated-vector) location)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    `(destroy-0-vector ,location ',element-type)))

(define-type-method writer-function ((type null-terminated-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-0-vector element-type vector)))))

(define-type-method reader-function ((type null-terminated-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-0-vector 'vector #'identity (sap-ref-sap location offset) 
	   element-type)))))

(define-type-method destroy-function ((type null-terminated-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'null-terminated-vector type))
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (sap-ref-sap location offset))
	    (destroy-0-vector 
	     (sap-ref-sap location offset) element-type)
	    (setf (sap-ref-sap location offset) (make-pointer 0))))))

(define-type-method unbound-value ((type null-terminated-vector))
  (declare (ignore type))
  nil)




;;; Counted vector

(defun make-counted-vector (type content)
  (let* ((size-of-type (size-of type))
	 (length (length content))
	 (location 
	  (allocate-memory (+ +size-of-int+ (* size-of-type length)))))
    (setf (sap-ref-32 location 0) length)
    (make-c-vector type length content (sap+ location +size-of-int+))))

(defun map-counted-vector (seqtype function location element-type)
  (let ((length (sap-ref-32 location 0)))
    (map-c-vector 
     seqtype function (sap+ location +size-of-int+)
     element-type length)))

(defun destroy-counted-vector (location element-type)
  (loop
   with destroy = (destroy-function element-type)
   with element-size = (size-of element-type)
   for i from 0 below (sap-ref-32 location 0)
   as offset = +size-of-int+ then (+ offset element-size)
   do (funcall destroy location offset))
  (deallocate-memory location))


(deftype counted-vector (element-type) `(vector ,element-type))

(define-type-method alien-type ((type counted-vector))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type counted-vector))
  (declare (ignore type))
  (size-of 'pointer))

(define-type-method to-alien-form ((type counted-vector) vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    `(make-counted-vector ',element-type ,vector)))

(define-type-method from-alien-form ((type counted-vector) c-vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    `(let ((c-vector ,c-vector))
       (prog1
	   (map-counted-vector 'vector #'identity c-vector ',element-type)
	 (destroy-counted-vector c-vector ',element-type)))))

(define-type-method copy-from-alien-form ((type counted-vector) c-vector)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    `(map-counted-vector 'vector #'identity ,c-vector ',element-type)))

(define-type-method copy-from-alien-function ((type counted-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (c-vector)
	(map-counted-vector 'vector #'identity c-vector element-type))))

(define-type-method cleanup-form ((type counted-vector) location)
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    `(destroy-counted-vector ,location ',element-type)))

(define-type-method writer-function ((type counted-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-counted-vector element-type vector)))))

(define-type-method reader-function ((type counted-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-counted-vector 'vector #'identity 
	   (sap-ref-sap location offset) element-type)))))

(define-type-method destroy-function ((type counted-vector))
  (destructuring-bind (element-type)
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (destroy-counted-vector 
	   (sap-ref-sap location offset) element-type)
	  (setf (sap-ref-sap location offset) (make-pointer 0))))))
