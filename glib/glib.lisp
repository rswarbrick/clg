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

;; $Id: glib.lisp,v 1.32 2006-02-06 18:12:19 espen Exp $


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
  `(or (null (cons ,type list))))

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

(defmethod alien-type ((type (eql 'glist)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'glist)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (list (type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args    
    `(make-glist ',element-type ,list)))

(defmethod to-alien-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args    
    #'(lambda (list)
	(make-glist element-type list))))

(defmethod from-alien-form (glist (type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(let ((glist ,glist))
      (unwind-protect
	   (map-glist 'list #'identity glist ',element-type)
	(destroy-glist glist ',element-type)))))

(defmethod from-alien-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (glist)
	(unwind-protect
	     (map-glist 'list #'identity glist element-type)
	  (destroy-glist glist element-type)))))

(defmethod copy-from-alien-form (glist (type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(map-glist 'list #'identity ,glist ',element-type)))

(defmethod copy-from-alien-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (glist)
	(map-glist 'list #'identity glist element-type))))

(defmethod cleanup-form (glist (type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(destroy-glist ,glist ',element-type)))

(defmethod cleanup-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (glist)
	(destroy-glist glist element-type))))

(defmethod writer-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (list location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-glist element-type list)))))

(defmethod reader-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-glist 'list #'identity (sap-ref-sap location offset) element-type)))))

(defmethod destroy-function ((type (eql 'glist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (destroy-glist (sap-ref-sap location offset) element-type)
	  (setf (sap-ref-sap location offset) (make-pointer 0))))))



;;;; Single linked list (GSList)

(deftype gslist (type) `(or (null (cons ,type list))))

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

(defmethod alien-type ((type (eql 'gslist)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'gslist)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (list (type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args    
    `(make-sglist ',element-type ,list)))

(defmethod to-alien-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args    
    #'(lambda (list)
	(make-gslist element-type list))))

(defmethod from-alien-form (gslist (type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(let ((gslist ,gslist))
      (unwind-protect
	   (map-glist 'list #'identity gslist ',element-type)
	(destroy-gslist gslist ',element-type)))))

(defmethod from-alien-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (gslist)
	(unwind-protect
	     (map-glist 'list #'identity gslist element-type)
	  (destroy-gslist gslist element-type)))))

(defmethod copy-from-alien-form (gslist (type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(map-glist 'list #'identity ,gslist ',element-type)))

(defmethod copy-from-alien-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (gslist)
	(map-glist 'list #'identity gslist element-type))))

(defmethod cleanup-form (gslist (type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(destroy-gslist ,gslist ',element-type)))

(defmethod cleanup-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (gslist)
	(destroy-gslist gslist element-type))))

(defmethod writer-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (list location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-gslist element-type list)))))

(defmethod reader-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-glist 'list #'identity (sap-ref-sap location offset) element-type)))))

(defmethod destroy-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
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


(defmethod alien-type ((type (eql 'vector)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'vector)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (vector (type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
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

(defmethod from-alien-form (c-vector (type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(let ((c-vector ,c-vector))
	(prog1
	    (map-c-vector 'vector #'identity c-vector ',element-type ,length)
	  (destroy-c-vector c-vector ',element-type ,length))))))

(defmethod copy-from-alien-form (c-vector (type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(map-c-vector 'vector #'identity ,c-vector ',element-type ,length))))

(defmethod copy-from-alien-function ((type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      #'(lambda (c-vector)
	  (map-c-vector 'vector #'identity c-vector element-type length)))))

(defmethod cleanup-form (location (type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
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

(defmethod writer-function ((type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-c-vector element-type length vector)))))

(defmethod reader-function ((type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    (if (eq length '*)
	(error "Can't create reader function for vector of variable size")
      #'(lambda (location &optional (offset 0) weak-p)
	  (declare (ignore weak-p))
	  (unless (null-pointer-p (sap-ref-sap location offset))
	    (map-c-vector 'vector #'identity (sap-ref-sap location offset) 
	     element-type length))))))

(defmethod destroy-function ((type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
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

(defmethod alien-type ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (vector (type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(make-0-vector ',element-type ,vector)))

(defmethod from-alien-form (c-vector (type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(let ((c-vector ,c-vector))
       (prog1
	   (map-0-vector 'vector #'identity c-vector ',element-type)
	 (destroy-0-vector c-vector ',element-type)))))

(defmethod copy-from-alien-form (c-vector (type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(map-0-vector 'vector #'identity ,c-vector ',element-type)))

(defmethod cleanup-form (location (type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(destroy-0-vector ,location ',element-type)))

(defmethod writer-function ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-0-vector element-type vector)))))

(defmethod reader-function ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-0-vector 'vector #'identity (sap-ref-sap location offset) 
	   element-type)))))

(defmethod destroy-function ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    (unless (eq (alien-type element-type) (alien-type 'pointer))
      (error "Elements in null-terminated vectors need to be of pointer types"))
    #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (sap-ref-sap location offset))
	    (destroy-0-vector 
	     (sap-ref-sap location offset) element-type)
	    (setf (sap-ref-sap location offset) (make-pointer 0))))))

(defmethod unbound-value ((type (eql 'null-terminated-vector)) &rest args)
  (declare (ignore type args))
  (values t nil))


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

(defmethod alien-type ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type args))
  (alien-type 'pointer))

(defmethod size-of ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type args))
  (size-of 'pointer))

(defmethod to-alien-form (vector (type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(make-counted-vector ',element-type ,vector)))

(defmethod from-alien-form (c-vector (type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(let ((c-vector ,c-vector))
       (prog1
	   (map-counted-vector 'vector #'identity c-vector ',element-type)
	 (destroy-counted-vector c-vector ',element-type)))))

(defmethod copy-from-alien-form (c-vector (type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(map-counted-vector 'vector #'identity ,c-vector ',element-type)))

(defmethod copy-from-alien-function ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (c-vector)
	(map-counted-vector 'vector #'identity c-vector element-type))))

(defmethod cleanup-form (location (type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    `(destroy-counted-vector ,location ',element-type)))

(defmethod writer-function ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (sap-ref-sap location offset)
	 (make-counted-vector element-type vector)))))

(defmethod reader-function ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (location &optional (offset 0) weak-p)
	(declare (ignore weak-p))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (map-counted-vector 'vector #'identity 
	   (sap-ref-sap location offset) element-type)))))

(defmethod destroy-function ((type (eql 'counted-vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (sap-ref-sap location offset))
	  (destroy-counted-vector 
	   (sap-ref-sap location offset) element-type)
	  (setf (sap-ref-sap location offset) (make-pointer 0))))))
