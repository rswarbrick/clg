;; Common Lisp bindings for GTK+ v1.2.x
;; Copyright (C) 1999 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: glib.lisp,v 1.21 2004-12-09 23:31:50 espen Exp $


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
  (kernel:system-area-copy from 0 to 0 (* 8 length))
  to)


;;;; User data mechanism

(internal *user-data* *user-data-count*)

(declaim (fixnum *user-data-count*))

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

(defun destroy-user-data (id)
  (check-type id fixnum)
  (let ((user-data (gethash id *user-data*)))
    (when (cdr user-data)
      (funcall (cdr user-data) (car user-data))))
  (remhash id *user-data*))


;;;; Quarks

(internal *quark-counter* *quark-from-object* *quark-to-object*)

(deftype quark () 'unsigned)

;(defbinding %quark-get-reserved () quark)

(defbinding %quark-from-string () quark
  (string string))

(defvar *quark-counter* 0)

(defun %quark-get-reserved ()
  ;; The string is just a dummy
  (%quark-from-string (format nil "#@£$%&-quark-~D" (incf *quark-counter*))))

(defvar *quark-from-object* (make-hash-table))
(defvar *quark-to-object* (make-hash-table))

(defun quark-from-object (object &key (test #'eq))
  (let ((hash-code (sxhash object)))
    (or
     (assoc-ref object (gethash hash-code *quark-from-object*) :test test)
     (let ((quark (%quark-get-reserved)))
       (setf
	(gethash hash-code *quark-from-object*)
	(append
	 (gethash hash-code *quark-from-object*)
	 (list (cons object quark))))
       (setf (gethash quark *quark-to-object*) object)
       quark))))

(defun quark-to-object (quark) 
  (gethash quark *quark-to-object*))
  
(defun remove-quark (quark)
  (let* ((object (gethash quark *quark-to-object*))
	 (hash-code (sxhash object)))
    (remhash quark *quark-to-object*)
    (unless (setf
	     (gethash hash-code *quark-from-object*)
	     (assoc-delete object (gethash hash-code *quark-from-object*)))
      (remhash hash-code *quark-from-object*))))



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
  (declare (ignore type args))
  (destructuring-bind (element-type) args
    #'(lambda (glist)
	(destroy-glist glist element-type))))


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

(defmethod from-alien-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type) args
    #'(lambda (gslist)
	(map-glist 'list #'identity gslist element-type))))

(defmethod cleanup-form (gslist (type (eql 'gslist)) &rest args)
  (declare (ignore type args))
  (destructuring-bind (element-type) args
    `(destroy-gslist ,gslist ',element-type)))

(defmethod cleanup-function ((type (eql 'gslist)) &rest args)
  (declare (ignore type args))
  (destructuring-bind (element-type) args
    #'(lambda (gslist)
	(destroy-gslist gslist element-type))))



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
	    (map-c-vector 'vector #'identity ',element-type ,length c-vector)
	  (destroy-c-vector c-vector ',element-type ,length))))))

(defmethod copy-from-alien-form (c-vector (type (eql 'vector)) &rest args)
  (declare (ignore type))
  (destructuring-bind (element-type &optional (length '*)) args
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(map-c-vector 'vector #'identity ',element-type ',length ,c-vector))))

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
      #'(lambda (location &optional (offset 0))
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
