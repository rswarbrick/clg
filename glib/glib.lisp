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

;; $Id: glib.lisp,v 1.14 2004-10-31 11:37:11 espen Exp $


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
;(defun deallocate-memory (address)
;  (declare (ignore address)))

(defun copy-memory (from length &optional (to (allocate-memory length)))
  (kernel:system-area-copy from 0 to 0 (* 8 length))
  to)


;;;; User data mechanism

(internal *user-data* *user-data-count*)

(declaim (fixnum *user-data-count*))

(defvar *destroy-notify* (system:foreign-symbol-address "destroy_notify"))
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

(deftype glist (type) `(or (null (cons ,type list))))

(defbinding (%glist-append-unsigned "g_list_append") () pointer
  (glist pointer)
  (data unsigned))

(defbinding (%glist-append-signed "g_list_append") () pointer
  (glist pointer)
  (data signed))

(defbinding (%glist-append-sap "g_list_append") () pointer
  (glist pointer)
  (data pointer))

(defmacro glist-append (glist value type-spec)
  (ecase (first (mklist (translate-type-spec type-spec)))
    (unsigned `(%glist-append-unsigned ,glist ,value))
    (signed `(%glist-append-signed ,glist ,value))
    (system-area-pointer `(%glist-append-sap ,glist ,value))))

(defmacro glist-data (glist type-spec)
  (ecase (first (mklist (translate-type-spec type-spec)))
    (unsigned `(sap-ref-unsigned ,glist 0))
    (signed `(sap-ref-signed ,glist 0))
    (system-area-pointer `(sap-ref-sap ,glist 0))))

(defun glist-next (glist)
  (unless (null-pointer-p glist)
    (sap-ref-sap glist +size-of-sap+)))
  
(defbinding (glist-free "g_list_free") () nil
  (glist pointer))

(deftype-method translate-type-spec glist (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of glist (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien glist (type-spec list &optional weak-ref)
  (declare (ignore weak-ref))
  (let* ((element-type (second (type-expand-to 'glist type-spec)))
	 (element (translate-to-alien element-type 'element)))
    `(let ((glist (make-pointer 0))) 
       (dolist (element ,list glist)
	 (setq glist (glist-append glist ,element ,element-type))))))

(deftype-method translate-from-alien
    glist (type-spec glist &optional weak-ref)
  (let ((element-type (second (type-expand-to 'glist type-spec))))
    `(let ((glist ,glist)
	   (list nil))
       (do ((tmp glist (glist-next tmp)))
	   ((null-pointer-p tmp))
	 (push
	  ,(translate-from-alien
	    element-type `(glist-data tmp ,element-type) weak-ref)
	  list))
       ,(unless weak-ref
	  '(glist-free glist))
       (nreverse list))))

(deftype-method cleanup-alien glist (type-spec glist &optional weak-ref)
  (when weak-ref
    (unreference-alien type-spec glist)))

(deftype-method unreference-alien glist (type-spec glist)
  (let ((element-type (second (type-expand-to 'glist type-spec))))
    `(let ((glist ,glist))
       (unless (null-pointer-p glist)
	 ,(unless (atomic-type-p element-type)
	    `(do ((tmp glist (glist-next tmp)))
		 ((null-pointer-p tmp))
	       ,(unreference-alien
		 element-type `(glist-data tmp ,element-type))))
	 (glist-free glist)))))


;;;; Single linked list (GSList)

(deftype gslist (type) `(or (null (cons ,type list))))

(defbinding (%gslist-prepend-unsigned "g_slist_prepend") () pointer
  (gslist pointer)
  (data unsigned))

(defbinding (%gslist-prepend-signed "g_slist_prepend") () pointer
  (gslist pointer)
  (data signed))

(defbinding (%gslist-prepend-sap "g_slist_prepend") () pointer
  (gslist pointer)
  (data pointer))

(defmacro gslist-prepend (gslist value type-spec)
  (ecase (first (mklist (translate-type-spec type-spec)))
    (unsigned `(%gslist-prepend-unsigned ,gslist ,value))
    (signed `(%gslist-prepend-signed ,gslist ,value))
    (system-area-pointer `(%gslist-prepend-sap ,gslist ,value))))
  
(defbinding (gslist-free "g_slist_free") () nil
  (gslist pointer))

(deftype-method translate-type-spec gslist (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of gslist (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien gslist (type-spec list &optional weak-ref)
  (declare (ignore weak-ref))
  (let* ((element-type (second (type-expand-to 'gslist type-spec)))
	 (element (translate-to-alien element-type 'element)))
    `(let ((gslist (make-pointer 0))) 
       (dolist (element (reverse ,list) gslist)
	 (setq gslist (gslist-prepend gslist ,element ,element-type))))))

(deftype-method translate-from-alien
    gslist (type-spec gslist &optional weak-ref)
  (let ((element-type (second (type-expand-to 'gslist type-spec))))
    `(let ((gslist ,gslist)
	   (list nil))
       (do ((tmp gslist (glist-next tmp)))
	   ((null-pointer-p tmp))
	 (push
	  ,(translate-from-alien
	    element-type `(glist-data tmp ,element-type) weak-ref)
	  list))
       ,(unless weak-ref
	  '(gslist-free gslist))
       (nreverse list))))

(deftype-method cleanup-alien gslist (type-spec gslist &optional weak-ref)
  (when weak-ref
    (unreference-alien type-spec gslist)))

(deftype-method unreference-alien gslist (type-spec gslist)
  (let ((element-type (second (type-expand-to 'gslist type-spec))))
    `(let ((gslist ,gslist))
       (unless (null-pointer-p gslist)
	 ,(unless (atomic-type-p element-type)
	    `(do ((tmp gslist (glist-next tmp)))
		 ((null-pointer-p tmp))
	       ,(unreference-alien
		 element-type `(glist-data tmp ,element-type))))
	 (gslist-free gslist)))))



;;; Vector

(defvar *magic-end-of-array* (allocate-memory 1))

(deftype-method translate-type-spec vector (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of vector (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien vector (type-spec vector &optional weak-ref)
  (declare (ignore weak-ref))
  (destructuring-bind (element-type &optional (length '*))
      (cdr (type-expand-to 'vector type-spec))
    (let* ((element-size (size-of element-type))
	   (size (cond
		  ((not (eq length '*))
		   (* element-size length))
		  ((not (atomic-type-p element-type))
		   `(* ,element-size (1+ (length vector))))
		  (t
		   `(* ,element-size (length vector))))))
	  
      `(let ((vector ,vector))
	 (let ((c-vector (allocate-memory ,size)))
	   (dotimes (i ,(if (eq length '*) '(length vector) length))
	     (setf
	      (,(sap-ref-fname element-type) c-vector (* i ,element-size))
	      ,(translate-to-alien element-type '(aref vector i))))
	   ,(when (and
		   (eq length '*)
		   (not (atomic-type-p element-type)))
	      `(setf
		(sap-ref-sap c-vector (* (length vector) ,element-size))
		*magic-end-of-array*))
	   c-vector)))))

(deftype-method translate-from-alien
    vector (type-spec c-array &optional weak-ref)
  (destructuring-bind (element-type &optional (length '*))
      (cdr (type-expand-to 'vector type-spec))
    (when (eq length '*)
      (error "Can't use vectors of variable length as return type"))
    (let ((element-size (size-of element-type)))
      `(let ((c-array ,c-array)
	     (vector (make-array ,length :element-type ',element-type)))
	 (dotimes (i ,length)
	   (setf
	    (aref vector i)
	    ,(translate-from-alien
	      element-type
	      `(,(sap-ref-fname element-type) c-array (* i ,element-size))
	      weak-ref)))
	 ,(unless weak-ref
	    '(deallocate-memory c-vector))
	 vector))))
	 

(deftype-method cleanup-alien vector (type-spec c-vector &optional weak-ref)
  (when weak-ref
    (unreference-alien type-spec c-vector)))

(deftype-method unreference-alien vector (type-spec c-vector)
  (destructuring-bind (element-type &optional (length '*))
      (cdr (type-expand-to 'vector type-spec))
    `(let ((c-vector ,c-vector))
       (unless (null-pointer-p c-vector)
	 ,(unless (atomic-type-p element-type)
	    (let ((element-size (size-of element-type)))
	      (if (not (eq length '*))
		  `(dotimes (i ,length)
		     (unreference-alien
		      element-type (sap-ref-sap c-vector (* i ,element-size))))
		`(do ((offset 0 (+ offset ,element-size)))
		      ((sap=
			(sap-ref-sap c-vector offset)
			*magic-end-of-array*))
		     ,(unreference-alien
		       element-type '(sap-ref-sap c-vector offset))))))
	 (deallocate-memory c-vector)))))


(defun map-c-array (seqtype function location element-type length)
  (let ((reader (intern-reader-function element-type))
	(size (size-of element-type)))
    (case seqtype 
     ((nil)
      (dotimes (i length)
	(funcall function (funcall reader location (* i size)))))
     (list
      (let ((list nil))
	(dotimes (i length)
	  (push (funcall function (funcall reader location (* i size))) list))
	(nreverse list)))
     (t
      (let ((sequence (make-sequence seqtype length)))
	(dotimes (i length)
	  (setf
	   (elt sequence i)
	   (funcall function (funcall reader location (* i size)))))
	sequence)))))
