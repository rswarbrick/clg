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

;; $Id: glib.lisp,v 1.8 2001-02-11 20:21:13 espen Exp $


(in-package "GLIB")

(use-prefix "g")


;;;; Memory management

(define-foreign ("g_malloc0" allocate-memory) () pointer
  (size unsigned-long))

(define-foreign ("g_realloc" reallocate-memory) () pointer
  (address pointer)
  (size unsigned-long))

(define-foreign ("g_free" deallocate-memory) () nil
  (address pointer))

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

;(define-foreign %quark-get-reserved () quark)

(define-foreign %quark-from-string () quark
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

(define-foreign ("g_list_append" %glist-append-unsigned) () pointer
  (glist pointer)
  (data unsigned))

(define-foreign ("g_list_append" %glist-append-signed) () pointer
  (glist pointer)
  (data signed))

(define-foreign ("g_list_append" %glist-append-sap) () pointer
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
  
(define-foreign ("g_list_free" glist-free) () nil
  (glist pointer))

(deftype-method translate-type-spec glist (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of glist (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien glist (type-spec list &optional copy)
  (declare (ignore copy))
  (let* ((element-type-spec (second (type-expand-to 'glist type-spec)))
	 (to-alien (translate-to-alien element-type-spec 'element t)))
    `(let ((glist (make-pointer 0))) 
       (dolist (element ,list glist)
	 (setq glist (glist-append glist ,to-alien ,element-type-spec))))))

(deftype-method translate-from-alien
    glist (type-spec glist &optional (alloc :reference))
  (let ((element-type-spec (second (type-expand-to 'glist type-spec))))
    `(let ((glist ,glist)
	   (list nil))
       (do ((tmp glist (glist-next tmp)))
	   ((null-pointer-p tmp))
	 (push
	  ,(translate-from-alien
	    element-type-spec `(glist-data tmp ,element-type-spec) alloc)
	  list))
       ,(when (eq alloc :reference)
	  '(glist-free glist))
       (nreverse list))))

(deftype-method cleanup-alien glist (type-spec glist &optional copied)
  (declare (ignore copied))
  (let* ((element-type-spec (second (type-expand-to 'glist type-spec)))
	 (alien-type-spec (translate-type-spec element-type-spec)))
    `(let ((glist ,glist))
       (unless (null-pointer-p glist)
	 ,(when (eq alien-type-spec 'system-area-pointer)
	    `(do ((tmp glist (glist-next tmp)))
		 ((null-pointer-p tmp))
	       ,(cleanup-alien
		 element-type-spec `(glist-data tmp ,element-type-spec) t)))
	 (glist-free glist)))))



;;;; Single linked list (GSList)

(deftype gslist (type) `(or (null (cons ,type list))))

(define-foreign ("g_slist_prepend" %gslist-prepend-unsigned) () pointer
  (gslist pointer)
  (data unsigned))

(define-foreign ("g_slist_prepend" %gslist-prepend-signed) () pointer
  (gslist pointer)
  (data signed))

(define-foreign ("g_slist_prepend" %gslist-prepend-sap) () pointer
  (gslist pointer)
  (data pointer))

(defmacro gslist-prepend (gslist value type-spec)
  (ecase (first (mklist (translate-type-spec type-spec)))
    (unsigned `(%gslist-prepend-unsigned ,gslist ,value))
    (signed `(%gslist-prepend-signed ,gslist ,value))
    (system-area-pointer `(%gslist-prepend-sap ,gslist ,value))))
  
(define-foreign ("g_slist_free" gslist-free) () nil
  (gslist pointer))

(deftype-method translate-type-spec gslist (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of gslist (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien gslist (type-spec list &optional copy)
  (declare (ignore copy))
  (let* ((element-type-spec (second (type-expand-to 'gslist type-spec)))
	 (to-alien (translate-to-alien element-type-spec 'element t)))
    `(let ((gslist (make-pointer 0))) 
       (dolist (element (reverse ,list) gslist)
	 (setq gslist (gslist-prepend gslist ,to-alien ,element-type-spec))))))

(deftype-method translate-from-alien
    gslist (type-spec gslist &optional (alloc :reference))
  (let ((element-type-spec (second (type-expand-to 'gslist type-spec))))
    `(let ((gslist ,gslist)
	   (list nil))
       (do ((tmp gslist (glist-next tmp)))
	   ((null-pointer-p tmp))
	 (push
	  ,(translate-from-alien
	    element-type-spec `(glist-data tmp ,element-type-spec) alloc)
	  list))
       ,(when (eq alloc :reference)
	  '(gslist-free gslist))
       (nreverse list))))

(deftype-method cleanup-alien gslist (type-spec gslist &optional copied)
  (declare (ignore copied))
  (let* ((element-type-spec (second (type-expand-to 'gslist type-spec)))
	 (alien-type-spec (translate-type-spec element-type-spec)))
    `(let ((gslist ,gslist))
       (unless (null-pointer-p gslist)
	 ,(when (eq alien-type-spec 'system-area-pointer)
	    `(do ((tmp gslist (glist-next tmp)))
		 ((null-pointer-p tmp))
	       ,(cleanup-alien
		 element-type-spec `(glist-data tmp ,element-type-spec) t)))
	 (gslist-free gslist)))))



;;; Vector

(deftype-method translate-type-spec vector (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of vector (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien vector (type-spec vector &optional copy)
  (declare (ignore copy))
  (destructuring-bind (element-type &optional (length '*))
      (cdr (type-expand-to 'vector type-spec))
    (let ((element-size (size-of element-type)))
      `(let ((vector ,vector))
	 (let ((c-vector
		(allocate-memory
		 ,(if (eq length '*)
		      `(* ,element-size (length vector))
		    (* element-size length)))))
	   (dotimes (i ,(if (eq length '*) '(length vector) length) c-vector)
	     (setf
	      (,(sap-ref-fname element-type) c-vector (* i ,element-size))
	      ,(translate-to-alien element-type '(aref vector i) :copy))))))))

(deftype-method translate-from-alien
    vector (type-spec sap &optional (alloc :reference))
  (destructuring-bind (element-type &optional (length '*))
      (cdr (type-expand-to 'vector type-spec))
    (when (eq length '*)
      (error "Can't use vectors of variable length as return type"))
    (let ((element-size (size-of element-type)))
      `(let ((sap ,sap)
	     (vector (make-array ,length :element-type ',element-type)))
	 (dotimes (i ,length vector)
	   (setf
	    (aref vector i)
	    ,(translate-to-alien
	      element-type
	      `(,(sap-ref-fname element-type) sap (* i ,element-size))
	      alloc)))))))


(deftype-method cleanup-alien vector (type-spec sap &optional copied)
  (declare (ignore type-spec copied))
  ;; The individual elements also have to be cleaned up to avoid memory leaks,
  ;; but this is currently not possible because we can't always tell the
  ;; length of the vector
  `(deallocate-memory ,sap))
