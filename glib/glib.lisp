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

;; $Id: glib.lisp,v 1.6 2000-09-04 22:10:26 espen Exp $


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



;;;; Quarks

(deftype quark () 'unsigned)

;(define-foreign %quark-get-reserved () quark)

(define-foreign %quark-from-string () quark
  (string string))

(defvar *string-counter* 0)

(defun %quark-get-reserved ()
  (%quark-from-string (format nil "CLG-~D" (incf *string-counter*))))

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



;;;; Linked list

(deftype glist () 'pointer)
(deftype double-list (type) `(or (null (cons ,type list))))


(define-foreign ("g_list_append" %glist-append) () glist
  (glist glist)
  (data unsigned))

(defmacro glist-append (glist value type-spec)
  (ecase (first (mklist (translate-type-spec type-spec)))
    (unsigned `(%glist-append ,glist ,value))
;    (signed `(%glist-append ,glist (signed-to-unsigned ,value)))
    (system-area-pointer `(%glist-append ,glist (system:sap-int ,value)))))


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


(deftype-method translate-type-spec double-list (type-spec)
  (declare (ignore type-spec))
  (translate-type-spec 'pointer))

(deftype-method size-of double-list (type-spec)
  (declare (ignore type-spec))
  (size-of 'pointer))

(deftype-method translate-to-alien double-list (type-spec list &optional copy)
  (declare (ignore copy))
  (let* ((element-type-spec (second (type-expand-to 'double-list type-spec)))
	 (to-alien (translate-to-alien element-type-spec 'element t)))
    `(let ((glist (make-pointer 0))) 
       (dolist (element ,list glist)
	 (setq glist (glist-append glist ,to-alien ,element-type-spec))))))

(deftype-method
    translate-from-alien
    double-list (type-spec glist &optional (alloc :reference))
  (let ((element-type-spec (second (type-expand-to 'double-list type-spec))))
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

(deftype-method cleanup-alien double-list (type-spec glist &optional copied)
  (declare (ignore copied))
  (let* ((element-type-spec (second (type-expand-to 'double-list type-spec)))
	 (alien-type-spec (translate-type-spec element-type-spec)))
    `(let ((glist ,glist))
       (unless (null-pointer-p glist)
	 ,(when (eq alien-type-spec 'system-area-pointer)
	    `(do ((tmp glist (glist-next tmp)))
		 ((null-pointer-p tmp))
	       ,(cleanup-alien
		 element-type-spec `(glist-data tmp ,element-type-spec) t)))
	 (glist-free glist)))))



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
    (let ((element-to-alien (translate-to-alien element-type 'element :copy))
	  (element-size (size-of element-type)))
      `(let ((vector ,vector))
	 (let ((c-vector
		(allocate-memory
		 ,(if (eq length '*)
		      `(* ,element-size (length vector))
		    (* element-size length)))))
	   (dotimes (i ,(if (eq length '*) '(length vector) length) c-vector)
	     (setf
	      (,(sap-ref-fname element-type) c-vector (* i ,element-size))
	      ,(translate-to-alien element-type '(svref vector i) :copy))))))))

(deftype-method cleanup-alien vector (type-spec sap &optional copied)
  (declare (ignore type-spec copied))
  ;; The individual elements also have to be cleaned up to avoid memory leaks,
  ;; but this is currently not possible because we can't always tell the
  ;; length of the vector
  `(deallocate-memory ,sap))
