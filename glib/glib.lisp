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

;; $Id: glib.lisp,v 1.4 2000-08-23 14:27:41 espen Exp $


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

(define-foreign %quark-get-reserved () quark)

(defvar *quark-from-object* (make-hash-table))
(defvar *quark-to-object* (make-hash-table))

(defun quark-from-object (object &key (test #'eq))
  (let ((hash-code (sxhash object)))
    (or
     (assoc-ref object (gethash hash-code *quark-from-object*) :test test)
     (let ((quark (%quark-get-reserved)))
       (push (cons object quark) (gethash hash-code *quark-from-object*))
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
  'system-area-pointer)

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



;;; Array
#|
(define-foreign ("g_array_new" %array-new) () garray
  (zero-terminated boolean)
  (clear boolean)
  (element-size unsigned-int))

(defun array-new (&key zero-terminated clear (element-size 4) initial-contents)
  (let ((array (%array-new zero-terminated clear element-size)))
    (when initial-contents
      (dolist (element initial-contents)
	(array-append array element)))
    array))

(define-foreign ("g_array_free" %array-free) () none
  (array garray)
  (free-segment boolean))

(defun array-free (array &optional free-data (free-segment t))
  (when free-data
    (dotimes (i (array-get-size array))
      (free (array-get-pointer array i))))
  (%array-free array free-segment))

(defmacro with-array (binding &body body)
  (let ((array (gensym)))
    (destructuring-bind (var &rest args
			 &key (free-contents nil) (free-segment t)
			 &allow-other-keys )
        binding
      (remf args :free-contents)
      (remf args :free-segment)
      `(let* ((,array (array-new ,@args))
	      (,var (array-get-data ,array)))
	 (unwind-protect
	     ,@body
	   (array-free ,array ,free-contents ,free-segment))))))

;; cl-gtk.c
(define-foreign ("g_array_insert_int" array-insert-int) () garray
  (array garray)
  (index unsigned-int)
  (value int))

(defun array-insert-value (array index value)
  (etypecase value
    (null (array-insert-int array index 0))
    (integer (array-insert-int array index value))
    (string (array-insert-int array index (sap-int (gforeign::pointer-to-sap (%strdup value)))))
    (pointer (array-insert-int array index (sap-int (gforeign::pointer-to-sap value))))))

(defun array-prepend (array value)
  (array-insert-value array 0 value))

(defun array-append (array value)
  (array-insert-value array (array-get-size array) value))

;; cl-gtk.c
(define-foreign ("g_array_get_int" array-get-int) () int
  (array garray)
  (index unsigned-int))

(defun array-get-pointer (array index)
  (gforeign::sap-to-pointer (int-sap (array-get-int array index))))

;; cl-gtk.c
(define-foreign ("g_array_get_data" array-get-data) () pointer
  (array garray))

(define-foreign ("g_array_set_size" array-set-size) () garray
  (array garray)
  (size unsigned-int))

;; cl-gtk.c
(define-foreign ("g_array_get_size" array-get-size) () int
  (array garray))
|#