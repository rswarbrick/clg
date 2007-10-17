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

;; $Id: glib.lisp,v 1.42 2007-10-17 14:30:41 espen Exp $


(in-package "GLIB")

(use-prefix "g")

#-sb-thread
(progn
  (defun make-mutex ()
    nil)
  
  (defmacro with-mutex ((mutex) &body body)
    (declare (ignore mutex))
    `(progn ,@body)))


;;;; Memory management

(defbinding (%allocate-memory "g_malloc0") () pointer
  (size unsigned-long))

(defbinding (%deallocate-memory "g_free") () nil
  (address pointer))

;; (setf
;;  (symbol-function 'allocate-memory) #'%allocate-memory
;;  (symbol-function 'deallocate-memory) #'%deallocate-memory)

(setf *memory-allocator* #'%allocate-memory)
(setf *memory-deallocator* #'%deallocate-memory)

(defbinding (reallocate-memory "g_realloc") () pointer
  (address pointer)
  (size unsigned-long))

(deftype gsize () 'unsigned-int)

(defbinding (slice-alloc "g_slice_alloc0") () pointer
  (block-size gsize))

;;;; User data is a mechanism to store references to lisp objects in
;;;; foreign code

(defvar *user-data-lock* (make-mutex))
(defvar *user-data* (make-hash-table))
(defvar *user-data-next-id* 1)

(defun register-user-data (object &optional destroy-function)
  (check-type destroy-function (or null symbol function))
  (with-mutex (*user-data-lock*)
    (setf
     (gethash *user-data-next-id* *user-data*)
     (cons object destroy-function))
    (1- (incf *user-data-next-id*))))

(defun find-user-data (id)
  (check-type id fixnum)
  (with-mutex (*user-data-lock*)
    (multiple-value-bind (user-data p) (gethash id *user-data*)
      (values (car user-data) p))))

(defun user-data-exists-p (id)
  (nth-value 1 (find-user-data id)))

(defun update-user-data (id object)
  (check-type id fixnum)
  (with-mutex (*user-data-lock*)
    (multiple-value-bind (user-data exists-p) (gethash id *user-data*)
      (cond
       ((not exists-p) (error "User data id ~A does not exist" id))
       (t 
	(when (cdr user-data)
	  (funcall (cdr user-data) (car user-data)))
	(setf (car user-data) object))))))

(defun destroy-user-data (id)
  (check-type id fixnum)
  (with-mutex (*user-data-lock*)
    (multiple-value-bind (user-data exists-p) (gethash id *user-data*)
      (cond
;       ((not exists-p) (error "User data id ~A does not exist" id))
       (t
	(when (cdr user-data)
	  (funcall (cdr user-data) (car user-data)))
	(remhash id *user-data*))))))

(defun take-user-data (id)
  (check-type id fixnum)
  (multiple-value-bind (user-data exists-p) (gethash id *user-data*)
    (cond
      ((not exists-p) (error "User data id ~A does not exist" id))
      (t 
	(when (cdr user-data)
	  (funcall (cdr user-data) (car user-data)))
	(remhash id *user-data*)
	(car user-data)))))

(defmacro with-user-data ((var object) &body body)
  `(let ((,var (register-user-data ,object)))
     (unwind-protect
	  ,@body
       (destroy-user-data ,var))))


(deftype user-data-id () 'pointer-data)


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

(defbinding quark-to-string () (static string)
  (quark quark))


;;;; Linked list (GList)

(deftype glist (type) 
  `(or null (cons ,type list)))

(defbinding (%glist-append "g_list_append") () pointer
  (glist (or null pointer))
  (nil null))

(defun make-glist (element-type list &optional temp-p)
  (let ((writer (if (functionp element-type)
		    element-type
		  (writer-function element-type :temp temp-p))))
    (loop
     for element in list
     as glist = (%glist-append nil) then (%glist-append glist)
     do (funcall writer element glist)
     finally (return glist))))

(defun glist-next (glist)
  (unless (null-pointer-p glist)
    (ref-pointer glist #.(size-of 'pointer))))
  
;; Also used for gslists
(defun map-glist (seqtype function glist element-type &optional (ref :read))
  (let ((reader (if (functionp element-type)
		    element-type
		  (reader-function element-type :ref ref))))
    (case seqtype 
     ((nil)
      (loop
       as element = glist then (glist-next element)
       until (null-pointer-p element)
       do (funcall function (funcall reader element))))
     (list
      (loop
       as element = glist then (glist-next element)
       until (null-pointer-p element)
       collect (funcall function (funcall reader element))))
     (t
      (coerce 
       (loop
	as element = glist then (glist-next element)
	until (null-pointer-p element)
	collect (funcall function (funcall reader element)))
       seqtype)))))

(defbinding (glist-free "g_list_free") () nil
  (glist pointer))

(defun destroy-glist (glist element-type &optional temp-p)
  (let ((destroy (if (functionp element-type)
		     element-type
		   (destroy-function element-type :temp temp-p))))
    (loop
     as element = glist then (glist-next element)
     until (null-pointer-p element)
     do (funcall destroy element)))
  (glist-free glist))

(define-type-method alien-type ((type glist))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method argument-type ((type glist))
  'list)

(define-type-method return-type ((type glist))
  'list)

(define-type-method size-of ((type glist) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type glist) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method alien-arg-wrapper ((type glist) var list style form &optional copy-in-p)
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    (cond
      ((and (in-arg-p style) (not (out-arg-p style)))
       `(with-pointer (,var (make-glist ',element-type ,list ,(not copy-in-p)))
	  (unwind-protect
	      ,form
	    ,(unless copy-in-p
	       `(destroy-glist ,var ',element-type t)))))
      ((and (in-arg-p style) (out-arg-p style))
       (let ((glist (make-symbol "GLIST")))
	 `(with-pointer (,glist (make-glist ',element-type ,list ,(not copy-in-p)))
            (with-pointer (,var ,glist)		      
	      (unwind-protect
		  ,form
		,(unless copy-in-p
		   `(destroy-glist ,glist ',element-type t)))))))
      ((and (out-arg-p style) (not (in-arg-p style)))
       `(with-pointer (,var)
	  ,form)))))

(define-type-method to-alien-form ((type glist) list &optional copy-p)
  (declare (ignore copy-p))
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    `(make-glist ',element-type ,list)))

(define-type-method to-alien-function ((type glist) &optional copy-p)
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    (values
     #'(lambda (list)
	 (make-glist element-type list (not copy-p)))
     (unless copy-p
       #'(lambda (list glist)
	   (declare (ignore list))
	   (destroy-glist glist element-type t))))))

(define-type-method from-alien-form ((type glist) form &key (ref :free))
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    `(let ((glist ,form))
       (unwind-protect
	   (map-glist 'list #'identity glist ',element-type 
	    ,(ecase ref (:free :get) ((:static :temp) :peek) (:copy :read)))
	 ,(when (eq ref :free)
	    `(destroy-glist glist ',element-type))))))

(define-type-method from-alien-function ((type glist) &key (ref :free))
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    (ecase ref
      (:free 
       #'(lambda (glist)
	   (prog1
	       (map-glist 'list #'identity glist element-type :get)
	     (glist-free glist))))
      (:copy
       #'(lambda (glist)
	   (map-glist 'list #'identity glist element-type :read)))
      ((:static :temp)
       #'(lambda (glist)
	   (map-glist 'list #'identity glist element-type :peek))))))

(define-type-method writer-function ((type glist) &key temp inlined)
  (assert-not-inlined type inlined)
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (list location &optional (offset 0))
	(setf 
	 (ref-pointer location offset)
	 (make-glist element-type list temp)))))

(define-type-method reader-function ((type glist) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (let ((element-type (second (type-expand-to 'glist type))))
    (ecase ref
      ((:read :peek)
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (map-glist 'list #'identity (ref-pointer location offset) element-type ref))))
      (:get
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (prog1
		 (map-glist 'list #'identity (ref-pointer location offset) element-type :get)
	       (glist-free (ref-pointer location offset))
	       (setf (ref-pointer location offset) (make-pointer 0)))))))))

(define-type-method destroy-function ((type glist) &key temp inlined)
  (assert-not-inlined type inlined)
  (let ((element-type (second (type-expand-to 'glist type))))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (ref-pointer location offset))
	  (destroy-glist (ref-pointer location offset) element-type temp)
	  (setf (ref-pointer location offset) (make-pointer 0))))))

(define-type-method copy-function ((type glist) &key inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'glist type))
    (let ((copy-element (copy-function element-type)))
      #'(lambda (from to &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer from offset))
	    (loop
	     as from-list = (ref-pointer from offset) 
	                    then (glist-next from-list)
	     as to-list = (setf (ref-pointer to offset) (%glist-append nil)) 
	                  then (%glist-append to-list)
	     do (funcall copy-element from-list to-list)
	     while (glist-next from-lisT)))))))


;;;; Single linked list (GSList)

(deftype gslist (type) `(or null (cons ,type list)))

(defbinding (%gslist-prepend "g_slist_prepend") () pointer
  (gslist pointer)
  (nil null))

(defbinding (%gslist-append "g_slist_append") () pointer
  (glist (or null pointer))
  (nil null))


(defun make-gslist (element-type list &optional temp-p)
  (let ((writer (if (functionp element-type)
		    element-type
		  (writer-function element-type :temp temp-p))))
    (loop
     for element in (reverse list)
     as gslist = (%gslist-prepend (make-pointer 0)) then (%gslist-prepend gslist)
     do (funcall writer element gslist)
     finally (return gslist))))

(defbinding (gslist-free "g_slist_free") () nil
  (gslist pointer))

(defun destroy-gslist (gslist element-type &optional temp-p)
  (loop
   with destroy = (destroy-function element-type :temp temp-p)
   as element = gslist then (glist-next element)
   until (null-pointer-p element)
   do (funcall destroy element 0))
  (gslist-free gslist))

(define-type-method alien-type ((type gslist))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method argument-type ((type gslist))
  'list)

(define-type-method return-type ((type gslist))
  'list)

(define-type-method size-of ((type gslist) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type gslist) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method alien-arg-wrapper ((type gslist) var list style form &optional copy-in-p)
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    (cond
      ((and (in-arg-p style) (not (out-arg-p style)))
       `(with-pointer (,var (make-gslist ',element-type ,list ,(not copy-in-p)))
	  (unwind-protect
	      ,form
	    ,(unless copy-in-p
	       `(destroy-gslist ,var ',element-type t)))))
      ((and (in-arg-p style) (out-arg-p style))
       (let ((gslist (make-symbol "GSLIST")))
	 `(with-pointer (,gslist (make-gslist ',element-type ,list ,(not copy-in-p)))
            (with-pointer (,var ,gslist)		      
	      (unwind-protect
		  ,form
		,(unless copy-in-p
		   `(destroy-gslist ,gslist ',element-type t)))))))
      ((and (out-arg-p style) (not (in-arg-p style)))
       `(with-pointer (,var)
	  ,form)))))

(define-type-method to-alien-form ((type gslist) list &optional copy-p)
  (declare (ignore copy-p))
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    `(make-gslist ',element-type ,list)))

(define-type-method to-alien-function ((type gslist) &optional copy-p)
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    (values
     #'(lambda (list)
	 (make-gslist element-type list (not copy-p)))
     (unless copy-p
       #'(lambda (list gslist)
	   (declare (ignore list))
	   (destroy-gslist gslist element-type t))))))

(define-type-method from-alien-form ((type gslist) form &key (ref :free))
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    `(let ((gslist ,form))
       (unwind-protect
	   (map-glist 'list #'identity gslist ',element-type
	    ,(ecase ref (:free :get) ((:static :temp) :peek) (:copy :read)))
	 ,(when (eq ref :free)
	    `(destroy-gslist gslist ',element-type))))))

(define-type-method from-alien-function ((type gslist)  &key (ref :free))
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    (ecase ref
      (:free 
       #'(lambda (glist)
	   (prog1
	       (map-glist 'list #'identity glist element-type :get)
	     (gslist-free glist))))
      (:copy
       #'(lambda (glist)
	   (map-glist 'list #'identity glist element-type :read)))
      ((:static :temp)
       #'(lambda (glist)
	   (map-glist 'list #'identity glist element-type :peek))))))

(define-type-method writer-function ((type gslist) &key temp inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    (let ((element-writer (writer-function element-type :temp temp)))
      #'(lambda (list location &optional (offset 0))
	  (setf 
	   (ref-pointer location offset)
	   (make-gslist element-writer list))))))

(define-type-method reader-function ((type gslist) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (let ((element-type (second (type-expand-to 'gslist type))))
    (ecase ref
      ((:read :peek)
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (map-glist 'list #'identity (ref-pointer location offset) element-type ref))))
      (:get
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (prog1
		 (map-glist 'list #'identity (ref-pointer location offset) element-type :get)
	       (gslist-free (ref-pointer location offset))
	       (setf (ref-pointer location offset) (make-pointer 0)))))))))

(define-type-method destroy-function ((type gslist) &key temp inlined)
  (assert-not-inlined type inlined)
  (let ((element-type (second (type-expand-to 'gslist type))))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (ref-pointer location offset))
	  (destroy-gslist (ref-pointer location offset) element-type temp)
	  (setf (ref-pointer location offset) (make-pointer 0))))))

(define-type-method copy-function ((type gslist) &key inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'gslist type))
    (let ((copy-element (copy-function element-type)))
      #'(lambda (from to &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer from offset))
	    (loop
	     as from-list = (ref-pointer from offset) 
	                    then (glist-next from-list)
	     as to-list = (setf (ref-pointer to offset) (%gslist-append nil)) 
	                  then (%gslist-append to-list)
	     do (funcall copy-element from-list to-list)
	     while (glist-next from-list)))))))
