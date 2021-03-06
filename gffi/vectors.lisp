;; Common Lisp bindings for GTK+ 2.x
;; Copyright 1999-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: vectors.lisp,v 1.9 2008-12-10 02:41:40 espen Exp $


(in-package "GFFI")


;;; Accessor functions for raw memory access

(define-memory-accessor int-16)
(define-memory-accessor int-32)
(define-memory-accessor int-64)
(define-memory-accessor uint-16)
(define-memory-accessor uint-32)
(define-memory-accessor uint-64)
(define-memory-accessor single-float)
(define-memory-accessor double-float)


;;; Vector

(defun make-c-vector (type length &key content location temp)
  (let* ((element-size (size-of type))
	 (location (or location (allocate-memory (* element-size length))))
	 (writer (writer-function type :temp temp)))

    (cond
      #+(or cmu sbcl)
      ((and 
	(typep content 'simple-unboxed-array) 
	(type-equal-p type (array-element-type content)))
       (with-pinned-objects (content)
	 (copy-memory (vector-sap content) (* length element-size) location)))
      ((listp content)
       (loop
	for element in content
	for i below length
	for offset by element-size
	do (funcall writer element location offset)))
      (t
       (loop
	for element across content
	for i below length
	for offset by element-size
	do (funcall writer element location offset))))
    location))


(defun map-c-vector (seqtype function location element-type length 
		     &optional (ref :read))
  (let ((reader (reader-function element-type :ref ref))
	(element-size (size-of element-type)))
    (case seqtype
     ((nil)
      (loop
       for i below length
       for offset by element-size
       do (funcall function (funcall reader location offset))))
     (list
      (loop
       for i below length
       for offset by element-size
       collect (funcall function (funcall reader location offset))))
     (vector
      (let ((vector (make-array length :element-type element-type)))
	(cond
	  #+(or cmu sbcl)
	  ((and 
	    (typep vector 'simple-unboxed-array)
	    (or (eq function 'identity) (eq function #'identity)))
	   (with-pinned-objects (vector)
	     (copy-memory location (* length element-size) (vector-sap vector))))
	  (t
	   (loop
	    for i below length
	    for offset by element-size
	    do (setf 
		(aref vector i)
		(funcall function (funcall reader location offset))))))
	vector))
     (t
      (loop
       with sequence = (make-sequence seqtype length)
       for i below length
       for offset by element-size
       do (setf 
	   (elt sequence i)
	   (funcall function (funcall reader location offset)))
       finally (return sequence))))))


(defun unset-c-vector (location element-type length &optional temp-p)
  (loop
   with destroy = (destroy-function element-type :temp temp-p)
   with element-size = (size-of element-type)
   for i below length
   for offset by element-size
   do (funcall destroy location offset)))


(defun destroy-c-vector (location element-type length &optional temp-p)
  (unset-c-vector location element-type length temp-p)
  (deallocate-memory location))


(defmacro with-c-vector (var type content &body body)
  (let ((length (make-symbol "LENGTH")))
    `(let ((,length (length ,content)))
       (with-memory (,var (* ,(size-of type) ,length))
	 (make-c-vector ',type ,length :content ,content :location ,var :temp t)
	 (unwind-protect
	     (progn ,@body)
	   (unset-c-vector ,var ',type ,length t))))))


(define-type-method alien-type ((type vector))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method argument-type ((type vector))
  (declare (ignore type))
  'sequence)

(define-type-method return-type ((type vector))
  (destructuring-bind (element-type &optional (length '*)) 
      (rest (type-expand-to 'vector type))
    (if (constantp length)
	`(vector ,(return-type element-type) ,length)
      `(vector ,(return-type element-type) *))))

(define-type-method size-of ((type vector) &key inlined)
  (if inlined
      (destructuring-bind (element-type &optional (length '*)) 
	  (rest (type-expand-to 'vector type))
	(if (eq length '*)
	    (error "Can't inline vector with variable size: ~A" type)
	  (* (size-of element-type) length)))
    (size-of 'pointer)))

(define-type-method type-alignment ((type vector) &key inlined)
  (if inlined
      (destructuring-bind (element-type &optional (length '*)) 
	  (rest (type-expand-to 'vector type))
	(if (eq length '*)
	    (error "Can't inline vector with variable size: ~A" type)
	  (* (type-alignment element-type) length)))
    (type-alignment 'pointer)))

(define-type-method alien-arg-wrapper ((type vector) var vector style form &optional copy-in-p)
  (destructuring-bind (element-type &optional (length '*)) 
      (rest (type-expand-to 'vector type))
    (when (and (eq length '*) (out-arg-p style))
      (error "Can't use vector with variable size as return type"))
    (cond
      ((and (in-arg-p style) copy-in-p)
       `(with-pointer (,var `(make-c-vector ',element-type 
			      ,(if (eq length '*) `(length ,vector) length)
			      :content ,vector))
	 ,form))
      ((and (in-arg-p style) (not (out-arg-p style)))
       `(with-memory (,var ,(if (eq length '*)
				`(* ,(size-of element-type) 
				    (length ,vector))
			      `(* ,(size-of element-type) ,length)))
	  (make-c-vector ',element-type 
	   ,(if (eq length '*) `(length ,vector) length)
	   :content ,vector :location ,var :temp t)
	  (unwind-protect
	      ,form
	    (unset-c-vector ,var ',element-type 
	     ,(if (eq length '*) `(length ,vector) length) t))))
      ((and (in-arg-p style) (out-arg-p style))
       (let ((c-vector (make-symbol "C-VECTOR")))
	 `(with-memory (,c-vector (* ,(size-of element-type) length))
	    (make-c-vector ',element-type ,length 
             :content ,vector :location ,c-vector :temp t)
	    (with-pointer (,var ,c-vector)
	      (unwind-protect
		  ,form
		(unset-c-vector ,c-vector ',element-type ,length t))))))
      ((and (out-arg-p style) (not (in-arg-p style)))
       `(with-pointer (,var)
	  ,form)))))

;; This will enable us specify vectors with variable length in C callbacks
(define-type-method callback-wrapper ((type vector) var vector form)
  (funcall (find-applicable-type-method 'callback-wrapper t) type var vector form))

(define-type-method to-alien-form ((type vector) vector &optional copy-p)
  (declare (ignore copy-p))
  (destructuring-bind (element-type &optional (length '*)) 
      (rest (type-expand-to 'vector type))
    `(make-c-vector ',element-type 
      ,(if (eq length '*) `(length ,vector) length) :content ,vector)))


(define-type-method from-alien-form ((type vector) form &key (ref :free))
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if (eq length '*)
	(error "Can't use vector of variable size as return type")
      `(let ((c-vector ,form))
	 (prog1
	     (map-c-vector 'vector #'identity c-vector ',element-type ,length
	      ,(ecase ref (:free :get) ((:static :temp) :peek) (:copy :read)))
	   ,(when (eq ref :free)
	      `(deallocate-memory c-vector)))))))


(define-type-method writer-function ((type vector) &key temp inlined)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (if inlined
	(if (eq length '*)
	    (error "Can't inline vector with variable size: ~A" type)
	  #'(lambda (vector location &optional (offset 0))
	      (make-c-vector element-type length 
	       :location (pointer+ location offset)
	       :content vector :temp temp)))
      #'(lambda (vector location &optional (offset 0))
	  (setf 
	   (ref-pointer location offset)
	   (make-c-vector element-type length :content vector :temp temp))))))

(define-type-method reader-function ((type vector) &key (ref :read) inlined)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (cond
     ((eq length '*)
      (error "Can't create reader function for vector with variable size"))
     (inlined
      #'(lambda (location &optional (offset 0))
	  (map-c-vector 'vector #'identity (pointer+ location offset)
	   element-type length ref)))
     (t
      (ecase ref
	((:read :peek)
	 #'(lambda (location &optional (offset 0))
	     (unless (null-pointer-p (ref-pointer location offset))
	       (map-c-vector 'vector #'identity (ref-pointer location offset) 
		element-type length ref))))
	(:get
	 #'(lambda (location &optional (offset 0))
	     (unless (null-pointer-p (ref-pointer location offset))
	       (prog1
		   (map-c-vector 'vector #'identity 
		    (ref-pointer location offset) element-type length :get)
		 (deallocate-memory (ref-pointer location offset))
		 (setf (ref-pointer location offset) (make-pointer 0)))))))))))

(define-type-method destroy-function ((type vector) &key temp inlined)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (cond
     ((eq length '*)
      (error "Can't create destroy function for vector with variable size"))
     (inlined
      #'(lambda (location &optional (offset 0))
	  (unset-c-vector (pointer+ location offset) 
	   element-type length temp)))
     (t
      #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer location offset))
	    (destroy-c-vector (ref-pointer location offset) 
	     element-type length temp)
	    (setf (ref-pointer location offset) (make-pointer 0))))))))

(define-type-method copy-function ((type vector) &key inlined)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'vector type))
    (cond
     ((eq length '*) (error "Can't copy vector with variable size: ~A" type))
     (inlined
      (let ((copy-element (copy-function element-type))
	    (element-size (size-of element-type)))
	#'(lambda (from to &optional (offset 0))
	    (loop
	     repeat length
	     for element from offset by element-size
	     do (funcall copy-element from to element)))))
     (t
      (let ((size (* length (size-of element-type)))
	    (copy-content (copy-function type :inlined t)))
	#'(lambda (from to &optional (offset 0))
	    (unless (null-pointer-p (ref-pointer from offset))
	      (let ((vector (allocate-memory size)))
		(setf (ref-pointer to offset) vector)	    
		(funcall copy-content (ref-pointer from offset) vector)))))))))


;;;; Unboxed vector

(deftype unboxed-vector (element-type &optional (length '*)) 
  `(simple-array ,element-type (,length)))

(define-type-method argument-type ((type unboxed-vector))
  type)

(define-type-method return-type ((type unboxed-vector))
  (destructuring-bind (element-type &optional (length '*)) 
      (rest (type-expand-to 'unboxed-vector type))
    (if (constantp length)
	`(unboxed-vector ,(return-type element-type) ,length)
      `(unboxed-vector ,(return-type element-type) *))))

(defun check-unboxed-vector (type)
  #+(or sbcl cmu)
  (unless (subtypep type 'simple-unboxed-array)
    (error "~A is not a subtype of ~A" type 'simple-unboxed-array)))

#+(or sbcl cmu)
(progn
  (define-type-method alien-arg-wrapper ((type unboxed-vector) var vector style form &optional copy-in-p)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*)) 
	(rest (type-expand-to 'unboxed-vector type))
      (when (and (eq length '*) (out-arg-p style))
	(error "Can't use vector with variable size as return type"))
      (cond
	((and (in-arg-p style) copy-in-p)
	 `(with-pointer (,var (with-pinned-objects (,vector)
				(copy-memory (vector-sap ,vector) 
				 (* (length ,vector) ,(size-of element-type)))))
	    ,form))
	((in-arg-p style)
	 `(with-pinned-objects (,vector)
	    (let ((,var (vector-sap ,vector)))
	      ,form)))
	((out-arg-p style)
	 `(with-pointer (,var)
	    ,form)))))

  (define-type-method to-alien-form ((type unboxed-vector) vector &optional copy-p)
    (declare (ignore copy-p))
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*)) 
	(rest (type-expand-to 'unboxed-vector type))
      `(with-pinned-objects (,vector)
	 (copy-memory 
	  (vector-sap ,vector) 
	  (* ,(if (eq length '*) `(length ,vector) length) 
	     ,(size-of element-type))))))


  (define-type-method from-alien-form ((type unboxed-vector) form &key (ref :free))
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (when (eq length '*)
	(error "Can't use vector of variable size as return type"))
      `(let ((c-vector ,form)
	     (vector (make-array ,length :element-type ',element-type)))
	 (with-pinned-objects (vector)
	   (copy-memory c-vector (* ,length ,(size-of element-type)) (vector-sap vector))
	   ,(when (eq ref :free)
	     `(deallocate-memory c-vector))
	   vector))))

  (define-type-method writer-function ((type unboxed-vector) &key temp inlined)
    (declare (ignore temp))
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (if inlined
	  (if (eq length '*)
	      (error "Can't inline vector with variable size: ~A" type)
	      #'(lambda (vector location &optional (offset 0))
		  (with-pinned-objects (vector)
		    (copy-memory 
		     (vector-sap vector) 
		     (* length (size-of element-type))
		     (pointer+ location offset)))))
	  #'(lambda (vector location &optional (offset 0))
	      (setf 
	       (ref-pointer location offset)
	       (with-pinned-objects (vector)
		 (copy-memory (vector-sap vector) 
		  (* (length vector) (size-of element-type)))))))))

  (define-type-method reader-function ((type unboxed-vector) &key (ref :read) inlined)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (cond
	((eq length '*)
	 (error "Can't create reader function for vector with variable size"))
	(inlined
	 #'(lambda (location &optional (offset 0))
	     (let ((vector (make-array length :element-type element-type)))
	       (with-pinned-objects (vector)
		 (copy-memory 
		  (pointer+ location offset)
		  (* length (size-of element-type))
		  (vector-sap vector))
		 vector))))
	(t
	 #'(lambda (location &optional (offset 0))
	     (let ((vector (make-array length :element-type element-type)))
	       (unless (null-pointer-p (ref-pointer location offset))
		 (with-pinned-objects (vector)
		   (copy-memory 
		    (ref-pointer location offset)
		    (* (length vector) (size-of element-type))
		    (vector-sap vector)))
		 (when (eq ref :get)
		   (deallocate-memory (ref-pointer location offset))
		   (setf (ref-pointer location offset) (make-pointer 0)))
		 vector))))))))


#-(or sbcl cmu)
(progn
  (define-type-method alien-arg-wrapper ((type unboxed-vector) var vector style form &optional copy-in-p)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (alien-arg-wrapper `(vector ,element-type ,length) var vector style form copy-in-p)))

  (define-type-method to-alien-form ((type unboxed-vector) vector &optional copy-p)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (to-alien-form `(vector ,element-type ,length) vector copy-p)))

  (define-type-method from-alien-form ((type unboxed-vector) form &key (ref :free))
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (from-alien-form `(vector ,element-type ,length) form :ref ref)))

  (define-type-method writer-function ((type unboxed-vector) &key temp inlined)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (writer-function `(vector ,element-type ,length) :temp temp :inlined inlined)))

  (define-type-method reader-function ((type unboxed-vector) &key (ref :read) inlined)
    (check-unboxed-vector type)
    (destructuring-bind (element-type &optional (length '*))
	(rest (type-expand-to 'unboxed-vector type))
      (reader-function `(vector ,element-type ,length) :ref ref :inlined inlined))))

(define-type-method destroy-function ((type unboxed-vector) &key temp inlined)
  (declare (ignore temp))
  (check-unboxed-vector type)
  (destructuring-bind (element-type &optional (length '*))
      (rest (type-expand-to 'unboxed-vector type))
    (cond
     #+sbcl 
     ((eq length '*)
      (error "Can't create destroy function for vector with variable size"))
     (inlined
      #'(lambda (location &optional (offset 0))
	  (clear-memory location (* length (size-of element-type)) offset)))
     (t
      #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer location offset))
	    (deallocate-memory (ref-pointer location offset))
	    (setf (ref-pointer location offset) (make-pointer 0))))))))


;;;; Null terminated vector

(defun make-0-vector (type &key content location temp)
  (let* ((element-size (size-of type))
	 (length (length content))
	 (location (or location (allocate-memory (* element-size (1+ length))))))
    (make-c-vector type length :content content :location location :temp temp)))


(defun map-0-vector (seqtype function location element-type &optional (ref :read))
  (let ((reader (reader-function element-type :ref ref))
	(element-size (size-of element-type)))
    (case seqtype 
     ((nil)
      (loop
       for offset by element-size
       until (memory-clear-p (pointer+ location offset) element-size)
       do (funcall function (funcall reader location offset))))
     (list
      (loop
       for offset by element-size
       until (memory-clear-p (pointer+ location offset) element-size)
       collect (funcall function (funcall reader location offset))))
     (t
      (coerce 
       (loop
	for offset by element-size
	until (memory-clear-p (pointer+ location offset) element-size)
	collect (funcall function (funcall reader location offset)))
       seqtype)))))


(defun unset-0-vector (location element-type &optional temp-p)
  (loop
   with destroy = (destroy-function element-type :temp temp-p)
   with element-size = (size-of element-type)
   for offset by element-size
   until (memory-clear-p (pointer+ location offset) element-size)
   do (funcall destroy location offset)))

(defun destroy-0-vector (location element-type &optional temp-p)
  (unset-0-vector location element-type temp-p)
  (deallocate-memory location))


(deftype vector0 (element-type) `(vector ,element-type))
(deftype null-terminated-vector (element-type) `(vector0 ,element-type))

(define-type-method alien-type ((type vector0))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type vector0) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type vector0) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method alien-arg-wrapper ((type vector0) var vector style form &optional copy-in-p)
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    (cond
      ((and (in-arg-p style) copy-in-p)
       `(with-pointer (,var (make-0-vector ',element-type :content ,vector))
	  ,form))
      ((and (in-arg-p style) (not (out-arg-p style)))
       `(with-memory (,var (* ,(size-of element-type) (1+ (length ,vector))))
	  (make-0-vector ',element-type :content ,vector :location ,var :temp t)
	  (unwind-protect
	      ,form
	    (unset-0-vector ,var ',element-type t))))
      ((and (in-arg-p style) (out-arg-p style))
       (let ((c-vector (make-symbol "C-VECTOR")))
	 `(with-memory (,c-vector (* ,(size-of element-type) (1+ (length ,vector))))
	    (make-0-vector ',element-type :content ,vector :location ,c-vector :temp t)
	    (with-pointer (,var ,c-vector)
	      (unwind-protect
		  ,form
		(unset-0-vector ,c-vector ',element-type t))))))
      ((and (out-arg-p style) (not (in-arg-p style)))
       `(with-pointer (,var)
	  ,form)))))


(define-type-method to-alien-form ((type vector0) vector &optional copy-p)
  (declare (ignore copy-p))
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    `(make-0-vector ',element-type :content ,vector)))

(define-type-method from-alien-form ((type vector0) form  &key (ref :free))
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    `(let ((c-vector ,form))
       (prog1
	   (map-0-vector 'vector #'identity c-vector ',element-type
	    ,(ecase ref (:free :get) ((:static :temp) :peek) (:copy :read)))
	 ,(when (eq ref :free)	
	    `(deallocate-memory c-vector))))))


(define-type-method writer-function ((type vector0) &key temp inlined)  
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (ref-pointer location offset)
	 (make-0-vector element-type :content vector :temp temp)))))

(define-type-method reader-function ((type vector0) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    (ecase ref
      ((:read :peek)
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (map-0-vector 'vector #'identity (ref-pointer location offset) 
	      element-type ref))))
      (:get
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (prog1
		 (map-0-vector 'vector #'identity (ref-pointer location offset)
		  element-type :get)
	       (deallocate-memory (ref-pointer location offset))
	       (setf (ref-pointer location offset) (make-pointer 0)))))))))


(define-type-method destroy-function ((type vector0) &key temp inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    #'(lambda (location &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer location offset))
	    (destroy-0-vector 
	     (ref-pointer location offset) element-type temp)
	    (setf (ref-pointer location offset) (make-pointer 0))))))

(define-type-method copy-function ((type vector0) &key inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type) (rest (type-expand-to 'vector0 type))
    (let ((copy-element (copy-function element-type))
	  (element-size (size-of element-type)))
      #'(lambda (from to &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer from offset))
	    (let* ((from-vector (ref-pointer from offset))
		   (length 
		    (loop
		     for length from 0
		     for element by element-size
		     until (memory-clear-p from-vector element-size element)
		     finally (return length)))
		   (to-vector 
		    (setf (ref-pointer to offset)		  
		     (allocate-memory (* (1+ length) element-size)))))
	      (loop
	       repeat length
	       for element by element-size
	       do (funcall copy-element from-vector to-vector element))))))))

(define-type-method unbound-value ((type vector0))
  (declare (ignore type))
  nil)



;;;; Counted vector

(defun make-counted-vector (type &key content location (counter-type 'unsigned-int) temp)
  (let* ((element-size (size-of type))
	 (length (length content))
	 (location (or 
		    location
		    (allocate-memory 
		     (+ (size-of counter-type) (* element-size length))))))
    (funcall (writer-function counter-type :temp temp) length location)
    (make-c-vector type length :content content :location (pointer+ location (size-of counter-type)))
    location))

(defun map-counted-vector (seqtype function location element-type &optional (counter-type 'unsigned-int) (ref :read))
  (let ((length (funcall (reader-function counter-type) location :ref ref)))
    (map-c-vector 
     seqtype function (pointer+ location (size-of counter-type))
     element-type length)))

(defun unset-counted-vector (location element-type &optional (counter-type 'unsigned-int) temp-p)
  (let ((length (funcall (reader-function counter-type) location)))
    (unset-c-vector 
     (pointer+ location (size-of counter-type)) element-type length temp-p)))

(defun destroy-counted-vector (location element-type &optional (counter-type 'unsigned-int) temp-p)
  (unset-counted-vector location element-type counter-type temp-p)
  (deallocate-memory location))


(deftype counted-vector (element-type &optional counter-type) 
  (declare (ignore counter-type))
  `(vector ,element-type))

(define-type-method alien-type ((type counted-vector))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type counted-vector) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type counted-vector) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method alien-arg-wrapper ((type counted-vector) var vector style form &optional copy-in-p)
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    (cond
      ((and (in-arg-p style) copy-in-p)
       `(with-pointer (,var (make-counted-vector ',element-type 
	                     :content ,vector :counter-type ',counter-type))
	  ,form))
      ((and (in-arg-p style) (not (out-arg-p style)))
       `(with-memory (,var (+ (* ,(size-of element-type) (length ,vector)) ,(size-of counter-type)))
	  (make-counted-vector ',element-type :content ,vector 
	   :location ,var :counter-type ',counter-type :temp t)
	  (unwind-protect
	      ,form
	    (unset-counted-vector ,var ',element-type ',counter-type t))))
      ((and (in-arg-p style) (out-arg-p style))
       (let ((c-vector (make-symbol "C-VECTOR")))
	 `(with-memory (,c-vector (+ (* ,(size-of element-type) (length ,vector)) ,(size-of counter-type)))
	    (make-counted-vector ',element-type :content ,vector ,c-vector 
	     :counter-type ',counter-type :temp t)
	    (with-pointer (,var ,c-vector)
	      (unwind-protect
		  ,form
		(unset-counted-vector ,c-vector ',element-type ',counter-type t))))))
      ((and (out-arg-p style) (not (in-arg-p style)))
       `(with-pointer (,var)
	  ,form)))))


(define-type-method to-alien-form ((type counted-vector) vector &optional copy-p)
  (declare (ignore copy-p))
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    `(make-counted-vector ',element-type 
      :content ,vector :counter-type ',counter-type)))

(define-type-method from-alien-form ((type counted-vector) form  &key (ref :free))
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    `(let ((c-vector ,form))
       (prog1
	   (map-counted-vector 'vector #'identity c-vector ',element-type ',counter-type 
	    ,(ecase ref (:free :get) ((:static :temp) :peek) (:copy :read)))
	 ,(when (eq ref :free)
	    `(deallocate c-vector))))))

(define-type-method writer-function ((type counted-vector) &key temp inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (vector location &optional (offset 0))
	(setf 
	 (ref-pointer location offset)
	 (make-counted-vector element-type :content vector 
	  :counter-type counter-type :temp temp)))))

(define-type-method reader-function ((type counted-vector) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    (ecase ref
      ((:read :peek)
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (map-counted-vector 'vector #'identity 
	      (ref-pointer location offset) element-type counter-type ref))))
      (:get
       #'(lambda (location &optional (offset 0))
	   (unless (null-pointer-p (ref-pointer location offset))
	     (prog1
		 (map-counted-vector 'vector #'identity 
		  (ref-pointer location offset) element-type counter-type :get)
	       (deallocate-memory (ref-pointer location offset))
	       (setf (ref-pointer location offset) (make-pointer 0)))))))))

(define-type-method destroy-function ((type counted-vector) &key temp inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    #'(lambda (location &optional (offset 0))
	(unless (null-pointer-p (ref-pointer location offset))
	  (destroy-counted-vector (ref-pointer location offset) 
	   element-type counter-type temp)
	  (setf (ref-pointer location offset) (make-pointer 0))))))

(define-type-method copy-function ((type counted-vector) &key inlined)
  (assert-not-inlined type inlined)
  (destructuring-bind (element-type &optional (counter-type 'unsigned-int))
      (rest (type-expand-to 'counted-vector type))
    (let ((vector-length (reader-function counter-type))
	  (counter-size (size-of counter-type))
	  (copy-element (copy-function element-type))
	  (element-size (size-of element-type)))
      #'(lambda (from to &optional (offset 0))
	  (unless (null-pointer-p (ref-pointer from offset))
	    (let* ((from-vector (ref-pointer from offset))
		   (length (funcall vector-length from-vector))
		   (to-vector  (setf 
				(ref-pointer to offset)		  
				(allocate-memory (+ counter-size (* length element-size))))))	    
	      (copy-memory from-vector counter-size to-vector)
	      (loop
	       repeat length
	       for element from counter-size by element-size
	       do (funcall copy-element from-vector to-vector element))))))))


;;;; Accessor functions for raw memory access

(defun vector-reader-function (type &key (start 0) end)
  "Returns a function for reading values from raw C vectors"
  (let ((element-size (size-of type))
	(reader (reader-function type)))
    #'(lambda (vector index)
	(assert (and (>= index start) (or (not end) (< index end))))
	(funcall reader vector (* index element-size)))))

(defun vector-writer-function (type &key (start 0) end)
  "Returns a function for writing values to raw C vectors"
  (let ((element-size (size-of type))
	(writer (writer-function type)))
    #'(lambda (value vector index)
	(assert (and (>= index start) (or (not end) (< index end))))
	(funcall writer value vector (* index element-size)))))


(defmacro define-vector-accessor (type)
  (let ((name (intern (format nil "VECTOR-REF-~A" type)))
	(ref (intern (format nil "REF-~A" type))))
    `(progn     
       (declaim 
	(ftype (function (pointer fixnum) ,type) ,name)
	(inline ,name))
       (defun ,name (vector index)
	 (,ref vector (* ,(size-of type) index)))
       (declaim 
	(ftype (function (,type pointer fixnum) ,type) (setf ,name))
	(inline (setf ,name)))
       (defun (setf ,name) (value vector index)
	 (setf (,ref vector (* ,(size-of type) index)) value)))))

(define-vector-accessor int-8)
(define-vector-accessor uint-8)
(define-vector-accessor int-16)
(define-vector-accessor uint-16)
(define-vector-accessor int-32)
(define-vector-accessor uint-32)
(define-vector-accessor int-64)
(define-vector-accessor uint-64)
(define-vector-accessor double-float)
(define-vector-accessor single-float)

