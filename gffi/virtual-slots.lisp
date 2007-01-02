;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: virtual-slots.lisp,v 1.6 2007-01-02 16:04:57 espen Exp $

(in-package "GFFI")

;;;; Superclass for all metaclasses implementing some sort of virtual slots

(defclass virtual-slots-class (standard-class) 
  ())

(defclass direct-virtual-slot-definition (standard-direct-slot-definition)
  ((setter :reader slot-definition-setter :initarg :setter)
   (getter :reader slot-definition-getter :initarg :getter)
   (unbound :reader slot-definition-unbound :initarg :unbound)
   (boundp :reader slot-definition-boundp :initarg :boundp)
   (makunbound :reader slot-definition-makunbound :initarg :makunbound)
   #+clisp(type :initarg :type :reader slot-definition-type)))
  
(defclass effective-virtual-slot-definition (standard-effective-slot-definition)
  ((setter :reader slot-definition-setter :initarg :setter)
   (getter :reader slot-definition-getter :initarg :getter)
   (unbound :reader slot-definition-unbound :initarg :unbound)
   (boundp :reader slot-definition-boundp :initarg :boundp)
   (makunbound :reader slot-definition-makunbound :initarg :makunbound)
   #+clisp(reader-function)
   #+clisp(writer-function)
   #+clisp(boundp-function)
   makunbound-function
   #+clisp(type :initarg :type :reader slot-definition-type)))

(defclass direct-special-slot-definition (standard-direct-slot-definition)
  ((special :initarg :special :accessor slot-definition-special)))

(defclass effective-special-slot-definition (standard-effective-slot-definition)
  ((special :initarg :special :accessor slot-definition-special)))

(defclass virtual-slots-object (standard-object)
  ())

(defgeneric slot-readable-p (slotd))
(defgeneric slot-writable-p (slotd))
(defgeneric compute-slot-reader-function (slotd &optional signal-unbound-p))
(defgeneric compute-slot-boundp-function (slotd))
(defgeneric compute-slot-writer-function (slotd))
(defgeneric compute-slot-makunbound-function (slotd))

(defmethod slot-readable-p ((slotd standard-effective-slot-definition))
  (declare (ignore slotd))
  t)

(defmethod slot-writable-p ((slotd standard-effective-slot-definition))
  (declare (ignore slotd))
  t)


#+clisp
(defmethod slot-definition-type ((slotd t))
  (clos:slot-definition-type slotd))


(defmethod direct-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (cond
   ((eq (getf initargs :allocation) :virtual)
    (find-class 'direct-virtual-slot-definition))
   ((getf initargs :special)
    (find-class 'direct-special-slot-definition))
   (t (call-next-method))))

(defmethod effective-slot-definition-class ((class virtual-slots-class) &rest initargs)
  (cond
   ((eq (getf initargs :allocation) :virtual)
    (find-class 'effective-virtual-slot-definition))
   ((getf initargs :special)
    (find-class 'effective-special-slot-definition))
   (t (call-next-method))))


(defmethod slot-readable-p ((slotd effective-virtual-slot-definition))
  (slot-boundp slotd 'getter))

(define-condition unreadable-slot (cell-error)
  ((instance :reader unreadable-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S in the object ~S is not readable.~@:>"
	      (cell-error-name condition)
	      (unreadable-slot-instance condition)))))

(defmethod compute-slot-reader-function :around ((slotd effective-virtual-slot-definition) &optional (signal-unbound-p t))
  (if (not (slot-readable-p slotd))
      #'(lambda (object)
	  (error 'unreadable-slot :name (slot-definition-name slotd) :instance object))
    (let ((reader-function (call-next-method)))
      (cond
       ;; Don't create wrapper to signal unbound value
       ((not signal-unbound-p) reader-function)
       
       ;; An explicit boundp function has been supplied
       ((slot-boundp slotd 'boundp) 
	(let ((unbound-value (slot-value slotd 'boundp)))
	  #'(lambda (object)
	      (let ((value (funcall reader-function object)))
		(if (eq value unbound-value)
		    (slot-unbound (class-of object) object (slot-definition-name slotd))
		  value)))))
       
       ;; A type unbound value exists
       ((let ((unbound-method (find-applicable-type-method 'unbound-value 
			       (slot-definition-type slotd) nil)))
	  (when unbound-method
	    (let ((unbound-value (funcall unbound-method (slot-definition-type slotd))))
	      #'(lambda (object)
		  (let ((value (funcall reader-function object)))
		    (if (eq value unbound-value)
			(slot-unbound (class-of object) object (slot-definition-name slotd))
		      value)))))))
       
       ((let ((boundp-function (compute-slot-boundp-function slotd)))
	  #'(lambda (object)
	      (if (funcall boundp-function object)
		  (funcall reader-function object)
		(slot-unbound (class-of object) object (slot-definition-name slotd))))))))))

(defmethod compute-slot-reader-function ((slotd effective-virtual-slot-definition) &optional signal-unbound-p)
  (declare (ignore signal-unbound-p))
  (let ((getter (slot-value slotd 'getter)))
    #-sbcl getter
    #+sbcl
    (etypecase getter
      (symbol #'(lambda (object) (funcall getter object)))
      (function getter))))

(defmethod compute-slot-boundp-function ((slotd effective-virtual-slot-definition))
  (cond
   ;; Non readable slots are not bound per definition
   ((not (slot-readable-p slotd))
    #'(lambda (object) (declare (ignore object)) nil))

   ;; An explicit boundp function has been supplied
   ((slot-boundp slotd 'boundp)
    (let ((boundp (slot-value slotd 'boundp)))
      #-sbcl boundp
      #+sbcl
      (etypecase boundp
	(symbol #'(lambda (object) (funcall boundp object)))
	(function boundp))))

   ;; An unbound value has been supplied
   ((slot-boundp slotd 'unbound)
    (let ((reader-function (compute-slot-reader-function slotd nil))
	  (unbound-value (slot-value slotd 'unbound)))
      #'(lambda (object)
	  (not (eql (funcall reader-function object) unbound-value)))))
   
   ;; A type unbound value exists
   ((let ((unbound-method (find-applicable-type-method 'unbound-value 
			   (slot-definition-type slotd) nil)))
      (when unbound-method
	(let ((reader-function (compute-slot-reader-function slotd nil))
	      (unbound-value (funcall unbound-method (slot-definition-type slotd))))
	  #'(lambda (object)
	      (not (eql (funcall reader-function object) unbound-value)))))))
   
   ;; Slot has no unbound state
   (#'(lambda (object) (declare (ignore object)) t))))

(defmethod slot-writable-p ((slotd effective-virtual-slot-definition))
  (slot-boundp slotd 'setter))

(define-condition unwritable-slot (cell-error)
  ((instance :reader unwritable-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S in the object ~S is not writable.~@:>"
	      (cell-error-name condition)
	      (unwritable-slot-instance condition)))))

(defmethod compute-slot-writer-function :around ((slotd effective-virtual-slot-definition))
  (if (not (slot-writable-p slotd))
      #'(lambda (value object)
	  (declare (ignore value))
	  (error 'unwritable-slot :name (slot-definition-name slotd) :instance object))
    (call-next-method)))

(defmethod compute-slot-writer-function ((slotd effective-virtual-slot-definition))
  (let ((setter (slot-value slotd 'setter)))
    #-sbcl setter
    #+sbcl
    (etypecase setter
      (symbol #'(lambda (object value) (funcall setter object value)))
      (list #'(lambda (object value)
		(funcall setter value object)))
      (function setter))))

(define-condition slot-can-not-be-unbound (cell-error)
  ((instance :reader slot-can-not-be-unbound-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S in the object ~S can not be made unbound.~@:>"
	      (cell-error-name condition)
	      (slot-can-not-be-unbound-instance condition)))))

(defmethod compute-slot-makunbound-function ((slotd effective-virtual-slot-definition))
  (cond
   ((not (slot-writable-p slotd))
    #'(lambda (object)
	(error 'unwritable-slot :name (slot-definition-name slotd) :instance object)))
   ((slot-boundp slotd 'makunbound)
    (let ((makunbound (slot-value slotd 'makunbound)))
      #-sbcl makunbound
      #+sbcl
      (etypecase makunbound
	(symbol #'(lambda (object) (funcall makunbound object)))
	(function makunbound))))
   ((slot-boundp slotd 'unbound)
    #'(lambda (object)
	(funcall (slot-value slotd 'writer-function) (slot-value slotd 'unbound) object)))
   (t
    #'(lambda (object)
	(error 'slot-can-not-be-unbound :name (slot-definition-name slotd) :instance object)))))


#-clisp
(defmethod initialize-internal-slot-functions ((slotd effective-virtual-slot-definition))
  #?-(sbcl>= 0 9 15) ; Delayed to avoid recursive call of finalize-inheritanze
  (setf 
   (slot-value slotd 'reader-function) (compute-slot-reader-function slotd)
   (slot-value slotd 'boundp-function) (compute-slot-boundp-function slotd)
   (slot-value slotd 'writer-function) (compute-slot-writer-function slotd)
   (slot-value slotd 'makunbound-function) (compute-slot-makunbound-function slotd))

  #?-(sbcl>= 0 9 8)(initialize-internal-slot-gfs (slot-definition-name slotd)))


#-clisp
(defmethod compute-slot-accessor-info ((slotd effective-virtual-slot-definition) type gf)
  nil)


(defun slot-bound-in-some-p (instances slot)
  (find-if
   #'(lambda (ob)
       (and (slot-exists-p ob slot) (slot-boundp ob slot)))
   instances))

(defun most-specific-slot-value (instances slot &optional default)
  (let ((object (slot-bound-in-some-p instances slot)))
    (if object
	(slot-value object slot)
      default)))

(defun compute-most-specific-initargs (slotds slots)
  (loop
   for slot in slots
   as (slot-name initarg) = (if (atom slot)
				(list slot (intern (string slot) "KEYWORD"))
			      slot)
   when (slot-bound-in-some-p slotds slot-name)
   nconc (list initarg (most-specific-slot-value slotds slot-name))))

(defmethod compute-effective-slot-definition-initargs ((class virtual-slots-class) direct-slotds)
  (typecase (first direct-slotds)
    (direct-virtual-slot-definition
     (nconc
      (compute-most-specific-initargs direct-slotds
       '(getter setter unbound boundp makunbound
	 #?(or (sbcl>= 0 9 8) (featurep :clisp))
	 (#?-(sbcl>= 0 9 10)type #?(sbcl>= 0 9 10)sb-pcl::%type :type)))
      (call-next-method)))
    (direct-special-slot-definition
     (append '(:special t) (call-next-method)))
    (t (call-next-method))))

#?(or (not (sbcl>= 0 9 14)) (featurep :clisp))
(defmethod slot-value-using-class
    ((class virtual-slots-class) (object virtual-slots-object)
     (slotd effective-virtual-slot-definition))
    (funcall (slot-value slotd 'reader-function) object))

#?(or (not (sbcl>= 0 9 14)) (featurep :clisp))
(defmethod slot-boundp-using-class
    ((class virtual-slots-class) (object virtual-slots-object)
     (slotd effective-virtual-slot-definition))
    (funcall (slot-value slotd 'boundp-function) object))

#?(or (not (sbcl>= 0 9 14)) (featurep :clisp))
(defmethod (setf slot-value-using-class) 
    (value (class virtual-slots-class) (object virtual-slots-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'writer-function) value object))

(defmethod slot-makunbound-using-class
    ((class virtual-slots-class) (object virtual-slots-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'makunbound-function) object))


;; In CLISP and SBCL (0.9.15 or newler) a class may not have been
;; finalized when update-slots are called. So to avoid the possibility
;; of finalize-instance being called recursivly we have to delay the
;; initialization of slot functions until after an instance has been
;; created.
#?(or (sbcl>= 0 9 15) (featurep :clisp))
(defmethod slot-unbound (class (slotd effective-virtual-slot-definition) (name (eql 'reader-function)))
  (setf (slot-value slotd name) (compute-slot-reader-function slotd)))

#?(or (sbcl>= 0 9 15) (featurep :clisp))
(defmethod slot-unbound (class (slotd effective-virtual-slot-definition) (name (eql 'boundp-function)))
  (setf (slot-value slotd name) (compute-slot-boundp-function slotd)))

#?(or (sbcl>= 0 9 15) (featurep :clisp))
(defmethod slot-unbound (class (slotd effective-virtual-slot-definition) (name (eql 'writer-function)))
  (setf (slot-value slotd name) (compute-slot-writer-function slotd)))

#?(or (sbcl>= 0 9 15) (featurep :clisp))
(defmethod slot-unbound (class (slotd effective-virtual-slot-definition) (name (eql 'makunbound-function)))
  (setf (slot-value slotd name) (compute-slot-makunbound-function slotd)))


(defmethod validate-superclass
    ((class virtual-slots-class) (super standard-class))
  t)

(defmethod slot-definition-special ((slotd standard-direct-slot-definition))
  (declare (ignore slotd))
  nil)

(defmethod slot-definition-special ((slotd standard-effective-slot-definition))
  (declare (ignore slotd))
  nil)


;;; To determine if a slot should be initialized with the initform,
;;; CLISP checks whether it is unbound or not. This doesn't work with
;;; virtual slots that does not have an unbound state, so we have to
;;; implement initform initialization in a way similar to how it is
;;; done in PCL.
#+clisp
(defmethod shared-initialize ((object virtual-slots-object) names &rest initargs)
  (let* ((class (class-of object))
	 (slotds (class-slots class))
	 (keywords (loop
		    for args on initargs by #'cddr
		    collect (first args)))
	 (names
	  (loop
	   for slotd in slotds
	   as name = (slot-definition-name slotd)
	   as initargs = (slot-definition-initargs slotd)
	   as init-p = (and
			(or (eq names t) (find name names))
			(slot-definition-initfunction slotd)
			(not (intersection initargs keywords)))
	   as virtual-p = (typep slotd 'effective-virtual-slot-definition)
	   when (and init-p virtual-p)
	   do (setf 
	       (slot-value-using-class class object slotd)
	       (funcall (slot-definition-initfunction slotd)))
	   when (and init-p (not virtual-p))
	   collect name)))

      (apply #'call-next-method object names initargs)))
