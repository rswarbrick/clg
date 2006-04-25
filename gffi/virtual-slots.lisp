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

;; $Id: virtual-slots.lisp,v 1.1 2006-04-25 20:49:16 espen Exp $

(in-package "GFFI")

;;;; Superclass for all metaclasses implementing some sort of virtual slots

(eval-when (:compile-toplevel :load-toplevel :execute)
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
    ((special :initarg :special :accessor slot-definition-special))))

(defgeneric compute-slot-reader-function (slotd))
(defgeneric compute-slot-boundp-function (slotd))
(defgeneric compute-slot-writer-function (slotd))
(defgeneric compute-slot-makunbound-function (slotd))


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


(define-condition unreadable-slot (cell-error)
  ((instance :reader unreadable-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S in the object ~S is not readable.~@:>"
	      (cell-error-name condition)
	      (unreadable-slot-instance condition)))))

(defmethod compute-slot-reader-function ((slotd effective-virtual-slot-definition))
  (if (slot-boundp slotd 'getter)
      (slot-value slotd 'getter)
    #'(lambda (object)
	(error 'unreadable-slot :name (slot-definition-name slotd) :instance object))))

(defmethod compute-slot-boundp-function ((slotd effective-virtual-slot-definition))
  (cond
   ;; An explicit boundp function has been supplied
   ((slot-boundp slotd 'boundp) (slot-value slotd 'boundp))
   
   ;; An unbound value has been supplied
   ((slot-boundp slotd 'unbound)
    (let ((reader-function (slot-value slotd 'reader-function))
	  (unbound-value (slot-value slotd 'unbound)))
      #'(lambda (object)
	  (not (eql (funcall reader-function object) unbound-value)))))
   
   ;; A type unbound value exists
   ((let ((unbound-method (find-applicable-type-method 'unbound-value 
			   (slot-definition-type slotd) nil)))
      (when unbound-method
	(let ((reader-function (slot-value slotd 'reader-function))
	      (unbound-value (funcall unbound-method (slot-definition-type slotd))))
	  #'(lambda (object)
	      (not (eql (funcall reader-function object) unbound-value)))))))
   
   ;; Slot has no unbound state
   (#'(lambda (object) (declare (ignore object)) t))))

(define-condition unwritable-slot (cell-error)
  ((instance :reader unwritable-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S in the object ~S is not writable.~@:>"
	      (cell-error-name condition)
	      (unwritable-slot-instance condition)))))

(defmethod compute-slot-writer-function ((slotd effective-virtual-slot-definition))
  (if (slot-boundp slotd 'setter)
      (slot-value slotd 'setter)
    #'(lambda (value object)
	(declare (ignore value))
	(error 'unwritable-slot :name (slot-definition-name slotd) :instance object))))

(defmethod compute-slot-makunbound-function ((slotd effective-virtual-slot-definition))
  (cond
   ((slot-boundp slotd 'makunbound) (slot-value slotd 'makunbound))
   ((slot-boundp slotd 'unbound)
    #'(lambda (object)
	(funcall (slot-value slotd 'writer-function) (slot-value slotd 'unbound) object)))
   (t
    #'(lambda (object)
	(error 'unwritable-slot :name (slot-definition-name slotd) :instance object)))))


#-clisp
(defmethod initialize-internal-slot-functions ((slotd effective-virtual-slot-definition))
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


(defmethod slot-value-using-class
    ((class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  ;; This isn't optimal when we have an unbound value, as the reader
  ;; function gets invoke twice
  (if (funcall (slot-value slotd 'boundp-function) object)
      (funcall (slot-value slotd 'reader-function) object)
    (slot-unbound class object (slot-definition-name slotd))))

(defmethod slot-boundp-using-class
    ((class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (handler-case
      (funcall (slot-value slotd 'boundp-function) object)
    (unreadable-slot (condition) 
      (declare (ignore condition))
      nil)))

(defmethod (setf slot-value-using-class) 
    (value (class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'writer-function) value object))

(defmethod slot-makunbound-using-class
    ((class virtual-slots-class) (object standard-object)
     (slotd effective-virtual-slot-definition))
  (funcall (slot-value slotd 'makunbound-function) object))


;; In CLISP a class may not have been finalized when update-slots are
;; called. So to avoid the possibility of finalize-instance beeing
;; called recursivly  we have to delay the initialization of slot
;; functions until after an instance has been created. We therefor do
;; it in around methods for the generic functions used to access
;; slots.
#+clisp
(defmethod slot-value-using-class :around ((class virtual-slots-class) (object standard-object) (slotd effective-virtual-slot-definition))
  (unless (slot-boundp slotd 'reader-function)
    (setf 
     (slot-value slotd 'reader-function) (compute-slot-reader-function slotd)
     (slot-value slotd 'boundp-function) (compute-slot-boundp-function slotd)))
  (call-next-method))

#+clisp
(defmethod slot-boundp-using-class :around ((class virtual-slots-class) (object standard-object) (slotd effective-virtual-slot-definition))
  (unless (slot-boundp slotd 'boundp-function)
    (setf 
     (slot-value slotd 'reader-function) (compute-slot-reader-function slotd)
     (slot-value slotd 'boundp-function) (compute-slot-boundp-function slotd)))
  (call-next-method))
  
#+clisp
(defmethod (setf slot-value-using-class) :around (value (class virtual-slots-class) (object standard-object) (slotd effective-virtual-slot-definition))
  (declare (ignore value))
  (unless (slot-boundp slotd 'writer-function)
    (setf 
     (slot-value slotd 'writer-function) (compute-slot-writer-function slotd)))
  (call-next-method))

#+clisp
(defmethod slot-makunbound-using-class :around ((class virtual-slots-class) (object standard-object) (slotd effective-virtual-slot-definition))
  (unless (slot-boundp slotd 'makunbound-function)
    (setf 
     (slot-value slotd 'makunbound-function) 
     (compute-slot-makunbound-function slotd)))
  (call-next-method))

(defmethod validate-superclass
    ((class virtual-slots-class) (super standard-class))
  t)

(defmethod slot-definition-special ((slotd standard-direct-slot-definition))
  (declare (ignore slotd))
  nil)

(defmethod slot-definition-special ((slotd standard-effective-slot-definition))
  (declare (ignore slotd))
  nil)


(defclass virtual-slots-object (standard-object)
  ())


;;; To determine if a slot should be initialized with the initform,
;;; CLISP checks whether it is unbound or not. This doesn't work with
;;; virtual slots which does not have an unbound state, so we have to
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
