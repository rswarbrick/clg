;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

;;; Modifications for better AMOP conformance
;;; Copyright (C) 2001 Espen S. Johnsen <esj@stud.cs.uit.no>

(in-package "PCL")

;;;; Adding initargs parameter to change-class

(defun change-class-internal (instance new-class initargs)
  (let* ((old-class (class-of instance))
	 (copy (allocate-instance new-class))
	 (new-wrapper (get-wrapper copy))
	 (old-wrapper (class-wrapper old-class))
	 (old-layout (wrapper-instance-slots-layout old-wrapper))
	 (new-layout (wrapper-instance-slots-layout new-wrapper))
	 (old-slots (get-slots instance))
	 (new-slots (get-slots copy))
	 (old-class-slots (wrapper-class-slots old-wrapper)))

    ;;
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;;     
    (iterate ((new-slot (list-elements new-layout))
	      (new-position (interval :from 0)))
      (let ((old-position (posq new-slot old-layout)))
	(when old-position
	  (setf (instance-ref new-slots new-position)
		(instance-ref old-slots old-position)))))

    ;;
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    ;;
    (iterate ((slot-and-val (list-elements old-class-slots)))
      (let ((position (posq (car slot-and-val) new-layout)))
	(when position
	  (setf (instance-ref new-slots position) (cdr slot-and-val)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (swap-wrappers-and-slots instance copy)

    (apply #'update-instance-for-different-class copy instance initargs)
    instance))


(fmakunbound 'change-class)
(defgeneric change-class (instance new-class &rest initargs))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class)
			 &rest initargs)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs)
  (declare (ignore initargs))
  (error "Can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
	 instance new-class 'standard-class))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class standard-class)
			 &rest initargs)
  (declare (ignore initargs))
  (error "Can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
	 instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (change-class instance (find-class new-class) initargs))


;;;; Make the class finalization protocol behave as specified in AMOP

(defmethod ensure-class-using-class (name (class pcl-class) &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (if (eq (class-of class) meta)
	(apply #'reinitialize-instance class initargs)
      (apply #'change-class class meta initargs))
    (setf (find-class name) class)
    (inform-type-system-about-class class name)
    class))

(defmethod finalize-inheritance ((class std-class))
  (dolist (super (class-direct-superclasses class))
    (unless (class-finalized-p super) (finalize-inheritance super)))
  (update-cpl class (compute-class-precedence-list class))
  (update-slots class (compute-slots class))
  (update-gfs-of-class class)
  (update-inits class (compute-default-initargs class))
  (update-make-instance-function-table class))

(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "~A can't be finalized" class))

(defun update-class (class &optional finalizep)  
  (declare (ignore finalizep))
  (unless (class-has-a-forward-referenced-superclass-p class)
    (finalize-inheritance class)
    (dolist (sub (class-direct-subclasses class))
      (update-class sub))))

