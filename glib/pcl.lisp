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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (ext:package-lock (find-package "PCL")) nil))

(in-package "PCL")

(defstruct slot-info
  (name nil :type symbol)
  ;;
  ;; Specified slot allocation.or :INSTANCE.
  (allocation :instance :type symbol)
  ;;
  ;; Specified slot type or T.
  (type t :type (or symbol list number)))


(defmethod compute-slots :around ((class standard-class))
  (loop with slotds = (call-next-method) and location = -1
	for slot in slotds do
	  (setf (slot-definition-location slot)
		(case (slot-definition-allocation slot)
		  (:instance
		   (incf location))
		  (:class
		   (let* ((name (slot-definition-name slot))
			  (from-class (slot-definition-allocation-class slot))
			  (cell (assq name (class-slot-cells from-class))))
		     (assert (consp cell))
		     cell))))
	  (initialize-internal-slot-functions slot)
	finally
	  (return slotds)))



(defun update-slots (class eslotds)
  (collect ((instance-slots) (class-slots))
    (dolist (eslotd eslotds)
      (case (slot-definition-allocation eslotd)
	(:instance (instance-slots eslotd))
	(:class (class-slots eslotd))))
    ;;
    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
			    (sort (instance-slots) #'<
				  :key #'slot-definition-location)))
	   (nslots (length nlayout))
	   (nwrapper-class-slots (compute-class-slots (class-slots)))
	   (owrapper (when (class-finalized-p class)
		       (class-wrapper class)))
	   (olayout (when owrapper
		      (wrapper-instance-slots-layout owrapper)))
	   (nwrapper
	    (cond ((null owrapper)
		   (make-wrapper nslots class))
		  ;;
		  ;; We cannot reuse the old wrapper easily when it
		  ;; has class slot cells, even if these cells are
		  ;; EQUAL to the ones used in the new wrapper.  The
		  ;; class slot cells of OWRAPPER may be referenced
		  ;; from caches, and if we don't change the wrapper,
		  ;; the caches won't notice that something has
		  ;; changed.  We could do something here manually,
		  ;; but I don't think it's worth it.
		  ((and (equal nlayout olayout)
			(null (wrapper-class-slots owrapper)))
		   owrapper)
		  (t
		   ;;
		   ;; This will initialize the new wrapper to have the same
		   ;; state as the old wrapper.  We will then have to change
		   ;; that.  This may seem like wasted work (it is), but the
		   ;; spec requires that we call make-instances-obsolete.
		   (make-instances-obsolete class)
		   (class-wrapper class)))))

      (with-slots (wrapper slots finalized-p) class
	(update-lisp-class-layout class nwrapper)
	(setf slots eslotds
	      (wrapper-instance-slots-layout nwrapper) nlayout
	      (wrapper-class-slots nwrapper) nwrapper-class-slots
	      (wrapper-no-of-instance-slots nwrapper) nslots
	      wrapper nwrapper
	      finalized-p t))

      (unless (eq owrapper nwrapper)
	(update-inline-access class)
	(update-pv-table-cache-info class)
	(maybe-update-standard-class-locations class)))))

