;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gcallback.lisp,v 1.1 2000-11-09 20:29:19 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; Closures

(deftype gclosure () 'pointer)

(define-foreign lisp-callback-closure () gclosure
  (callback-id unsigned-int))




;;;; Callback mechanism

(defun register-callback-function (function)
  (check-type function (or null symbol function))
  (lisp-callback-closure (register-user-data function)))

(defun callback-trampoline (callback-id params return-value)
  (let* ((return-type (unless (null-pointer-p return-value)
			(type-from-number (gvalue-type return-value))))
	 (args nil)
	 (callback-function (find-user-data callback-id)))

    (destructuring-bind (nparams . param-values) params
      (dotimes (n nparams)
	(push (gvalue-value (sap+ param-values (* n +gvalue-size+))) args)))

    (labels ((invoke-callback ()
	       (restart-case
		   (unwind-protect
		       (let ((result (apply callback-function args)))
			 (when return-type
			   (setf (gvalue-value return-value) result))))
		
		 (continue nil :report "Return from callback function"
		  (when return-type
		    (format
		     *query-io*
		     "Enter return value of type ~S: "
		     return-type)
		    (force-output *query-io*)
		    (setf
		     (gvalue-value return-value)
		     (eval (read *query-io*)))))
		 (re-invoke nil :report "Re-invoke callback function"
		  (invoke-callback)))))
      (invoke-callback))))

(defun after-gc-hook ()
  (setf
   (extern-alien "callback_trampoline" system-area-pointer)
   (make-pointer (kernel:get-lisp-obj-address #'callback-trampoline))
   (extern-alien "destroy_user_data" system-area-pointer)
   (make-pointer (kernel:get-lisp-obj-address #'destroy-user-data))))

(pushnew 'after-gc-hook ext:*after-gc-hooks*)
(after-gc-hook)



;;;; Signals

(defun signal-name-to-string (name)
  (substitute #\_ #\- (string-downcase (string name))))

(define-foreign signal-lookup (name itype) unsigned-int
  ((signal-name-to-string name) string)
  (itype type-number))

(define-foreign signal-name () string
  (signal-id unsigned-int))

(defun %ensure-signal-id (signal-id instance)
  (etypecase signal-id
    (integer signal-id)
    (string (signal-lookup signal-id (type-number-of instance)))
    (symbol (signal-lookup signal-id (type-number-of instance)))))
  
(define-foreign signal-stop-emission (instance signal-id) nil
  (instance ginstance)
  ((%ensure-signal-id signal-id instance) unsigned-int))

; (define-foreign ("g_signal_add_emission_hook_full" signal-add-emisson-hook)
;     () unsigned-int
;   (signal-id unsigned-int)
;   (closure gclosure))

; (define-foreign signal-remove-emisson-hook () nil
;   (signal-id unsigned-int)
;   (hook-id unsigned-int))

(define-foreign ("g_signal_has_handler_pending" signal-has-handler-pending-p)
    (instance signal-id &key detail blocked) boolean
  (instance ginstance)
  ((%ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (blocked boolean))
    
(define-foreign ("g_signal_connect_closure_by_id" signal-connect-closure)
    (instance signal-id closure &key detail after) unsigned-int
  (instance ginstance)
  ((%ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (closure gclosure)
  (after boolean))

(define-foreign signal-handler-block () nil
  (instance ginstance)
  (handler unsigned-int))

(define-foreign signal-handler-unblock () nil
  (instance ginstance)
  (handler unsigned-int))

(define-foreign signal-handler-disconnect () nil
  (instance ginstance)
  (handler unsigned-int))


(defun signal-connect (instance signal function &key after object)
  (let ((callback
	 (cond
	  ((or (eq object t) (eq object instance)) function)
	  ((not object)
	   #'(lambda (&rest args) (apply function (cdr args))))
	  (t
	   #'(lambda (&rest args) (apply function object (rest args)))))))
    
    (signal-connect-closure
     instance signal (register-callback-function callback) :after after)))
