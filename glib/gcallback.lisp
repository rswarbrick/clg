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

;; $Id: gcallback.lisp,v 1.13 2004-11-07 01:23:38 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; Callback mechanism

(deftype gclosure () 'pointer)

(defbinding (callback-closure-new "clg_callback_closure_new") () gclosure
  (callback-id unsigned-int) 
  (callback pointer)
  (destroy-notify pointer))

(defun register-callback-function (function)
  (check-type function (or null symbol function))
  (register-user-data function))

(defcallback closure-callback-marshal (nil
				       (gclosure pointer)
				       (return-value gvalue)
				       (n-params unsigned-int) 
				       (param-values pointer)
				       (invocation-hint pointer) 
				       (callback-id unsigned-int))
  (callback-trampoline callback-id n-params param-values return-value))

(defcallback %destroy-user-data (nil (id unsigned-int))
  (destroy-user-data id))
 
(defun make-callback-closure (function)
  (callback-closure-new 
   (register-callback-function function)
   (callback closure-callback-marshal) (callback %destroy-user-data)))


(defun callback-trampoline (callback-id n-params param-values return-value)
  (let* ((return-type (unless (null-pointer-p return-value)
			(gvalue-type return-value)))
	 (args (loop
		for n from 0 below n-params
		collect (gvalue-get (sap+ param-values (* n +gvalue-size+))))))
    (let ((result (apply #'invoke-callback callback-id return-type args)))
      (when return-type
	(gvalue-set return-value result)))))


(defun invoke-callback (callback-id return-type &rest args)
  (restart-case
      (apply (find-user-data callback-id) args)
    (continue nil :report "Return from callback function"
	      (when return-type
		(format *query-io* "Enter return value of type ~S: " return-type)
		(force-output *query-io*)
		(eval (read *query-io*))))
    (re-invoke nil :report "Re-invoke callback function"
	       (apply #'invoke-callback callback-id return-type args))))


;;;; Timeouts and idle functions

(defcallback source-callback-marshal (nil (callback-id unsigned-int))
  (callback-trampoline callback-id 0 nil (make-pointer 0)))

(defbinding (timeout-add "g_timeout_add_full")
    (function interval &optional (priority 0)) unsigned-int 
  (priority int)
  (interval unsigned-int)
  (*source-callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  ((callback %destroy-user-data) pointer))

(defbinding (idle-add "g_idle_add_full")
    (function &optional (priority 0)) unsigned-int 
  (priority int)
  (*source-callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  ((callback %destroy-user-data) pointer))



;;;; Signals

(defbinding signal-lookup (name itype) unsigned-int
  ((signal-name-to-string name) string)
  (itype type-number))

(defbinding signal-name () string
  (signal-id unsigned-int))

(defun ensure-signal-id (signal-id instance)
  (etypecase signal-id
    (integer signal-id)
    (string (signal-lookup signal-id (type-number-of instance)))
    (symbol (signal-lookup signal-id (type-number-of instance)))))
  
(defbinding signal-stop-emission (instance signal-id) nil
  (instance ginstance)
  ((ensure-signal-id signal-id instance) unsigned-int))

; (defbinding (signal-add-emisson-hook "g_signal_add_emission_hook_full")
;     () unsigned-int
;   (signal-id unsigned-int)
;   (closure gclosure))

; (defbinding signal-remove-emisson-hook () nil
;   (signal-id unsigned-int)
;   (hook-id unsigned-int))

(defbinding (signal-has-handler-pending-p "g_signal_has_handler_pending")
    (instance signal-id &key detail blocked) boolean
  (instance ginstance)
  ((ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (blocked boolean))
    
(defbinding (signal-connect-closure "g_signal_connect_closure_by_id")
    (instance signal-id closure &key detail after) unsigned-int
  (instance ginstance)
  ((ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (closure gclosure)
  (after boolean))

(defbinding signal-handler-block () nil
  (instance ginstance)
  (handler unsigned-int))

(defbinding signal-handler-unblock () nil
  (instance ginstance)
  (handler unsigned-int))

(defbinding signal-handler-disconnect () nil
  (instance ginstance)
  (handler unsigned-int))


(defmethod signal-connect ((gobject gobject) signal function &key after object)
"Connects a callback function to a signal for a particular object. If :OBJECT 
 is T, the object connected to is passed as the first argument to the callback 
 function, or if :OBJECT is any other non NIL value, it is passed as the first 
 argument instead. If :AFTER is non NIL, the handler will be called after the 
 default handler of the signal."
  (let ((callback-id
	 (make-callback-closure
	  (cond
	   ((or (eq object t) (eq object gobject)) function)
	   ((not object)
	    #'(lambda (&rest args) (apply function (cdr args))))
	   (t
	    #'(lambda (&rest args) (apply function object (rest args))))))))
    (signal-connect-closure gobject signal callback-id :after after)))


;;; Message logging

;; TODO: define and signal conditions based on log-level
;(defun log-handler (domain log-level message)
(def-callback log-handler (c-call:void (domain c-call:c-string) 
				       (log-level c-call:int) 
				       (message c-call:c-string))
  (error "~A: ~A" domain message))

(setf (extern-alien "log_handler" system-area-pointer) (callback log-handler))
