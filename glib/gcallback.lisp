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

;; $Id: gcallback.lisp,v 1.8 2002-03-24 15:43:16 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; Closures

(deftype gclosure () 'pointer)

(defbinding lisp-callback-closure-new () gclosure
  (callback-id unsigned-int))

(defun register-callback-function (function)
  (check-type function (or null symbol function))
  (register-user-data function))

(defun make-callback-closure (function)
  (lisp-callback-closure-new (register-callback-function function)))


;;;; Callback mechanism

(defun callback-trampoline (callback-id params return-value)
  (let* ((return-type (unless (null-pointer-p return-value)
			(gvalue-type return-value)))
	 (args nil)
	 (callback-function (find-user-data callback-id)))

    (destructuring-bind (nparams . param-values) params
      (dotimes (n nparams)
	(push (gvalue-get (sap+ param-values (* n +gvalue-size+))) args)))

    (labels ((invoke-callback ()
	       (restart-case
		   (unwind-protect
		       (let ((result (apply callback-function (reverse args))))
			 (when return-type
			   (gvalue-set (print return-value) result))))
		
		 (continue nil :report "Return from callback function"
		  (when return-type
		    (format
		     *query-io*
		     "Enter return value of type ~S: "
		     return-type)
		    (force-output *query-io*)
		    (gvalue-set return-value (eval (read *query-io*)))))
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


;;;; Timeouts and idle functions

(defvar *source-callback-marshal*
  (system:foreign-symbol-address "source_callback_marshal"))
(defvar *destroy-notify*
  (system:foreign-symbol-address "destroy_notify"))

(defbinding (timeout-add "g_timeout_add_full")
    (function interval &optional (priority 0)) unsigned-int 
  (priority int)
  (interval unsigned-int)
  (*source-callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-notify* pointer))

(defbinding (idle-add "g_idle_add_full")
    (function &optional (priority 0)) unsigned-int 
  (priority int)
  (*source-callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-notify* pointer))



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
  (let ((callback-id
	 (make-callback-closure
	  (cond
	   ((or (eq object t) (eq object gobject)) function)
	   ((not object)
	    #'(lambda (&rest args) (apply function (cdr args))))
	   (t
	    #'(lambda (&rest args) (apply function object (rest args))))))))
    (signal-connect-closure gobject signal callback-id :after after)))
