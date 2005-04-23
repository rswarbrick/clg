;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gcallback.lisp,v 1.24 2005-04-23 16:48:50 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; Callback invokation

(defun register-callback-function (function)
  (check-type function (or null symbol function))
  (register-user-data function))

;; Callback marshal for regular signal handlers
(defcallback closure-marshal (nil
			      (gclosure pointer)
			      (return-value gvalue)
			      (n-params unsigned-int) 
			      (param-values pointer)
			      (invocation-hint pointer) 
			      (callback-id unsigned-int))
  (declare (ignore gclosure invocation-hint))
  (callback-trampoline callback-id n-params param-values return-value))

;; Callback function for emission hooks
(defcallback signal-emission-hook (nil
				   (invocation-hint pointer)
				   (n-params unsigned-int) 
				   (param-values pointer)
				   (callback-id unsigned-int))
  (callback-trampoline callback-id n-params param-values))

(defun callback-trampoline (callback-id n-params param-values &optional
			    (return-value (make-pointer 0)))
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

(defconstant +priority-high+ -100)
(defconstant +priority-default+ 0)
(defconstant +priority-high-idle+ 100)
(defconstant +priority-default-idle+ 200)
(defconstant +priority-low+ 300)

(defbinding source-remove () boolean
  (tag unsigned-int))

(defcallback source-callback-marshal (nil (callback-id unsigned-int))
  (callback-trampoline callback-id 0 nil))

(defbinding (timeout-add "g_timeout_add_full")
    (interval function &optional (priority +priority-default+)) unsigned-int 
  (priority int)
  (interval unsigned-int)
  ((callback source-callback-marshal) pointer)
  ((register-callback-function function) unsigned-long)
  ((callback user-data-destroy-func) pointer))

(defun timeout-remove (timeout)
  (source-remove timeout))

(defbinding (idle-add "g_idle_add_full")
    (function &optional (priority +priority-default-idle+)) unsigned-int 
  (priority int)
  ((callback source-callback-marshal) pointer)
  ((register-callback-function function) unsigned-long)
  ((callback user-data-destroy-func) pointer))

(defun idle-remove (idle)
  (source-remove idle))


;;;; Signal information querying

(defbinding signal-lookup (name type) unsigned-int
  ((signal-name-to-string name) string)
  ((find-type-number type t) type-number))

(defbinding signal-name () (copy-of string)
  (signal-id unsigned-int))

(defbinding signal-list-ids (type) (vector unsigned-int n-ids)
  ((find-type-number type t) type-number)
  (n-ids unsigned-int :out))

(defun signal-list-names (type)
  (map 'list #'signal-name (signal-list-ids type)))

(defun ensure-signal-id-from-type (signal-id type)
  (etypecase signal-id
    (integer (if (signal-name signal-id)
		 signal-id
	       (error "Invalid signal id: ~D" signal-id)))
    ((or symbol string) 
     (let ((numeric-id (signal-lookup signal-id type)))
       (if (zerop numeric-id)
	   (error "Invalid signal name for ~S: ~D" type signal-id)
	 numeric-id)))))

(defun ensure-signal-id (signal-id instance)
  (ensure-signal-id-from-type signal-id (type-of instance)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype signal-flags () 
    '(flags :run-first :run-last :run-cleanup :no-recurse 
	    :detailed :action :no-hooks))

  (defclass signal-query (struct)
    ((id :allocation :alien :type unsigned-int)
     (name :allocation :alien :type (copy-of string))
     (type :allocation :alien :type type-number)
     (flags :allocation :alien :type signal-flags)
     (return-type :allocation :alien :type type-number)
     (n-params :allocation :alien :type unsigned-int)
     (param-types :allocation :alien :type pointer))
    (:metaclass struct-class)))

(defbinding signal-query 
    (signal-id &optional (signal-query (make-instance 'signal-query))) nil
  (signal-id unsigned-int)
  (signal-query signal-query :return))

(defun signal-param-types (info)
  (with-slots (n-params param-types) info
   (map-c-vector 'list 
    #'(lambda (type-number) 
	(type-from-number type-number))
    param-types 'type-number n-params)))


(defun describe-signal (signal-id &optional type)
  (let ((info (signal-query (ensure-signal-id-from-type signal-id type))))
    (with-slots (id name type flags return-type n-params) info
      (format t "The signal with id ~D is named '~A' and may be emitted on instances of type ~S~%~%" id name (type-from-number type t))
      (format t "Signal handlers should return ~A and take ~A~%"
       (cond
	((= return-type (find-type-number "void")) "no values")
	((not (type-from-number return-type)) "values of unknown type")
	((format nil "values of type ~S" (type-from-number return-type))))
       (if (zerop n-params)
	   "no arguments"
	 (format nil "arguments with the following types: ~A"
	  (signal-param-types info)))))))


;;;; Signal connecting and controlling

(defbinding %signal-stop-emission () nil
  (instance ginstance)
  (signal-id unsigned-int)
  (detail quark))

(defvar *signal-stop-emission* nil)
(declaim (special *signal-stop-emission*))

(defun signal-stop-emission ()
  (if *signal-stop-emission*
      (funcall *signal-stop-emission*)
    (error "Not inside a signal handler")))


(defbinding signal-add-emission-hook (type signal function &key (detail 0))
    unsigned-int
  ((ensure-signal-id-from-type signal type) unsigned-int)
  (detail quark)
  ((callback signal-emission-hook) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback user-data-destroy-func) pointer))

(defbinding signal-remove-emission-hook (type signal hook-id) nil
  ((ensure-signal-id-from-type signal type) unsigned-int)
  (hook-id unsigned-int))


(defbinding (signal-has-handler-pending-p "g_signal_has_handler_pending")
    (instance signal-id &key detail blocked) boolean
  (instance ginstance)
  ((ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (blocked boolean))
    
(defbinding %signal-connect-closure-by-id () unsigned-int
  (instance ginstance)
  (signal-id unsigned-int)
  (detail quark)
  (closure pointer)
  (after boolean))

(defbinding signal-handler-block () nil
  (instance ginstance)
  (handler-id unsigned-int))

(defbinding signal-handler-unblock () nil
  (instance ginstance)
  (handler-id unsigned-int))

(defbinding signal-handler-disconnect () nil
  (instance ginstance)
  (handler-id unsigned-int))

(defbinding signal-handler-is-connected-p () boolean
  (instance ginstance)
  (handler-id unsigned-int))

(deftype gclosure () 'pointer)

(defbinding (callback-closure-new "clg_callback_closure_new") () gclosure
  (callback-id unsigned-int) 
  (callback pointer)
  (destroy-notify pointer))

(defun make-callback-closure (function)
  (let ((callback-id (register-callback-function function)))
    (values
     (callback-closure-new 
      callback-id (callback closure-marshal) 
      (callback user-data-destroy-func))
     callback-id)))

(defgeneric compute-signal-function (gobject signal function object))

(defmethod compute-signal-function ((gobject gobject) signal function object)
  (declare (ignore signal))
  (cond
   ((or (eq object t) (eq object gobject)) function)
   ((not object)
    #'(lambda (&rest args) (apply function (rest args))))
   (t
    #'(lambda (&rest args) (apply function object (rest args))))))


(defgeneric compute-signal-id (gobject signal))

(defmethod compute-signal-id ((gobject gobject) signal)
  (ensure-signal-id signal gobject))


(defgeneric signal-connect (gobject signal function &key detail after object remove))

(defmethod signal-connect :around ((gobject gobject) signal function &rest args)
  (declare (ignore gobject signal args))
  (when function
    (call-next-method)))


(defmethod signal-connect ((gobject gobject) signal function
			   &key detail after object remove)
"Connects a callback function to a signal for a particular object. If
:OBJECT is T, the object connected to is passed as the first argument
to the callback function, or if :OBJECT is any other non NIL value, it
is passed as the first argument instead. If :AFTER is non NIL, the
handler will be called after the default handler for the signal. If
:REMOVE is non NIL, the handler will be removed after beeing invoked
once."
(let* ((signal-id (compute-signal-id gobject signal))
       (detail-quark (if detail (quark-intern detail) 0))
       (signal-stop-emission
	#'(lambda ()
	    (%signal-stop-emission gobject signal-id detail-quark)))
       (callback (compute-signal-function gobject signal function object))
       (wrapper #'(lambda (&rest args)
		    (let ((*signal-stop-emission* signal-stop-emission))
		      (apply callback args)))))
      (multiple-value-bind (closure-id callback-id)
	  (make-callback-closure wrapper)
	(let ((handler-id (%signal-connect-closure-by-id 
			   gobject signal-id detail-quark closure-id after)))
	  (when remove
	    (update-user-data callback-id
	     #'(lambda (&rest args)
		 (unwind-protect
		     (let ((*signal-stop-emission* signal-stop-emission))
		       (apply callback args))
		   (signal-handler-disconnect gobject handler-id)))))
	  handler-id))))


;;;; Signal emission

(defbinding %signal-emitv () nil
  (gvalues pointer)
  (signal-id unsigned-int)
  (detail quark)
  (return-value gvalue))

(defvar *signal-emit-functions* (make-hash-table))

(defun create-signal-emit-function (signal-id)
  (let ((info (signal-query signal-id)))
    (let* ((type (type-from-number (slot-value info 'type)))
	   (param-types (cons type (signal-param-types info)))
	   (return-type (type-from-number (slot-value info 'return-type)))
	   (n-params (1+ (slot-value info 'n-params)))
	   (params (allocate-memory (* n-params +gvalue-size+))))
      #'(lambda (detail object &rest args)
 	  (unless (= (length args) (1- n-params))
 	    (error "Invalid number of arguments: ~A" (+ 2 (length args))))
	  (unwind-protect
	      (loop
	       for arg in (cons object args)
	       for type in param-types
	       as tmp = params then (sap+ tmp +gvalue-size+)
	       do (gvalue-init tmp type arg)	      
	       finally 
	       (if return-type
		   (return 
		    (with-gvalue (return-value)
		      (%signal-emitv params signal-id detail return-value)))
		 (%signal-emitv params signal-id detail (make-pointer 0))))
	    (loop
	     repeat n-params
	     as tmp = params then (sap+ tmp +gvalue-size+)
	     while (gvalue-p tmp)
	     do (gvalue-unset tmp)))))))

(defun signal-emit-with-detail (object signal detail &rest args)
  (let* ((signal-id (ensure-signal-id signal object))
	 (function (or 
		    (gethash signal-id *signal-emit-functions*)
		    (setf 
		     (gethash signal-id *signal-emit-functions*)
		     (create-signal-emit-function signal-id)))))
    (apply function detail object args)))

(defun signal-emit (object signal &rest args)
  (apply #'signal-emit-with-detail object signal 0 args))


;;;; Convenient macros

(defmacro def-callback-marshal (name (return-type &rest args))
  (let ((names (loop 
		for arg in args 
		collect (if (atom arg) (gensym) (first arg))))
	(types (loop 
		for arg in args 
		collect (if (atom arg) arg (second arg)))))
    `(defcallback ,name (,return-type ,@(mapcar #'list names types)
			 (callback-id unsigned-int))
      (invoke-callback callback-id ',return-type ,@names))))

(defmacro with-callback-function ((id function) &body body)
  `(let ((,id (register-callback-function ,function)))
    (unwind-protect
	 (progn ,@body)
      (destroy-user-data ,id))))
