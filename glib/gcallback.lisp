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

;; $Id: gcallback.lisp,v 1.42 2007-05-10 20:25:09 espen Exp $

(in-package "GLIB")

(use-prefix "g")


;;;; Callback invocation

(deftype gclosure () 'pointer)
(register-type 'gclosure '|g_closure_get_type|)

(defun register-callback-function (function)
  (check-type function (or null symbol function))
  (register-user-data function))

;; Callback marshaller for regular signal handlers
(define-callback signal-handler-marshal nil
    ((gclosure gclosure) (return-value gvalue) (n-params unsigned-int) 
     (param-values pointer) (invocation-hint pointer) 
     (callback-id pointer-data))
  (declare (ignore gclosure invocation-hint))
  (callback-trampoline #'invoke-signal-handler callback-id n-params param-values return-value))

;; Callback marshaller for class handlers
(define-callback class-handler-marshal nil
    ((gclosure gclosure) (return-value gvalue) (n-params unsigned-int) 
     (param-values pointer) (invocation-hint pointer) 
     (callback-id pointer-data))
  (declare (ignore gclosure invocation-hint))
  (callback-trampoline #'invoke-callback callback-id n-params param-values return-value))

;; Callback marshaller for emission hooks
(define-callback emission-hook-marshal nil
    ((invocation-hint pointer) (n-params unsigned-int) (param-values pointer)
     (callback-id pointer-data))
  (declare (ignore invocation-hint))
  (callback-trampoline #'invoke-callback callback-id n-params param-values))

(defun callback-trampoline (restart-wrapper callback-id n-params param-values 
			    &optional (return-value (make-pointer 0)))
  (let* ((return-type (unless (null-pointer-p return-value)
			(gvalue-type return-value)))
	 (args (loop
		for n from 0 below n-params
		for offset from 0 by +gvalue-size+
		collect (gvalue-peek (pointer+ param-values offset)))))
    (unwind-protect
	(multiple-value-bind (result aborted-p)
	    (apply restart-wrapper callback-id nil args)
	  (when (and return-type (not aborted-p))
	    (gvalue-set return-value result)))
      ;; TODO: this should be made more general, by adding a type
      ;; method to return invalidating functions.
      (loop 
       for arg in args
       when (typep arg 'struct)
       do (invalidate-instance arg)))))

(defun invoke-signal-handler (callback-id return-type &rest args)
  (declare (ignore return-type))
  (let* ((instance (first args))
	 (handler-id (signal-handler-find instance '(:data) 
		      0 0 nil nil callback-id)))
    (signal-handler-block instance handler-id)
    (unwind-protect
	(restart-case (apply #'invoke-callback callback-id nil args)
	  (abort () :report "Disconnect and exit signal handler"
	    (when (signal-handler-is-connected-p instance handler-id)
	      (signal-handler-disconnect instance handler-id))
	    (values nil t))))
      (when (signal-handler-is-connected-p instance handler-id)
	(signal-handler-unblock instance handler-id))))

(defun invoke-callback (callback-id return-type &rest args)
  (restart-case (apply (find-user-data callback-id) args)
    (continue nil :report "Return from callback function"
      (cond
       (return-type
	(format *query-io* "Enter return value of type ~S: " return-type)
	(force-output *query-io*)
	(eval (read *query-io*)))
       (t (values nil t))))
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

(define-callback source-callback-marshal nil ((callback-id unsigned-int))
  (callback-trampoline #'invoke-callback callback-id 0 nil))

(defbinding (timeout-add "g_timeout_add_full")
    (interval function &optional (priority +priority-default+)) unsigned-int 
  (priority int)
  (interval unsigned-int)
  (source-callback-marshal callback)
  ((register-callback-function function) unsigned-long)
  (user-data-destroy-callback callback))

(defun timeout-remove (timeout)
  (source-remove timeout))

(defbinding (idle-add "g_idle_add_full")
    (function &optional (priority +priority-default-idle+)) unsigned-int 
  (priority int)
  (source-callback-marshal callback)
  ((register-callback-function function) unsigned-long)
  (user-data-destroy-callback callback))

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

  (define-flags-type signal-match-type
    :id :detail :closure :func :data :unblocked)

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
  (signal-query signal-query :in/return))

(defun signal-param-types (info)
  (with-slots (n-params param-types) info
   (map-c-vector 'list 
    #'(lambda (type-number) 
	(type-from-number type-number))
    param-types 'type-number n-params)))


(defun describe-signal (signal-id &optional type)
  (let ((info (signal-query (ensure-signal-id-from-type signal-id type))))
    (with-slots (id name type flags return-type n-params) info
      (format t "The signal with id ~D is named '~A' and may be emitted on instances of type ~S." id name (type-from-number type t))
      (when flags
	(format t " It has the followin invocation flags: ~{~S ~}" flags))
      (format t "~%~%Signal handlers should take ~A and return ~A~%"
       (if (zerop n-params)
	   "no arguments"
	 (format nil "arguments with the following types: ~A"
	  (signal-param-types info)))
       (cond
	((= return-type (find-type-number "void")) "no values")
	((not (type-from-number return-type)) "values of unknown type")
	((format nil "values of type ~S" (type-from-number return-type))))))))


;;;; Signal connecting and controlling

(defvar *overridden-signals* (make-hash-table :test 'equalp))

(defbinding %signal-override-class-closure () nil
  (signal-id unsigned-int)
  (type-number type-number)
  (callback-closure pointer))


(defun signal-override-class-closure (name type function)
  (let* ((signal-id (ensure-signal-id-from-type name type))
	 (type-number (find-type-number type t))
	 (callback-id (gethash (cons type-number signal-id) *overridden-signals*)))
    (if callback-id
	(update-user-data callback-id function)
      (multiple-value-bind (callback-closure callback-id)
	  (make-callback-closure function class-handler-marshal)
	(%signal-override-class-closure signal-id type-number callback-closure)
	(setf 
	 (gethash (cons type-number signal-id) *overridden-signals*)
	 callback-id)))))


(defbinding %signal-chain-from-overridden () nil
  (args pointer)
  (return-value (or null gvalue)))


(defun %call-next-handler (n-params types args return-type)
  (let ((params (allocate-memory (* n-params +gvalue-size+))))
    (loop 
     for arg in args
     for type in types
     for offset from 0 by +gvalue-size+
     do (gvalue-init (pointer+ params offset) type arg))

    (unwind-protect
	(if return-type
	    (with-gvalue (return-value return-type)
	      (%signal-chain-from-overridden params return-value))
	  (%signal-chain-from-overridden params nil))
      (progn
	(loop
	 repeat n-params
	 for offset from 0 by +gvalue-size+
	 do (gvalue-unset (pointer+ params offset)))
	(deallocate-memory params)))))


(defmacro define-signal-handler (name ((object class) &rest args) &body body)
  (let* ((info (signal-query (ensure-signal-id-from-type name class)))
	 (types (cons class (signal-param-types info)))
	 (n-params (1+ (slot-value info 'n-params)))
	 (return-type (type-from-number (slot-value info 'return-type)))
	 (vars (loop
		for arg in args
		until (eq arg '&rest)
		collect arg))
	 (rest (cadr (member '&rest args)))
	 (next (make-symbol "ARGS"))
	 (default (make-symbol "DEFAULT")))

    `(progn
       (signal-override-class-closure ',name ',class 
	#'(lambda (,object ,@args)
	    (let ((,default (list* ,object ,@vars ,rest)))
	      (flet ((call-next-handler (&rest ,next)
		       (%call-next-handler 
			,n-params ',types (or ,next ,default) ',return-type)))
	      ,@body))))
       ',name)))


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
    unsigned-long
  ((ensure-signal-id-from-type signal type) unsigned-int)
  (detail quark)
  (emission-hook-marshal callback)
  ((register-callback-function function) unsigned-int)
  (user-data-destroy-callback callback))

(defbinding signal-remove-emission-hook (type signal hook-id) nil
  ((ensure-signal-id-from-type signal type) unsigned-int)
  (hook-id unsigned-long))


(defbinding (signal-has-handler-pending-p "g_signal_has_handler_pending")
    (instance signal-id &key detail blocked) boolean
  (instance ginstance)
  ((ensure-signal-id signal-id instance) unsigned-int)
  ((or detail 0) quark)
  (blocked boolean))
    
(defbinding %signal-connect-closure-by-id () unsigned-long
  (instance ginstance)
  (signal-id unsigned-int)
  (detail quark)
  (closure pointer)
  (after boolean))

(defbinding signal-handler-block () nil
  (instance ginstance)
  (handler-id unsigned-long))

(defbinding signal-handler-unblock () nil
  (instance ginstance)
  (handler-id unsigned-long))

;; Internal
(defbinding signal-handler-find () unsigned-long
  (instance gobject)
  (mask signal-match-type)
  (signal-id unsigned-int)
  (detail quark)
  (closure (or null pointer))
  (func (or null pointer))
  (data pointer-data))

(defbinding signal-handler-disconnect () nil
  (instance ginstance)
  (handler-id unsigned-long))

(defbinding signal-handler-is-connected-p () boolean
  (instance ginstance)
  (handler-id unsigned-long))

(defbinding (closure-new "g_cclosure_new") () gclosure
  ((make-pointer #xFFFFFFFF) pointer)
  (callback-id unsigned-int) 
  (destroy-notify callback))

(defbinding closure-set-meta-marshal () nil
  (gclosure gclosure)
  (callback-id unsigned-int)
  (callback callback))

(defun callback-closure-new (callback-id callback destroy-notify)
  (let ((gclosure (closure-new callback-id destroy-notify)))
    (closure-set-meta-marshal gclosure callback-id callback)
    gclosure))

(defun make-callback-closure (function &optional (marshaller signal-handler-marshal))
  (let ((callback-id (register-callback-function function)))
    (values
     (callback-closure-new callback-id marshaller user-data-destroy-callback)
     callback-id)))

(defgeneric compute-signal-function (gobject signal function object args))

(defmethod compute-signal-function ((gobject gobject) signal function object args)
  (declare (ignore signal))
  (cond
   ((or (eq object t) (eq object gobject))
    (if args 
	#'(lambda (&rest emission-args) 
	    (apply function (nconc emission-args args)))
      function))
   (object
    (if args 
	#'(lambda (&rest emission-args) 
	    (apply function object (nconc (rest emission-args) args)))
      #'(lambda (&rest emission-args) 
	  (apply function object (rest emission-args)))))
   (args 
    #'(lambda (&rest emission-args) 
	(apply function (nconc (rest emission-args) args))))
   (t
    #'(lambda (&rest emission-args) 
	(apply function (rest emission-args))))))

(defgeneric compute-signal-id (gobject signal))

(defmethod compute-signal-id ((gobject gobject) signal)
  (ensure-signal-id signal gobject))


(defgeneric signal-connect (gobject signal function &key detail after object remove args))

(defmethod signal-connect :around ((gobject gobject) signal function &rest args)
  (declare (ignore gobject signal args))
  (when function
    (call-next-method)))


(defmethod signal-connect ((gobject gobject) signal function
			   &key detail after object remove args)
"Connects a callback function to a signal for a particular object. If
:OBJECT is T, the object connected to is passed as the first argument
to the callback function, or if :OBJECT is any other non NIL value, it
is passed as the first argument instead. If :AFTER is non NIL, the
handler will be called after the default handler for the signal. If
:REMOVE is non NIL, the handler will be removed after beeing invoked
once. ARGS is a list of additional arguments passed to the callback
function."
(let* ((signal-id (compute-signal-id gobject signal))
       (detail-quark (if detail (quark-intern detail) 0))
       (signal-stop-emission
	#'(lambda ()
	    (%signal-stop-emission gobject signal-id detail-quark)))
       (callback (compute-signal-function gobject signal function object args))
       (wrapper #'(lambda (&rest args)
		    (let ((*signal-stop-emission* signal-stop-emission))
		      (apply callback args)))))
      (multiple-value-bind (closure-id callback-id)
	  (make-callback-closure wrapper signal-handler-marshal)
	(let ((handler-id (%signal-connect-closure-by-id 
			   gobject signal-id detail-quark closure-id after)))
	  (when remove
	    (update-user-data callback-id
	     #'(lambda (&rest args)
		 (unwind-protect
		     (let ((*signal-stop-emission* signal-stop-emission))
		       (apply callback args))
		   (when (signal-handler-is-connected-p gobject handler-id)
		     (signal-handler-disconnect gobject handler-id))))))
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
	       as tmp = params then (pointer+ tmp +gvalue-size+)
	       do (gvalue-init tmp type arg)	      
	       finally 
	       (if return-type
		   (return 
		    (with-gvalue (return-value)
		      (%signal-emitv params signal-id detail return-value)))
		 (%signal-emitv params signal-id detail (make-pointer 0))))
	    (loop
	     repeat n-params
	     as tmp = params then (pointer+ tmp +gvalue-size+)
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


;;;; Signal registration

(defbinding %signal-newv (name itype flags return-type param-types) 
    unsigned-int
  ((signal-name-to-string name) string)
  (itype gtype)
  (flags signal-flags)
  (nil null) ; class closure
  (nil null) ; accumulator
  (nil null) ; accumulator data
  (nil null) ; c marshaller
  (return-type gtype)
  ((length param-types) unsigned-int)
  (param-types (vector gtype)))

(defun signal-new (name itype flags return-type param-types)
  (when (zerop (signal-lookup name itype))
    (%signal-newv name itype flags return-type param-types)))

;;;; Convenient macros

(defmacro define-callback-marshal (name return-type args &key (callback-id :last))
  (let* ((ignore ())
	 (params ())
	 (names (loop 
		 for arg in args 
		 collect (if (or 
			      (eq arg :ignore) 
			      (and (consp arg) (eq (first arg) :ignore)))
			     (let ((name (gensym "IGNORE")))
			       (push name ignore)
			       name)
			   (let ((name (if (atom arg)
					   (gensym (string arg))
					 (first arg))))
			     (push name params)
			     name))))
	 (types (loop 
		 for arg in args 
		 collect (cond
			  ((eq arg :ignore) 'pointer)
			  ((atom arg) arg)
			  (t (second arg))))))
    `(define-callback ,name ,return-type 
       ,(ecase callback-id
	  (:first `((callback-id pointer-data) ,@(mapcar #'list names types)))
	  (:last `(,@(mapcar #'list names types) (callback-id pointer-data))))
       (declare (ignore ,@ignore))
       (invoke-callback callback-id ',return-type ,@(nreverse params)))))

(defmacro with-callback-function ((id function) &body body)
  `(let ((,id (register-callback-function ,function)))
    (unwind-protect
	 (progn ,@body)
      (destroy-user-data ,id))))
