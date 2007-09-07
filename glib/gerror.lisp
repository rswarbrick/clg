;; Common Lisp bindings for GTK+ v2.0
;; Copyright 2005-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gerror.lisp,v 1.8 2007-09-07 07:32:26 espen Exp $


(in-package "GLIB")

(defclass gerror (struct)
  ((domain :allocation :alien :type quark :reader gerror-domain)
   (code :allocation :alien :type int :reader gerror-code)
   (message :allocation :alien :type string :reader gerror-message))
  (:metaclass struct-class)
  (:ref %gerror-copy)
  (:unref %gerror-free))

(defbinding (%gerror-copy "g_error_copy") () pointer
  (location pointer))

(defbinding (%gerror-free "g_error_free") () nil
  (location pointer))

(define-condition glib-error (error)
  ((code :initarg :domain :reader gerror-code)
   (message :initarg :message :reader gerror-message))
  (:report (lambda (condition stream)
	     (write-string (gerror-message condition) stream))))

(define-condition glib-file-error (glib-error)
  ())

(defbinding file-error-quark () quark)

(defun signal-gerror (gerror)
  (let ((condition
	 (cond
	  ((= (gerror-domain gerror) (file-error-quark)) 'glib-file-error)
	  (t 'glib-error))))
    (error condition :code (gerror-code gerror) :message (gerror-message gerror))))


(deftype gerror-signal () 'gerror)

(define-type-method from-alien-form ((type gerror-signal) gerror &key (ref :free))
  (declare (ignore type))
  `(let ((gerror ,(from-alien-form 'gerror gerror :ref ref)))
     (when gerror
       (signal-gerror gerror))))

 
;;; Message logging

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype log-levels () 
    '(flags 
      :recursion :fatal ;; These are not real log-levels, but flags
			;; which may be set
      error-log-level
      critical-log-level
      warning-log-level
      message-log-level
      info-log-level
      debug-log-level)))

(define-condition log-level (warning)
  ((domain :initarg :domain :reader log-domain)
   (message :initarg :message :reader log-message))
  (:report (lambda (condition stream)
	     (format stream "~A: ~A" 
	      (log-domain condition) (log-message condition)))))

(define-condition unknown-log-level (log-level)
  ())

(define-condition error-log-level (log-level)
  ())

(define-condition critical-log-level (log-level)
  ())

(define-condition warning-log-level (log-level)
  ())

(define-condition message-log-level (log-level)
  ())

(define-condition info-log-level (log-level)
  ())

(define-condition debug-log-level (log-level)
  ())

(defparameter *fatal-log-levels* '(error-log-level critical-log-level))

(define-callback log-handler nil 
    ((domain string) (log-level log-levels) (message string))
  (let ((fatal-p (or
		  (find :fatal log-level)
		  (some 
		   #'(lambda (level) (find level *fatal-log-levels*))
		   log-level)))
	(condition (or
		    (find-if 
		     #'(lambda (level) (subtypep level 'condition))
		     log-level)
		    'unknown-log-level)))
    (funcall (if fatal-p #'error #'warn) condition
     :domain domain :message message)))

#-clisp
(setf 
 #+cmu(alien:extern-alien "log_handler" alien:system-area-pointer) 
 #+sbcl(sb-alien:extern-alien "log_handler" sb-alien:system-area-pointer)
 (callback-address log-handler))


#?(pkg-exists-p "glib-2.0" :atleast-version "2.6.0")
(progn
  ;; Unfortunately this will only work as long as we don't abort to
  ;; toplevel from within the log handler. If we do that, the next
  ;; invocation of g_log will be handled as a recursion and cause an
  ;; abort (SIGABORT being signaled). To make things even worse, SBCL
  ;; doesn't handle SIGABRT at all.
  (defbinding %log-set-default-handler (nil) pointer
    (log-handler callback)
    (nil null))
  (%log-set-default-handler))
