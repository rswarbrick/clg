;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gerror.lisp,v 1.1 2005-02-14 00:39:39 espen Exp $


(in-package "GLIB")

(defclass gerror (struct)
  ((domain :allocation :alien :type quark :reader gerror-domain)
   (code :allocation :alien :type int :reader gerror-code)
   (message :allocation :alien :type string :reader gerror-message))
  (:metaclass struct-class))

(defbinding (%gerror-copy "g_error_copy") () pointer
  (location pointer))

(defbinding (%gerror-free "g_error_free") () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'gerror))) location)
  (declare (ignore class))
  (%gerror-copy location))

(defmethod unreference-foreign ((class (eql (find-class 'gerror))) location)
  (declare (ignore class))
  (%gerror-free location))


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

(define-condition info-log-level (log-level)
  ())

(define-condition debug-log-level (log-level)
  ())

(defparameter *fatal-log-levels* '(error-log-level critical-log-level))

(defcallback log-handler (nil 
			   (domain (copy-of string))
			   (log-level log-levels)
			   (message (copy-of string)))
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

(setf (extern-alien "log_handler" system-area-pointer) (callback log-handler))


#+glib2.6
(progn
  ;; Unfortunately this will only work as long as we don't abort to
  ;; toplevel from within the log handler. If we do that, the next
  ;; invocation of g_log will be handled as a recursion and cause an
  ;; abort (SIGABORT being signaled). To make things even worse, SBCL
  ;; doesn't handle SIGABRT at all.
  (defbinding %log-set-default-handler () pointer
    ((callback log-handler) pointer)
    (nil null))
  (%log-set-default-handler))
