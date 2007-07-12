;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: utils.lisp,v 1.3 2007-07-12 09:02:53 espen Exp $

(defpackage #:clg-utils
  (:use #:common-lisp)
  (:export #:read-lines #:mklist #:namep #:funcallable #:return-if #:when-bind
	   #:visible-char-p #:whitespace-p #:split-string-if #:split-string
	   #:concatenate-strings #:string-prefix-p #:get-all #:plist-remove
	   #:delete-collect-if))
	   
(in-package #:clg-utils)

(defun read-lines (&optional (stream *standard-input*))
  "Read lines from STREAM until end of file."
  (loop
   as line = (read-line stream nil)
   while line
   collect line))

(defun mklist (obj)
  (if (and obj (atom obj)) (list obj) obj))

(defun namep (obj)
  (and (symbolp obj) (not (member obj '(t nil)))))

(defun funcallable (object)
  (if (consp object)
      (fdefinition object)
    object))

(defmacro return-if (form)
  (let ((result (make-symbol "RESULT")))
    `(let ((,result ,form))
       (when ,result
	 (return ,result)))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun visible-char-p (char)
  (and (graphic-char-p char) (char/= char #\space)))

(defun whitespace-p (char)
  (not (visible-char-p char)))

(defun split-string-if (string predicate)
  (declare (simple-string string))
  (let ((pos (position-if predicate string :start 1)))
    (if (not pos)
        (list string)
      (cons
       (subseq string 0 pos)
       (split-string-if (subseq string pos) predicate)))))

(defun split-string (string &key (delimiter #'whitespace-p) 
		     (start 0) (end (length string)))
  (let* ((predicate (if (functionp delimiter)
			delimiter
		      #'(lambda (char)
			  (find char (mklist delimiter) :test #'char=))))
	 (from (position-if-not predicate string :start start)))
    (when from
      (let ((to (position-if predicate string :start from :end end)))
	(cons 
	 (subseq string from (or to end))
	 (when to
	   (split-string string :delimiter predicate :start to :end end)))))))

(defun concatenate-strings (strings &optional delimiter)
  (if (not (rest strings))
      (first strings)
    (concatenate
     'string
     (first strings)
     (if delimiter (string delimiter) "")
     (concatenate-strings (rest strings) delimiter))))

(defun string-prefix-p (prefix string)
  (and
   (>= (length string) (length prefix))
   (string= prefix string :end2 (length prefix))))

(defun get-all (plist property)
  (multiple-value-bind (property value tail)
      (get-properties plist (list property))
    (when tail
      (cons value (get-all (cddr tail) property)))))

(defun plist-remove (key plist &key (test #'eq))
  (loop
   for (%key value) on plist by #'cddr
   while (and %key value)
   unless (funcall test key %key)
   nconc (list %key value)))

(defun delete-collect-if (predicate seq)
  (let ((head (cons nil seq)))
    (values
     (loop
      for tmp on head
      while (cdr tmp)
      when (funcall predicate (second tmp))
      collect (let ((elm (second tmp)))
		(setf (cdr tmp) (cddr tmp))
		elm))
     (cdr head))))
