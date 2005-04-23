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

;; $Id: gtkutils.lisp,v 1.8 2005-04-23 16:48:52 espen Exp $


(in-package "GTK")

(defun create-button (specs &optional callback &key object)
  (destructuring-bind (label &rest initargs) (mklist specs)
    (let ((button
	   (apply #'make-instance 'button :label label :visible t initargs)))
      (if callback
	  (signal-connect button 'clicked callback :object object)
	(setf (widget-sensitive-p button) nil))
      button)))


(defun create-label (label &rest args)
  (apply #'make-instance 'label :label label args))
  

;; TODO: same syntax as create-button
(defun %create-toggleable-button (class label callback initstate initargs)
  (let ((button 
	 (apply #'make-instance class :label label :active initstate :visible t
		initargs)))
    (signal-connect
     button 'toggled
     #'(lambda ()
	 (funcall (funcallable callback) (toggle-button-active-p button))))
    (funcall (funcallable callback) initstate)
    button))

(defun create-toggle-button (label callback &optional initstate &rest initargs)
  (%create-toggleable-button 'toggle-button label callback initstate initargs))

(defun create-check-button (label callback &optional initstate &rest initargs)
  (%create-toggleable-button 'check-button label callback initstate initargs))

(defun adjustment-new (value lower upper step-increment page-increment page-size)
  (make-instance 'adjustment 
   :value value :lower lower :upper upper :step-increment step-increment
   :page-increment page-increment :page-size page-size))

(defun make-radio-group (type specs callback &rest initargs)
  (let* ((active ())
	 (widgets
	  (loop
	   for spec in specs
	   as widget = (apply #'make-instance type (append spec initargs))
	   do (when callback
	       (apply #'add-activate-callback widget (mklist callback)))
	      (when (and (not active) (getf spec :active))
		(setq active widget))
	  collect widget)))

    (let ((active (or active (first widgets))))
      (loop
       for widget in widgets
       unless (eq widget active)
       do (add-to-radio-group widget active))
      (activate-radio-widget active))

    widgets))
