;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: gtkutils.lisp,v 1.2 2004-10-31 12:05:52 espen Exp $


(in-package "GTK")


(defun v-box-new (&optional homogeneous (spacing 0))
  (make-instance 'v-box :homogeneous homogeneous :spacing spacing))

(defun create-button (specs &optional callback &rest args)
  (destructuring-bind (label &rest initargs) (mklist specs)
    (let ((button
	   (apply #'make-instance 'button :label label :visible t initargs)))
      (if callback
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (apply (funcallable callback) args)))
	(setf (widget-sensitive-p button) nil))
      button)))

(defun button-new (label &optional callback)
  (let ((button (make-instance 'button :label label)))
    (when callback
      (signal-connect button 'clicked callback))
    button))

(defun label-new (label)
  (make-instance 'label :label label))
  


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

(defun create-radio-button-group (specs active &optional callback &rest args)
  (let ((group nil)
	(i 0))
    (mapcar
     #'(lambda (spec)
	 (destructuring-bind
	     (label &optional object &rest initargs) (mklist spec)
	   (let ((button
		  (apply
		   #'make-instance 'radio-button
		   :label label :visible t initargs)))
	     (when group (%radio-button-set-group button group))
	     (setq group (%radio-button-get-group button))
	     (cond
	      (callback
	       (signal-connect
		button 'toggled
		#'(lambda ()
		    (when (toggle-button-active-p button)
		      (apply (funcallable callback) object args)))))
	      (object
	       (signal-connect
		button 'toggled
		#'(lambda ()
		    (apply
		     (funcallable object)
		     (toggle-button-active-p button) args)))))
	     (when (= i active)
	       (setf (toggle-button-active-p button) t))
	     (incf i)
	     button)))
     specs)))

(defun create-option-menu (specs active &optional callback &rest initargs)
  (let ((menu (make-instance 'menu))
	(group nil)
	(i 0))
    (dolist (spec specs)
      (destructuring-bind (label &optional item-callback) (mklist spec)
	(let ((menu-item
	       (apply
		#'make-instance 'radio-menu-item
		:label label :active (= i active) initargs)))
	  (when group (%radio-menu-item-set-group menu-item group))
	  (setq group (%radio-menu-item-get-group menu-item))
	  (cond
	   (callback
	    (signal-connect menu-item 'activated callback :object t))
	   (item-callback
	    (signal-connect menu-item 'toggled  item-callback :object t)))
	  (incf i)
	  (menu-shell-append menu menu-item))))
    
    (make-instance 'option-menu :history active :menu menu)))

;; (defun sf (n)
;;   (coerce n 'single-float))

(defun adjustment-new (value lower upper step-increment page-increment page-size)
  (make-instance 'adjustment 
   :value value :lower lower :upper upper :step-increment step-increment
   :page-increment page-increment :page-size page-size))
