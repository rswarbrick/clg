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

;; $Id: gtkutils.lisp,v 1.4 2004-12-04 18:24:01 espen Exp $


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


(defun adjustment-new (value lower upper step-increment page-increment page-size)
  (make-instance 'adjustment 
   :value value :lower lower :upper upper :step-increment step-increment
   :page-increment page-increment :page-size page-size))

(defun create-action (name &optional stock-id label accelerator tooltip 
		      callback &rest initargs)
  (let ((action (apply #'make-instance 'action
		 :name (string name) :stock-id stock-id  :label label
		 :tooltip tooltip :accelerator accelerator initargs)))
    (when callback
      (signal-connect action 'activate callback))
    action))

(defun create-toggle-action (name &optional stock-id label accelerator 
			     tooltip active callback &rest initargs)
  (let ((action (apply #'make-instance 'toggle-action 
		 :name (string name) :stock-id stock-id :label label
		 :tooltip tooltip :active active :accelerator accelerator
		 initargs)))
    (when callback
      (signal-connect action 'activate
       #'(lambda ()
	   (funcall callback (toggle-action-active-p action))))
      (funcall callback active))
    action))

(defun create-radio-actions (specs &optional active callback &rest initargs)
  (loop
   with group = nil
   for spec in specs
   collect (destructuring-bind (name &optional stock-id label accelerator 
				tooltip (value name)) 
	       (mklist spec)
	     (let ((action (apply #'make-instance 'radio-action 
			    :name (string name) :stock-id stock-id 
			    :label label :tooltip tooltip 
			    :accelerator accelerator initargs)))
	       (when (equal active value)
		 (setf (toggle-action-active-p action) t)
		 (when callback
		   (funcall callback value)))

	       (if (not group)
		   (setq group action)
		 (radio-action-add-to-group action group))
	       (when callback
		 (signal-connect action 'activate
		  #'(lambda ()
		      (funcall callback value))))
	       action))))
