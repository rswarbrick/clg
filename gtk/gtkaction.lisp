;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gtkaction.lisp,v 1.11 2007-01-14 23:22:19 espen Exp $


(in-package "GTK")

;;; Action

(defmethod initialize-instance ((action action) &key callback)
  (call-next-method)
  (when callback
    (apply #'signal-connect action 'activate (mklist callback))))

(defbinding (action-is-sensitive-p "gtk_action_is_sensitive") () boolean
  (action action))

(defbinding (action-is-visible-p "gtk_action_is_visible") () boolean
  (action action))

(defbinding action-activate () nil
  (action action))


;;; Action Group

(defmethod initialize-instance ((action-group action-group) &rest initargs 
				&key action actions)
  (declare (ignore action actions))
  (prog1
      (call-next-method)
    (initial-add action-group #'action-group-add-action
     initargs :action :actions)))

(defbinding action-group-get-action () action
  (action-group action-group)
  (name string))

(defbinding action-group-list-actions () (glist action)
  (action-group action-group))

(defbinding %action-group-add-action () nil
  (action-group action-group)
  (action action))

(defbinding %action-group-add-action-with-accel () nil
  (action-group action-group)
  (action action)
  (accelerator (or null string)))

(defun action-group-add-action (action-group action)
  (if (slot-boundp action 'accelerator)
      (%action-group-add-action-with-accel action-group action (action-accelerator action))
    (%action-group-add-action action-group action)))

(defbinding action-group-remove-action () nil
  (action-group action-group)
  (action action))


;;; Radio Action

(defmethod initialize-instance ((action radio-action) &key group)
  (call-next-method)
  (setf (slot-value action 'self) (pointer-address (foreign-location action)))
  (when group
    (add-to-radio-group action group)))

(defbinding %radio-action-get-group () pointer
  (radio-action radio-action))

(defbinding %radio-action-set-group () nil
  (radio-button radio-button)
  (group pointer))

(defmethod add-to-radio-group ((action1 radio-action) (action2 radio-action))
  "Add ACTION1 to the group which ACTION2 belongs to."
  (%radio-action-set-group action1 (%radio-action-get-group action2)))

(defmethod activate-radio-widget ((action radio-action))
  (action-activate action))

(defmethod add-activate-callback ((action radio-action) function &key object after)
  (%add-activate-callback action 'activate function object after))

(defbinding (radio-action-get-current "gtk_radio_action_get_current_value") 
    () radio-action
  "Returns the current active radio action in the group the give radio action belongs to."
  (radio-action radio-action))

(defun radio-action-get-current-value (action)
  (radio-action-value (radio-action-get-current action)))



;;; Toggle Action

(defmethod initialize-instance ((action toggle-action) &rest initargs &key callback #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")active)
  (remf initargs :callback)
  (apply #'call-next-method action initargs)
  (when callback
    (destructuring-bind (function &key object after) (mklist callback)
      (signal-connect action 'activate
       (if object 
	   #'(lambda (object)
	       (funcall function object (toggle-action-active-p action)))
	 #'(lambda ()
	     (funcall function (toggle-action-active-p action))))
       :object object :after after)))
  #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
  (when active
    (action-activate action)))

(defbinding toggle-action-toggled () nil
  (toggle-action toggle-action))


;;; UI Manager

(defmethod initialize-instance ((ui-manager ui-manager) &rest initargs 
				&key ui action-group)
  (declare (ignore ui action-group))
  (call-next-method)
  (mapc #'(lambda (action-group)
	    (ui-manager-insert-action-group ui-manager action-group))
	(get-all initargs :action-group))
  (mapc #'(lambda (ui)
	    (ui-manager-add-ui ui-manager ui))
	(get-all initargs :ui)))


(defbinding ui-manager-insert-action-group 
    (ui-manager action-group &optional (pos :end)) nil
  (ui-manager ui-manager)
  (action-group action-group)
  ((case pos
     (:first 0)
     (:end -1)
     (t pos)) int))

(defbinding ui-manager-remove-action-group () nil
  (ui-manager ui-manager)
  (action-group action-group))

(defbinding ui-manager-get-widget () widget
  (ui-manager ui-manager)
  (path string))

(defbinding ui-manager-get-toplevels () (glist widget)
  (ui-manager ui-manager)
  (types ui-manager-item-type))

(defbinding ui-manager-get-action () action
  (ui-manager ui-manager)
  (path string))

(defbinding %ui-manager-add-ui-from-string (ui-manager ui) int
  (ui-manager ui-manager)
  (ui string)
  ((length ui) int)
  (gerror gerror-signal :out))

(defgeneric ui-manager-add-ui (ui-manager ui-spec))

(defmethod ui-manager-add-ui ((ui-manager ui-manager) (ui-spec string))
  (%ui-manager-add-ui-from-string ui-manager ui-spec))

(defbinding %ui-manager-add-ui-from-file () int
  (ui-manager ui-manager)
  (filename pathname)
  (gerror gerror-signal :out))

(defmethod ui-manager-add-ui ((ui-manager ui-manager) (path pathname))
  (%ui-manager-add-ui-from-file ui-manager path))

(defbinding %ui-manager-new-merge-id () unsigned-int
  (ui-manager ui-manager))

(defbinding %ui-manager-add-ui () nil
  (ui-manager ui-manager)
  (merge-id unsigned-int)
  (path string)
  (name string)
  (action (or null string))
  (type ui-manager-item-type)
  (top boolean))

(defvar *valid-ui-elements*
  '((:ui :menubar :toolbar :popup :accelerator)
    (:menubar :menuitem :separator :placeholder :menu)
    (:menu :menuitem :separator :placehoder :menu)
    (:popup :menuitem :separator :placehoder :menu)
    (:toolbar :toolitem :separator :placehoder)
    (:placeholder :menuitem :toolitem :separator :placeholder :menu)
    (:menuitem)
    (:toolitem)
    (:separator)
    (:accelerator)))

(defvar *anonymous-element-counter* 0)
(internal *anonymous-element-counter*)

(defmethod ui-manager-add-ui ((ui-manager ui-manager) (ui-spec list))
  (let ((id (%ui-manager-new-merge-id ui-manager)))
    (labels 
	((parse-ui-spec (path ui-spec element)
	   (loop
	    for definition in ui-spec
	    do (destructuring-bind (type &optional name &rest rest)
		   (mklist definition)
		 (cond
		  ((not (find type (cdr (assoc element *valid-ui-elements*))))
		   (ui-manager-remove-ui ui-manager id)
		   (error "~S not valid subelement in ~S" type element))
		  ((multiple-value-bind (action children)
		       (if (and rest (atom (first rest)) 
				(not (keywordp (first rest))))
			   (values (first rest) (rest rest))
			 (values name rest))
		     (%ui-manager-add-ui ui-manager 
		      id (or path "/") 
		      (or name (format nil "~A~D" 
			        (string-capitalize type) 
				(incf *anonymous-element-counter*)))
		      action type nil)
		     (when children
		       (parse-ui-spec (concatenate 'string path "/" name) 
				      children type)))))))))
      (parse-ui-spec nil ui-spec :ui))
    id))

(defbinding ui-manager-remove-ui () nil
  (ui-manager ui-manager)
  (merge-id unsigned-int))
  
(defbinding ui-manager-get-ui () string
  (ui-manager ui-manager))

(defbinding ui-manager-ensure-update () nil
  (ui-manager ui-manager))
