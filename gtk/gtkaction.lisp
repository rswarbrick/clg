;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gtkaction.lisp,v 1.1 2004-12-04 18:03:21 espen Exp $


(in-package "GTK")

;;; Action

(defmethod initialize-instance ((action action) &key accelerator)
  (call-next-method)
  (setf (object-data action 'accelerator) accelerator))

(defmethod action-accelerator ((action action))
  (object-data action 'accelerator))

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
  (call-next-method)
  (flet ((add-action (action)
	   (action-group-add-action action-group action)))
    (loop 
     as (initarg value . rest) = initargs then rest
     do (case initarg
	  (:action (add-action value))
	  (:actions (mapc #'add-action value)))
     while rest)))

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
  (multiple-value-bind (accelerator accelerator-p) 
      (object-data action 'accelerator)
    (if accelerator-p
	(%action-group-add-action-with-accel action-group action accelerator)
      (%action-group-add-action action-group action))))

(defbinding action-group-remove-action () nil
  (action-group action-group)
  (action action))


;;; Radio Action

(defmethod initialize-instance ((action radio-action) &key group value)
  (call-next-method)
  (setf (slot-value action '%value) (system:sap-int (proxy-location action)))
  (setf (object-data action 'radio-action-value) value)
  (when group
    (radio-action-add-to-group action group)))

(defmethod radio-value-action ((action radio-action))
  (object-data action 'radio-action-value))

(defbinding %radio-action-get-group () pointer
  (radio-action radio-action))

(defbinding %radio-action-set-group () nil
  (radio-button radio-button)
  (group pointer))

(defun radio-action-add-to-group (action1 action2)
  "Add ACTION1 to the group which ACTION2 belongs to."
  (%radio-action-set-group action1 (%radio-action-get-group action2)))

(defbinding (radio-action-get-current "gtk_radio_action_get_current_value") 
    () radio-action
  (radio-action radio-action))

(defun radio-action-get-current-value (action)
  (radio-value-action (radio-action-get-current action)))



;;; Toggle Action

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
  (gerror pointer :out))

(defmethod ui-manager-add-ui ((ui-manager ui-manager) (ui-spec string))
  (let ((id (%ui-manager-add-ui-from-string ui-manager ui-spec)))
    (when (zerop id)
      (error "We need to handle GError in som way"))
    id))

(defbinding %ui-manager-add-ui-from-file () int
  (ui-manager ui-manager)
  (filename pathname)
  (gerror pointer :out))

(defmethod ui-manager-add-ui ((ui-manager ui-manager) (path pathname))
  (let ((id (%ui-manager-add-ui-from-file ui-manager path)))
    (when (zerop id)
      (error "We need to handle GError in som way"))
    id))

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
		     (%ui-manager-add-ui ui-manager id (or path "/") name action type nil)
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
