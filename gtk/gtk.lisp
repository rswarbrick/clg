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

;; $Id: gtk.lisp,v 1.14 2004-11-03 10:41:23 espen Exp $


(in-package "GTK")

;;; Gtk version

(defbinding check-version () string
  (required-major unsigned-int)
  (required-minor unsigned-int)
  (required-micro unsigned-int))

(defbinding query-version () nil
  (major unsigned-int :out)
  (minor unsigned-int :out)
  (micro unsigned-int :out))

(defun gtk-version ()
  (multiple-value-bind (major minor micro)
      (query-version)
    (if (zerop micro)
	(format nil "Gtk+ v~A.~A" major minor) 
      (format nil "Gtk+ v~A.~A.~A" major minor micro))))

(defbinding get-default-language () string)


;;; Acccel group



;;; Acccel label

(defbinding accel-label-refetch () boolean
  (accel-label accel-label))


;;; Adjustment

(defmethod shared-initialize ((adjustment adjustment) names &key value)
  (prog1
      (call-next-method)
    ;; we need to make sure that the value is set last, otherwise it
    ;; may be outside current limits
    (when value
      (setf (slot-value adjustment 'value) value))))


(defbinding adjustment-changed () nil
  (adjustment adjustment))

(defbinding adjustment-value-changed () nil
  (adjustment adjustment))

(defbinding adjustment-clamp-page () nil
  (adjustment adjustment)
  (lower single-float)
  (upper single-float))


;;; Arrow -- no functions



;;; Aspect frame


;;; Bin

(defun (setf bin-child) (child bin)
  (when-bind (current-child (bin-child bin))
    (container-remove bin current-child))
  (container-add bin child)
  child)


;;; Binding



;;; Box

(defbinding box-pack-start () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(defbinding box-pack-end () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(defun box-pack (box child &key from-end expand fill (padding 0))
  (if from-end
      (box-pack-end box child expand fill padding)
    (box-pack-start box child expand fill padding)))

(defbinding box-reorder-child () nil
  (box box)
  (child widget)
  (position int))

(defbinding box-query-child-packing () nil
  (box box)
  (child widget)
  (expand boolean :out)
  (fill boolean :out)
  (padding unsigned-int :out)
  (pack-type pack-type :out))

(defbinding box-set-child-packing () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int)
  (pack-type pack-type))



;;; Button

(defbinding button-pressed () nil
  (button button))

(defbinding button-released () nil
  (button button))

(defbinding button-clicked () nil
  (button button))

(defbinding button-enter () nil
  (button button))

(defbinding button-leave () nil
  (button button))



;;; Calendar

(defbinding calendar-select-month () int
  (calendar calendar)
  (month unsigned-int)
  (year unsigned-int))

(defbinding calendar-select-day () nil
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-mark-day () int
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-unmark-day () int
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-clear-marks () nil
  (calendar calendar))

(defbinding calendar-display-options () nil
  (calendar calendar)
  (options calendar-display-options))

(defbinding (calendar-date "gtk_calendar_get_date") () nil
  (calendar calendar)
  (year unsigned-int :out)
  (month unsigned-int :out)
  (day unsigned-int :out))

(defbinding calendar-freeze () nil
  (calendar calendar))

(defbinding calendar-thaw () nil
  (calendar calendar))



;;; Cell editable



;;; Cell renderer



;;; Cell renderer pixbuf -- no functions



;;; Cell renderer text



;;; Cell renderer toggle -- no functions



;;; Check button -- no functions



;;; Check menu item

(defbinding check-menu-item-toggled () nil
  (check-menu-item check-menu-item))



;;; Clipboard


;;; Color selection

(defbinding (color-selection-is-adjusting-p
	     "gtk_color_selection_is_adjusting") () boolean
  (colorsel color-selection))



;;; Color selection dialog -- no functions



;;; Combo

(defmethod shared-initialize ((combo combo) names &rest initargs
			      &key popdown-strings)
  (call-next-method)
  (when popdown-strings
    (combo-set-popdown-strings combo popdown-strings)))
			    
(defbinding combo-set-popdown-strings () nil
  (combo combo)
  (strings (glist string)))

(defbinding combo-disable-activate () nil
  (combo combo))



;;;; Dialog

(defmethod shared-initialize ((dialog dialog) names &rest initargs &key button)
  (call-next-method)
  (dolist (button-definition (get-all initargs :button))
    (apply #'dialog-add-button dialog (mklist button-definition))))
  

(defvar %*response-id-key* (gensym))

(defun %dialog-find-response-id-num (dialog id &optional create-p error-p)
  (or
   (cadr (assoc id (rest (type-expand-1 'response-type))))
   (let ((response-ids (object-data dialog %*response-id-key*)))
    (cond
      ((and response-ids (position id response-ids :test #'equal)))
      (create-p
       (cond
	 (response-ids
	  (vector-push-extend id response-ids)
	  (1- (length response-ids)))
	 (t
	  (setf 
	   (object-data dialog %*response-id-key*)
	   (make-array 1 :adjustable t :fill-pointer t :initial-element id))
	  0)))
      (error-p
       (error "Invalid response: ~A" id))))))

(defun %dialog-find-response-id (dialog response-id-num)
  (if (< response-id-num 0)
      (car
       (rassoc
	(list response-id-num)
	(rest (type-expand-1 'response-type)) :test #'equal))
    (aref (object-data dialog %*response-id-key*) response-id-num )))


(defmethod signal-connect ((dialog dialog) signal function &key object after)
  (let ((response-id-num (%dialog-find-response-id-num dialog signal)))
    (cond
     (response-id-num
      (call-next-method
       dialog 'response
       #'(lambda (dialog id)
	   (when (= id response-id-num)
	     (cond
	      ((eq object t) (funcall function dialog))
	      (object (funcall function object))
	      (t (funcall function)))))
       :object t :after after))
    ((call-next-method)))))


(defbinding dialog-run () nil
  (dialog dialog))

(defbinding dialog-response (dialog response-id) nil
  (dialog dialog)
  ((%dialog-find-response-id-num dialog response-id nil t) int))


(defbinding %dialog-add-button () button
  (dialog dialog)
  (text string)
  (response-id-num int))

(defun dialog-add-button (dialog label &optional (response label)
			  &key default object after)
  "Adds a button to the dialog. If no response is given, then label
   will be used."
  (let* ((id (if (functionp response)
		 label
	       response))
	 (id-num (%dialog-find-response-id-num dialog id t))
	 (button (%dialog-add-button dialog label id-num)))
    (when (functionp response)
       (signal-connect dialog id response :object object :after after))
    (when default
      (%dialog-set-default-response dialog id-num))
    button))


(defbinding %dialog-add-action-widget () button
  (dialog dialog)
  (action-widget widget)
  (response-id-num int))

(defun dialog-add-action-widget (dialog widget &optional (response widget)
				 &key default object after)
  (let* ((id (if (functionp response)
		 widget
	       response))
	 (id-num (%dialog-find-response-id-num dialog id t)))
    (%dialog-add-action-widget dialog widget id-num)
    (when (functionp response)
       (signal-connect dialog id response :object object :after after))
    (when default
      (%dialog-set-default-response dialog id-num))
    widget))


(defbinding %dialog-set-default-response () nil
  (dialog dialog)
  (response-id-num int))

(defun dialog-set-default-response (dialog response-id)
  (%dialog-set-default-response
   dialog (%dialog-find-response-id-num dialog response-id nil t)))

(defbinding dialog-set-response-sensitive (dialog response-id sensitive) nil
  (dialog dialog)
  ((%dialog-find-response-id-num dialog response-id nil t) int)
  (sensitive boolean))


;; Addition dialog functions

(defmethod container-add ((dialog dialog) (child widget) &rest args)
  (apply #'container-add (dialog-vbox dialog) child args))

(defmethod container-remove ((dialog dialog) (child widget))
  (container-remove (dialog-vbox dialog) child))

(defmethod container-children ((dialog dialog))
  (container-children (dialog-vbox dialog)))

(defmethod (setf container-children) (children (dialog dialog))
  (setf (container-children (dialog-vbox dialog)) children))



;;; Drawing area -- no functions


;;; Entry

(defbinding  entry-get-layout () pango:layout
  (entry entry))

(defbinding entry-get-layout-offsets () nil
  (entry entry)
  (x int :out)
  (y int :out))


;;; Image

(defbinding image-set-from-file () nil
  (image image)
  (filename pathname))

(defbinding image-set-from-pixmap () nil
  (image image)
  (pixmap gdk:pixmap)
  (mask gdk:bitmap))

(defbinding image-set-from-stock () nil
  (image image)
  (stock-id string)
  (icon-size icon-size))

(defun image-set-from-pixmap-data (image pixmap-data)
  (multiple-value-bind (pixmap mask) (gdk:pixmap-create pixmap-data)
    (image-set-from-pixmap image pixmap mask)))

(defun image-set-from-source (image source)
  (etypecase source
    (pathname (image-set-from-file image source))
    (string (if (stock-lookup source)
		(setf (image-stock image) source)
	      (image-set-from-file image source)))
    (vector (image-set-from-pixmap-data image source))))


(defmethod shared-initialize ((image image) names &rest initargs 
			      &key file pixmap source)
  (prog1
      (if (vectorp pixmap)
	  (progn
	    (remf initargs :pixmap)
	    (apply #'call-next-method image names initargs))
	(call-next-method))
    (cond
      (file (image-set-from-file image file))
      ((vectorp pixmap) (image-set-from-pixmap-data image pixmap))
      (source (image-set-from-source image source)))))


;;; Label

(defbinding label-get-layout-offsets () nil
  (label label)
  (x int :out)
  (y int :out))

(defbinding label-select-region () nil
  (label label)
  (start int)
  (end int))

(defbinding label-get-text () string
  (label label))

(defbinding label-get-layout () pango:layout
  (label label))

(defbinding label-get-selection-bounds () boolean
  (label label)
  (start int :out)
  (end int :out))



;;; Radio button

(defbinding %radio-button-get-group () pointer
  (radio-button radio-button))

(defbinding %radio-button-set-group () nil
  (radio-button radio-button)
  (group pointer))

(defun radio-button-add-to-group (button1 button2)
  "Add BUTTON1 to the group which BUTTON2 belongs to."
  (%radio-button-set-group button1 (%radio-button-get-group button2)))


(defmethod initialize-instance ((button radio-button)
				&rest initargs &key group-with)
  (declare (ignore initargs))
  (call-next-method)
  (when group-with
    (radio-button-add-to-group button group-with)))


;;; Option menu

(defbinding %option-menu-set-menu () nil
  (option-menu option-menu)
  (menu widget))

(defbinding %option-menu-remove-menu () nil
  (option-menu option-menu))

(defun (setf option-menu-menu) (menu option-menu)
  (if (not menu)
      (%option-menu-remove-menu option-menu)
    (%option-menu-set-menu option-menu menu))
  menu)
    


;;; Item

(defbinding item-select () nil
  (item item))

(defbinding item-deselect () nil
  (item item))

(defbinding item-toggle () nil
  (item item))



;;; Menu item

(defun (setf menu-item-label) (label menu-item)
  (make-instance 'accel-label
   :label label :xalign 0.0 :yalign 0.5 :accel-widget menu-item
   :visible t :parent menu-item)
  label)

(defun menu-item-label (menu-item)
  (with-slots (child) menu-item
    (when (typep child 'label)
      (label-label child))))

(defbinding %menu-item-set-submenu () nil
  (menu-item menu-item)
  (submenu menu))

(defbinding %menu-item-remove-submenu () nil
  (menu-item menu-item))

(defun (setf menu-item-submenu) (submenu menu-item)
  (if (not submenu)
      (%menu-item-remove-submenu menu-item)
    (%menu-item-set-submenu menu-item submenu))
  submenu)

(defbinding menu-item-set-accel-path () nil
  (menu-item menu-item)
  (accel-path string))

(defbinding menu-item-select () nil
  (menu-item menu-item))

(defbinding menu-item-deselect () nil
  (menu-item menu-item))

(defbinding menu-item-activate () nil
  (menu-item menu-item))

(defbinding menu-item-toggle-size-request () nil
  (menu-item menu-item)
  (requisition int :out))

(defbinding menu-item-toggle-size-allocate () nil
  (menu-item menu-item)
  (allocation int))



;;; Radio menu item

(defbinding %radio-menu-item-get-group () pointer
  (radio-menu-item radio-menu-item))

(defbinding %radio-menu-item-set-group () nil
  (radio-menu-item radio-menu-item)
  (group pointer))

(defun radio-menu-item-add-to-group (item1 item2)
  "Add ITEM1 to the group which ITEM2 belongs to."
  (%radio-menu-item-set-group item1 (%radio-menu-item-get-group item2)))

(defmethod initialize-instance ((item radio-menu-item)
				&rest initargs &key group-with)
  (declare (ignore initargs))
  (prog1
      (call-next-method)
    (when group-with
      (radio-menu-item-add-to-group item group-with))))
  


;;; Toggle button

(defbinding toggle-button-toggled () nil
  (toggle-button toggle-button))



;;; Window

(defbinding window-set-wmclass () nil
  (window window)
  (wmclass-name string)
  (wmclass-class string))

(defbinding window-add-accel-group () nil
  (window window)
  (accel-group accel-group))

(defbinding window-remove-accel-group () nil
  (window window)
  (accel-group accel-group))

(defbinding window-activate-focus () int
  (window window))

(defbinding window-activate-default () int
  (window window))

(defbinding window-set-default-size (window width height) int
  (window window)
  ((or width -1) int)
  ((or height -1) int))

;(defbinding window-set-geometry-hints)

(defbinding window-list-toplevels () (glist window))

(defbinding window-add-mnemonic (window key target) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (target widget))

(defbinding window-remove-mnemonic (window key target) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (target widget))

(defbinding window-mnemonic-activate (window key modifier) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifier gdk:modifier-type))

(defbinding window-present () nil
  (window window))

(defbinding window-iconify () nil
  (window window))

(defbinding window-deiconify () nil
  (window window))

(defbinding window-stick () nil
  (window window))

(defbinding window-unstick () nil
  (window window))

(defbinding window-maximize () nil
  (window window))

(defbinding window-unmaximize () nil
  (window window))

(defbinding window-begin-resize-drag () nil
  (window window)
  (edge gdk:window-edge)
  (button int)
  (root-x int) (root-y int)
  (timestamp (unsigned-int 32)))

(defbinding window-begin-move-drag () nil
  (window window)
  (edge gdk:window-edge)
  (button int)
  (root-x int) (root-y int)
  (timestamp (unsigned-int 32)))

(defbinding window-set-frame-dimensions () nil
  (window window)
  (left int) (top int) (rigth int) (bottom int))

(defbinding (window-default-icons "gtk_window_get_default_icon_list")
    () (glist gdk:pixbuf))

(defbinding %window-get-default-size () nil
  (window window)
  (width int :out)
  (height int :out))

(defun window-get-default-size (window)
  (multiple-value-bind (width height) (%window-get-default-size window)
    (values (unless (= width -1) width) (unless (= height -1) height))))

(defbinding window-get-frame-dimensions () nil
  (window window)
  (left int :out) (top int :out) (rigth int :out) (bottom int :out))

(defbinding %window-get-icon-list () (glist gdk:pixbuf)
  (window window))

(defmethod window-icon ((window window))
  (let ((icon-list (%window-get-icon-list window)))
    (if (endp (rest icon-list))
	(first icon-list)
      icon-list)))

(defbinding window-get-position () nil
  (window window)
  (root-x int :out)
  (root-y int :out))

(defbinding window-get-size () nil
  (window window)
  (width int :out)
  (height int :out))

(defbinding window-move () nil
  (window window)
  (x int)
  (y int))

(defbinding window-parse-geometry () boolean
  (window window)
  (geometry string))

(defbinding window-reshow-with-initial-size () nil
  (window window))

(defbinding window-resize () nil
  (window window)
  (width int)
  (heigth int))

(defbinding %window-set-icon-list () nil
  (window window)
  (icon-list (glist gdk:pixbuf)))

(defmethod (setf window-icon) (icon (window window))
  (%window-set-icon-list window (mklist icon)))




;;; File chooser




;;; Scrolled window

(defun (setf scrolled-window-scrollbar-policy) (policy window)
  (setf (scrolled-window-hscrollbar-policy window) policy)
  (setf (scrolled-window-vscrollbar-policy window) policy))

(defbinding scrolled-window-add-with-viewport () nil
   (scrolled-window scrolled-window)
   (child widget))







;;; Statusbar

(defbinding (statusbar-context-id "gtk_statusbar_get_context_id")
    () unsigned-int
  (statusbar statusbar)
  (context-description string))

(defbinding statusbar-push () unsigned-int
  (statusbar statusbar)
  (context-id unsigned-int)  
  (text string))

(defbinding statusbar-pop () nil
  (statusbar statusbar)
  (context-id unsigned-int))

(defbinding statusbar-remove () nil
  (statusbar statusbar)
  (context-id unsigned-int)
  (message-id unsigned-int))



;;; Fixed

(defbinding fixed-put () nil
  (fixed fixed)
  (widget widget)
  (x int) (y int))

(defbinding fixed-move () nil
  (fixed fixed)
  (widget widget)
  (x int) (y int))



;;; Notebook

(defun %notebook-position (notebook page)
  (etypecase page
    (int page)
    (keyword (case page
	       (:first 0)
	       (:last -1)
	       (t (error "Invalid position keyword: ~A" page))))
    (widget (notebook-page-num notebook page t))))

(defun %notebook-child (notebook position)
  (typecase position
     (widget position)
     (t (notebook-nth-page-child notebook position))))


(defbinding (notebook-insert "gtk_notebook_insert_page_menu")
    (notebook position child tab-label &optional menu-label) nil
  (notebook notebook)
  (child widget)
  ((if (stringp tab-label)
       (make-instance 'label :label tab-label)
     tab-label) widget)
  ((if (stringp menu-label)
       (make-instance 'label :label menu-label)
     menu-label) (or null widget))
  ((%notebook-position notebook position) int))

(defun notebook-append (notebook child tab-label &optional menu-label)
  (notebook-insert notebook :last child tab-label menu-label))

(defun notebook-prepend (notebook child tab-label &optional menu-label)
  (notebook-insert notebook :first child tab-label menu-label))
  
(defbinding notebook-remove-page (notebook page) nil
  (notebook notebook)
  ((%notebook-position notebook page) int))

(defbinding %notebook-page-num () int
  (notebook notebook)
  (child widget))

(defun notebook-page-num (notebook child &optional error-p)
  (let ((page-num (%notebook-page-num notebook child)))
    (if (= page-num -1)
	(when error-p
	  (error "~A is not a child of ~A" child notebook))
      page-num)))

(defbinding notebook-next-page () nil
  (notebook notebook))

(defbinding notebook-prev-page () nil
  (notebook notebook))

(defbinding notebook-reorder-child (notebook child position) nil
  (notebook notebook)
  (child widget)
  ((%notebook-position notebook position) int))

(defbinding notebook-popup-enable () nil
  (notebook notebook))

(defbinding notebook-popup-disable () nil
  (notebook notebook))

(defbinding (notebook-nth-page-child "gtk_notebook_get_nth_page")
    (notebook page) widget
  (notebook notebook)
  ((case page
     (:first 0)
     (:last -1)
     (t page)) int))


(defbinding %notebook-get-current-page () int
  (notebook notebook))

(defun notebook-current-page-num (notebook)
  (let ((num (%notebook-get-current-page notebook)))
    (when (>= num 0)
      num)))

(defun notebook-current-page (notebook)
  (let ((page-num (notebook-current-page-num notebook)))
    (when page-num
      (notebook-nth-page-child notebook page-num))))

(defbinding  %notebook-set-current-page () nil
  (notebook notebook)
  (page-num int))

(defun (setf notebook-current-page) (page notebook)
  (%notebook-set-current-page notebook (%notebook-position notebook page))
  page)


(defbinding (notebook-tab-label "gtk_notebook_get_tab_label")
    (notebook page) widget
  (notebook notebook)
  ((%notebook-child notebook page) widget))

(defbinding (notebook-tab-label-text "gtk_notebook_get_tab_label_text")
    (notebook page) string
  (notebook notebook)
  ((%notebook-child notebook page) widget))

(defbinding %notebook-set-tab-label () nil
  (notebook notebook)
  (page widget)
  (tab-label widget))

(defun (setf notebook-tab-label) (tab-label notebook page)
  (let ((widget (if (stringp tab-label)
		    (make-instance 'label :label tab-label)
		  tab-label)))
    (%notebook-set-tab-label notebook (%notebook-child notebook page) widget)
    widget))


(defbinding (notebook-menu-label "gtk_notebook_get_menu_label")
    (notebook page) widget
  (notebook notebook)
  ((%notebook-child notebook page) widget))

(defbinding (notebook-menu-label-text "gtk_notebook_get_menu_label_text")
    (notebook page) string
  (notebook notebook)
  ((%notebook-child notebook page) widget))

(defbinding %notebook-set-menu-label () nil
  (notebook notebook)
  (page widget)
  (menu-label widget))

(defun (setf notebook-menu-label) (menu-label notebook page)
  (let ((widget (if (stringp menu-label)
		    (make-instance 'label :label menu-label)
		  menu-label)))
    (%notebook-set-menu-label notebook (%notebook-child notebook page) widget)
    widget))


(defbinding notebook-query-tab-label-packing (notebook page) nil
  (notebook notebook)
  ((%notebook-child notebook page) widget)
  (expand boolean :out)
  (fill boolean :out)
  (pack-type pack-type :out))

(defbinding notebook-set-tab-label-packing
    (notebook page expand fill pack-type) nil
  (notebook notebook)
  ((%notebook-child notebook page) widget)
  (expand boolean)
  (fill boolean)
  (pack-type pack-type))



;;; Paned

(defbinding paned-pack1 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))

(defbinding paned-pack2 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))

(defun (setf paned-child1) (child paned)
  (paned-pack1 paned child nil t)
  child)

(defun (setf paned-child2) (child paned)
  (paned-pack2 paned child t t)
  child)

;; Defined in gtkglue.c
(defbinding paned-child1 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))

;; Defined in gtkglue.c
(defbinding paned-child2 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))



;;; Layout

(defbinding layout-put () nil
  (layout layout)
  (widget widget)
  (x int)
  (y int))

(defbinding layout-move () nil
  (layout layout)
  (widget widget)
  (x int)
  (y int))



;;; Menu shell

(defbinding menu-shell-insert (menu-shell menu-item position) nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  ((case position
     (:first 0)
     (:last -1)
     (t position)) int))

(defun menu-shell-append (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item :last))

(defun menu-shell-prepend (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item :fisrt))

(defbinding menu-shell-deactivate () nil
  (menu-shell menu-shell))

(defbinding menu-shell-select-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item))

(defbinding menu-shell-deselect () nil
  (menu-shell menu-shell))

(defbinding menu-shell-activate-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  (fore-deactivate boolean))



;;; Menu

(defun %menu-position (menu child)
  (etypecase child
    (int child)
    (keyword (case child
	       (:first 0)
	       (:last -1)
	       (t (error "Invalid position keyword: ~A" child))))
    (widget (menu-child-position menu child))))


(defbinding menu-reorder-child (menu menu-item position) nil
  (menu menu)
  (menu-item menu-item)
  ((%menu-position menu position) int))

(def-callback menu-position-callback-marshal 
    (c-call:void (x c-call:int) (y c-call:int) (push-in c-call:int) 
		 (callback-id c-call:unsigned-int))
  (invoke-callback callback-id nil x y (not (zerop push-in))))

(defbinding %menu-popup () nil
  (menu menu)
  (parent-menu-shell (or null menu-shell))
  (parent-menu-item (or null menu-item))
  (callback-func (or null pointer))
  (callback-id unsigned-int)
  (button unsigned-int)
  (activate-time (unsigned 32)))

(defun menu-popup (menu button activate-time &key callback parent-menu-shell
		   parent-menu-item)
  (if callback
      (let ((callback-id (register-callback-function callback)))
	(unwind-protect
	    (%menu-popup
	     menu parent-menu-shell parent-menu-item
	     (callback menu-position-callback-marshal)
	     callback-id button activate-time)
	  (destroy-user-data callback-id)))
    (%menu-popup
     menu parent-menu-shell parent-menu-item nil 0 button activate-time)))
 
(defbinding menu-set-accel-path () nil
  (menu menu)
  (accel-path string))

(defbinding menu-reposition () nil
  (menu menu))

(defbinding menu-popdown () nil
  (menu menu))

(defun menu-child-position (menu child)
  (position child (container-children menu)))

(defun menu-active-num (menu)
  (menu-child-position menu (menu-active menu)))

(defbinding %menu-set-active () nil
  (menu menu)
  (index unsigned-int))

(defun (setf menu-active) (menu child)
  (%menu-set-active menu (%menu-position menu child))
  child)
  


;;; Table

(defbinding table-resize () nil
  (table table)
  (rows unsigned-int)
  (columns unsigned-int))

(defbinding table-attach (table child left right top bottom
			  &key (x-options '(:expand :fill))
			       (y-options '(:expand :fill))
			       (x-padding 0) (y-padding 0)) nil
  (table table)
  (child widget)
  (left unsigned-int)
  (right unsigned-int)
  (top unsigned-int)
  (bottom unsigned-int)
  (x-options attach-options)
  (y-options attach-options)
  (x-padding unsigned-int)
  (y-padding unsigned-int))


(defbinding %table-set-row-spacing () nil
  (table table)
  (row unsigned-int)
  (spacing unsigned-int))

(defbinding %table-set-row-spacings () nil
  (table table)
  (spacing unsigned-int))

(defun (setf table-row-spacing) (spacing table &optional row)
  (if row
      (%table-set-row-spacing table row spacing)
    (%table-set-row-spacings table spacing))
  spacing)

(defbinding %table-get-row-spacing () unsigned-int
  (table table)
  (row unsigned-int))

(defbinding %table-get-default-row-spacing () unsigned-int
  (table table))

(defun table-row-spacing (table &optional row)
  (if row
      (%table-get-row-spacing table row)
    (%table-get-default-row-spacing table)))


(defbinding %table-set-col-spacing () nil
  (table table)
  (col unsigned-int)
  (spacing unsigned-int))

(defbinding %table-set-col-spacings () nil
  (table table)
  (spacing unsigned-int))

(defun (setf table-col-spacing) (spacing table &optional col)
  (if col
      (%table-set-col-spacing table col spacing)
    (%table-set-col-spacings table spacing))
  spacing)

(defbinding %table-get-col-spacing () unsigned-int
  (table table)
  (col unsigned-int))

(defbinding %table-get-default-col-spacing () unsigned-int
  (table table))

(defun table-col-spacing (table &optional col)
  (if col
      (%table-get-col-spacing table col)
    (%table-get-default-col-spacing table)))
  


;;; Toolbar

(defbinding %toolbar-insert-element () widget
  (toolbar toolbar)
  (type toolbar-child-type)
  (widget (or null widget))
  (text string)
  (tooltip-text string)
  (tooltip-private-text string)
  (icon (or null widget))
  (nil null)
  (nil null)
  (position int))

(defbinding %toolbar-insert-stock () widget
  (toolbar toolbar)
  (stock-id string)
  (tooltip-text string)
  (tooltip-private-text string)
  (nil null)
  (nil null)
  (position int))

(defun toolbar-insert (toolbar position element
		       &key tooltip-text tooltip-private-text
		       type icon group callback object)
  (let* ((numpos (case position
		   (:first -1)
		   (:last 0)
		   (t position)))
	 (widget
	  (cond
	   ((or
	     (eq type :space)
	     (and (not type) (eq element :space)))
	    (%toolbar-insert-element
	     toolbar :space nil nil
	     tooltip-text tooltip-private-text nil numpos))
	   ((or
	     (eq type :widget)
	     (and (not type) (typep element 'widget)))
	    (%toolbar-insert-element
	     toolbar :widget element nil
	     tooltip-text tooltip-private-text nil numpos))
	   ((or
	     (eq type :stock)
	     (and
	      (not type)
	      (typep element 'string)
	      (stock-lookup element)))
	    (%toolbar-insert-stock
	     toolbar element tooltip-text tooltip-private-text numpos))
	   ((typep element 'string)
	    (%toolbar-insert-element
	     toolbar (or type :button) (when (eq type :radio-button) group)
	     element tooltip-text tooltip-private-text 
	     (etypecase icon
	       (null nil)
	       (widget icon)
	       ((or pathname string vector)
		(make-instance 'image 
		 :source icon ; :icon-size (toolbar-icon-size toolbar)
		 )))
	     numpos))
	   ((error "Invalid element type: ~A" element)))))
    (when callback
      (signal-connect widget 'clicked callback :object object))
    widget))

(defun toolbar-append (toolbar element &key tooltip-text tooltip-private-text
		       type icon group callback object)
  (toolbar-insert
   toolbar :first element :type type :icon icon :group group
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
   :callback callback :object object))

(defun toolbar-prepend (toolbar element &key tooltip-text tooltip-private-text
			type icon group callback object)
  (toolbar-insert
   toolbar :last element :type type :icon icon :group group
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
   :callback callback :object object))


(defun toolbar-insert-space (toolbar position)
  (toolbar-insert toolbar position :space))

(defun toolbar-append-space (toolbar)
  (toolbar-append toolbar :space))

(defun toolbar-prepend-space (toolbar)
  (toolbar-prepend toolbar :space))


(defun toolbar-enable-tooltips (toolbar)
  (setf (toolbar-tooltips-p toolbar) t))

(defun toolbar-disable-tooltips (toolbar)
  (setf (toolbar-tooltips-p toolbar) nil))


(defbinding toolbar-remove-space () nil
  (toolbar toolbar)
  (position int))

(defbinding toolbar-unset-icon-size () nil
  (toolbar toolbar))

(defbinding toolbar-unset-style () nil
  (toolbar toolbar))


;;; Editable

(defbinding editable-select-region (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(defbinding editable-get-selection-bounds (editable) nil
  (editable editable)
  (start int :out)
  (end int :out))

(defbinding editable-insert-text
    (editable text &optional (position 0)) nil
  (editable editable)
  (text string)
  ((length text) int)
  ((or position -1) int :in-out))

(defun editable-append-text (editable text)
  (editable-insert-text editable text nil))

(defun editable-prepend-text (editable text)
  (editable-insert-text editable text 0))

(defbinding editable-delete-text (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(defbinding (editable-text "gtk_editable_get_chars")
    (editable &optional (start 0) end) string
  (editable editable)
  (start int)
  ((or end -1) int))

(defun (setf editable-text) (text editable)
  (if text
      (editable-delete-text
       editable
       (editable-insert-text editable text))
    (editable-delete-text editable))
  text)

(defbinding editable-cut-clipboard () nil
  (editable editable))

(defbinding editable-copy-clipboard () nil
  (editable editable))

(defbinding editable-paste-clipboard () nil
  (editable editable))

(defbinding editable-delete-selection () nil
  (editable editable))



;;; Spin button

(defun spin-button-value-as-int (spin-button)
  (round (spin-button-value spin-button)))

(defbinding spin-button-spin () nil
  (spin-button spin-button)
  (direction spin-type)
  (increment single-float))

(defbinding spin-button-update () nil
  (spin-button spin-button))



; ;;; Ruler

(defbinding ruler-set-range () nil
  (ruler ruler)
  (lower single-float)
  (upper single-float)
  (position single-float)
  (max-size single-float))

(defbinding ruler-draw-ticks () nil
  (ruler ruler))

(defbinding ruler-draw-pos () nil
  (ruler ruler))



;;; Range

(defun range-lower (range)
  (adjustment-lower (range-adjustment range)))

(defun range-upper (range)
  (adjustment-upper (range-adjustment range)))

(defun (setf range-lower) (value range)
  (setf (adjustment-lower (range-adjustment range)) value))

(defun (setf range-upper) (value range)
  (setf (adjustment-upper (range-adjustment range)) value))

(defun range-page-increment (range)
  (adjustment-page-increment (range-adjustment range)))

(defun range-step-increment (range)
  (adjustment-step-increment (range-adjustment range)))

(defun (setf range-page-increment) (value range)
  (setf (adjustment-page-increment (range-adjustment range)) value))

(defun (setf range-step-increment) (value range)
  (setf (adjustment-step-increment (range-adjustment range)) value))

(defbinding range-set-range () nil
  (range range)
  (lower double-float)
  (upper double-float))

(defbinding range-set-increments () nil
  (range range)
  (step double-float)
  (page double-float))


;;; Scale

; (defbinding scale-draw-value () nil
;   (scale scale))



;;; Progress bar

(defbinding progress-bar-pulse () nil
  (progress-bar progress-bar))



;;; Stock items

(defbinding stock-lookup () boolean
  (stock-id string)
  (stock-item stock-item :out))
  



;;; Tooltips

(defbinding tooltips-enable () nil
  (tooltips tooltips))

(defbinding tooltips-disable () nil
  (tooltips tooltips))

(defun (setf tooltips-enabled-p) (enable tooltips)
  (if enable
      (tooltips-enable tooltips)
    (tooltips-disable tooltips)))

(defbinding tooltips-set-tip () nil
  (tooltips tooltips)
  (widget widget)
  (tip-text string)
  (tip-private string))

(defbinding tooltips-force-window () nil
  (tooltips tooltips))



;;; Rc

(defbinding rc-add-default-file (filename) nil
  ((namestring (truename filename)) string))

(defbinding rc-parse (filename) nil
  ((namestring (truename filename)) string))

(defbinding rc-parse-string () nil
  (rc-string string))

(defbinding rc-reparse-all () nil)

(defbinding rc-get-style () style
  (widget widget))



;;; Accelerator Groups
#|
(defbinding accel-group-get-default () accel-group)

(deftype-method alien-ref accel-group (type-spec)
  (declare (ignore type-spec))
  '%accel-group-ref)

(deftype-method alien-unref accel-group (type-spec)
  (declare (ignore type-spec))
  '%accel-group-unref)

(defbinding %accel-group-ref () accel-group
  (accel-group (or accel-group pointer)))

(defbinding %accel-group-unref () nil
  (accel-group (or accel-group pointer)))

(defbinding accel-group-activate (accel-group key modifiers) boolean
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding accel-groups-activate (object key modifiers) boolean
  (object object)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding accel-group-attach () nil
  (accel-group accel-group)
  (object object))

(defbinding accel-group-detach () nil
  (accel-group accel-group)
  (object object))

(defbinding accel-group-lock () nil
  (accel-group accel-group))

(defbinding accel-group-unlock () nil
  (accel-group accel-group))


;;; Accelerator Groups Entries

(defbinding accel-group-get-entry (accel-group key modifiers) accel-entry
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding accel-group-lock-entry (accel-group key modifiers) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding accel-group-unlock-entry (accel-group key modifiers) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding accel-group-add
    (accel-group key modifiers flags object signal) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags)
  (object object)
  ((name-to-string signal) string))

(defbinding accel-group-add (accel-group key modifiers object) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (object object))


;;; Accelerator Signals

(defbinding accel-group-handle-add
    (object signal-id accel-group key modifiers flags) nil
  (object object)
  (signal-id unsigned-int)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags))

(defbinding accel-group-handle-remove
    (object accel-group key modifiers) nil
  (object object)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))
|#
