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

;; $Id: gtk.lisp,v 1.10 2002-03-24 21:54:06 espen Exp $


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

(defbinding adjustment-changed () nil
  (adjustment adjustment))

(defbinding adjustment-value-changed () nil
  (adjustment adjustment))

(defbinding adjustment-clamp-page () nil
  (adjustment adjustment)
  (lower single-float)
  (upper single-float))



;;; Alignment -- no functions
;;; Arrow -- no functions



;;; Aspect frame


;;; Bin

(defun (setf bin-child) (child bin)
  (when-bind (current-child (bin-child bin))
    (container-remove bin current-child))
  (container-add bin child)
  child)



;;; Button box -- no functions


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

(defun box-pack (box child &key (pack :start) (expand t) (fill t) (padding 0))
  (if (eq pack :start)
      (box-pack-start box child expand fill padding)
    (box-pack-end box child expand fill padding)))

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

(defbinding combo-set-value-in-list () nil
  (combo combo)
  (value boolean)
  (ok-if-empty boolean))

(defbinding combo-set-item-string () nil
  (combo combo)
  (item item)
  (item-value string))

(defbinding combo-set-popdown-strings () nil
  (combo combo)
  (strings (glist string)))

(defbinding combo-disable-activate () nil
  (combo combo))



;;;; Dialog

(defmethod shared-initialize ((dialog dialog) names &rest initargs)
  (call-next-method)
  (dolist (button-definition (get-all initargs :button))
    (apply #'dialog-add-button dialog button-definition)))
  

(defvar %*response-id-key* (gensym))

(defun %dialog-find-response-id-num (dialog response-id &optional create-p error-p)
  (or
   (cadr (assoc response-id (rest (type-expand-1 'response-type))))
   (let* ((response-ids (object-data dialog %*response-id-key*))
	  (response-id-num (position response-id response-ids)))
    (cond
     (response-id-num)
     (create-p
      (cond
       (response-ids
	(setf (cdr (last response-ids)) (list response-id))
	(1- (length response-ids)))
       (t
	(setf (object-data dialog %*response-id-key*) (list response-id))
	0)))
     (error-p
      (error "Invalid response: ~A" response-id))))))

(defun %dialog-find-response-id (dialog response-id-num)
  (if (< response-id-num 0)
      (car
       (rassoc
	(list response-id-num)
	(rest (type-expand-1 'response-type)) :test #'equalp))
    (nth response-id-num (object-data dialog %*response-id-key*))))


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
    (t
     (call-next-method)))))


(defbinding dialog-run () nil
  (dialog dialog))

(defbinding dialog-response (dialog response-id) nil
  (dialog dialog)
  ((%dialog-find-response-id-num dialog response-id nil t) int))


(defbinding %dialog-add-button () button
  (dialog dialog)
  (text string)
  (response-id-num int))

(defun dialog-add-button (dialog label &optional response-id default-p)
  (let* ((response-id-num
	  (if response-id
	      (%dialog-find-response-id-num dialog response-id t)
	    (length (object-data dialog %*response-id-key*))))
	 (button (%dialog-add-button dialog label response-id-num)))
    (unless response-id
      (%dialog-find-response-id-num dialog button t))
    (when default-p
      (%dialog-set-default-response dialog response-id-num))
    button))


(defbinding %dialog-add-action-widget () button
  (dialog dialog)
  (action-widget widget)
  (response-id-num int))

(defun dialog-add-action-widget (dialog widget &optional (response-id widget)
				 default-p)
  (let ((response-id-num (%dialog-find-response-id-num dialog response-id t)))
    (%dialog-add-action-widget dialog widget response-id-num)
    (when default-p
      (%dialog-set-default-response dialog response-id-num))
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
  (apply #'container-add (slot-value dialog 'main-area) child args))

(defmethod container-remove ((dialog dialog) (child widget))
  (container-remove (slot-value dialog 'main-area) child))

(defmethod container-children ((dialog dialog))
  (container-children (dialog-main-area dialog)))

(defmethod (setf container-children) (children (dialog dialog))
  (setf (container-children (dialog-main-area dialog)) children))



;;; Drawing area -- no functions



;;; Toggle button

(defbinding toggle-button-toggled () nil
  (toggle-button toggle-button))


;;; Label

(defbinding label-select-region () nil
  (label label)
  (start int)
  (end int))




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
    (radio-button-add-to-group item group-with)))


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

(defbinding menu-item-select () nil
  (menu-item menu-item))

(defbinding menu-item-deselect () nil
  (menu-item menu-item))

(defbinding menu-item-activate () nil
  (menu-item menu-item))



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
  (call-next-method)
  (when group-with
    (radio-menu-item-add-to-group item group-with)))
  


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




;;; File selection

(defbinding file-selection-complete () nil
  (file-selection file-selection)
  (pattern string))



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
  (x (signed 16))
  (y (signed 16)))

(defbinding fixed-move () nil
  (fixed fixed)
  (widget widget)
  (x (signed 16))
  (y (signed 16)))



;;; Notebook

(defbinding (notebook-insert-page "gtk_notebook_insert_page_menu")
    (notebook position child tab-label &optional menu-label) nil
  (notebook notebook)
  (child widget)
  ((if (stringp tab-label)
       (label-new tab-label)
     tab-label) widget)
  ((if (stringp menu-label)
       (label-new menu-label)
     menu-label) (or null widget))
  (position int))

(defun notebook-append-page (notebook child tab-label &optional menu-label)
  (notebook-insert-page notebook -1 child tab-label menu-label))

(defun notebook-prepend-page (notebook child tab-label &optional menu-label)
  (notebook-insert-page notebook 0 child tab-label menu-label))
  
(defbinding notebook-remove-page () nil
  (notebook notebook)
  (page-num int))

; (defun notebook-current-page-num (notebook)
;   (let ((page-num (notebook-current-page notebook)))
;     (if (= page-num -1)
; 	nil
;       page-num)))

(defbinding (notebook-nth-page-child "gtk_notebook_get_nth_page") () widget
  (notebook notebook)
  (page-num int))

(defun notebook-page-child (notebook)
  (notebook-nth-page-child notebook (notebook-page notebook)))

(defbinding %notebook-page-num () int
  (notebook notebook)
  (child widget))

(defun notebook-child-num (notebook child)
  (let ((page-num (%notebook-page-num notebook child)))
    (if (= page-num -1)
	nil
      page-num)))

(defbinding notebook-next-page () nil
  (notebook notebook))

(defbinding notebook-prev-page () nil
  (notebook notebook))

(defbinding notebook-popup-enable () nil
  (notebook notebook))

(defbinding notebook-popup-disable () nil
  (notebook notebook))

; (defbinding (notebook-tab-label "gtk_notebook_get_tab_label")
;     (notebook ref) widget
;   (notebook notebook)
;   ((if (typep ref 'widget)
;        ref
;      (notebook-nth-page-child notebook ref))
;    widget))

; (defbinding %notebook-set-tab-label () nil
;   (notebook notebook)
;   (reference widget)
;   (tab-label widget))

; (defun (setf notebook-tab-label) (tab-label notebook reference)
;   (let ((tab-label-widget (if (stringp tab-label)
; 			      (label-new tab-label)
; 			    tab-label)))
;     (%notebook-set-tab-label
;      notebook
;      (if (typep reference 'widget)
; 	 reference
;        (notebook-nth-page-child notebook reference))
;      tab-label-widget)
;     tab-label-widget))
   
; (defbinding (notebook-menu-label "gtk_notebook_get_menu_label")
;     (notebook ref) widget
;   (notebook notebook)
;   ((if (typep ref 'widget)
;        ref
;      (notebook-nth-page-child notebook ref))
;    widget))

; (defbinding %notebook-set-menu-label () nil
;   (notebook notebook)
;   (reference widget)
;   (menu-label widget))

; (defun (setf notebook-menu-label) (menu-label notebook reference)
;   (let ((menu-label-widget (if (stringp menu-label)
; 			      (label-new menu-label)
; 			    menu-label)))
;     (%notebook-set-menu-label
;      notebook
;      (if (typep reference 'widget)
; 	 reference
;        (notebook-nth-page-child notebook reference))
;      menu-label-widget)
;     menu-label-widget))

(defbinding notebook-query-tab-label-packing (notebook ref) nil
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget)
  (expand boolean :out)
  (fill boolean :out)
  (pack-type pack-type :out))

(defbinding
    notebook-set-tab-label-packing (notebook ref expand fill pack-type) nil
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget)
  (expand boolean)
  (fill boolean)
  (pack-type pack-type))

(defbinding notebook-reorder-child () nil
  (notebook notebook)
  (child widget)
  (position int))



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

;; gtkglue.c
(defbinding paned-child1 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))

;; gtkglue.c
(defbinding paned-child2 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))

(defun (setf paned-child1) (child paned)
  (paned-pack1 paned child nil t))

(defun (setf paned-child2) (child paned)
  (paned-pack2 paned child t t))



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

(defbinding layout-set-size () nil
  (layout layout)
  (width int)
  (height int))

(defbinding layout-get-size () nil
  (layout layout)
  (width int :out)
  (height int :out))



;;; Menu shell

(defbinding menu-shell-insert () nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  (position int))

(defun menu-shell-append (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item -1))

(defun menu-shell-prepend (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item 0))

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



; ;;; Menu bar

; (defbinding menu-bar-insert () nil
;   (menu-bar menu-bar)
;   (menu menu)
;   (position int))

; (defun menu-bar-append (menu-bar menu)
;   (menu-bar-insert menu-bar menu -1))

; (defun menu-bar-prepend (menu-bar menu)
;   (menu-bar-insert menu-bar menu 0))



; ;;; Menu

;(defun menu-popup ...)

(defbinding menu-reposition () nil
  (menu menu))

(defbinding menu-popdown () nil
  (menu menu))

(defbinding %menu-set-active () nil
  (menu menu)
  (index unsigned-int))

(defun (setf menu-active) (menu index)
  (%menu-set-active menu index))
  
(defbinding menu-reorder-child () nil
  (menu menu)
  (menu-item menu-item)
  (position int))


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

;; gtkglue.c
(defbinding toolbar-num-children () int
  (toolbar toolbar))

(defun %toolbar-position-num (toolbar position)
  (case position
    (:prepend 0)
    (:append (toolbar-num-children toolbar))
    (t
     (assert (and (>= position 0) (< position (toolbar-num-children toolbar))))
     position)))

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

(defun toolbar-insert-element (toolbar position
			       &key tooltip-text tooltip-private-text
			       type widget icon text callback)
  (let* ((icon-widget (typecase icon
		       ((or null widget) icon)
		       (t (pixmap-new icon))))
	 (toolbar-child
	  (%toolbar-insert-element
	   toolbar (or type (and widget :widget) :button)
	   widget text tooltip-text tooltip-private-text icon-widget
	   (%toolbar-position-num toolbar position))))
    (when callback
      (signal-connect toolbar-child 'clicked callback))
    toolbar-child))

(defun toolbar-append-element (toolbar &key tooltip-text tooltip-private-text
			       type widget icon text callback)
  (toolbar-insert-element
   toolbar :append :type type :widget widget :icon icon :text text
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
   :callback callback))

(defun toolbar-prepend-element (toolbar &key tooltip-text tooltip-private-text
			        type widget icon text callback)
  (toolbar-insert-element
   toolbar :prepend :type type :widget widget :icon icon :text text
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
   :callback callback))

(defun toolbar-insert-space (toolbar position)
  (toolbar-insert-element toolbar position :type :space))

(defun toolbar-append-space (toolbar)
  (toolbar-insert-space toolbar :append))

(defun toolbar-prepend-space (toolbar)
  (toolbar-insert-space toolbar :prepend))

(defun toolbar-insert-widget (toolbar widget position &key tooltip-text
			      tooltip-private-text callback)
  (toolbar-insert-element
   toolbar position :widget widget :tooltip-text tooltip-text
   :tooltip-private-text tooltip-private-text :callback callback))
 
(defun toolbar-append-widget (toolbar widget &key tooltip-text
			      tooltip-private-text callback)
  (toolbar-insert-widget
   toolbar widget :append :tooltip-text tooltip-text
   :tooltip-private-text tooltip-private-text :callback callback))

(defun toolbar-prepend-widget (toolbar widget &key tooltip-text
			       tooltip-private-text callback)
  (toolbar-insert-widget
   toolbar widget :prepend :tooltip-text tooltip-text
   :tooltip-private-text tooltip-private-text :callback callback))

(defun toolbar-insert-item (toolbar text icon position &key tooltip-text
			    tooltip-private-text callback)
  (toolbar-insert-element
   toolbar position :text text :icon icon :callback callback
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

(defun toolbar-append-item (toolbar text icon &key tooltip-text
			    tooltip-private-text callback)
  (toolbar-insert-item
   toolbar text icon :append :callback callback
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

		       
(defun toolbar-prepend-item (toolbar text icon &key tooltip-text
			     tooltip-private-text callback)
  (toolbar-insert-item
   toolbar text icon :prepend :callback callback
   :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

(defun toolbar-enable-tooltips (toolbar)
  (setf (toolbar-tooltips-p toolbar) t))

(defun toolbar-disable-tooltips (toolbar)
  (setf (toolbar-tooltips-p toolbar) nil))








;;; Editable
#|
(defbinding editable-select-region (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

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

; (defbinding editable-claim-selection () nil
;   (editable editable)
;   (claim boolean)
;   (time unsigned-int))

(defbinding editable-delete-selection () nil
  (editable editable))

; (defbinding editable-changed () nil
;   (editable editable))
|#


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
#|
(defbinding range-draw-background () nil
  (range range))

(defbinding range-clear-background () nil
  (range range))

(defbinding range-draw-trough () nil
  (range range))

(defbinding range-draw-slider () nil
  (range range))

(defbinding range-draw-step-forw () nil
  (range range))

(defbinding range-slider-update () nil
  (range range))

(defbinding range-trough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(defbinding range-default-hslider-update () nil
  (range range))

(defbinding range-default-vslider-update () nil
  (range range))

(defbinding range-default-htrough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(defbinding range-default-vtrough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(defbinding range-default-hmotion () int
  (range range)
  (x-delta int)
  (y-delta int))

(defbinding range-default-vmotion () int
  (range range)
  (x-delta int)
  (y-delta int))
|#


;;; Scale

; (defbinding scale-draw-value () nil
;   (scale scale))



;;; Progress bar

(defbinding progress-bar-pulse () nil
  (progress-bar progress-bar))





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
