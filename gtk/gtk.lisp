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

;; $Id: gtk.lisp,v 1.1 2000-08-14 16:44:51 espen Exp $


(in-package "GTK")

;;; Gtk version

(define-foreign check-version () string
  (required-major unsigned-int)
  (required-minor unsigned-int)
  (required-micro unsigned-int))

(define-foreign query-version () nil
  (major unsigned-int :out)
  (minor unsigned-int :out)
  (micro unsigned-int :out))

(defun gtk-version ()
  (multiple-value-bind (major minor micro)
      (query-version)
    (if (zerop micro)
	(format nil "Gtk+ v~A.~A" major minor) 
      (format nil "Gtk+ v~A.~A.~A" major minor micro))))

(export '*clg-version*)



;;; InitializationInitialization, exit, mainloop and miscellaneous routines


(define-foreign grab-add () nil
  (widget widget))

(define-foreign grab-get-current () widget)

(define-foreign grab-remove () nil
  (widget widget))

(define-foreign ("gtk_timeout_add_full" timeout-add)
    (interval function) unsigned-int
  (interval (unsigned 32))
  (0 unsigned-long)
  (*callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-marshal* pointer))

(define-foreign timeout-remove () nil
  (timeout-handler-id unsigned-int))
  
(define-foreign ("gtk_idle_add_full" idle-add)
    (function &optional (priority 200)) unsigned-int
  (priority int)
  (0 unsigned-long)
  (*callback-marshal* pointer)
  ((register-callback-function function) unsigned-long)
  (*destroy-marshal* pointer))

(define-foreign idle-remove () nil
  (idle-handler-id unsigned-int))

(define-foreign get-current-event () gdk:event)

(define-foreign get-event-widget () widget
  (event gdk:event))


;;; should be moved to gobject

; (define-foreign ("gtk_object_set_data_full" object-set-data)
;                  (object key data &optional destroy-function) nil
;   (object object)		 
;   ((string key) string)
;   ((register-user-data data destroy-function) unsigned-long)
;   (*destroy-marshal* pointer))

; (defun (setf object-data) (data object key)
;   (object-set-data object key data)
;   data)

; (define-foreign %object-get-data (object key) unsigned-long
;   (object object)		 
;   ((string key) string))

; (defun object-data (object key)
;   (find-user-data (%object-get-data object key)))

; (define-foreign object-remove-data (object key) nil
;   (object object)
;   ((string key) string))

; (defun object-user-data (object)
;   (object-data object :user-data))

; (defun (setf object-user-data) (data object)
;   (setf (object-data object :user-data) data))


;;; Label

(define-foreign label-new () label
  (text string))

(define-foreign label-parse-uline () unsigned-int
  (label label)
  (string string))



;;; Acccel label

(define-foreign accel-label-new () accel-label
  (text string))

(define-foreign accel-label-refetch () boolean
  (accel-label accel-label))



;;; Tips query

(define-foreign tips-query-new () tips-query)

(define-foreign tips-query-start-query () nil
  (tips-query tips-query))

(define-foreign tips-query-stop-query () nil
  (tips-query tips-query))



;;; Arrow

(define-foreign arrow-new () arrow
  (arrow-type arrow-type)
  (shadow-type shadow-type))



;;; Pixmap

; (defun %pixmap-create (source)
;   (cond
;    ((not source) nil)
;    ((typep source gdk:pixmap) source)
;    ((and (consp source) (typep (first source) gdk:pixmap)) (values-list source))
;    (t (gdk:pixmap-create source))))

(define-foreign %pixmap-new () pixmap
  (pixmap gdk:pixmap)
  (mask (or null gdk:bitmap)))

(defun pixmap-new (source)
  (multiple-value-bind (pixmap mask)
      (%pixmap-create source)
    (%pixmap-new pixmap mask)))

(define-foreign %pixmap-set () nil
  (pixmap pixmap)
  (gdk:pixmap gdk:pixmap)
  (mask (or null gdk:bitmap)))

(defun (setf pixmap-pixmap) (source pixmap)
  (multiple-value-bind (gdk:pixmap mask)
      (%pixmap-create source)
    (%pixmap-set pixmap gdk:pixmap mask)
    (values gdk:pixmap mask)))

(define-foreign ("gtk_pixmap_get" pixmap-pixmap) () nil
  (pixmap pixmap)
  (val gdk:pixmap :out)
  (mask gdk:bitmap :out))



;;; Bin

(defun bin-child (bin)
  (first (container-children bin)))

(defun (setf bin-child) (child bin)
  (let ((old-child (bin-child bin)))
    (when old-child
      (container-remove bin old-child)))
  (container-add bin child)
  child)


;;; Alignment

(define-foreign alignment-new () alignment
  (xalign single-float)
  (ylign single-float)
  (xscale single-float)
  (yscale single-float))



;;; Frame

(define-foreign frame-new (&optional label) frame
  (label string))



;;; Aspect frame

(define-foreign aspect-frame-new () alignment
  (xalign single-float)
  (ylign single-float)
  (ratio single-float)
  (obey-child boolean))



;;; Button

(define-foreign %button-new () button)

(define-foreign %button-new-with-label () button
  (label string))

(defun button-new (&optional label)
  (if label
      (%button-new-with-label label)
    (%button-new)))

(define-foreign button-pressed () nil
  (button button))

(define-foreign button-released () nil
  (button button))

(define-foreign button-clicked () nil
  (button button))

(define-foreign button-enter () nil
  (button button))

(define-foreign button-leave () nil
  (button button))



;;; Toggle button

(define-foreign %toggle-button-new () toggle-button)

(define-foreign %toggle-button-new-with-label () toggle-button
  (label string))

(defun toggle-button-new (&optional label)
  (if label
      (%toggle-button-new-with-label label)
    (%toggle-button-new)))

(define-foreign toggle-button-toggled () nil
  (toggle-button toggle-button))



;;; Check button

(define-foreign %check-button-new () check-button)

(define-foreign %check-button-new-with-label () check-button
  (label string))

(defun check-button-new (&optional label)
  (if label
      (%check-button-new-with-label label)
    (%check-button-new)))



;;; Radio button

(define-foreign %radio-button-new () radio-button
  (group (or null radio-button-group)))

(define-foreign %radio-button-new-with-label-from-widget () radio-button
  (widget (or null widget))
  (label string))

(define-foreign %radio-button-new-from-widget () radio-button
  (widget (or null widget)))

(define-foreign %radio-button-new-with-label () radio-button
  (group (or null radio-button-group))
  (label string))

(defun radio-button-new (group &key label from-widget)
  (cond
   ((and from-widget label)
    (%radio-button-new-with-label-from-widget group label))
   (from-widget
    (%radio-button-new-from-widget group))
   (label
    (%radio-button-new-with-label group label))
   (t
    (%radio-button-new group))))
    
(define-foreign radio-button-group () radio-button-group
  (radio-button radio-button))



;;; Option menu

; (define-foreign option-menu-new () option-menu)

; (define-foreign %option-menu-set-menu () nil
;   (option-menu option-menu)
;   (menu widget))

; (define-foreign %option-menu-remove-menu () nil
;   (option-menu option-menu))

; (defun (setf option-menu-menu) (menu option-menu)
;   (if (not menu)
;       (%option-menu-remove-menu option-menu)
;     (%option-menu-set-menu option-menu menu))
;   menu)
    


;;; Item

(define-foreign item-select () nil
  (item item))

(define-foreign item-deselect () nil
  (item item))

(define-foreign item-toggle () nil
  (item item))



;;; Menu item

; (define-foreign %menu-item-new () menu-item)

; (define-foreign %menu-item-new-with-label () menu-item
;   (label string))

; (defun menu-item-new (&optional label)
;   (if label
;       (%menu-item-new-with-label label)
;     (%menu-item-new)))

; (defun (setf menu-item-label) (label menu-item)
;   (let ((accel-label (accel-label-new label)))
;     (setf (misc-xalign accel-label) 0.0)
;     (setf (misc-yalign accel-label) 0.5)

;     (container-add menu-item accel-label)
;     (setf (accel-label-accel-widget accel-label) menu-item)
;     (widget-show accel-label))
;   label)

; (define-foreign %menu-item-set-submenu () nil
;   (menu-item menu-item)
;   (submenu menu))

; (define-foreign %menu-item-remove-submenu () nil
;   (menu-item menu-item))

; (defun (setf menu-item-submenu) (submenu menu-item)
;   (if (not submenu)
;       (%menu-item-remove-submenu menu-item)
;     (%menu-item-set-submenu menu-item submenu))
;   submenu)

; (define-foreign %menu-item-configure () nil
;   (menu-item menu-item)
;   (show-toggle-indicator boolean)
;   (show-submenu-indicator boolean))

; (defun (setf menu-item-toggle-indicator-p) (show menu-item)
;   (%menu-item-configure
;    menu-item
;    show
;    (menu-item-submenu-indicator-p menu-item))
;   show)

; (defun (setf menu-item-submenu-indicator-p) (show menu-item)
;   (%menu-item-configure
;    menu-item
;    (menu-item-toggle-indicator-p menu-item)
;    show))

; (define-foreign menu-item-select () nil
;   (menu-item menu-item))

; (define-foreign menu-item-deselect () nil
;   (menu-item menu-item))

; (define-foreign menu-item-activate () nil
;   (menu-item menu-item))

; (define-foreign menu-item-right-justify () nil
;   (menu-item menu-item))



; ;;; Check menu item

; (define-foreign %check-menu-item-new
;     () check-menu-item)

; (define-foreign %check-menu-item-new-with-label () check-menu-item
;   (label string))

; (defun check-menu-item-new (&optional label)
;   (if label
;       (%check-menu-item-new-with-label label)
;     (%check-menu-item-new)))

; (define-foreign check-menu-item-toggled () nil
;   (check-menu-item check-menu-item))



; ;;; Radio menu item

; (define-foreign %radio-menu-item-new
;                  () radio-menu-item
;   (group (or null radio-menu-item-group)))

; (define-foreign %radio-menu-item-new-with-label () radio-menu-item
;   (group (or null radio-menu-item-group))
;   (label string))

; (defun radio-menu-item-new (group &optional label)
;   (if label
;       (%radio-menu-item-new-with-label group label)
;     (%radio-menu-item-new group)))



; ;;; Tearoff menu item

; (define-foreign tearoff-menu-item-new () tearoff-menu-item)



;;; List item

(define-foreign %list-item-new () list-item)

(define-foreign %list-item-new-with-label () list-item
  (label string))

(defun list-item-new (&optional label)
  (if label
      (%list-item-new-with-label label)
    (%list-item-new)))
      
(define-foreign list-item-select () nil
  (list-item list-item))

(define-foreign list-item-deselect () nil
  (list-item list-item))



;;; Tree item

(define-foreign %tree-item-new () tree-item)

(define-foreign %tree-item-new-with-label () tree-item
  (label string))

(defun tree-item-new (&optional label)
  (if label
      (%tree-item-new-with-label label)
    (%tree-item-new)))

(define-foreign %tree-item-set-subtree () nil
  (tree-item tree-item)
  (subtree tree))

(define-foreign %tree-item-remove-subtree () nil
  (tree-item tree-item))

(defun (setf tree-item-subtree) (subtree tree-item)
  (if subtree
      (%tree-item-set-subtree tree-item subtree)
    (%tree-item-remove-subtree tree-item))
  subtree)

(define-foreign tree-item-select () nil
  (tree-item tree-item))

(define-foreign tree-item-deselect () nil
  (tree-item tree-item))

(define-foreign tree-item-expand () nil
  (tree-item tree-item))

(define-foreign tree-item-collapse () nil
  (tree-item tree-item))



;;; Window

(define-foreign window-new () window
  (type window-type))

(define-foreign %window-set-wmclass () nil
  (window window)
  (wmclass-name string)
  (wmclass-class string))

(defun (setf window-wmclass) (wmclass window)
  (%window-set-wmclass window (svref wmclass 0) (svref wmclass 1))
  (values (svref wmclass 0) (svref wmclass 1)))

;; cl-gtk.c
(define-foreign window-wmclass () nil
  (window window)
  (wmclass-name string :out)
  (wmclass-class string :out))

(define-foreign window-add-accel-group () nil
  (window window)
  (accel-group accel-group))

(define-foreign window-remove-accel-group () nil
  (window window)
  (accel-group accel-group))

(define-foreign window-activate-focus () int
  (window window))

(define-foreign window-activate-default () int
  (window window))

(define-foreign window-set-transient-for () nil
  (window window)
  (parent window))

;(define-foreign window-set-geometry-hints)



;;; Color selection dialog

; (define-foreign color-selection-dialog-new () color-selection-dialog
;   (title string))



;;; Dialog

; (define-foreign dialog-new () dialog)



;;; Input dialog

; (define-foreign input-dialog-new () dialog)



;;; File selection

; (define-foreign file-selection-new () file-selection
;   (title string))

; (define-foreign file-selection-complete () nil
;   (file-selection file-selection)
;   (pattern string))

; (define-foreign file-selection-show-fileop-buttons () nil
;   (file-selection file-selection))

; (define-foreign file-selection-hide-fileop-buttons () nil
;   (file-selection file-selection))



; ;;; Handle box

; (define-foreign handle-box-new () handle-box)



; ;;; Scrolled window

(define-foreign scrolled-window-new
    (&optional hadjustment vadjustment) scrolled-window
  (hadjustment (or null adjustment))
  (vadjustment (or null adjustment)))

(defun (setf scrolled-window-scrollbar-policy) (policy window)
  (setf (scrolled-window-hscrollbar-policy window) policy)
  (setf (scrolled-window-vscrollbar-policy window) policy))

(define-foreign scrolled-window-add-with-viewport () nil
   (scrolled-window scrolled-window)
   (child widget))



; ;;; Viewport

; (define-foreign viewport-new () viewport
;   (hadjustment adjustment)
;   (vadjustment adjustment))
  


;;; Box

(define-foreign box-pack-start () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(define-foreign box-pack-end () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(defun box-pack (box child &key (pack :start) (expand t) (fill t) (padding 0))
  (if (eq pack :start)
      (box-pack-start box child expand fill padding)
    (box-pack-end box child expand fill padding)))

(define-foreign box-reorder-child () nil
  (box box)
  (child widget)
  (position int))

(define-foreign box-query-child-packing () nil
  (box box)
  (child widget :out)
  (expand boolean :out)
  (fill boolean :out)
  (padding unsigned-int :out)
  (pack-type pack-type :out))

(define-foreign box-set-child-packing () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int)
  (pack-type pack-type))



;;; Button box

(define-foreign ("gtk_button_box_get_child_size_default"
		  button-box-default-child-size) () nil
  (min-width int :out)
  (min-height int :out))

(define-foreign ("gtk_button_box_get_child_ipadding_default"
		  button-box-default-child-ipadding) () nil
  (ipad-x int :out)
  (ipad-y int :out))

(define-foreign %button-box-set-child-size-default () nil
  (min-width int)
  (min-height int))

(defun (setf button-box-default-child-size) (size)
  (%button-box-set-child-size-default (svref size 0) (svref size 1))
  (values (svref size 0) (svref size 1)))

(define-foreign %button-box-set-child-ipadding-default () nil
  (ipad-x int)
  (ipad-y int))

(defun (setf button-box-default-child-ipadding) (ipad)
  (%button-box-set-child-ipadding-default (svref ipad 0) (svref ipad 1))
  (values (svref ipad 0) (svref ipad 1)))

(define-foreign
    ("gtk_button_box_get_child_size" button-box-child-size) () nil
  (button-box button-box)
  (min-width int :out)
  (min-height int :out))

(define-foreign
    ("gtk_button_box_get_child_ipadding" button-box-child-ipadding) () nil
  (button-box button-box)  
  (ipad-x int :out)
  (ipad-y int :out))

(define-foreign %button-box-set-child-size () nil
  (button-box button-box)
  (min-width int)
  (min-height int))

(defun (setf button-box-child-size) (size button-box)
  (%button-box-set-child-size button-box (svref size 0) (svref size 1))
  (values (svref size 0) (svref size 1)))

(define-foreign %button-box-set-child-ipadding () nil
  (button-box button-box)
  (ipad-x int)
  (ipad-y int))

(defun (setf button-box-child-ipadding) (ipad button-box)
  (%button-box-set-child-ipadding  button-box (svref ipad 0) (svref ipad 1))
  (values (svref ipad 0) (svref ipad 1)))



;;; HButton box

;(define-foreign hbutton-box-new () hbutton-box)

(define-foreign ("gtk_hbutton_box_get_spacing_default"
		  hbutton-box-default-spacing) () int)

(define-foreign ("gtk_hbutton_box_set_spacing_default"
		  (setf hbutton-box-default-spacing)) () nil
  (spacing int))
  
(define-foreign ("gtk_hbutton_box_get_layout_default"
		  hbutton-box-default-layout) () button-box-style)

(define-foreign ("gtk_hbutton_box_set_layout_default"
		  (setf hbutton-box-default-layout)) () nil
  (layout button-box-style))



;;; VButton Box

;(define-foreign vbutton-box-new () vbutton-box)

(define-foreign ("gtk_vbutton_box_get_spacing_default"
		  vbutton-box-default-spacing) () int)

(define-foreign ("gtk_vbutton_box_set_spacing_default"
		  (setf vbutton-box-default-spacing)) () nil
  (spacing int))
  
(define-foreign ("gtk_vbutton_box_get_layout_default"
		  vbutton-box-default-layout) () button-box-style)

(define-foreign ("gtk_vbutton_box_set_layout_default"
		  (setf vbutton-box-default-layout)) () nil
  (layout button-box-style))



;;; VBox

(define-foreign vbox-new () vbox
  (homogeneous boolean)
  (spacing int))



;;; Color selection

; (define-foreign color-selection-new () color-selection)

; ;; cl-gtk.c
; (define-foreign %color-selection-set-color-by-values () nil
;   (colorsel color-selection)
;   (red double-float)
;   (green double-float)
;   (blue double-float)
;   (opacity double-float))

; (defun (setf color-selection-color) (color colorsel)
;   (%color-selection-set-color-by-values
;    colorsel
;    (svref color 0) (svref color 1) (svref color 2)
;    (if (> (length color) 3)
;        (svref color 3)
;      1.0))
;   color)

; ;; cl-gtk.c
; (define-foreign %color-selection-get-color-as-values () nil
;   (colorsel color-selection)
;   (red double-float :out)
;   (green double-float :out)
;   (blue double-float :out)
;   (opacity double-float :out))

; (defun color-selection-color (colorsel)
;   (multiple-value-bind (red green blue opacity)
;       (%color-selection-get-color-as-values colorsel)
;     (if (color-selection-use-opacity-p colorsel)
; 	(vector red green blue opacity)
;       (vector red green blue))))




; ;;; Gamma curve

; (define-foreign gamma-curve-new () gamma-curve)



;;; HBox

(define-foreign hbox-new () hbox
  (homogeneous boolean)
  (spacing int))



;;; Combo

; (define-foreign combo-new () combo)

; (define-foreign combo-set-value-in-list () nil
;   (combo combo)
;   (val boolean)
;   (ok-if-empty boolean))

; (define-foreign ("gtk_combo_set_item_string" (setf combo-item-string)) () nil
;   (combo combo)
;   (item item)
;   (item-value string))

; (define-foreign ("gtk_combo_set_popdown_strings"
; 		  (setf combo-popdown-strings)) () nil
;   (combo combo)
;   (strings (double-list string)))
  
; (define-foreign combo-disable-activate () nil
;   (combo combo))



; ;;; Statusbar

; (define-foreign statusbar-new () statusbar)

; (define-foreign
;     ("gtk_statusbar_get_context_id" statusbar-context-id) () unsigned-int
;   (statusbar statusbar)
;   (context-description string))

; (define-foreign statusbar-push () unsigned-int
;   (statusbar statusbar)
;   (context-id unsigned-int)  
;   (text string))

; (define-foreign statusbar-pop () nil
;   (statusbar statusbar)
;   (context-id unsigned-int))

; (define-foreign statusbar-remove () nil
;   (statusbar statusbar)
;   (context-id unsigned-int)
;   (message-id unsigned-int))



;;; CList

; (define-foreign %clist-new () clist
;   (columns int))

; (define-foreign %clist-new-with-titles () clist
;   (columns int)
;   (titles pointer))

; (defun clist-new (columns)
;   (if (atom columns)
;       (%clist-new columns)
;     (with-array (titles :initial-contents columns :free-contents t)
;       (%clist-new-with-titles (length columns) titles))))

; (define-foreign ("gtk_clist_set_button_actions"
; 		  (setf clist-button-actions)) () nil
;   (clist clist)
;   (button unsigned-int)
;   (button-actions button-actions))

; (define-foreign clist-freeze () nil
;   (clist clist))

; (define-foreign clist-thaw () nil
;   (clist clist))

; (define-foreign clist-column-titles-show () nil
;   (clist clist))

; (define-foreign clist-column-titles-hide () nil
;   (clist clist))

; (defun (setf clist-titles-visible-p) (visible clist)
;   (if visible
;       (clist-column-titles-hide clist)
;     (clist-column-titles-show clist)))

; (define-foreign clist-column-title-active () nil
;   (clist clist)
;   (column int))

; (define-foreign clist-column-title-passive () nil
;   (clist clist)
;   (column int))

; (define-foreign clist-column-titles-active () nil
;   (clist clist))

; (define-foreign clist-column-titles-passive () nil
;   (clist clist))

; (define-foreign ("gtk_clist_set_column_title"
; 		  (setf clist-column-title)) () nil
;   (clist clist)
;   (column int)
;   (title string))

; (define-foreign ("gtk_clist_get_column_title" clist-column-title) () string
;   (clist clist)
;   (column int))

; (define-foreign ("gtk_clist_set_column_widget"
; 		  (setf clist-column-widget)) () nil
;   (clist clist)
;   (column int)
;   (widget widget))

; (define-foreign ("gtk_clist_get_column_widget" clist-column-widget) () widget
;   (clist clist)
;   (column int))

; (define-foreign ("gtk_clist_set_column_justification"
; 		  (setf clist-column-justification)) () nil
;   (clist clist)
;   (column int)
;   (justification justification))

; (define-foreign clist-column-justification (clist column) justification
;   (clist clist)
;   ((progn
;      (assert (and (>= column 0) (< column (clist-n-columns clist))))
;      column)
;    int))

; (define-foreign ("gtk_clist_set_column_visibility"
; 		  (setf clist-column-visible-p)) () nil
;   (clist clist)
;   (column int)
;   (visible boolean))

; ;; cl-gtk.c
; (define-foreign clist-column-visible-p (clist column) boolean
;   (clist clist)
;   ((progn
;      (assert (and (>= column 0) (< column (clist-n-columns clist))))
;      column)
;    int))

; (define-foreign ("gtk_clist_set_column_resizeable"
; 		  (setf clist-column-resizeable-p)) () nil
;   (clist clist)
;   (column int)
;   (resizeable boolean))

; ;; cl-gtk.c
; (define-foreign clist-column-resizeable-p (clist column) boolean
;   (clist clist)
;   ((progn
;      (assert (and (>= column 0) (< column (clist-n-columns clist))))
;      column)
;    int))

; (define-foreign ("gtk_clist_set_column_auto_resize"
; 		  (setf clist-column-auto-resize-p)) () nil
;   (clist clist)
;   (column int)
;   (auto-resize boolean))

; ;; cl-gtk.c
; (define-foreign clist-column-auto-resize-p (clist column) boolean
;   (clist clist)
;   ((progn
;      (assert (and (>= column 0) (< column (clist-n-columns clist))))
;      column)
;    int))

; (define-foreign clist-columns-autosize () int
;   (clist clist))

; (define-foreign clist-optimal-column-width () int
;   (clist clist)
;   (column int))

; (define-foreign ("gtk_clist_set_column_width"
; 		  (setf clist-column-width)) () nil
;   (clist clist)
;   (column int)
;   (width int))

; ;; cl-gtk.c
; (define-foreign clist-column-width (clist column) int
;   (clist clist)
;   ((progn
;      (assert (and (>= column 0) (< column (clist-n-columns clist))))
;      column)
;    int))

; (define-foreign ("gtk_clist_set_column_min_width"
; 		  (setf clist-column-min-width)) (min-width clist column) nil
;   (clist clist)
;   (column int)
;   ((or min-width -1) int))

; (define-foreign ("gtk_clist_set_column_max_width"
; 		  (setf clist-column-max-width)) (max-width clist column) nil
;   (clist clist)
;   (column int)
;   ((or max-width -1) int))

; (define-foreign clist-moveto () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (row-align single-float)
;   (columnt-align single-float))

; (define-foreign
;     ("gtk_clist_row_is_visible" clist-row-visiblie-p) () visibility
;   (clist clist)
;   (row int))

; (define-foreign ("gtk_clist_get_cell_type" clist-cell-type) () cell-type
;   (clist clist)
;   (row int)
;   (column int))

; (define-foreign ("gtk_clist_set_text" (setf clist-cell-text)) () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (text string))

; (define-foreign %clist-set-pixmap () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (gdk:pixmap gdk:pixmap)
;   (mask (or null gdk:bitmap)))

; (defun (setf clist-cell-pixmap) (pixmap clist row column)
;   (multiple-value-bind (gdk:pixmap mask)
;       (%pixmap-create pixmap)
;     (%clist-set-pixmap clist row column gdk:pixmap mask)
;     (values pixmap mask)))

; (define-foreign %clist-set-pixtext () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (text string)
;   (spacing uint8)
;   (pixmap gdk:pixmap)
;   (mask (or null gdk:bitmap)))

; (defun clist-set-cell-pixtext (clist row column text spacing pixmap)
;   (multiple-value-bind (gdk:pixmap mask)
;       (%pixmap-create pixmap)
;     (%clist-set-pixtext clist row column text spacing gdk:pixmap mask)))

; (define-foreign %clist-get-text () boolean
;   (clist clist)
;   (row int)
;   (column int)
;   (text string :out))

; (defun clist-cell-text (clist row column)
;   (multiple-value-bind (success text)
;       (%clist-get-text clist row column)
;     (unless success
;       (error
;        "Cell at row ~D column ~D in ~A is not of type :text"
;        row column clist))
;     text))

; (define-foreign ("gtk_clist_get_pixmap" %clist-get-pixmap) () boolean
;   (clist clist)
;   (row int)
;   (column int)
;   (pixmap gdk:pixmap :out)
;   (mask gdk:bitmap :out))

; (defun clist-cell-pixmap (clist row column)
;   (multiple-value-bind (success pixmap mask)
;       (%clist-get-pixmap clist row column)
;     (unless success
;       (error
;        "Cell at row ~D column ~D in ~A is not of type :pixmap"
;        row column clist))
;     (values pixmap mask)))

; (define-foreign %clist-get-pixtext () boolean
;   (clist clist)
;   (row int)
;   (column int)
;   (text string :out)
;   (spacing unsigned-int :out)
;   (pixmap gdk:pixmap :out)
;   (mask gdk:bitmap :out))

; (defun clist-cell-pixtext (clist row column)
;   (multiple-value-bind (success text spacing pixmap mask)
;       (%clist-get-pixtext clist row column)
;     (unless success
;       (error
;        "Cell at row ~D column ~D in ~A is not of type :pixtext"
;        row column clist))
;     (values text spacing pixmap mask)))

; (define-foreign %clist-set-foreground () nil
;   (clist clist)
;   (row int)
;   (color gdk:color))

; (defun (setf clist-foreground) (color clist row)
;   (gdk:with-colors ((color color))
;     (%clist-set-foreground clist row color))
;   color)

; (define-foreign %clist-set-background () nil
;   (clist clist)
;   (row int)
;   (color gdk:color))

; (defun (setf clist-background) (color clist row)
;   (gdk:with-colors ((color color))
;     (%clist-set-background clist row color))
;   color)

; (define-foreign ("gtk_clist_set_cell_style"
; 		  (setf clist-cell-style)) () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (style style))
  
; (define-foreign ("gtk_clist_get_cell_style" clist-cell-style) () style
;   (clist clist)
;   (row int)
;   (column int))

; (define-foreign ("gtk_clist_set_row_style"
; 		  (setf clist-row-style)) () nil
;   (clist clist)
;   (row int)
;   (style style))

; (define-foreign ("gtk_clist_get_row_style" clist-row-style) () style
;   (clist clist)
;   (row int))

; (define-foreign clist-set-shift () nil
;   (clist clist)
;   (row int)
;   (column int)
;   (vertical int)
;   (horizontal int))

; (define-foreign ("gtk_clist_set_selectable"
; 		  (setf clist-selectable-p)) () nil
;   (clist clist)
;   (row int)
;   (selectable boolean))

; (define-foreign ("gtk_clist_get_selectable" clist-selectable-p) () boolean
;   (clist clist)
;   (row int))

; (define-foreign ("gtk_clist_insert" %clist-insert) () int
;   (clist clist)
;   (row int)
;   (text pointer))

; (defun clist-insert (clist row text)
;   (unless (= (length text) (clist-n-columns clist))
;     (error "Wrong number of elements in ~A" text))
;   (with-array (data :initial-contents text :free-contents t)
;     (%clist-insert clist row data)))

; (defun clist-prepend (clist text)
;   (clist-insert clist 0 text))

; (defun clist-append (clist text)
;   (clist-insert clist -1 text))

; (define-foreign clist-remove () nil
;   (clist clist)
;   (row int))

; (define-foreign ("gtk_clist_set_row_data_full" clist-set-row-data)
;                  (clist row data &optional destroy-function) nil
;   (clist clist)
;   (row int)
;   ((register-user-data data destroy-function) unsigned-long)
;   (*destroy-marshal* pointer))

; (defun (setf clist-row-data) (data clist row)
;   (clist-set-row-data clist row data)
;   data)

; (define-foreign %clist-get-row-data () unsigned-long
;   (clist clist)
;   (row int))

; (defun clist-row-data (clist row)
;   (find-user-data (%clist-get-row-data clist row)))

; (define-foreign %clist-find-row-from-data () int
;   (clist clist)
;   (id unsigned-long))

; (define-foreign clist-select-row (clist row &optional (column -1)) nil
;   (clist clist)
;   (row int)
;   (column int))

; (define-foreign clist-unselect-row (clist row &optional (column -1)) nil
;   (clist clist)
;   (row int)
;   (column int))

; (define-foreign clist-undo-selection () nil
;   (clist clist))

; (define-foreign clist-clear () nil
;   (clist clist))

; (define-foreign ("gtk_clist_get_selection_info" clist-selection-info) () int
;   (clist clist)
;   (x int)
;   (y int)
;   (row int :out)
;   (column int :out))
  
; (define-foreign clist-select-all () nil
;   (clist clist))

; (define-foreign clist-unselect-all () nil
;   (clist clist))

; (define-foreign clist-swap-rows () nil
;   (clist clist)
;   (row1 int)
;   (row2 int))

; (define-foreign ("gtk_clist_row_move" clist-move-row) () nil
;   (clist clist)
;   (source-row int)
;   (dest-row int))

; ;(define-foreign clist-set-compare-func ...)

; (define-foreign clist-sort () nil
;   (clist clist))

; (define-foreign ("gtk_clist_set_auto_sort"
; 		  (setf clist-auto-sort-p)) () nil
;   (clist clist)
;   (auto-sort boolean))

; ;; cl-gtk.c
; (define-foreign clist-auto-sort-p () boolean
;   (clist clist))

; (defun clist-focus-row (clist)
;   (let ((row (%clist-focus-row clist)))
;     (when (>= row 0)
;       row)))

; ;; cl-gtk.c
; (define-foreign clist-selection () (list int)
;   (clist clist))



; ;;; CTree

; (define-foreign %ctree-new () ctree
;   (columns int)
;   (tree-column int))

; (define-foreign %ctree-new-with-titles () ctree
;   (columns int)
;   (tree-column int)
;   (titles pointer))

; (defun ctree-new (columns &optional (tree-column 0))
;   (if (atom columns)
;       (%ctree-new columns tree-column)
;     (with-array (titles :initial-contents columns :free-contents t)
;       (%ctree-new-with-titles (length columns) tree-column titles))))

; (define-foreign %ctree-insert-node () ctree-node
;   (ctree ctree)
;   (parent (or null ctree-node))
;   (sibling (or null ctree-node)) 
;   (text pointer)
;   (spacing uint8)
;   (pixmap-closed (or null gdk:pixmap))
;   (bitmap-closed (or null gdk:bitmap))
;   (pixmap-opened (or null gdk:pixmap))
;   (bitmap-opened (or null gdk:bitmap))
;   (leaf boolean)
;   (expaned boolean))

; (defun ctree-insert-node (ctree parent sibling text spacing
; 			  &key pixmap closed opened leaf expanded)
;   (multiple-value-bind (pixmap-closed mask-closed)
;       (%pixmap-create (or closed pixmap))
;     (multiple-value-bind (pixmap-opened mask-opened)
; 	(%pixmap-create (or opened (and (not leaf) pixmap)))
;       (with-array (data :clear t :initial-contents text :free-contents t)
;         (%ctree-insert-node
; 	 ctree parent sibling data spacing pixmap-closed mask-closed
; 	 pixmap-opened mask-opened leaf expanded)))))

; (define-foreign ctree-remove-node () nil
;   (ctree ctree)
;   (node ctree-node))

; (defun ctree-insert-from-list (ctree parent tree function)
;   (clist-freeze ctree)
;   (labels ((insert-node (node parent)
; 	     (let ((ctree-node
; 		    (ctree-insert-node
; 		     ctree parent nil
; 		     (make-list (clist-n-columns ctree) :initial-element "")
; 		     0 :leaf (not (rest node)))))
; 	       (funcall function ctree-node (car node))
; 	       (dolist (child (rest node))
; 		 (insert-node child ctree-node)))))
;     (if parent
; 	(insert-node tree parent)
;       (dolist (node tree)
; 	(insert-node node nil))))
;   (clist-thaw ctree))

; (defun ctree-map-to-list (ctree node function)
;   (labels ((map-children (child)
; 	     (when child
; 	       (let ((sibling (ctree-node-sibling child)))
; 		 (cons
; 		  (ctree-map-to-list ctree child function)
; 		  (map-children sibling))))))
;     (if node
; 	(cons
; 	 (funcall function node)
; 	 (map-children (ctree-node-child node)))
;       (map-children (ctree-nth-node ctree 0)))))


; (defun %ctree-apply-recursive (ctree node pre function depth)
;   (when (and pre node (or (not depth) (<= (ctree-node-level node) depth)))
;     (funcall function node))
  
;   (let ((first-child (if node
; 			 (ctree-node-child node)
; 		       (ctree-nth-node ctree 0))))
;     (when (and
; 	   first-child
; 	   (or (not depth) (<= (ctree-node-level first-child) depth)))
;       (labels ((foreach-child (child)
; 	         (when child
; 		   (let ((sibling (ctree-node-sibling child)))
; 		     (%ctree-apply-recursive ctree child pre function depth)
; 		     (foreach-child sibling)))))
; 	(foreach-child first-child))))
  
;   (when (and
; 	 (not pre) node (or (not depth) (<= (ctree-node-level node) depth)))
;     (funcall function node)))

; (defun ctree-apply-post-recursive (ctree node function &optional depth)
;   (%ctree-apply-recursive ctree node nil function depth))

; (defun ctree-apply-pre-recursive (ctree node function &optional depth)
;   (%ctree-apply-recursive ctree node t function depth))

; (define-foreign ("gtk_ctree_is_viewable" ctree-node-viewable-p) () boolean
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-last () ctree-node
;   (ctree ctree))

; (define-foreign ("gtk_ctree_node_nth" ctree-nth-node) () ctree-node
;   (ctree ctree)
;   (row int))

; (define-foreign ctree-find () boolean
;   (ctree ctree)
;   (node ctree-node)
;   (child ctree-node))

; (define-foreign ("gtk_ctree_is_ancestor" ctree-ancestor-p) () boolean
;   (ctree ctree)
;   (node ctree-node)
;   (child ctree-node))

; (define-foreign %ctree-find-by-row-data () int
;   (clist clist)
;   (node ctree-node)
;   (id unsigned-long))

; (define-foreign ("gtk_ctree_is_hot_spot" ctree-hot-spot-p) () boolean
;   (ctree ctree)
;   (x int)
;   (y int))

; (define-foreign ctree-move () nil
;   (ctree ctree)
;   (node ctree-node)
;   (new-parent ctree-node)
;   (new-sibling ctree-node))

; (define-foreign ctree-expand () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-expand-recursive () nil
;   (ctree ctree)
;   (node (or null ctree-node)))

; (define-foreign ctree-expand-to-depth () nil
;   (ctree ctree)
;   (node (or null ctree-node))
;   (depth int))

; (define-foreign ctree-collapse () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-collapse-recursive () nil
;   (ctree ctree)
;   (node (or null ctree-node)))

; (define-foreign ctree-collapse-to-depth () nil
;   (ctree ctree)
;   (node (or null ctree-node))
;   (depth int))

; (define-foreign ctree-toggle-expansion () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-toggle-expansion-recursive () nil
;   (ctree ctree)
;   (node (or null ctree-node)))

; (define-foreign ctree-select () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-unselect () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign %ctree-real-select-recursive () nil
;   (ctree ctree)
;   (node (or null ctree-node))
;   (state boolean))

; (defun ctree-select-recursive (ctree node)
;   (%ctree-real-select-recursive ctree node t))

; (defun ctree-unselect-recursive (ctree node)
;   (%ctree-real-select-recursive ctree node nil))

; (define-foreign ("gtk_ctree_node_set_text" (setf ctree-cell-text)) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (text string))

; (define-foreign %ctree-node-set-pixmap () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (gdk:pixmap gdk:pixmap)
;   (mask (or null gdk:bitmap)))

; (defun (setf ctree-cell-pixmap) (source ctree node column)
;   (multiple-value-bind (pixmap mask)
;       (%pixmap-create source)
;     (%ctree-node-set-pixmap ctree node column pixmap mask)
;     (values pixmap mask)))

; (define-foreign %ctree-node-set-pixtext () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (text string)
;   (spacing uint8)
;   (pixmap gdk:pixmap)
;   (mask (or null gdk:bitmap)))

; (defun ctree-set-cell-pixtext (ctree node column text spacing source)
;   (multiple-value-bind (pixmap mask)
;       (%pixmap-create source)
;     (%ctree-node-set-pixtext ctree node column text spacing pixmap mask)))

; (define-foreign %ctree-set-node-info () ctree-node
;   (ctree ctree)
;   (node (or null ctree-node))
;   (text string)
;   (spacing uint8)
;   (pixmap-closed (or null gdk:pixmap))
;   (bitmap-closed (or null gdk:bitmap))
;   (pixmap-opened (or null gdk:pixmap))
;   (bitmap-opened (or null gdk:bitmap))
;   (leaf boolean)
;   (expaned boolean))

; (defun ctree-set-node-info (ctree node text spacing
; 			    &key pixmap closed opened leaf expanded)
;   (multiple-value-bind (pixmap-closed mask-closed)
;       (%pixmap-create (or closed pixmap))
;     (multiple-value-bind (pixmap-opened mask-opened)
; 	(%pixmap-create (or opened (and (not leaf) pixmap)))
;       (%ctree-set-node-info
;        ctree node text spacing pixmap-closed mask-closed
;        pixmap-opened mask-opened leaf expanded))))

; (define-foreign ("gtk_ctree_node_set_shift" ctree-set-shift) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (vertical int)
;   (horizontal int))

; (define-foreign ("gtk_ctree_node_set_selectable"
; 		  (setf ctree-selectable-p)) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (selectable boolean))

; (define-foreign ("gtk_ctree_node_get_selectable"
; 		  ctree-selectable-p) () boolean
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ("gtk_ctree_node_get_cell_type" ctree-cell-type) () cell-type
;   (ctree ctree)
;   (node ctree-node)
;   (column int))

; (define-foreign %ctree-node-get-text () boolean
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (text string :out))

; (defun ctree-cell-text (ctree node column)
;   (multiple-value-bind (success text)
;       (%ctree-node-get-text ctree node column)
;     (unless success
;       (error
;        "Cell in node ~A, column ~D in ~A is not of type :text"
;        node column ctree))
;     text))

; (define-foreign %ctree-node-get-pixmap () boolean
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (pixmap gdk:pixmap :out)
;   (mask gdk:bitmap :out))

; (defun ctree-cell-pixmap (ctree node column)
;   (multiple-value-bind (success pixmap mask)
;       (%ctree-node-get-pixmap ctree node column)
;     (unless success
;       (error
;        "Cell in node ~A column ~D in ~A is not of type :text"
;        node column ctree))
;     (values pixmap mask)))

; (define-foreign %ctree-node-get-pixtext () boolean
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (text string :out)
;   (spacing unsigned-int :out)
;   (pixmap gdk:pixmap :out)
;   (mask gdk:bitmap :out))

; (defun ctree-cell-pixtext (ctree node column)
;   (multiple-value-bind (success text spacing pixmap mask)
;       (%ctree-node-get-pixtext ctree node column)
;     (unless success
;       (error
;        "Cell in node ~A column ~D in ~A is not of type :text"
;        node column ctree))
;     (values text spacing pixmap mask)))

; (define-foreign ("gtk_ctree_get_node_info" ctree-node-info) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (text string :out)
;   (spacing unsigned-int :out)
;   (pixmap-closed gdk:pixmap :out)
;   (mask-closed gdk:bitmap :out)
;   (pixmap-opened gdk:pixmap :out)
;   (mask-opened gdk:bitmap :out)
;   (leaf boolean :out)
;   (expanded boolean :out))

; (define-foreign ("gtk_ctree_node_set_row_style"
; 		  (setf ctree-row-style)) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (style (or null style)))

; (define-foreign ("gtk_ctree_node_get_row_style" ctree-row-style) () style
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ("gtk_ctree_node_set_cell_style"
; 		  (setf ctree-cell-style)) () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (style (or null style)))

; (define-foreign ("gtk_ctree_node_get_cell_style"
; 		  ctree-cell-style) () style
;   (ctree ctree)
;   (node ctree-node)
;   (column int))

; (define-foreign %ctree-node-set-foreground () nil
;   (ctree ctree)
;   (node ctree-node)
;   (color gdk:color))

; (defun (setf ctree-node-foreground) (color clist row)
;   (gdk:with-colors ((color color))
;     (%ctree-node-set-foreground clist row color))
;   color)

; (define-foreign %ctree-node-set-background () nil
;   (ctree ctree)
;   (node ctree-node)
;   (color gdk:color))

; (defun (setf ctree-node-background) (color clist row)
;   (gdk:with-colors ((color color))
;     (%ctree-node-set-background clist row color))
;   color)

; (define-foreign ("gtk_ctree_node_set_row_data_full" ctree-set-node-data)
;                  (ctree node data &optional destroy-function) nil
;   (ctree ctree)
;   (node ctree-node)
;   ((register-user-data data destroy-function) unsigned-long)
;   (*destroy-marshal* pointer))

; (defun (setf ctree-node-data) (data ctree node)
;   (ctree-set-node-data ctree node data)
;   data)

; (define-foreign %ctree-node-get-row-data () unsigned-long
;   (ctree ctree)
;   (node ctree-node))

; (defun ctree-node-data (ctree node)
;   (find-user-data (%ctree-node-get-row-data ctree node)))

; (define-foreign ctree-node-moveto () nil
;   (ctree ctree)
;   (node ctree-node)
;   (column int)
;   (row-aling single-float)
;   (column-aling single-float))

; (define-foreign ("gtk_ctree_node_is_visible"
; 		  ctree-node-visibility) () visibility
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-sort-node () nil
;   (ctree ctree)
;   (node ctree-node))

; (define-foreign ctree-sort-recursive (ctree &optional node) nil
;   (ctree ctree)
;   (node (or null ctree-node)))

; ;; cl-gtk.c
; (define-foreign ("gtk_clist_selection" ctree-selection) () (list ctree-node)
;   (ctree ctree))

; ;; cl-gtk.c
; (define-foreign ctree-node-leaf-p () boolean
;   (node ctree-node))

; ;; cl-gtk.c
; (define-foreign ctree-node-parent () ctree-node
;   (node ctree-node))

; ;; cl-gtk.c
; (define-foreign ctree-node-child () ctree-node
;   (node ctree-node))

; ;; cl-gtk.c
; (define-foreign ctree-node-sibling () ctree-node
;   (node ctree-node))

; ;; cl-gtk.c
; (define-foreign ctree-node-level () int
;   (node ctree-node))


;;; Fixed

; (define-foreign fixed-new () fixed)

; (define-foreign fixed-put () nil
;   (fixed fixed)
;   (widget widget)
;   (x int) (y int16))

; (define-foreign fixed-move () nil
;   (fixed fixed)
;   (widget widget)
;   (x int16) (y int16))



; ;;; Notebook

; (define-foreign notebook-new () notebook)

; (define-foreign ("gtk_notebook_insert_page_menu" notebook-insert-page)
;     (notebook position child tab-label &optional menu-label) nil
;   (notebook notebook)
;   (child widget)
;   ((if (stringp tab-label)
;        (label-new tab-label)
;      tab-label) widget)
;   ((if (stringp menu-label)
;        (label-new menu-label)
;      menu-label) (or null widget))
;   (position int))

; (defun notebook-append-page (notebook child tab-label &optional menu-label)
;   (notebook-insert-page notebook -1 child tab-label menu-label))

; (defun notebook-prepend-page (notebook child tab-label &optional menu-label)
;   (notebook-insert-page notebook 0 child tab-label menu-label))
  
; (define-foreign notebook-remove-page () nil
;   (notebook notebook)
;   (page-num int))

; (defun notebook-current-page-num (notebook)
;   (let ((page-num (notebook-current-page notebook)))
;     (if (= page-num -1)
; 	nil
;       page-num)))

; (define-foreign ("gtk_notebook_get_nth_page" notebook-nth-page) () widget
;   (notebook notebook)
;   (page-num int))

; (define-foreign %notebook-page-num () int
;   (notebook notebook)
;   (page-num int))

; (defun notebook-child-page-num (notebook child)
;   (let ((page-num (%notebook-page-num notebook child)))
;     (if (= page-num -1)
; 	nil
;       page-num)))

; (define-foreign notebook-next-page () nil
;   (notebook notebook))

; (define-foreign notebook-prev-page () nil
;   (notebook notebook))

; (define-foreign notebook-popup-enable () nil
;   (notebook notebook))

; (define-foreign notebook-popup-disable () nil
;   (notebook notebook))

; (define-foreign
;     ("gtk_notebook_get_tab_label" notebook-tab-label) (notebook ref) widget
;   (notebook notebook)
;   ((if (widget-p ref)
;        ref
;      (notebook-nth-page notebook ref))
;    widget))

; (define-foreign %notebook-set-tab-label () nil
;   (notebook notebook)
;   (reference widget)
;   (tab-label widget))

; (defun (setf notebook-tab-label) (tab-label notebook reference)
;   (let ((tab-label-widget (if (stringp tab-label)
; 			      (label-new tab-label)
; 			    tab-label)))
;     (%notebook-set-tab-label
;      notebook
;      (if (widget-p reference)
; 	 reference
;        (notebook-nth-page notebook reference))
;      tab-label-widget)
;     (when (stringp tab-label)
;       (widget-unref tab-label-widget))
;     tab-label-widget))
   
; (define-foreign
;     ("gtk_notebook_get_menu_label" notebook-menu-label) (notebook ref) widget
;   (notebook notebook)
;   ((if (widget-p ref)
;        ref
;      (notebook-nth-page notebook ref))
;    widget))

; (define-foreign %notebook-set-menu-label () nil
;   (notebook notebook)
;   (reference widget)
;   (menu-label widget))

; (defun (setf notebook-menu-label) (menu-label notebook reference)
;   (let ((menu-label-widget (if (stringp menu-label)
; 			      (label-new menu-label)
; 			    menu-label)))
;     (%notebook-set-menu-label
;      notebook
;      (if (widget-p reference)
; 	 reference
;        (notebook-nth-page notebook reference))
;      menu-label-widget)
;     (when (stringp menu-label)
;       (widget-unref menu-label-widget))
;     menu-label-widget))

; (define-foreign notebook-query-tab-label-packing (notebook ref) nil
;   (notebook notebook)
;   ((if (widget-p ref)
;        ref
;      (notebook-nth-page notebook ref))
;    widget)
;   (expand boolean :out)
;   (fill boolean :out)
;   (pack-type pack-type :out))

; (define-foreign
;     notebook-set-tab-label-packing (notebook ref expand fill pack-type) nil
;   (notebook notebook)
;   ((if (widget-p ref)
;        ref
;      (notebook-nth-page notebook ref))
;    widget)
;   (expand boolean)
;   (fill boolean)
;   (pack-type pack-type))

; (define-foreign notebook-reorder-child () nil
;   (notebook notebook)
;   (child widget)
;   (position int))



; ;;; Font selection




; ;;; Paned

; (define-foreign paned-add1 () nil
;   (paned paned)
;   (child widget))

; (define-foreign paned-add2 () nil
;   (paned paned)
;   (child widget))

; (define-foreign paned-pack1 () nil
;   (paned paned)
;   (child widget)
;   (resize boolean)
;   (shrink boolean))

; (define-foreign paned-pack2 () nil
;   (paned paned)
;   (child widget)
;   (resize boolean)
;   (shrink boolean))

; ; (define-foreign ("gtk_paned_set_position" (setf paned-position)) () nil
; ;   (paned paned)
; ;   (position int))

; ;; cl-gtk.c
; (define-foreign paned-child1 () widget
;   (paned paned)
;   (resize boolean :out)
;   (shrink boolean :out))

; ;; cl-gtk.c
; (define-foreign paned-child2 () widget
;   (paned paned)
;   (resize boolean :out)
;   (shrink boolean :out))

; (define-foreign vpaned-new () vpaned)

; (define-foreign hpaned-new () hpaned)



; ;;; Layout

; (define-foreign layout-new (&optional hadjustment vadjustment) layout
;   (hadjustment (or null adjustment))
;   (vadjustment (or null adjustment)))

; (define-foreign layout-put () nil
;   (layout layout)
;   (widget widget)
;   (x int) (y int))

; (define-foreign layout-move () nil
;   (layout layout)
;   (widget widget)
;   (x int) (y int))

; (define-foreign %layout-set-size () nil
;   (layout layout)
;   (width int)
;   (height int))

; (defun (setf layout-size) (size layout)
;   (%layout-set-size layout (svref size 0) (svref size 1))
;   (values (svref size 0) (svref size 1)))

; ;; cl-gtk.c
; (define-foreign layout-size () nil
;   (layout layout)
;   (width int :out)
;   (height int :out))

; (define-foreign layout-freeze () nil
;   (layout layout))

; (define-foreign layout-thaw () nil
;   (layout layout))

; (define-foreign layout-offset () nil
;   (layout layout)
;   (x int :out)
;   (y int :out))



;;; List

; (define-foreign list-new () list-widget)

; (define-foreign list-insert-items () nil
;   (list list-widget)
;   (items (list list-item))
;   (position int))

; (define-foreign list-append-items () nil
;   (list list-widget)
;   (items (double-list list-item)))

; (define-foreign list-prepend-items () nil
;   (list list-widget)
;   (items (double-list list-item)))

; (define-foreign %list-remove-items () nil
;   (list list-widget)
;   (items (double-list list-item)))

; (define-foreign %list-remove-items-no-unref () nil
;   (list list-widget)
;   (items (double-list list-item)))

; (defun list-remove-items (list items &key no-unref)
;   (if no-unref
;       (%list-remove-items-no-unref list items)
;     (%list-remove-items list items)))

; (define-foreign list-clear-items () nil
;   (list list-widget)
;   (start int)
;   (end int))

; (define-foreign list-select-item () nil
;   (list list-widget)
;   (item int))

; (define-foreign list-unselect-item () nil
;   (list list-widget)
;   (item int))

; (define-foreign list-select-child () nil
;   (list list-widget)
;   (child widget))

; (define-foreign list-unselect-child () nil
;   (list list-widget)
;   (child widget))

; (define-foreign list-child-position () int
;   (list list-widget)
;   (child widget))

; (define-foreign list-extend-selection () nil
;   (list list-widget)
;   (scroll-type scroll-type)
;   (position single-float)
;   (auto-start-selection boolean))

; (define-foreign list-start-selection () nil
;   (list list-widget))

; (define-foreign list-end-selection () nil
;   (list list-widget))

; (define-foreign list-select-all () nil
;   (list list-widget))

; (define-foreign list-unselect-all () nil
;   (list list-widget))

; (define-foreign list-scroll-horizontal () nil
;   (list list-widget)
;   (scroll-type scroll-type)
;   (position single-float))

; (define-foreign list-scroll-vertical () nil
;   (list list-widget)
;   (scroll-type scroll-type)
;   (position single-float))

; (define-foreign list-toggle-add-mode () nil
;   (list list-widget))

; (define-foreign list-toggle-focus-row () nil
;   (list list-widget))

; (define-foreign list-toggle-row () nil
;   (list list-widget)
;   (item list-item))

; (define-foreign list-undo-selection () nil
;   (list list-widget))

; (define-foreign list-end-drag-selection () nil
;   (list list-widget))

; ;; cl-gtk.c
; (define-foreign list-selection () (double-list list-item)
;   (list list-widget))



;;; Menu shell

; (define-foreign menu-shell-insert () nil
;   (menu-shell menu-shell)
;   (menu-item menu-item)
;   (position int))

; (defun menu-shell-append (menu-shell menu-item)
;   (menu-shell-insert menu-shell menu-item -1))

; (defun menu-shell-prepend (menu-shell menu-item)
;   (menu-shell-insert menu-shell menu-item 0))

; (define-foreign menu-shell-deactivate () nil
;   (menu-shell menu-shell))

; (define-foreign menu-shell-select-item () nil
;   (menu-shell menu-shell)
;   (menu-item menu-item))

; (define-foreign menu-shell-deselect () nil
;   (menu-shell menu-shell))

; (define-foreign menu-shell-activate-item () nil
;   (menu-shell menu-shell)
;   (menu-item menu-item)
;   (fore-deactivate boolean))



; ;;; Menu bar

; (define-foreign menu-bar-new () menu-bar)

; (define-foreign menu-bar-insert () nil
;   (menu-bar menu-bar)
;   (menu menu)
;   (position int))

; (defun menu-bar-append (menu-bar menu)
;   (menu-bar-insert menu-bar menu -1))

; (defun menu-bar-prepend (menu-bar menu)
;   (menu-bar-insert menu-bar menu 0))



; ;;; Menu

; (define-foreign menu-new () menu)

; (defun menu-insert (menu menu-item position)
;   (menu-shell-insert menu menu-item position))

; (defun menu-append (menu menu-item)
;   (menu-shell-append menu menu-item))

; (defun menu-prepend (menu menu-item)
;   (menu-shell-prepend menu menu-item))

; ;(defun menu-popup ...)

; (define-foreign menu-reposition () nil
;   (menu menu))

; (define-foreign menu-popdown () nil
;   (menu menu))

; (define-foreign ("gtk_menu_get_active" menu-active) () widget
;   (menu menu))

; (define-foreign ("gtk_menu_set_active" (setf menu-active)) () nil
;   (menu menu)
;   (index unsigned-int))

; ;(defun menu-attach-to-widget ...)

; (define-foreign menu-detach () nil
;   (menu menu))

; (define-foreign ("gtk_menu_get_attach_widget" menu-attach-widget) () widget
;   (menu menu))

; (define-foreign menu-reorder-child () nil
;   (menu menu)
;   (menu-item menu-item)
;   (position int))



;;; Packer

; (define-foreign packer-new () packer)

; (define-foreign packer-add
;     (packer child side anchor
;      &key
;      options
;      (border-width (packer-default-border-width packer))
;      (pad-x (packer-default-pad-x packer))
;      (pad-y (packer-default-pad-y packer))
;      (ipad-x (packer-default-ipad-x packer))
;      (ipad-y (packer-default-ipad-y packer))) nil
;   (packer packer)
;   (child widget)
;   (side side-type)
;   (anchor anchor-type)
;   (options packer-options)
;   (border-width unsigned-int)
;   (pad-x unsigned-int)
;   (pad-y unsigned-int)
;   (ipad-x unsigned-int)
;   (ipad-y unsigned-int))

; (define-foreign packer-set-child-packing () nil
;   (packer packer)
;   (child widget)
;   (side side-type)
;   (anchor anchor-type)
;   (options packer-options)
;   (border-width unsigned-int)
;   (pad-x unsigned-int)
;   (pad-y unsigned-int)
;   (ipad-x unsigned-int)
;   (ipad-y unsigned-int))

; (define-foreign packer-reorder-child () nil
;   (packer packer)
;   (child widget)
;   (position int))



; ;;; Table

; (define-foreign table-new () table
;   (rows unsigned-int)
;   (columns unsigned-int)
;   (homogeneous boolean))

; (define-foreign table-resize () nil
;   (table table)
;   (rows unsigned-int)
;   (columns unsigned-int))

; (define-foreign table-attach (table child left right top bottom
; 			       &key (x-options '(:expand :fill))
; 			            (y-options '(:expand :fill))
; 			            (x-padding 0) (y-padding 0)) nil
;   (table table)
;   (child widget)
;   (left unsigned-int)
;   (right unsigned-int)
;   (top unsigned-int)
;   (bottom unsigned-int)
;   (x-options attach-options)
;   (y-options attach-options)
;   (x-padding unsigned-int)
;   (y-padding unsigned-int))

; (define-foreign ("gtk_table_set_row_spacing" (setf table-row-spacing)) () nil
;   (table table)
;   (row unsigned-int)
;   (spacing unsigned-int))

; ;; cl-gtk.c
; (define-foreign table-row-spacing (table row) unsigned-int
;   (table table)
;   ((progn
;      (assert (and (>= row 0) (< row (table-rows table))))
;      row) unsigned-int))

; (define-foreign ("gtk_table_set_col_spacing"
; 		  (setf table-column-spacing)) () nil
;   (table table)
;   (col unsigned-int)
;   (spacing unsigned-int))

; ;; cl-gtk.c
; (define-foreign table-column-spacing (table col) unsigned-int
;   (table table)
;   ((progn
;      (assert (and (>= col 0) (< col (table-columns table))))
;      col) unsigned-int))


; (defun %set-table-child-option (object slot flag value)
;   (let ((options (container-child-slot-value object slot)))
;     (cond
;      ((and value (not (member flag options)))
;       (setf (container-child-slot-value object slot) (cons flag options)))
;      ((and (not value) (member flag options))
;       (setf
;        (container-child-slot-value object slot) (delete flag options))))))


; (macrolet ((define-option-accessor (name slot flag)
; 	     `(progn
; 		(defun ,name (object)
; 		  (member ,flag (container-child-slot-value object ,slot)))
; 		(defun (setf ,name) (value object)
; 		  (%set-table-child-option object ,slot ,flag value)))))
;   (define-option-accessor table-child-x-expand-p :x-options :expand)
;   (define-option-accessor table-child-y-expand-p :y-options :expand)
;   (define-option-accessor table-child-x-shrink-p :x-options :shrink)
;   (define-option-accessor table-child-y-shrink-p :y-options :shrink)
;   (define-option-accessor table-child-x-fill-p :x-options :fill)
;   (define-option-accessor table-child-y-fill-p :y-options :fill))



; ;;; Toolbar

; (define-foreign toolbar-new () toolbar
;   (orientation orientation)
;   (style toolbar-style))


; ;; cl-gtk.c
; (define-foreign toolbar-num-children () int
;   (toolbar toolbar))

; (defun %toolbar-position-num (toolbar position)
;   (case position
;     (:prepend 0)
;     (:append (toolbar-num-children toolbar))
;     (t
;      (assert (and (>= position 0) (< position (toolbar-num-children toolbar))))
;      position)))

; (define-foreign %toolbar-insert-element () widget
;   (toolbar toolbar)
;   (type toolbar-child-type)
;   (widget (or null widget))
;   (text string)
;   (tooltip-text string)
;   (tooltip-private-text string)
;   (icon (or null widget))
;   (nil null)
;   (nil null)
;   (position int))

; (defun toolbar-insert-element (toolbar position
; 			       &key tooltip-text tooltip-private-text
; 			       type widget icon text callback)
;   (let* ((icon-widget (typecase icon
; 		       ((or null widget) icon)
; 		       (t (pixmap-new icon))))
; 	 (toolbar-child
; 	  (%toolbar-insert-element
; 	   toolbar (or type (and widget :widget) :button)
; 	   widget text tooltip-text tooltip-private-text icon-widget
; 	   (%toolbar-position-num toolbar position))))
;     (when callback
;       (signal-connect toolbar-child 'clicked callback))
;     toolbar-child))

; (defun toolbar-append-element (toolbar &key tooltip-text tooltip-private-text
; 			       type widget icon text callback)
;   (toolbar-insert-element
;    toolbar :append :type type :widget widget :icon icon :text text
;    :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
;    :callback callback))

; (defun toolbar-prepend-element (toolbar &key tooltip-text tooltip-private-text
; 			        type widget icon text callback)
;   (toolbar-insert-element
;    toolbar :prepend :type type :widget widget :icon icon :text text
;    :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text
;    :callback callback))

; (defun toolbar-insert-space (toolbar position)
;   (toolbar-insert-element toolbar position :type :space))

; (defun toolbar-append-space (toolbar)
;   (toolbar-insert-space toolbar :append))

; (defun toolbar-prepend-space (toolbar)
;   (toolbar-insert-space toolbar :prepend))

; (defun toolbar-insert-widget (toolbar widget position &key tooltip-text
; 			      tooltip-private-text callback)
;   (toolbar-insert-element
;    toolbar position :widget widget :tooltip-text tooltip-text
;    :tooltip-private-text tooltip-private-text :callback callback))
 
; (defun toolbar-append-widget (toolbar widget &key tooltip-text
; 			      tooltip-private-text callback)
;   (toolbar-insert-widget
;    toolbar widget :append :tooltip-text tooltip-text
;    :tooltip-private-text tooltip-private-text :callback callback))

; (defun toolbar-prepend-widget (toolbar widget &key tooltip-text
; 			       tooltip-private-text callback)
;   (toolbar-insert-widget
;    toolbar widget :prepend :tooltip-text tooltip-text
;    :tooltip-private-text tooltip-private-text :callback callback))

; (defun toolbar-insert-item (toolbar text icon position &key tooltip-text
; 			    tooltip-private-text callback)
;   (toolbar-insert-element
;    toolbar position :text text :icon icon :callback callback
;    :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

; (defun toolbar-append-item (toolbar text icon &key tooltip-text
; 			    tooltip-private-text callback)
;   (toolbar-insert-item
;    toolbar text icon :append :callback callback
;    :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

		       
; (defun toolbar-prepend-item (toolbar text icon &key tooltip-text
; 			     tooltip-private-text callback)
;   (toolbar-insert-item
;    toolbar text icon :prepend :callback callback
;    :tooltip-text tooltip-text :tooltip-private-text tooltip-private-text))

; (defun toolbar-enable-tooltips (toolbar)
;   (setf (toolbar-tooltips-p toolbar) t))

; (defun toolbar-disable-tooltips (toolbar)
;   (setf (toolbar-tooltips-p toolbar) nil))



;;; Tree

(define-foreign tree-new () tree)

(define-foreign tree-append () nil
  (tree tree)
  (tree-item tree-item))

(define-foreign tree-prepend () nil
  (tree tree)
  (tree-item tree-item))

(define-foreign tree-insert () nil
  (tree tree)
  (tree-item tree-item)
  (position int))

; (define-foreign tree-remove-items () nil
;   (tree tree)
;   (items (double-list tree-item)))

(define-foreign tree-clear-items () nil
  (tree tree)
  (start int)
  (end int))

(define-foreign tree-select-item () nil
  (tree tree)
  (item int))

(define-foreign tree-unselect-item () nil
  (tree tree)
  (item int))

(define-foreign tree-select-child () nil
  (tree tree)
  (tree-item tree-item))

(define-foreign tree-unselect-child () nil
  (tree tree)
  (tree-item tree-item))

(define-foreign tree-child-position () int
  (tree tree)
  (tree-item tree-item))

(defun root-tree-p (tree)
  (eq (tree-root-tree tree) tree))

;; cl-gtk.c
(define-foreign tree-selection () (double-list tree-item)
  (tree tree))



;;; Calendar

(define-foreign calendar-new () calendar)

(define-foreign calendar-select-month () int
  (calendar calendar)
  (month unsigned-int)
  (year unsigned-int))

(define-foreign calendar-select-day () nil
  (calendar calendar)
  (day unsigned-int))

(define-foreign calendar-mark-day () int
  (calendar calendar)
  (day unsigned-int))

(define-foreign calendar-unmark-day () int
  (calendar calendar)
  (day unsigned-int))

(define-foreign calendar-clear-marks () nil
  (calendar calendar))

(define-foreign calendar-display-options () nil
  (calendar calendar)
  (options calendar-display-options))

(define-foreign ("gtk_calendar_get_date" calendar-date) () nil
  (calendar calendar)
  (year unsigned-int :out)
  (month unsigned-int :out)
  (day unsigned-int :out))

(define-foreign calendar-freeze () nil
  (calendar calendar))

(define-foreign calendar-thaw () nil
  (calendar calendar))



;;; Drawing area

; (define-foreign drawing-area-new () drawing-area)

; (define-foreign ("gtk_drawing_area_size" %drawing-area-set-size) () nil
;   (drawing-area drawing-area)
;   (width int)
;   (height int))

; (defun (setf drawing-area-size) (size drawing-area)
;   (%drawing-area-set-size drawing-area (svref size 0) (svref size 1))
;   (values (svref size 0) (svref size 1)))

; ;; cl-gtk.c
; (define-foreign ("gtk_drawing_area_get_size" drawing-area-size) () nil
;   (drawing-area drawing-area)
;   (width int :out)
;   (height int :out))



; ;;; Curve



; ;;; Editable

; (define-foreign editable-select-region () nil
;   (editable editable)
;   (start int)
;   (end int))

; (define-foreign editable-insert-text
;     (editable text &optional (position 0)) nil
;   (editable editable)
;   (text string)
;   ((length text) int)
;   (position int))

; (define-foreign editable-delete-text (editable &optional (start 0) end) nil
;   (editable editable)
;   (start int)
;   ((or end -1) int))

; (define-foreign ("gtk_editable_get_chars" editable-text)
;     (editable &optional (start 0) end) string
;   (editable editable)
;   (start int)
;   ((or end -1) int))

; (defun (setf editable-text) (text editable)
;   (editable-delete-text editable)
;   (when text
;     (editable-insert-text editable text))
;   text)

; (define-foreign editable-cut-clipboard () nil
;   (editable editable))

; (define-foreign editable-copy-clipboard () nil
;   (editable editable))

; (define-foreign editable-paste-clipboard () nil
;   (editable editable))

; (define-foreign editable-claim-selection () nil
;   (editable editable)
;   (claim boolean)
;   (time unsigned-int))

; (define-foreign editable-delete-selection () nil
;   (editable editable))

; (define-foreign editable-changed () nil
;   (editable editable))



; ;;; Entry

; (define-foreign %entry-new() entry)

; (define-foreign %entry-new-with-max-length () entry
;   (max uint16))

; (defun entry-new (&optional max)
;   (if max
;       (%entry-new-with-max-length max)
;     (%entry-new)))

; (define-foreign entry-append-text () nil
;   (entry entry)
;   (text string))

; (define-foreign entry-prepend-text () nil
;   (entry entry)
;   (text string))

; (define-foreign entry-select-region () nil
;   (entry entry)
;   (start int)
;   (end int))



; ;;; Spin button

; (define-foreign spin-button-new () spin-button
;   (adjustment adjustment)
;   (climb-rate single-float)
;   (digits unsigned-int))

; (defun spin-button-value-as-int (spin-button)
;   (round (spin-button-value spin-button)))

; (define-foreign spin-button-spin () nil
;   (spin-button spin-button)
;   (direction spin-type)
;   (increment single-float))

; (define-foreign spin-button-update () nil
;   (spin-button spin-button))



; ;;; Text

; (define-foreign text-new (&optional hadjustment vadjustment) text
;   (hadjustment (or null adjustment))
;   (vadjustment (or null adjustment)))

; (define-foreign text-freeze () nil
;   (text text))

; (define-foreign text-thaw () nil
;   (text text))

; (define-foreign %text-insert () nil
;   (text text)
;   (font (or null gdk:font))
;   (fore (or null gdk:color))
;   (back (or null gdk:color))
;   (string string)
;   (-1 int))

; (defun text-insert (text string &key font foreground background (start 0) end)
;   (let ((real-font (gdk:ensure-font font)))
;     (gdk:with-colors ((fore-color foreground)
; 		      (back-color background))
;     (%text-insert
;      text real-font fore-color back-color (subseq string start end))
;     (gdk:font-maybe-unref real-font font))))

; (define-foreign text-backward-delete () int
;   (text text)
;   (n-chars unsigned-int))

; (define-foreign text-forward-delete () nil
;   (text text)
;   (nchars unsigned-int))



; ;;; Ruler

; (define-foreign ruler-set-range () nil
;   (ruler ruler)
;   (lower single-float)
;   (upper single-float)
;   (position single-float)
;   (max-size single-float))

; (define-foreign ruler-draw-ticks () nil
;   (ruler ruler))

; (define-foreign ruler-draw-pos () nil
;   (ruler ruler))

; (define-foreign hruler-new () hruler)

; (define-foreign vruler-new () vruler)



; ;;; Range

; (define-foreign range-draw-background () nil
;   (range range))

; (define-foreign range-clear-background () nil
;   (range range))

; (define-foreign range-draw-trough () nil
;   (range range))

; (define-foreign range-draw-slider () nil
;   (range range))

; (define-foreign range-draw-step-forw () nil
;   (range range))

; (define-foreign range-slider-update () nil
;   (range range))

; (define-foreign range-trough-click () int
;   (range range)
;   (x int)
;   (y int)
;   (jump-perc single-float :out))

; (define-foreign range-default-hslider-update () nil
;   (range range))

; (define-foreign range-default-vslider-update () nil
;   (range range))

; (define-foreign range-default-htrough-click () int
;   (range range)
;   (x int)
;   (y int)
;   (jump-perc single-float :out))

; (define-foreign range-default-vtrough-click () int
;   (range range)
;   (x int)
;   (y int)
;   (jump-perc single-float :out))

; (define-foreign range-default-hmotion () int
;   (range range)
;   (x-delta int)
;   (y-delta int))

; (define-foreign range-default-vmotion () int
;   (range range)
;   (x-delta int)
;   (y-delta int))



; ;;; Scale

; (define-foreign scale-draw-value () nil
;   (scale scale))

; (define-foreign hscale-new () hscale
;   (adjustment adjustment))

; (define-foreign vscale-new () hscale
;   (adjustment adjustment))



; ;;; Scrollbar

; (define-foreign hscrollbar-new () hscrollbar
;   (adjustment adjustment))

; (define-foreign vscrollbar-new () vscrollbar
;   (adjustment adjustment))



; ;;; Separator

; (define-foreign vseparator-new () vseparator)

; (define-foreign hseparator-new () hseparator)



; ;;; Preview



; ;;; Progress

; (define-foreign progress-configure () adjustment
;   (progress progress)
;   (value single-float)
;   (min single-float)
;   (max single-float))

; (define-foreign ("gtk_progress_get_text_from_value"
; 		  progress-text-from-value) () string
;   (progress progress))

; (define-foreign ("gtk_progress_get_percentage_from_value"
; 		  progress-percentage-from-value) () single-float
;   (progress progress))



; ;;; Progress bar

; (define-foreign %progress-bar-new () progress-bar)

; (define-foreign %progress-bar-new-with-adjustment () progress-bar
;   (adjustment adjustment))

; (defun progress-bar-new (&optional adjustment)
;   (if adjustment
;       (%progress-bar-new-with-adjustment adjustment)
;     (%progress-bar-new)))

; (define-foreign progress-bar-update () nil
;   (progress-bar progress-bar)
;   (percentage single-float))



;;; Adjustment

(define-foreign adjustment-new () adjustment
  (value single-float)
  (lower single-float)
  (upper single-float)
  (step-increment single-float)
  (page-increment single-float)
  (page-size single-float))

(define-foreign adjustment-changed () nil
  (adjustment adjustment))

(define-foreign adjustment-value-changed () nil
  (adjustment adjustment))

(define-foreign adjustment-clamp-page () nil
  (adjustment adjustment)
  (lower single-float)
  (upper single-float))



;;; Tooltips

; (define-foreign tooltips-new () tooltips)

; (define-foreign tooltips-enable () nil
;   (tooltips tooltips))

; (define-foreign tooltips-disable () nil
;   (tooltips tooltips))

; (define-foreign tooltips-set-tip () nil
;   (tooltips tooltips)
;   (widget widget)
;   (tip-text string)
;   (tip-private string))

; (declaim (inline tooltips-set-colors-real))
; (define-foreign ("gtk_tooltips_set_colors" tooltips-set-colors-real) () nil
;   (tooltips tooltips)
;   (background gdk:color)
;   (foreground gdk:color))

; (defun tooltips-set-colors (tooltips background foreground)
;   (gdk:with-colors ((background background)
; 	  	    (foreground foreground))
;     (tooltips-set-colors-real tooltips background foreground)))

; (define-foreign tooltips-force-window () nil
;   (tooltips tooltips))




; ;;; Rc

; (define-foreign rc-add-default-file (filename) nil
;   ((namestring (truename filename)) string))

; (define-foreign rc-parse (filename) nil
;   ((namestring (truename filename)) string))

; (define-foreign rc-parse-string () nil
;   (rc-string string))

; (define-foreign rc-reparse-all () nil)

; ;(define-foreign rc-get-style () style
; ;  (widget widget))



;;; Accelerator Groups

(define-foreign accel-group-new () accel-group)

(define-foreign accel-group-get-default () accel-group)

(define-foreign accel-group-ref () accel-group
  (accel-group accel-group))

(define-foreign accel-group-unref () nil
  (accel-group accel-group))

(define-foreign accel-group-activate (accel-group key modifiers) boolean
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign accel-groups-activate (object key modifiers) boolean
  (object object)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign accel-group-attach () nil
  (accel-group accel-group)
  (object object))

(define-foreign accel-group-detach () nil
  (accel-group accel-group)
  (object object))

(define-foreign accel-group-lock () nil
  (accel-group accel-group))

(define-foreign accel-group-unlock () nil
  (accel-group accel-group))


;;; Accelerator Groups Entries

(define-foreign accel-group-get-entry (accel-group key modifiers) accel-entry
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign accel-group-lock-entry (accel-group key modifiers) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign accel-group-unlock-entry (accel-group key modifiers) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign accel-group-add
    (accel-group key modifiers flags object signal) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags)
  (object object)
  ((name-to-string signal) string))

(define-foreign accel-group-add (accel-group key modifiers object) nil
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (object object))


;;; Accelerator Signals

(define-foreign accel-group-handle-add
    (object signal-id accel-group key modifiers flags) nil
  (object object)
  (signal-id unsigned-int)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags))

(define-foreign accel-group-handle-remove
    (object accel-group key modifiers) nil
  (object object)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))



;;; Style

; (define-foreign style-new () style)

; (define-foreign style-copy () style
;   (style style))

; (define-foreign style-ref () style
;   (style style))

; (define-foreign style-unref () nil
;   (style style))

; (define-foreign style-get-color () gdk:color
;   (style style)
;   (color-type color-type)
;   (state-type state-type))

; (define-foreign
;     ("gtk_style_set_color" style-set-color-from-color) () gdk:color
;   (style style)
;   (color-type color-type)
;   (state-type state-type)
;   (color gdk:color))

; (defun style-set-color (style color-type state-type color)
;   (gdk:with-colors ((color color))
;     (style-set-color-from-color style color-type state-type color)))

; (define-foreign ("gtk_style_get_font" style-font) () gdk:font
;   (style style))

; (define-foreign style-set-font () gdk:font
;   (style style)
;   (font gdk:font))

; (defun (setf style-font) (font style)
;   (let ((font (gdk:ensure-font font)))
;     (gdk:font-unref (style-font style))
;     (style-set-font style font)))

; (defun style-fg (style state)
;   (style-get-color style :foreground state))

; (defun (setf style-fg) (color style state)
;   (style-set-color style :foreground state color))

; (defun style-bg (style state)
;   (style-get-color style :background state))

; (defun (setf style-bg) (color style state)
;   (style-set-color style :background state color))

; (defun style-text (style state)
;   (style-get-color style :text state))

; (defun (setf style-text) (color style state)
;   (style-set-color style :text state color))

; (defun style-base (style state)
;   (style-get-color style :base state))

; (defun (setf style-base) (color style state)
;   (style-set-color style :base state color))

; (defun style-white (style)
;   (style-get-color style :white :normal))

; (defun (setf style-white) (color style)
;   (style-set-color style :white :normal color))

; (defun style-black (style)
;   (style-get-color style :black :normal))

; (defun (setf style-black) (color style)
;   (style-set-color style :black :normal color))

; (define-foreign style-get-gc
;     (style color-type &optional (state-type :normal)) gdk:gc
;   (style style)
;   (color-type color-type)
;   (state-type state-type))








