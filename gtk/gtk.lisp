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

;; $Id: gtk.lisp,v 1.5 2001-05-31 21:52:57 espen Exp $


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



;;; Label

(defbinding label-select-region () nil
  (label label)
  (start int)
  (end int))



;;; Acccel label

(defbinding accel-label-refetch () boolean
  (accel-label accel-label))



;;; Bin

(defun bin-child (bin)
  (first (container-children bin)))

(defun (setf bin-child) (child bin)
  (let ((old-child (bin-child bin)))
    (when old-child
      (container-remove bin old-child)))
  (container-add bin child)
  child)

(defmethod initialize-instance ((bin bin) &rest initargs &key child)
  (declare (ignore initargs))
  (call-next-method)
  (cond
   ((consp child)
    (container-add bin (first child))
    (setf
     (slot-value (first child) 'child-slots)
     (apply
      #'make-instance
      (slot-value (class-of bin) 'child-class)
      :parent bin :child (first child) (cdr child))))
   (child
    (container-add bin child))))


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



;;; Toggle button

(defbinding toggle-button-toggled () nil
  (toggle-button toggle-button))



;;; Check button

(defmethod (setf button-label) ((label string) (button check-button))
  (call-next-method)
  (setf (misc-xalign (bin-child button)) 0.0)
  label)



;;; Radio button

(defbinding (%radio-button-get-group "gtk_radio_button_group") () pointer
  (radio-button radio-button))

(defbinding %radio-button-set-group () nil
  (radio-button radio-button)
  (group pointer))

(defun radio-button-add-to-group (button1 button2)
  "Add BUTTON1 to the group which BUTTON2 belongs to."
  (%radio-button-set-group button1 (%radio-button-get-group button2)))

(defmethod initialize-instance ((button radio-button)
				&rest initargs &key group)
  (call-next-method)
  (when group
    (radio-button-add-to-group item group)))


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

(defbinding %menu-item-configure () nil
  (menu-item menu-item)
  (show-toggle-indicator boolean)
  (show-submenu-indicator boolean))

(defun (setf menu-item-toggle-indicator-p) (show menu-item)
  (%menu-item-configure
   menu-item
   show
   (menu-item-submenu-indicator-p menu-item))
  show)

(defun (setf menu-item-submenu-indicator-p) (show menu-item)
  (%menu-item-configure
   menu-item
   (menu-item-toggle-indicator-p menu-item)
   show))

(defbinding menu-item-select () nil
  (menu-item menu-item))

(defbinding menu-item-deselect () nil
  (menu-item menu-item))

(defbinding menu-item-activate () nil
  (menu-item menu-item))

(defbinding menu-item-right-justify () nil
  (menu-item menu-item))



;;; Check menu item

(defbinding check-menu-item-toggled () nil
  (check-menu-item check-menu-item))



;;; Radio menu item

(defbinding (%radio-menu-item-get-group
	     "gtk_radio_menu_item_group") () pointer
  (radio-menu-item radio-menu-item))

(defbinding %radio-menu-item-set-group () nil
  (radio-menu-item radio-menu-item)
  (group pointer))

(defun radio-menu-item-add-to-group (item1 item2)
  "Add ITEM1 to the group which ITEM2 belongs to."
  (%radio-menu-item-set-group item1 (%radio-menu-item-get-group item2)))

(defmethod initialize-instance ((item radio-menu-item)
				&rest initargs &key group)
  (call-next-method)
  (when group
    (radio-menu-item-add-to-group item group)))
  


;;; Window

(defbinding %window-set-wmclass () nil
  (window window)
  (wmclass-name string)
  (wmclass-class string))

(defun (setf window-wmclass) (wmclass window)
  (%window-set-wmclass window (svref wmclass 0) (svref wmclass 1))
  (values (svref wmclass 0) (svref wmclass 1)))

;; gtkglue.c
(defbinding window-wmclass () nil
  (window window)
  (wmclass-name string :out)
  (wmclass-class string :out))

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

(defbinding window-set-transient-for () nil
  (window window)
  (parent window))

;(defbinding window-set-geometry-hints)



;;; File selection

(defbinding file-selection-complete () nil
  (file-selection file-selection)
  (pattern string))

(defbinding file-selection-show-fileop-buttons () nil
  (file-selection file-selection))

(defbinding file-selection-hide-fileop-buttons () nil
  (file-selection file-selection))



;;; Scrolled window

(defun (setf scrolled-window-scrollbar-policy) (policy window)
  (setf (scrolled-window-hscrollbar-policy window) policy)
  (setf (scrolled-window-vscrollbar-policy window) policy))

(defbinding scrolled-window-add-with-viewport () nil
   (scrolled-window scrolled-window)
   (child widget))



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
  (child widget :out)
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



;;; Button box

(defbinding button-box-get-child-size () nil
  (button-box button-box)
  (min-width int :out)
  (min-height int :out))

(defbinding button-box-set-child-size () nil
  (button-box button-box)
  (min-width int)
  (min-height int))

(defbinding button-box-get-child-ipadding () nil
  (button-box button-box)
  (ipad-x int :out)
  (ipad-y int :out))

(defbinding button-box-set-child-ipadding () nil
  (button-box button-box)
  (ipad-x int)
  (ipad-y int))



;;; Color selection

; (defbinding %color-selection-get-previous-color () nil
;   (colorsel color-selection)
;   (color pointer))

; (defun color-selection-previous-color (colorsel)
;   (let ((color (allocate-memory (* (size-of 'double-float) 4))))
;     (%color-selection-get-previous-color colorsel color)
;     (funcall (get-from-alien-function '(vector double-float 4)) color)))

; (defbinding %color-selection-set-previous-color () nil
;   (colorsel color-selection)
;   (color (vector double-float 4)))

; (defun (setf color-selection-previous-color) (color colorsel)
;   (%color-selection-set-previous-color colorsel color)
;   color)

(defbinding (color-selection-is-adjusting-p
	     "gtk_color_selection_is_adjusting") () boolean
  (colorsel color-selection))



;;; Combo

(defbinding combo-set-value-in-list () nil
  (combo combo)
  (val boolean)
  (ok-if-empty boolean))

; (defbinding ("gtk_combo_set_item_string" (setf combo-item-string)) () nil
;   (combo combo)
;   (item item)
;   (item-value string))

(defbinding %combo-set-popdown-strings () nil
  (combo combo)
  (strings (glist string)))

(defun (setf combo-popdown-strings) (strings combo)
  (%combo-set-popdown-strings combo strings)
  strings)

(defbinding combo-disable-activate () nil
  (combo combo))



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

(defbinding (notebook-tab-label "gtk_notebook_get_tab_label")
    (notebook ref) widget
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget))

(defbinding %notebook-set-tab-label () nil
  (notebook notebook)
  (reference widget)
  (tab-label widget))

(defun (setf notebook-tab-label) (tab-label notebook reference)
  (let ((tab-label-widget (if (stringp tab-label)
			      (label-new tab-label)
			    tab-label)))
    (%notebook-set-tab-label
     notebook
     (if (typep reference 'widget)
	 reference
       (notebook-nth-page-child notebook reference))
     tab-label-widget)
    tab-label-widget))
   
(defbinding (notebook-menu-label "gtk_notebook_get_menu_label")
    (notebook ref) widget
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget))

(defbinding %notebook-set-menu-label () nil
  (notebook notebook)
  (reference widget)
  (menu-label widget))

(defun (setf notebook-menu-label) (menu-label notebook reference)
  (let ((menu-label-widget (if (stringp menu-label)
			      (label-new menu-label)
			    menu-label)))
    (%notebook-set-menu-label
     notebook
     (if (typep reference 'widget)
	 reference
       (notebook-nth-page-child notebook reference))
     menu-label-widget)
    menu-label-widget))

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

;; gtkglue.c
(defbinding layout-get-size () nil
  (layout layout)
  (width int :out)
  (height int :out))

(defun layout-x-size (layout)
  (nth-value 0 (layout-get-size layout)))

(defun layout-y-size (layout)
  (nth-value 1 (layout-get-size layout)))

(defun (setf layout-x-size) (x layout)
  (layout-set-size layout x (layout-y-size layout)))

(defun (setf layout-y-size) (y layout)
  (layout-set-size layout (layout-x-size layout) y))

(defbinding layout-freeze () nil
  (layout layout))

(defbinding layout-thaw () nil
  (layout layout))



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

; (defun menu-insert (menu menu-item position)
;   (menu-shell-insert menu menu-item position))

; (defun menu-append (menu menu-item)
;   (menu-shell-append menu menu-item))

; (defun menu-prepend (menu menu-item)
;   (menu-shell-prepend menu menu-item))

;(defun menu-popup ...)

(defbinding menu-reposition () nil
  (menu menu))

(defbinding menu-popdown () nil
  (menu menu))

(defbinding (menu-active "gtk_menu_get_active") () widget
  (menu menu))

(defbinding %menu-set-active () nil
  (menu menu)
  (index unsigned-int))

(defun (setf menu-active) (menu index)
  (%menu-set-active menu index))
  
;(defun menu-attach-to-widget ...)

(defbinding menu-detach () nil
  (menu menu))

(defbinding (menu-attach-widget "gtk_menu_get_attach_widget") () widget
  (menu menu))

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

(defun (setf table-row-spacing) (spacing table row)
  (%table-set-row-spacing table row spacing)
  spacing)

;; gtkglue.c
(defbinding table-row-spacing (table row) unsigned-int
  (table table)
  ((progn
     (assert (and (>= row 0) (< row (table-rows table))))
     row) unsigned-int))

(defbinding %table-set-col-spacing () nil
  (table table)
  (col unsigned-int)
  (spacing unsigned-int))

(defun (setf table-column-spacing) (spacing table column)
  (%table-set-col-spacing table column spacing)
  spacing)

;; gtkglue.c
(defbinding table-column-spacing (table col) unsigned-int
  (table table)
  ((progn
     (assert (and (>= col 0) (< col (table-columns table))))
     col) unsigned-int))


(defun %set-table-child-option (object slot flag value)
  (let ((options (child-slot-value object slot)))
    (cond
     ((and value (not (member flag options)))
      (setf (child-slot-value object slot) (cons flag options)))
     ((and (not value) (member flag options))
      (setf (child-slot-value object slot) (delete flag options))))))

(macrolet ((define-option-accessor (name slot flag)
	     `(progn
		(defun ,name (object)
		  (member ,flag (child-slot-value object ,slot)))
		(defun (setf ,name) (value object)
		  (%set-table-child-option object ,slot ,flag value)))))
  (define-option-accessor table-child-x-expand-p :x-options :expand)
  (define-option-accessor table-child-y-expand-p :y-options :expand)
  (define-option-accessor table-child-x-shrink-p :x-options :shrink)
  (define-option-accessor table-child-y-shrink-p :y-options :shrink)
  (define-option-accessor table-child-x-fill-p :x-options :fill)
  (define-option-accessor table-child-y-fill-p :y-options :fill))



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



;;; Drawing area


; (defbinding ("gtk_drawing_area_size" %drawing-area-set-size) () nil
;   (drawing-area drawing-area)
;   (width int)
;   (height int))

; (defun (setf drawing-area-size) (size drawing-area)
;   (%drawing-area-set-size drawing-area (svref size 0) (svref size 1))
;   (values (svref size 0) (svref size 1)))

; ;; gtkglue.c
; (defbinding ("gtk_drawing_area_get_size" drawing-area-size) () nil
;   (drawing-area drawing-area)
;   (width int :out)
;   (height int :out))



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

(defbinding scale-draw-value () nil
  (scale scale))



;;; Progress

(defbinding progress-configure () adjustment
  (progress progress)
  (value single-float)
  (min single-float)
  (max single-float))

(defbinding (progress-text-from-value
	     "gtk_progress_get_text_from_value") () string
  (progress progress))

(defbinding (progress-percentage-from-value
	     "gtk_progress_get_percentage_from_value") () single-float
  (progress progress))



;;; Progress bar

(defbinding progress-bar-pulse () nil
  (progress-bar progress-bar))



;;; Adjustment

(defbinding adjustment-changed () nil
  (adjustment adjustment))

(defbinding adjustment-value-changed () nil
  (adjustment adjustment))

(defbinding adjustment-clamp-page () nil
  (adjustment adjustment)
  (lower single-float)
  (upper single-float))



;;; Tooltips

(defbinding tooltips-enable () nil
  (tooltips tooltips))

(defbinding tooltips-disable () nil
  (tooltips tooltips))

(defbinding tooltips-set-tip () nil
  (tooltips tooltips)
  (widget widget)
  (tip-text string)
  (tip-private string))

(defbinding tooltips-set-colors (tooltips background foreground) nil
  (tooltips tooltips)
  ((gdk:ensure-color background) gdk:color)
  ((gdk:ensure-color foreground) gdk:color))

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


;;; Style

; (defbinding style-new () style)

; (defbinding style-copy () style
;   (style style))
#|
(defbinding %style-get-color () gdk:color
  (style style)
  (color-type color-type)
  (state-type state-type))

(defbinding %style-set-color () gdk:color
  (style style)
  (color-type color-type)
  (state-type state-type)
  (color gdk:color))

(defun style-fg (style state)
  (%style-get-color style :foreground state))

(defun (setf style-fg) (color style state)
  (%style-set-color style :foreground state color))

(defun style-bg (style state)
  (%style-get-color style :background state))

(defun (setf style-bg) (color style state)
  (%style-set-color style :background state color))

(defun style-text (style state)
  (%style-get-color style :text state))

(defun (setf style-text) (color style state)
  (%style-set-color style :text state color))

(defun style-base (style state)
  (%style-get-color style :base state))

(defun (setf style-base) (color style state)
  (%style-set-color style :base state color))

(defun style-white (style)
  (%style-get-color style :white :normal))

(defun (setf style-white) (color style)
  (%style-set-color style :white :normal color))

(defun style-black (style)
  (%style-get-color style :black :normal))

(defun (setf style-black) (color style)
  (%style-set-color style :black :normal color))

(defbinding style-get-gc () gdk:gc
  (style style)
  (color-type color-type)
  (state-type state-type))

|#
(defbinding draw-hline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x1 int)
  (x2 int)
  (y int))

(defbinding draw-vline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (y1 int)
  (y2 int)
  (x int))

(defbinding draw-shadow () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (defbinding draw-polygon () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (points (vector gdk:point))
;   ((length points) int)
;   (fill boolean))

(defbinding draw-arrow () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (arrow arrow-type)
  (fill boolean)
  (x int)
  (y int)
  (width int)
  (height int))
  
(defbinding draw-diamond () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (defbinding draw-oval () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

(defbinding draw-string () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (string string))

(defbinding draw-box () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-flat-box () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-check () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-option () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (defbinding draw-cross () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

; (defbinding draw-ramp () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (arrow arrow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

(defbinding draw-tab () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-shadow-gap () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (gap-side position-type)
  (gap-x int)
  (gap-width int))

(defbinding draw-box-gap () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (gap-side position-type)
  (gap-x int)
  (gap-width int))

(defbinding draw-extension () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-focus () nil
  (style style)
  (window gdk:window)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding draw-slider () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(defbinding draw-handle () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(defbinding draw-handle () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(defbinding paint-hline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x1 int)
  (x2 int)
  (y int))
