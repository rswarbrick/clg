;; Common Lisp bindings for GTK+ v2.0.x
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

;; $Id: gtktypes.lisp,v 1.7 2001-05-29 15:56:58 espen Exp $


(in-package "GTK")

(defclass requisition (boxed)
  ((width
    :allocation :alien
    :accessor requisition-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor requisition-height
    :initarg :height
    :type int))
  (:metaclass boxed-class)
  (:alien-name "GtkRequisition"))

(defclass border (boxed)
  ((left
    :allocation :alien
    :accessor border-left
    :initarg :left
    :type int)
   (right
    :allocation :alien
    :accessor border-right
    :initarg :right
    :type int)
   (top
    :allocation :alien
    :accessor border-top
    :initarg :top
    :type int)
   (bottom
    :allocation :alien
    :accessor border-bottom
    :initarg :bottom
    :type int))
  (:metaclass boxed-class)
  (:alien-name "GtkBorder"))

(defclass adjustment (%object)
  ((lower
    :allocation :alien
    :accessor adjustment-lower
    :initarg :lower
    :type single-float)
   (upper
    :allocation :alien
    :accessor adjustment-upper
    :initarg :upper
    :type single-float)
   (value
    :allocation :alien
    :accessor adjustment-value
    :initarg :value
    :type single-float)
   (step-increment
    :allocation :alien
    :accessor adjustment-step-increment
    :initarg :step-increment
    :type single-float)
   (page-increment
    :allocation :alien
    :accessor adjustment-page-increment
    :initarg :page-increment
    :type single-float)
   (page-size
    :allocation :alien
    :accessor adjustment-page-size
    :initarg :page-size
    :type single-float))
  (:metaclass gobject-class)
  (:alien-name "GtkAdjustment"))


(define-types-by-introspection "Gtk"
  ;; Manually defined
  ("GtkObject" :ignore t)
  ("GtkRequisition" :ignore t)
  ("GtkBorder" :ignore t)
  ("GtkAdjustment" :ignore t)

  ;; Temporary disabled
  ("GtkCellRenderer" :ignore-prefix t)

  ;; Manual override
  
  ;; Not needed
  ("GtkFundamentalType" :ignore t)
  ("GtkArgFlags" :ignore t)
 
  ;; Deprecated
  ("GtkCList" :ignore-prefix t)
  ("GtkCTree" :ignore-prefix t)
  ("GtkList" :ignore t)
  ("GtkTree" :ignore t)
  ("GtkTreeItem" :ignore t)
  ("GtkText" :ignore-prefix t)
  ("GtkPacker" :ignore-prefix t)
  ("GtkPixmap" :ignore t)
  ("GtkPreview" :ignore-prefix t)
  ("GtkTipsQuery" :ignore t)
  ("GtkOldEditable" :ignore t))
  
  

#|
(deftype color-type ()
  '(enum
    :foreground
    :background
    :light
    :dark
    :mid
    :text
    :base
    :white
    :black))


(defclass style (gobject)
  ((white
    :allocation :virtual
    :location style-white
    :initarg :white
    :type gdk:color)
   (black
    :allocation :virtual
    :location style-black
    :initarg :black
    :type gdk:color)
   (font
    :allocation :virtual
    :location ("gtk_style_get_font" "gtk_style_set_font")
    :accessor style-font
    :initarg :font
    :type gdk:font))
  (:metaclass gobject-class)
  (:alien-name "GtkStyle"))


;(deftype accel-entry () 'pointer) ; internal?

  

(defclass tooltips (%object)
  ((delay
    :allocation :virtual
    :location ("gtk_tooltips_get_delay" "gtk_tooltips_set_delay")
    :accessor tooltips-delay
    :type unsigned-int)
   (enabled
    :allocation :virtual
    :location ("gtk_tooltips_get_enabled" (setf tooltips-enabled-p))
    :reader tooltips-enabled-p
    :initarg :enabled
    :type boolean))
  (:metaclass object-class)
  (:alien-name "GtkTooltips"))


(defclass widget (object)
  ((child-slots
    :allocation :instance
    :accessor widget-child-slots
    :type container-child)
   (state
    :allocation :virtual
    :location ("gtk_widget_get_state" "gtk_widget_set_state")
    :accessor widget-state
    :initarg :state
    :type state-type)
   (window
    :allocation :virtual
    :location "gtk_widget_get_window"
    :reader widget-window
    :type gdk:window)
   (colormap
    :allocation :virtual
    :location "gtk_widget_get_colormap"
    :reader widget-colormap
    :type gdk:colormap)
   (visual
    :allocation :virtual
    :location "gtk_widget_get_visual"
    :reader widget-visual
    :type gdk:visual))
  (:metaclass object-class)
  (:alien-name "GtkWidget"))



(defclass accel-label (label)
   (width
    :allocation :virtual
    :location "gtk_accel_label_get_accel_width"
    :reader width
    :type unsigned-int))
  (:metaclass widget-class)
  (:alien-name "GtkAccelLabel"))


(defclass container (widget)
   (children
    :allocation :virtual
    :location container-children)
   (focus-child
    :allocation :virtual
    :location ("gtk_container_get_focus_child" "gtk_container_set_focus_child")
    :accessor container-focus-child
    :initarg :focus-child
    :type widget)
   (focus-hadjustment
    :allocation :virtual
    :location (nil "gtk_container_set_focus_hadjustment")
    :writer (setf container-focus-hadjustment)
    :initarg :focus-hadjustment
    :type adjustment)   
   (focus-vadjustment
    :allocation :virtual
    :location (nil "gtk_container_set_focus_vadjustment")
    :writer (setf container-focus-vadjustment)
    :initarg :focus-vadjustment
    :type adjustment))
  (:metaclass widget-class)
  (:alien-name "GtkContainer"))



(defclass bin (container)
  ((child
    :allocation :virtual
    :location bin-child
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkBin"))



(defclass radio-button (check-button)
  ((group
    :allocation :virtual
    :location ("gtk_radio_button_group")
    :reader radio-button-group
    :type (static (gslist widget))))
  (:metaclass container-class)
  (:alien-name "GtkRadioButton"))

(defclass option-menu (button)
  ((menu
    :allocation :virtual
    :location ("gtk_option_menu_get_menu" (setf option-menu-menu))
    :reader option-menu-menu
    :initarg :menu
    :type widget)
   (history
    :allocation :virtual
    :location (nil "gtk_option_menu_set_history")
    :writer (setf option-menu-history)
    :initarg :history
    :type unsigned-int))
  (:metaclass container-class)
  (:alien-name "GtkOptionMenu"))

(defclass menu-item (item)
  ((label
    :allocation :virtual
    :location menu-item-label
    :initarg :label
    :type string)
   (submenu
    :allocation :virtual
    :location ("gtk_menu_item_get_submenu" (setf menu-item-submenu))
    :reader menu-item-submenu
    :initarg :submenu
    :type menu-item)
   (placement
    :allocation :virtual
    :location ("gtk_menu_item_get_placement" "gtk_menu_item_set_placement")
    :accessor menu-item-placement
    :initarg :placement
    :type submenu-placement)
   (toggle-indicator
    :allocation :virtual
    :location ("gtk_menu_item_get_show_toggle"
	       (setf menu-item-toggle-indicator-p))
    :reader menu-item-toggle-indicator-p
    :initarg :toggle-indicator
    :type boolean)
   (submenu-indicator
    :allocation :virtual
    :location ("gtk_menu_item_get_show_submenu"
	       (setf menu-item-submenu-indicator-p))
    :reader menu-item-submenu-indicator-p
    :initarg :submenu-indicator
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkMenuItem"))


(defclass radio-menu-item (check-menu-item)
  ((group
    :allocation :virtual
    :location ("gtk_radio_menu_item_group")
    :reader radio-menu-item-group
    :type (static (gslist widget))))
  (:metaclass container-class)
  (:alien-name "GtkRadioMenuItem"))



(defclass dialog (window)
  ((main-box
    :allocation :alien
    :reader dialog-main-box
    :type widget)
   (action-area
    :allocation :alien
    :reader dialog-action-area
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkDialog"))


(defclass color-selection-dialog (dialog)
  ((colorsel
    :allocation :alien
    :reader color-selection-dialog-colorsel
    :type widget)
   (ok-button
    :allocation :alien
    :reader color-selection-dialog-ok-button
    :type widget)
   (cancel-button
    :allocation :alien
    :reader color-selection-dialog-cancel-button
    :type widget)
   (help-button
    :allocation :alien
    :reader color-selection-dialog-help-button
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkColorSelectionDialog"))


(defclass file-selection (window)
  ((filename
    :allocation :virtual
    :location ("gtk_file_selection_get_filename"
	       "gtk_file_selection_set_filename")
    :accessor file-selection-filename
    :initarg :filename
    :type string)
    (action-area
     :allocation :virtual
     :location "gtk_file_selection_get_action_area"
     :reader file-selection-action-area
     :type widget)
    (ok-button
     :allocation :virtual
     :location "gtk_file_selection_get_ok_button"
     :reader file-selection-ok-button
     :type widget)
    (cancel-button
     :allocation :virtual
     :location "gtk_file_selection_get_cancel_button"
     :reader file-selection-cancel-button
     :type widget))
  (:metaclass container-class)
  (:alien-name "GtkFileSelection"))


(defclass scrolled-window (bin)
   (hscrollbar
    :allocation :virtual
    :location "gtk_scrolled_window_get_hscrollbar"
    :accessor scrolled-window-hscrollbar
    :type widget)
   (vscrollbar
    :allocation :virtual
    :location "gtk_scrolled_window_get_vscrollbar"
    :accessor scrolled-window-vscrollbar
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkScrolledWindow"))


(defclass button-box (box)
  ((spacing
    :allocation :virtual
    :location ("gtk_button_box_get_spacing" "gtk_button_box_set_spacing")
    :accessor button-box-spacing
    :initarg :spacing
    :type int)
   (child-min-width
    :allocation :alien
    :offset #.(size-of 'int)
    :accessor button-box-child-min-width
    :initarg :child-min-width
    :type int)
   (child-min-height
    :allocation :alien
    :accessor button-box-child-min-height
    :initarg :child-min-height
    :type int)
   (child-ipad-x
    :allocation :alien
    :accessor button-box-child-ipad-x
    :initarg :child-ipad-x
    :type int)
   (child-ipad-y
    :allocation :alien
    :accessor button-box-child-ipad-y
    :initarg :child-ipad-y
    :type int)
   (layout
    :allocation :virtual
    :location ("gtk_button_box_get_layout" "gtk_button_box_set_layout")
    :accessor button-box-layout
    :initarg :layout
    :type button-box-style))
  (:metaclass container-class)
  (:alien-name "GtkButtonBox"))



(defclass color-selection (vbox)
  ((use-opacity
    :allocation :virtual
    :location ("gtk_color_selection_get_use_opacity"
	       "gtk_color_selection_set_use_opacity")
    :accessor color-selection-use-opacity-p
    :initarg :use-opacity
    :type boolean)
   (use-palette
    :allocation :virtual
    :location ("gtk_color_selection_get_use_palette"
	       "gtk_color_selection_set_use_palette")
    :accessor color-selection-use-palette-p
    :initarg :use-palette
    :type boolean)
   (color
    :allocation :virtual
    :location color-selection-color
    :initarg :color)
   (old-color
    :allocation :virtual
    :location color-selection-old-color
    :initarg :old-color
    :type (vector double-float 4)))
  (:metaclass container-class)
  (:alien-name "GtkColorSelection"))




(defclass paned (container)
   (position
    :allocation :virtual
    :location ("gtk_paned_get_position" "gtk_paned_set_position")
    :accessor paned-position
    :initarg :position
    :type int)
   (child1
    :allocation :virtual
    :location paned-child1
    :initarg :child1
    :type widget)
   (child2
    :allocation :virtual
    :location paned-child2
    :initarg :child2
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkPaned"))


(defclass layout (container)
  ((hadjustment
    :allocation :virtual
    :location ("gtk_layout_get_hadjustment" "gtk_layout_set_hadjustment")
    :accessor layout-hadjustment
    :initarg :hadjustment
    :type adjustment)
   (vadjustment
    :allocation :virtual
    :location ("gtk_layout_get_vadjustment" "gtk_layout_set_vadjustment")
    :accessor layout-vadjustment
    :initarg :vadjustment
    :type adjustment)
   (x-size
    :allocation :virtual
    :location layout-x-size
    :initarg :x-size)
   (y-size
    :allocation :virtual
    :location layout-y-size
    :initarg :y-size)
   (x-offset
    :allocation :alien
    :offset #.(+ (size-of 'pointer) (* (size-of 'int) 2))
    :accessor layout-x-offset
    :initarg :x-offset
    :type unsigned-int)
   (y-offset
    :allocation :alien
    :accessor layout-y-offset
    :initarg :y-offset
    :type unsigned-int))
  (:metaclass container-class)
  (:alien-name "GtkLayout"))



(defclass menu (menu-shell)
  ((accel-group
    :allocation :virtual
    :location ("gtk_menu_get_accel_group" "gtk_menu_set_accel_group")
    :accessor menu-accel-group
    :initarg :accel-group
    :type accel-group)
   (tornoff
    :allocation :virtual
    :location ("gtk_menu_get_tearoff_state" "gtk_menu_set_tearoff_state")
    :accessor menu-tornoff-p
    :initarg :tearoff
    :type boolean)
   (title
    :allocation :virtual
    :location ("gtk_menu_get_title" "gtk_menu_set_title")
    :accessor menu-title
    :initarg :title
    :type string))
  (:metaclass container-class)
  (:alien-name "GtkMenu"))


(defclass table-child (container-child)
   (x-expand
    :allocation :virtual
    :location table-child-x-expand-p
    :initarg :x-expand
    :type boolean)   
   (y-expand
    :allocation :virtual
    :location table-child-y-expand-p
    :initarg :y-expand
    :type boolean)
   (x-shrink
    :allocation :virtual
    :location table-child-x-shrink-p
    :initarg :x-shrink
    :type boolean)   
   (y-shrink
    :allocation :virtual
    :location table-child-y-shrink-p
    :initarg :y-shrink
    :type boolean)   
   (x-fill
    :allocation :virtual
    :location table-child-x-fill-p
    :initarg :x-fill
    :type boolean)   
   (y-fill
    :allocation :virtual
    :location table-child-y-fill-p
    :initarg :y-fill
    :type boolean))
  (:metaclass child-class))
  

(defclass toolbar (container)
   (tooltips
    :allocation :virtual
    :location ("gtk_toolbar_get_tooltips" "gtk_toolbar_set_tooltips")
    :accessor toolbar-tooltips-p
    :initarg :tooltips
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkToolbar"))



(defclass combo (hbox)
  ((entry
    :allocation :virtual
    :location "gtk_combo_get_entry"
    :reader combo-entry
    :type entry))
  (:metaclass widget-class)
  (:alien-name "GtkCombo"))
  

(defclass ruler (widget)
   (metric
    :allocation :virtual
    :location (nil "gtk_ruler_set_metric")
    :accessor ruler-metric
    :initarg :metric
    :type metric-type))
  (:metaclass widget-class)
  (:alien-name "GtkRuler"))

(defclass scale (range)
   (value-width
    :allocation :virtual
    :location "gtk_scale_get_value_width"
    :reader ruler-value-width
    :type int))
  (:metaclass widget-class)
  (:alien-name "GtkScale"))



(defclass progress (widget)
   (format-string
    :allocation :virtual
    :location ("gtk_progress_get_format_string"
	       "gtk_progress_set_format_string")
    :accessor progress-format-string
    :initarg :format-string
    :type string)
   (adjustment
    :allocation :virtual
    :location ("gtk_progress_get_adjustment"
	       "gtk_progress_set_adjustment")
    :accessor progress-adjustment
    :initarg :adjustment
    :type adjustment)
   (percentage
    :allocation :virtual
    :location ("gtk_progress_get_current_percentage"
	       "gtk_progress_set_percentage")
    :accessor progress-percentage
    :initarg :percentage
    :type single-float)
   (value
    :allocation :virtual
    :location ("gtk_progress_get_value" "gtk_progress_set_value")
    :accessor progress-value
    :initarg :value
    :type single-float)
  (:metaclass widget-class)
  (:alien-name "GtkProgress"))
  

(defclass progress-bar (progress)
   (activity-blocks ;; deprecated
    :allocation :param
    :accessor progress-bar-activity-blocks
    :initarg :activity-blocks
    :type unsigned-int)
  (:metaclass widget-class)
  (:alien-name "GtkProgressBar"))


|#
