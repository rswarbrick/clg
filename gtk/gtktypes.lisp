;; Common Lisp bindings for GTK+ v2.0.x
;; Copyright (C) 1999-2001 Espen S. Johnsen <espen@users.sourceforge.org>
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

;; $Id: gtktypes.lisp,v 1.9 2001-11-12 22:32:17 espen Exp $


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
   (%value ; to get the offset right
    :allocation :alien
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
    :type single-float)
   (value
    :allocation :virtual
    :getter "gtk_adjustment_get_value"
    :setter "gtk_adjustment_set_value"
    :accessor adjustment-value
    :initarg :value
    :type single-float))
  (:metaclass gobject-class)
  (:alien-name "GtkAdjustment"))



(define-types-by-introspection "Gtk"
  ;; Manually defined
  ("GtkObject" :ignore t)
  ("GtkRequisition" :ignore t)
  ("GtkBorder" :ignore t)
  ("GtkAdjustment" :ignore t)

  
  ;; Manual override
  ("GtkWidget"
   :slots
   ((child-slots
    :allocation :instance
    :accessor widget-child-slots
    :type container-child)
    (parent
     :allocation :virtual
     :getter "gtk_widget_get_parent"
     :setter "gtk_widget_set_parent"
     :accessor widget-parent
     :type container
     :documentation "The parent widget of this widget. Must be a container widget.")
    (window
     :allocation :virtual
     :getter "gtk_widget_get_window"
     :reader widget-window
     :type gdk:window)
    (state
     :allocation :virtual
     :getter "gtk_widget_get_state"
     :setter "gtk_widget_set_state"
     :accessor widget-state
     :initarg :state
     :type state-type)
    (colormap
     :allocation :virtual
     :getter "gtk_widget_get_colormap"
     :reader widget-colormap
     :type gdk:colormap)
    (visual
     :allocation :virtual
     :getter "gtk_widget_get_visual"
     :reader widget-visual
     :type gdk:visual)))

  ("GtkContainer"
   :slots
   ((child
     :ignore t)
    (children
     :allocation :virtual
     :getter container-children
     :setter (setf container-children))
    (focus-child
     :allocation :virtual
     :getter "gtk_container_get_focus_child"
     :setter "gtk_container_set_focus_child"
     :accessor container-focus-child
     :initarg :focus-child
     :type widget)
    (focus-hadjustment
     :allocation :virtual
     :getter "gtk_container_get_focus_hadjustment"
     :setter "gtk_container_set_focus_hadjustment"
     :accessor container-focus-hadjustment
     :initarg :focus-hadjustment
     :type adjustment)
    (focus-vadjustment
     :allocation :virtual
     :getter "gtk_container_get_focus_vadjustment"
     :setter "gtk_container_set_focus_vadjustment"
     :accessor container-focus-vadjustment
     :initarg :focus-vadjustment
     :type adjustment)))
      
  ("GtkBin"
   :slots
   ((child
     :allocation :virtual
     :getter "gtk_bin_get_child"
     :setter (setf bin-child)
     :reader bin-child
     :type widget)))
  
  ("GtkTooltips"
   :slots
   ((enabled
     :allocation :virtual
     :getter "gtk_tooltips_get_enabled"
     :setter (setf tooltips-enabled-p)
     :reader tooltips-enabled-p
     :initarg :enabled
     :type boolean)))
  
  ("GtkOptionMenu"
   :slots
   ((menu
     :allocation :virtual
     :getter "gtk_option_menu_get_menu"
     :setter (setf option-menu-menu)
     :reader option-menu-menu
     :initarg :menu
     :type widget)
    (history
     :allocation :virtual
     :getter "gtk_option_menu_get_history"
     :setter "gtk_option_menu_set_history"
     :accessor option-menu-history
     :initarg :history
     :type unsigned-int)))

  ("GtkMenuItem"
   :slots
   ((label
     :allocation :virtual
     :setter menu-item-label
     :initarg :label
     :type string)
    (submenu
     :allocation :virtual
     :getter "gtk_menu_item_get_submenu"
     :setter (setf menu-item-submenu)
     :reader menu-item-submenu
     :initarg :submenu
     :type menu-item)
    (placement
     :allocation :virtual
     :getter "gtk_menu_item_get_placement"
     :setter "_gtk_menu_item_set_placement"  ; why underscore?
     :accessor menu-item-placement
     :initarg :placement
     :type submenu-placement)
    (submenu-indicator
     :allocation :virtual
     :getter "gtk_menu_item_get_show_submenu"
     :setter "gtk_menu_item_set_show_submenu"
     :accessor menu-item-submenu-indicator-p
     :initarg :submenu-indicator
     :type boolean)))

  ("GtkColorSelectionDialog"
   :slots
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
     :type widget)))

  ("GtkScrolledWindow"
   :slots
   ((hscrollbar
     :allocation :alien
     :reader scrolled-window-hscrollbar
     :type widget)
    (vscrollbar
     :allocation :alien
     :reader scrolled-window-vscrollbar
     :type widget)))

  ("GtkPaned"
   :slot
   ((child1
    :allocation :virtual
    :getter paned-child1
    :setter (setf paned-child1)
    :initarg :child1
    :type widget)
   (child2
    :allocation :virtual
    :getter paned-child2
    :setter (setf paned-child2)
    :initarg :child2
    :type widget)))

  ("GtkMenu"
   :slots
   ((accel-group
     :allocation :virtual
     :getter "gtk_menu_get_accel_group"
     :setter "gtk_menu_set_accel_group"
     :accessor menu-accel-group
     :initarg :accel-group
     :type accel-group)
    (active
     :allocation :virtual
     :getter "gtk_menu_get_active"
     :setter (setf menu-active)
     :reader menu-active
     :initarg :active
     :type widget)
    (tornoff
     :allocation :virtual
     :getter "gtk_menu_get_tearoff_state"
     :setter "gtk_menu_set_tearoff_state"
     :accessor menu-tornoff-p
     :initarg :tearoff
     :type boolean)))

  ("GtkToolbar"
   :slots
   ((tooltips
     :allocation :virtual
     :getter "gtk_toolbar_get_tooltips"
     :setter "gtk_toolbar_set_tooltips"
     :accessor toolbar-tooltips-p
     :initarg :tooltips
     :type boolean)
    (icon-size
     :allocation :virtual
     :getter "gtk_toolbar_get_icon_size"
     :setter "gtk_toolbar_set_icon_size"
     :accessor toolbar-icon-size
     :initarg :icon-size
     :type icon-size)))

  ("GtkRuler"
   :slots
   ((metric
     :allocation :virtual
     :getter "gtk_ruler_get_metric"
     :setter "gtk_ruler_set_metric"
     :accessor ruler-metric
     :initarg :metric
     :type metric-type)))

  ("GtkProgressBar"
   :slots
   ; deprecated properties
   ((bar-style :ignore t)
    (adjustment :ignore t)
    (activity-step :ignore t)
    (activity-blocks :ignore t)
    (discrete-blocks :ignore t)))

  ("GtkTable"
   :slots
   ((column-spacing
     :allocation :virtual
     :getter "gtk_table_get_default_col_spacing"
     :setter "gtk_table_set_col_spacings"
     :initarg :column-spacing
     :type unsigned-int)
    (row-spacing
     :allocation :virtual
     :getter "gtk_table_get_default_row_spacing"
     :setter "gtk_table_set_row_spacings"
     :initarg :row-spacing
     :type unsigned-int)))

  ("GtkDialog"
   :slots
   ((vbox
     :allocation :virtual
     :getter "gtk_dialog_get_vbox"
     :reader dialog-vbox
     :type widget)
    (action-area
     :allocation :virtual
     :getter "gtk_dialog_get_action_area"
     :reader dialog-action-area
     :type widget)))

  ("GtkCombo"
   :slots
   ((entry
     :allocation :virtual
     :getter "gtk_combo_get_entry"
     :reader combo-entry
     :type entry)))

  ("GtkRadioButton"
   :slots
   ((group
     :allocation :virtual
     :getter "gtk_radio_button_get_group"
     :reader radio-button-group
     :type (static (gslist widget)))))

  ("GtkRadioMenuItem"
   :slots
   ((group
     :allocation :virtual
     :getter "gtk_radio_menu_item_get_group"
     :reader radio-menu-item-group
     :type (static (gslist widget)))))

  ("GtkFileSelection"
   :slots
   ((action-area
     :allocation :virtual
     :getter "gtk_file_selection_get_action_area"
     :reader file-selection-action-area
     :type widget)
    (ok-button
     :allocation :virtual
     :getter "gtk_file_selection_get_ok_button"
     :reader file-selection-ok-button
     :type widget)
    (cancel-button
     :allocation :virtual
     :getter "gtk_file_selection_get_cancel_button"
     :reader file-selection-cancel-button
     :type widget)))

  ("GtkLayout"
   :slots
   ((bin-window
     :getter "gtk_layout_get_bin_window"
     :reader layout-bin-window
     :type gdk:window)
    (x-offset
     :getter "gtk_layout_get_xoffset"
     :setter "gtk_layout_set_xoffset"
     :accessor layout-x-offset
     :initarg :x-offset
     :type unsigned-int)
    (y-offset
     :getter "gtk_layout_get_yoffset"
     :setter "gtk_layout_set_yoffset"
     :accessor layout-y-offset
     :initarg :x-offset
     :type unsigned-int)))

     
  ;; Not needed
  ("GtkFundamentalType" :ignore t)
  ("GtkArgFlags" :ignore t)

  
  ;; Deprecated widgets
  ("GtkCList" :ignore-prefix t)
  ("GtkCTree" :ignore-prefix t)
  ("GtkList" :ignore-prefix t)
  ("GtkTree" :ignore t)
  ("GtkTreeItem" :ignore t)
  ("GtkText" :ignore-prefix t)
  ("GtkPacker" :ignore-prefix t)
  ("GtkPixmap" :ignore t)
  ("GtkPreview" :ignore-prefix t)
  ("GtkTipsQuery" :ignore t)
  ("GtkOldEditable" :ignore t))
