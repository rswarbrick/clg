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

;; $Id: gtktypes.lisp,v 1.25 2004-12-20 22:43:26 espen Exp $


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
  (:metaclass boxed-class))


(defclass allocation (struct)
  ((x
    :allocation :alien
    :accessor allocation-width
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor allocation-width
    :initarg :width
    :type int)
   (width
    :allocation :alien
    :accessor allocation-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor allocation-height
    :initarg :height
    :type int))
  (:metaclass struct-class))

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
  (:metaclass boxed-class))

(defclass stock-item (struct)
  ((id
    :allocation :alien
    :accessor stock-item-id
    :initarg :id
    :type string)
   (label
    :allocation :alien
    :accessor stock-item-label
    :initarg :label
    :type string)
   (modifier
    :allocation :alien
    :accessor stock-item-modifier
    :initarg :modifier
    :type gdk:modifier-type)
   (keyval
    :allocation :alien
    :accessor stock-item-keyval
    :initarg :keyval
    :type int)
   (translation-domain
    :allocation :alien
    :accessor stock-item-translation-domain
    :initarg :translation-domain
    :type string))
  (:metaclass struct-class))

;; We don't really need to access any of these slots, but we need to
;; specify the size of the struct somehow 
(defclass tree-iter (boxed)
  ((stamp :allocation :alien :type int)
   (user-data :allocation :alien :type pointer)
   (user-data2 :allocation :alien :type pointer)
   (user-data3 :allocation :alien :type pointer))
  (:metaclass boxed-class))


;; (defclass tree-path (boxed)
;;   ((depth :allocation :alien :type int)
;;    (indices  :allocation :alien :type pointer))
;;   (:metaclass boxed-class))

(deftype tree-path () '(vector integer))
(register-type 'tree-path "GtkTreePath")


(define-types-by-introspection "Gtk"
  ;; Manually defined
  ("GtkObject" :ignore t)
  ("GtkRequisition" :ignore t)
  ("GtkBorder" :ignore t)
  ("GtkTreeIter" :ignore t)
  ("GtkTreePath" :ignore t)
;  ("GtkStyle" :ignore t)

  ;; Manual override
  ("GtkWidget"
   :slots
   ((child-properties
     :allocation :instance
     :accessor widget-child-properties
     :type container-child)
    (window
     :allocation :virtual
     :getter "gtk_widget_get_window"
     :unbound nil
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
     :setter "gtk_widget_set_colormap"
     :unbound nil
     :initarg :colormap
     :accessor widget-colormap
     :type gdk:colormap)
    (visual
     :allocation :virtual
     :getter "gtk_widget_get_visual"
     :unbound nil
     :reader widget-visual
     :type gdk:visual)
    (direction
     :allocation :virtual
     :getter "gtk_widget_get_direction"
     :setter "gtk_widget_set_direction"
     :accessor widget-direction
     :initarg :direction
     :type text-direction)
    (composite-name
     :allocation :virtual
     :getter "gtk_widget_get_composite_name"
     :setter "gtk_widget_set_composite_name"
     :accessor widget-composite-name
     :initarg :composite-name
     :type string)
    (settings
     :allocation :virtual
     :getter "gtk_widget_get_settings"
     :unbound nil
     :accessor widget-settings
     :type settings)
    (child-visible
     :allocation :virtual
     :getter "gtk_widget_get_child_visible"
     :setter "gtk_widget_set_child_visible"
     :accessor widget-child-visible-p
     :initarg :child-visible
     :type boolean)
    (width-request
     :merge t :unbound -1)
    (height-request
     :merge t :unbound -1)))
     
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
    (focus-chain
     :allocation :virtual
     :getter container-focus-chain
     :setter (setf container-focus-chain))
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

  ("GtkWindow"
   :slots
   ((gravity
     :allocation :virtual
     :getter "gtk_window_get_gravity"
     :setter "gtk_window_set_gravity"
     :accessor window-gravity
     :initarg :gravity
     :type gdk:gravity)
    (focus-widget
     :allocation :virtual
     :getter "gtk_window_get_focus"
     :setter "gtk_window_set_focus"
     :accessor window-focus-widget
     :initarg :focus-widget
     :type widget)
    (default-widget
     :allocation :virtual
     :getter "gtk_window_get_default"
     :setter "gtk_window_set_default"
     :accessor window-default-widget
     :initarg :default-widget
     :type widget)
    (decorated
     :allocation :virtual
     :getter "gtk_window_get_decorated"
     :setter "gtk_window_set_decorated"
     :accessor window-decorated-p
     :initarg :decorated
     :type boolean)
    (has-frame
     :allocation :virtual
     :getter "gtk_window_get_has_frame"
     :setter "gtk_window_set_has_frame"
     :accessor window-has-frame-p
     :initarg :has-frame
     :type boolean)
    (role
     :allocation :virtual
     :getter "gtk_window_get_role"
     :setter "gtk_window_set_role"
     :accessor window-role
     :initarg :role
     :type string)
    (type-hint
     :allocation :virtual
     :getter "gtk_window_get_type_hint"
     :setter "gtk_window_set_type_hint"
     :accessor window-type-hint
     :initarg :type-hint
     :type gdk:window-type-hint)
    (icon
     :allocation :virtual
     :getter window-icon
     :setter (setf window-icon)
     :initarg :icon)
    (mnemonic-modifier
     :allocation :virtual
     :getter "gtk_window_get_mnemonic_modifier"
     :setter "gtk_window_set_mnemonic_modifier"
     :accessor window-mnemonic-modifier
     :initarg :mnemonic-modifier
     :type gdk:modifier-type)
    (transient-for
     :allocation :virtual
     :getter "gtk_window_get_transient_for"
     :setter "gtk_window_set_transient_for"
     :accessor window-transient-for
     :initarg :transient-for
     :type window)))
  
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
     :type menu)
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
     :getter menu-item-label
     :setter (setf menu-item-label)
     :initarg :label
     :type string)
    (right-justified
     :allocation :virtual
     :getter "gtk_menu_item_get_right_justified"
     :setter "gtk_menu_item_set_right_justified"
     :accessor menu-item-right-justified-p
     :initarg :right-justified
     :type boolean)
    (submenu
     :allocation :virtual
     :getter "gtk_menu_item_get_submenu"
     :setter (setf menu-item-submenu)
     :reader menu-item-submenu
     :initarg :submenu
     :type menu-item)))

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
   :slots
   ((child1
    :allocation :virtual
    :getter "gtk_paned_get_child1"
    :setter "gtk_paned_add1"
    :accessor paned-child1
    :initarg :child1
    :type widget)
   (child2
    :allocation :virtual
    :getter "gtk_paned_get_child2"
    :setter "gtk_paned_add2"
    :accessor paned-child2
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
    (title
     :allocation :virtual
     :getter "gtk_menu_get_title"
     :setter "gtk_menu_set_title"
     :accessor menu-title
     :initarg :title
     :type string)
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
     :type icon-size)
    (toolbar-style
     :allocation :property
     :pname "toolbar-style"
     :initarg :toolbar-style
     :accessor toolbar-style
     :type toolbar-style)))

  ("GtkNotebook"
   :slots
   ((current-page
     :allocation :virtual
     :getter notebook-current-page
     :setter (setf notebook-current-page)
     :initarg :current-page)
    (page :ignore t)))
  
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
  
  ("GtkEntryCompletion"
   :slots
   ((entry
     :allocation :virtual
     :getter "gtk_entry_completion_get_entry"
     :reader entry-completion-entry
     :type entry)
    (minimum-key-length
     :merge t :unbound -1)
    #+gtk2.6
    (text-column
     :merge t :unbound -1)))

  ("GtkRadioButton"
   :slots
   ((group
     :allocation :virtual
     :getter "gtk_radio_button_get_group"
     :reader radio-button-group
     :type (copy-of (gslist widget)))))

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
     :allocation :virtual
     :getter "gtk_layout_get_bin_window"
     :reader layout-bin-window
     :type gdk:window)))

  ("GtkFixed"
   :slots
   ((has-window
     :allocation :virtual
     :getter "gtk_fixed_get_has_window"
     :setter "gtk_fixed_set_has_window"
     :reader fixed-has-window-p
     :initarg :has-window
     :type boolean)))

  ("GtkRange"
   :slots
   ((value
     :allocation :virtual
     :getter "gtk_range_get_value"
     :setter "gtk_range_set_value"
     :initarg :value
     :accessor range-value
     :type double-float)
   (upper
     :allocation :virtual
     :getter range-upper
     :setter (setf range-upper)
     :initarg :upper)
   (lower
     :allocation :virtual
     :getter range-lower
     :setter (setf range-lower)
     :initarg :lower)
   (step-increment
     :allocation :virtual
     :getter range-step-increment
     :setter (setf range-step-increment)
     :initarg :step-increment)
   (page-increment
     :allocation :virtual
     :getter range-page-increment
     :setter (setf range-page-increment)
     :initarg :page-increment)))

  ("GtkImage"
   :slots
   ((file :ignore t)))
       
  ("GtkEditable"
   :slots
   ((editable
     :allocation :virtual
     :getter "gtk_editable_get_editable"
     :setter "gtk_editable_set_editable"
     :reader editable-editable-p
     :initarg :editable
     :type boolean)
    (position
     :allocation :virtual
     :getter "gtk_editable_get_position"
     :setter "gtk_editable_set_position"
     :reader editable-position
     :initarg :position
     :type int)
    (text
     :allocation :virtual
     :getter editable-text
     :setter (setf editable-text)
     :initarg text)))

  ("GtkFileChooser"
   :slots
   ((filename
     :allocation :virtual
     :getter "gtk_file_chooser_get_filename"
     :setter "gtk_file_chooser_set_filename"
     :accessor file-chooser-filename
     :initarg :filename
     :type string)
    (current-name
     :allocation :virtual
     :setter "gtk_file_chooser_set_current_name"
     :accessor file-choser-current-name
     :initarg :current-name
     :type string)
    (current-folder
     :allocation :virtual
     :setter "gtk_file_chooser_set_current_folder"
     :setter "gtk_file_chooser_get_current_folder"
     :accessor file-choser-current-folder
     :initarg :current-folder
     :type string)
    (uri
     :allocation :virtual
     :getter "gtk_file_chooser_get_uri"
     :setter "gtk_file_chooser_set_uri"
     :accessor file-choser-uri
     :initarg :uri
     :type string)
    (current-folder-uri
     :allocation :virtual
     :setter "gtk_file_chooser_set_current_folder_uri"
     :setter "gtk_file_chooser_get_current_folder_uri"
     :accessor file-choser-current-folder-uri
     :initarg :current-folder-uri
     :type string)))

  ("GtkTreeView"
   :slots
   ((columns
     :allocation :virtual
     :getter "gtk_tree_view_get_columns"
     :reader tree-view-columns 
     :type (glist tree-view-column))
    (selection
     :allocation :virtual
     :getter "gtk_tree_view_get_selection"
     :reader tree-view-selection
     :type tree-selection)))

  ("GtkTreeModel"
   :slots
   ((n-columns
     :allocation :virtual
     :getter "gtk_tree_model_get_n_columns"
     :reader tree-model-n-columns 
     :type int)))

  ("GtkTreeSelection"
   :slots
   ((mode
     :allocation :virtual
     :getter "gtk_tree_selection_get_mode"
     :setter "gtk_tree_selection_set_mode"
     :accessor tree-selection-mode
     :initarg :mode
     :type selection-mode)
    (tree-view
     :allocation :virtual
     :getter "gtk_tree_selection_get_mode"
     :reader tree-selection-mode
     :type tree-view)))

  ("GtkComboBox"
   :slots
   ((active-iter
     :allocation :virtual
     :getter "gtk_combo_box_get_active_iter"
     :setter "gtk_combo_box_set_active_iter"
     :accessor combo-box-active-iter 
     :type tree-iter)))

  ("GtkTextBuffer"
   :slots
   ((line-count
     :allocation :virtual
     :getter "gtk_text_buffer_get_line_count"
     :reader text-buffer-line-count
     :type int)
    (char-count
     :allocation :virtual
     :getter "gtk_text_buffer_get_char_count"
     :reader text-buffer-char-count
     :type int)
    (modified
     :allocation :virtual
     :getter "gtk_text_buffer_get_modified"
     :setter "gtk_text_buffer_set_modified"
     :accessor text-buffer-modifed-p
     :type boolean)))

  ("GtkTextView"
   :slots
   ((default-attributes
     :allocation :virtual
     :getter "gtk_text_view_get_default_attributes"
     :reader text-view-default-attributes
     :type text-attributes)))

  ("GtkTextTagTable"
   :slots
   ((size
     :allocation :virtual
     :getter "gtk_text_tag_table_get_size"
     :reader text-tag-table-size
     :type int)))

  ("GtkTextTag"
   :slots
   ((priority
     :allocation :virtual
     :getter "gtk_text_tag_get_priority"
     :setter "gtk_text_tag_set_priority"
     :accessor text-tag-priority
     :type int)
    (weight
     :merge t :type pango:weight)))

  ("GtkUIManager"
   :type ui-manager
   :slots
   ((action-groups
     :allocation :virtual
     :getter "gtk_ui_manager_get_action_groups"
     :reader ui-manager-action-groups
     :type (copy-of (glist action-group)))
    (accel-group
     :allocation :virtual
     :getter "gtk_ui_manager_get_accel_group"
     :reader ui-manager-accel-group
     :type accel-group)))

  ("GtkUIManagerItemType"
   :type ui-manager-item-type)

  ("GtkToggle"
   :slots
   ((accelerator
     :allocation :virtual
     :getter action-accelerator)))

  ("GtkToggleAction"
   :slots
   ((active
     :allocation :virtual
     :getter "gtk_toggle_action_get_active"
     :setter "gtk_toggle_action_set_active"
     :initarg :active
     :accessor toggle-action-active-p
     :type boolean)))

  ("GtkRadioAction"
   :slots
   ((group
     :allocation :virtual
     :getter "gtk_radio_button_get_group"
     :reader radio-button-group
     :type (copy-of (gslist widget)))
    (%value
     :allocation :property  :pname "value"
     :readable nil :type int)
    (value 
     :allocation :virtual
     :getter radio-action-value)))


  ;; Not needed
  ("GtkFundamentalType" :ignore t)
  ("GtkArgFlags" :ignore t)

  
  ;; Deprecated widgets
  ("GtkCList" :ignore-prefix t)
  ("GtkCTree" :ignore-prefix t)
  ("GtkList" :ignore t)
  ("GtkListItem" :ignore t)
  ("GtkTree" :ignore t)
  ("GtkTreeItem" :ignore t)
  ("GtkItemFactory" :ignore t)
  ("GtkText" :ignore t)
  ("GtkPacker" :ignore-prefix t)
  ("GtkPixmap" :ignore t)
  ("GtkPreview" :ignore-prefix t)
  ("GtkProgres" :ignore t)
  ("GtkTipsQuery" :ignore t)
  ("GtkOldEditable" :ignore t)
  ("GtkCombo" :ignore t)
  ("GtkOptionMenu" :ignore t)

  ;; What are these?
  ("GtkFileSystemModule" :ignore t)
  ("GtkIMModule" :ignore t)
  ("GtkThemeEngine" :ignore t)

  )


(defclass text-iter (boxed)
  ((buffer
    :allocation :virtual
    :getter "gtk_text_iter_get_buffer"
    :reader text-iter-buffer
    :type text-buffer)
   (offset
    :allocation :virtual
    :getter "gtk_text_iter_get_offset"
    :setter "gtk_text_iter_set_offset"
    :accessor text-iter-offset
    :type int)
   (line
    :allocation :virtual
    :getter "gtk_text_iter_get_line"
    :setter "gtk_text_iter_set_line"
    :accessor text-iter-line
    :type int)
   (line-offset
    :allocation :virtual
    :getter "gtk_text_iter_get_line_offset"
    :setter "gtk_text_iter_set_line_offset"
    :accessor text-iter-line-offset
    :type int)
   (line-index
    :allocation :virtual
    :getter "gtk_text_iter_get_line_index"
    :setter "gtk_text_iter_set_line_index"
    :accessor text-iter-line-index
    :type int)
   (visible-line-index
    :allocation :virtual
    :getter "gtk_text_iter_get_visible_line_index"
    :setter "gtk_text_iter_set_visible_line_index"
    :accessor text-iter-visible-line-index
    :type int)
   (visible-line-offset
    :allocation :virtual
    :getter "gtk_text_iter_get_visible_line_offset"
    :setter "gtk_text_iter_set_visible_line_offset"
    :accessor text-iter-visible-line-offset
    :type int)
   ;; Workaround to get correct size 
   (dummy14
     :allocation :alien :offset #.(* 13 (size-of 'pointer))
     :type pointer))
  (:metaclass boxed-class 
   ;; I am pretty sure this was working in older versons on CMUCL
   :size #.(* 14 (size-of 'pointer))))
