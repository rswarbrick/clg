;; Common Lisp bindings for GTK+ v2.0.x
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

;; $Id: gtktypes.lisp,v 1.5 2001-01-28 14:21:04 espen Exp $



(in-package "GTK")


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


(defclass accel-group (alien-object)
  ()
  (:metaclass alien-class)
  (:alien-name "GtkAccelGroup"))

(deftype accel-entry () 'pointer) ; internal?



;; Forward declaration of widget and container
(defclass widget (object)
  ()
  (:metaclass object-class)
  (:alien-name "GtkWidget"))

(defclass container (widget)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkContainer"))



(defclass data (object)
  ()
  (:metaclass object-class)
  (:alien-name "GtkData"))


(defclass adjustment (data)
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
  (:metaclass object-class)
  (:alien-name "GtkAdjustment"))
  

(defclass tooltips (data)
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
   (name
    :allocation :param
    :accessor widget-name
    :initarg :name
    :type string)
   (parent
    :allocation :param
    :accessor widget-parent
;   :initarg :parent
    :type container)
   (x
    :allocation :param
    :accessor widget-x-position
    :initarg :x
    :type int)
   (y
    :allocation :param
    :accessor widget-y-position
    :initarg :y
    :type int)
   (width
    :allocation :param
    :accessor widget-width
    :initarg :width
    :type int)
   (height
    :allocation :param
    :accessor widget-height
    :initarg :height
    :type int)
   (visible
    :allocation :param
    :accessor widget-visible-p
    :initarg :visible
    :type boolean)
   (sensitive
    :allocation :param
    :accessor widget-sensitive-p
    :initarg :sensitive
    :type boolean)
   (app-paintable
    :allocation :param
    :reader widget-app-paintable-p
    :type boolean)
   (can-focus
    :allocation :param
    :accessor widget-can-focus-p
    :initarg :can-focus
    :type boolean)
   (has-focus
    :allocation :param
    :accessor widget-has-focus-p
    :initarg :has-focus
    :type boolean)
   (can-default
    :allocation :param
    :accessor widget-can-default-p
    :initarg :can-default
    :type boolean)
   (has-default
    :allocation :param
    :accessor widget-has-default-p
    :initarg :has-default
    :type boolean)
   (receives-default
    :allocation :param
    :accessor widget-receives-default-p
    :initarg :receives-default
    :type boolean)
   (composite-child
    :allocation :param
    :accessor widget-composite-child-p
    :initarg :composite-child
    :type boolean)
   (style
    :allocation :param
    :accessor widget-style
    :initarg :style
    :type style)
   (events
    :allocation :param
    :accessor widget-events
    :initarg :events
    :type gdk:event-mask)
   (extension-events
    :allocation :param
    :accessor widget-extension-events
    :initarg :extpension-events
    :type gdk:event-mask)
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


(defclass misc (widget)
  ((xalign
    :allocation :param
    :accessor misc-xalign
    :initarg :xalign
    :type single-float)
   (yalign
    :allocation :param
    :accessor misc-yalign
    :initarg :yalign
    :type single-float)
   (xpad
    :allocation :param
    :accessor misc-xpad
    :initarg :xpad
    :type int)
   (ypad
    :allocation :param
    :accessor misc-ypad
    :initarg :ypad
    :type int))
  (:metaclass widget-class)
  (:alien-name "GtkMisc"))


(defclass label (misc)
  ((label
    :allocation :param
    :accessor label-label
    :initarg :label
    :type string)
   (pattern
    :allocation :param
    :accessor label-pattern
    :initarg :pattern
    :type string)
   (justify
    :allocation :param
    :accessor label-justify
    :initarg :justify
    :type justification)
   (wrap
    :allocation :param
    :accessor label-line-wrap-p
    :initarg :wrap
    :type boolean))
  (:metaclass widget-class)
  (:alien-name "GtkLabel"))


(defclass accel-label (label)
  ((widget
    :allocation :param
    :location "GtkAccelLabel::accel_widget"
    :accessor accel-label-accel-widget
    :initarg :accel-widget
    :type widget)
   (width
    :allocation :virtual
    :location "gtk_accel_label_get_accel_width"
    :reader width
    :type unsigned-int))
  (:metaclass widget-class)
  (:alien-name "GtkAccelLabel"))


(defclass tips-query (label)
  ((emit-always
    :allocation :param
    :accessor tips-query-emit-always-p
    :initarg :emit-always
    :type boolean)
   (caller
    :allocation :param
    :accessor tips-query-caller
    :initarg :caller
    :type widget)
   (label-inactive
    :allocation :param
    :accessor tips-query-label-inactive
    :initarg :label-inactive
    :type string)
   (label-no-tip
    :allocation :param
    :accessor tips-query-label-no-tip
    :initarg :label-no-tip
    :type string))
  (:metaclass widget-class)
  (:alien-name "GtkTipsQuery"))


(defclass arrow (misc)
  ((arrow-type
    :allocation :param
    :accessor arrow-arrow-type
    :initarg :arrow-type
    :type arrow-type)
   (shadow-type
    :allocation :param
    :accessor arrow-shadow-type
    :initarg :shadow-type
    :type shadow-type))
  (:metaclass widget-class)
  (:alien-name "GtkArrow"))


(defclass image (misc)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkImage"))


(defclass pixmap (misc)
  ((source
    :allocation :virtual
    :location pixmap-source)
   (mask
    :allocation :virtual
    :location pixmap-mask
    :type gdk:bitmap))
  (:metaclass widget-class)
  (:alien-name "GtkPixmap"))


(defclass container (widget)
  ((border-width
    :allocation :param
    :accessor container-border-width
    :initarg :border-width
    :type unsigned-long)
   (resize-mode
    :allocation :param
    :accessor container-resize-mode
    :initarg :resize-mode
    :type resize-mode)
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

(defclass container-child ()
  ((parent
    :initarg :parent
    :type container)
   (child
    :initarg :child
    :type widget)))


(defclass bin (container)
  ((child
    :allocation :virtual
    :location bin-child
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkBin"))

(defclass bin-child (container-child))


(defclass alignment (bin)
  ((xalign
    :allocation :param
    :accessor alignment-xalign
    :initarg :xalign
    :type single-float)
   (yalign
    :allocation :param
    :accessor alignment-yalign
    :initarg :yalign
    :type single-float)
   (xscale
    :allocation :param
    :accessor alignment-xscale
    :initarg :xscale
    :type single-float)
   (yscale
    :allocation :param
    :accessor alignment-yscale
    :initarg :yscale
    :type single-float))
  (:metaclass container-class)
  (:alien-name "GtkAlignment"))

(defclass alignment-child (bin-child))


(defclass frame (bin)
  ((label
    :allocation :virtual
    :location ("gtk_frame_get_label" "gtk_frame_set_label")
    :accessor frame-label
    :initarg :label
    :type string)
   (label-xalign
    :allocation :param
    :accessor frame-label-xalign
    :initarg :label-xalign
    :type single-float)
   (label-yalign
    :allocation :param
    :accessor frame-label-yalign
    :initarg :label-yalign
    :type single-float)
   (shadow-type
    :allocation :param
    :location "GtkFrame::shadow"
    :accessor frame-shadow-type
    :initarg :shadow-type
    :type shadow-type))
  (:metaclass container-class)
  (:alien-name "GtkFrame"))

(defclass frame-child (bin-child))
  

; (defclass aspect-frame (frame)
;   ((xalign
;     :allocation :param
;     :accessor aspect-frame-xalign
;     :initarg :xalign
;     :type single-float)
;    (yalign
;     :allocation :param
;     :accessor aspect-frame-yalign
;     :initarg :yalign
;     :type single-float)
;    (ratio
;     :allocation :param
;     :accessor aspect-frame-ratio
;     :initarg :ratio
;     :type single-float)
;    (obey-child
;     :allocation :param
;     :accessor aspect-frame-obey-child-p
;     :initarg :obey-child
;     :type boolean))
;   (:metaclass container-class)
;   (:alien-name "GtkAspectFrame"))

; (defclass aspect-frame-child (aspect-child))


(defclass button (bin)
  ((label
    :allocation :param
    :initarg :label
    :type string)
   (relief
    :allocation :param
    :accessor button-relief
    :initarg :relief
    :type relief-style))
  (:metaclass container-class)
  (:alien-name "GtkButton"))

(defclass button-child (bin-child)
  ()
  (:metaclass child-class))


(defclass toggle-button (button)
  ((active
    :allocation :param
    :accessor toggle-button-active-p
    :initarg :active
    :type boolean)
   (draw-indicator
    :allocation :param
    :accessor toggle-button-draw-indicator-p
    :initarg :draw-indicator
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkToggleButton"))

(defclass toggle-button-child (button-child)
  ()
  (:metaclass child-class))


(defclass check-button (toggle-button)
  ()
  (:metaclass container-class)
  (:alien-name "GtkCheckButton"))

(defclass check-button-child (toggle-button-child)
  ()
  (:metaclass child-class))


(defclass radio-button (check-button)
  ((group
    :allocation :virtual
    :location ("gtk_radio_button_group")
    :reader radio-button-group
    :type (static (gslist widget))))
  (:metaclass container-class)
  (:alien-name "GtkRadioButton"))

(defclass radio-button-child (check-button-child)
  ()
  (:metaclass child-class))
  

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

(defclass option-menu-child (button-child)
  ()
  (:metaclass child-class))


(defclass item (bin)
  ()
  (:metaclass container-class)
  (:alien-name "GtkItem"))

(defclass item-child (bin-child)
  ()
  (:metaclass child-class))

;; Forward declaration
(defclass menu-item (item)
  ()
  (:metaclass container-class)
  (:alien-name "GtkMenuItem"))

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
  
(defclass menu-item-child (item-child)
  ()
  (:metaclass child-class))

  
(defclass check-menu-item (menu-item)
  ((active
    :allocation :virtual
    :location ("gtk_check_menu_item_get_active"
	       "gtk_check_menu_item_set_active")
    :accessor check-menu-item-active-p
    :initarg :active
    :type boolean)
   (toggle-indicator
    :allocation :virtual
    :location ("gtk_check_menu_item_get_show_toggle"
	       "gtk_check_menu_item_set_show_toggle")
    :accessor check-menu-item-toggle-indicator-p
    :initarg :toggle-indicator
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkCheckMenuItem"))

(defclass check-menu-item-child (menu-item-child)
  ()
  (:metaclass child-class))


(defclass radio-menu-item (check-menu-item)
  ((group
    :allocation :virtual
    :location ("gtk_radio_menu_item_group")
    :reader radio-menu-item-group
    :type (static (gslist widget))))
  (:metaclass container-class)
  (:alien-name "GtkRadioMenuItem"))

(defclass radio-menu-item-child (check-menu-item-child)
  ()
  (:metaclass child-class))


(defclass tearoff-menu-item (menu-item)
  ()
  (:metaclass container-class)
  (:alien-name "GtkTearoffMenuItem"))

(defclass tearoff-menu-item-child (menu-item-child)
  ()
  (:metaclass child-class))


(defclass list-item (item)
  ()
  (:metaclass container-class)
  (:alien-name "GtkListItem"))

(defclass list-item-child (item-child)
  ()
  (:metaclass child-class))
  

;; deprecated
(defclass tree-item (item)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((subtree                :write-method :lisp :type tree-item)))
  (:metaclass container-class)
  (:alien-name "GtkTreeItem"))

(defclass tree-item-child (item-child)
  ()
  (:metaclass child-class))


(defclass window (bin)
  ((type
    :allocation :param
    :accessor window-type
    :initarg :type
    :type window-type)
   (title
    :allocation :param
    :accessor window-title
    :initarg :title
    :type string)
   (auto-shrink
    :allocation :param
    :accessor window-auto-shrink-p
    :initarg :auto-shrink
    :type boolean)
   (allow-shrink
    :allocation :param
    :accessor window-allow-shrink-p
    :initarg :allow-shrink
    :type boolean)
   (allow-grow
    :allocation :param
    :accessor window-allow-grow-p
    :initarg :allow-grow
    :type boolean)
   (modal
    :allocation :param
    :accessor window-modal-p
    :initarg :modal
    :type boolean)
   (position
    :allocation :param
    :location "GtkWindow::window_position"
    :accessor window-position
    :initarg :position
    :type window-position)
   (default-width
    :allocation :param
    :accessor window-default-width
    :initarg :default-width
    :type int)
   (default-height
    :allocation :param
    :accessor window-default-height
    :initarg :default-height
    :type int))
  (:metaclass container-class)
  (:alien-name "GtkWindow"))

(defclass window-child (bin-child)
  ()
  (:metaclass child-class))


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

(defclass dialog-child (window-child)
  ()
  (:metaclass child-class))


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

(defclass color-selection-dialog-child (dialog-child)
  ()
  (:metaclass child-class))


(defclass input-dialog (dialog)
  ()
  (:metaclass container-class)
  (:alien-name "GtkInputDialog"))

(defclass input-dialog-child (dialog-child)
  ()
  (:metaclass child-class))


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

(defclass file-selection-child (window-child)
  ()
  (:metaclass child-class))


; (defclass plug window)


(defclass event-box (bin)
  ()
  (:metaclass container-class)
  (:alien-name "GtkEventBox"))

(defclass event-box-child (bin-child)
  ()
  (:metaclass child-class))


(defclass handle-box (bin)
  ((shadow-type
    :allocation :param
    :location "GtkHandleBox::shadow"
    :accessor handle-box-shadow-type
    :initarg :shadow-type
    :type shadow-type)   
   (handle-position
    :allocation :param
    :accessor handle-box-handle-position
    :initarg :handle-position
    :type position-type)
   (snap-edge
    :allocation :param
    :accessor handle-box-snap-edge
    :initarg :snap-edge
    :type position-type))
  (:metaclass container-class)
  (:alien-name "GtkHandleBox"))

(defclass handle-box-child (bin-child)
  ()
  (:metaclass child-class))


(defclass scrolled-window (bin)
  ((hadjustment
    :allocation :param
    :accessor scrolled-window-hadjustment
    :initarg :hadjustment
    :type adjustment)   
   (vadjustment
    :allocation :param
    :accessor scrolled-window-vadjustment
    :initarg :vadjustment
    :type adjustment)
   (hscrollbar-policy
    :allocation :param
    :accessor scrolled-window-hscrollbar-policy
    :initarg :hscrollbar-policy
    :type policy-type)
   (vscrollbar-policy
    :allocation :param
    :accessor scrolled-window-vscrollbar-policy
    :initarg :vscrollbar-policy
    :type policy-type)
   (placement
    :allocation :param
    :location "GtkScrolledWindow::window_placement"
    :accessor scrolled-window-placement
    :initarg :placement
    :type corner-type)
   (shadow-type
    :allocation :param
    :location "GtkScrolledWindow::shadow"
    :accessor scrolled-window-shadow-type
    :initarg :shadow-type
    :type shadow-type)
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

(defclass scrolled-window-child (bin-child)
  ()
  (:metaclass child-class))


(defclass viewport (bin)
  ((hadjustment
    :allocation :param
    :accessor viewport-hadjustment
    :initarg :hadjustment
    :type adjustment)   
   (vadjustment
    :allocation :param
    :accessor viewport-vadjustment
    :initarg :vadjustment
    :type adjustment)
   (shadow-type
    :allocation :param
    :accessor viewport-shadow-type
    :initarg :shadow-type
    :type shadow-type))
  (:metaclass container-class)
  (:alien-name "GtkViewport"))

(defclass viewport-child (bin-child)
  ()
  (:metaclass child-class))
  

(defclass box (container)
  ((spacing
    :allocation :param
    :accessor box-spacing
    :initarg :spacing
    :type int)
   (homogeneous
    :allocation :param
    :accessor box-homogeneous-p
    :initarg :homogeneous
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkBox"))

(defclass box-child (container-child)
  ((expand
    :allocation :arg
    :accessor box-child-expand-p
    :initarg :expand
    :type boolean)
   (fill
    :allocation :arg
    :accessor box-child-fill-p
    :initarg :fill
    :type boolean)
   (padding
    :allocation :arg
    :accessor box-child-padding
    :initarg :padding
    :type unsigned-long)
   (pack-type
    :allocation :arg
    :accessor box-child-pack-type
    :initarg :pack-type
    :type pack-type)
   (position
    :allocation :arg
    :accessor box-child-position
    :initarg :position
    :type long))
  (:metaclass child-class))
   

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

(defclass button-box-child (box-child)
  ()
  (:metaclass child-class))


(defclass hbutton-box (button-box)
  ()
  (:metaclass container-class)
  (:alien-name "GtkHButtonBox"))

(defclass hbutton-box-child (button-box-child)
  ()
  (:metaclass child-class))


(defclass vbutton-box (button-box)
  ()
  (:metaclass container-class)
  (:alien-name "GtkVButtonBox"))

(defclass vbutton-box-child (button-box-child)
  ()
  (:metaclass child-class))


(defclass vbox (box)
  ()
  (:metaclass container-class)
  (:alien-name "GtkVBox"))

(defclass vbox-child (box-child)
  ()
  (:metaclass child-class))


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

(defclass color-selection-child (vbox-child)
  ()
  (:metaclass child-class))


(defclass hbox (box)
  ()
  (:metaclass container-class)
  (:alien-name "GtkHBox"))

(defclass hbox-child (box-child)
  ()
  (:metaclass child-class))


(defclass statusbar (hbox)
  ()
  (:metaclass container-class)
  (:alien-name "GtkStatusbar"))

(defclass statusbar-child (hbox-child)
  ()
  (:metaclass child-class))


(defclass fixed (container)
  ()
  (:metaclass container-class)
  (:alien-name "GtkFixed"))

(defclass fixed-child (container-child)
  ()
  (:metaclass child-class))


(defclass notebook (container)
  ((tab-pos
    :allocation :param
    :accessor notebook-tab-pos
    :initarg :tab-pos
    :type position-type)
   (show-tabs
    :allocation :param
    :accessor notebook-show-tabs-p
    :initarg :show-tabs
    :type boolean)
   (show-border
    :allocation :param
    :accessor notebook-show-border-p
    :initarg :show-border
    :type boolean)
   (scrollable
    :allocation :param
    :accessor notebook-scrollable-p
    :initarg :scrollable
    :type boolean)
   (tab-border
    :allocation :param
    :accessor notebook-tab-border
    :initarg :tab-border
    :type unsigned-int)
   (tab-hborder
    :allocation :param
    :accessor notebook-tab-hborder
    :initarg :tab-hborder
    :type unsigned-int)
   (tab-vborder
    :allocation :param
    :accessor notebook-tab-vborder
    :initarg :tab-vborder
    :type unsigned-int)
   (page
    :allocation :param
    :accessor notebook-page
    :initarg :page
    :type int)
   (enable-popup
    :allocation :param
    :accessor notebook-enable-popup-p
    :initarg :enable-popup
    :type boolean)
   (homogeneous
    :allocation :param
    :accessor notebook-homogeneous-p
    :initarg :homogeneous
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkNotebook"))

(defclass notebook-child (container-child)
  ((tab-label
    :allocation :arg
    :accessor notebook-child-tab-label
    :initarg :tab-label
    :type string)
   (menu-label
    :allocation :arg
    :accessor notebook-child-menu-label
    :initarg :menu-label
    :type string)
   (position
    :allocation :arg
    :accessor notebook-child-position
    :initarg :position
    :type int)
   (tab-fill
    :allocation :arg
    :accessor notebook-child-tab-fill-p
    :initarg :tab-fill
    :type boolean)
   (tab-pack
    :allocation :arg
    :accessor notebook-child-tab-pack-p
    :initarg :tab-pack
    :type boolean))
  (:metaclass child-class))


(defclass font-selection (notebook)
  ()
  (:metaclass container-class)
  (:alien-name "GtkFontSelection"))

(defclass font-selection-child (notebook-child)
  ()
  (:metaclass child-class))


(defclass paned (container)
  ((handle-size
    :allocation :param
    :accessor paned-handle-size
    :initarg handle-size
    :type unsigned-int)
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

(defclass paned-child (container-child)
  ()
  (:metaclass child-class))


(defclass hpaned (paned)
  ()
  (:metaclass container-class)
  (:alien-name "GtkHPaned"))

(defclass hpaned-child (paned-child)
  ()
  (:metaclass child-class))


(defclass vpaned (paned)
  ()
  (:metaclass container-class)
  (:alien-name "GtkVPaned"))

(defclass vpaned-child (paned-child)
  ()
  (:metaclass child-class))


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

(defclass layout-child (container-child)
  ()
  (:metaclass child-class))
  

; (defclass list-widget container
;   :c-name "GtkList"
;   :slots
;   ((selection-mode         :read-method :arg :accessor list-selection-mode
; 			   :c-writer "gtk_list_set_selection_mode"
; 			   :type selection-mode)))


(defclass menu-shell (container)
  ()
  (:metaclass container-class)
  (:alien-name "GtkMenuShell"))

(defclass menu-shell-child (container-child)
  ()
  (:metaclass child-class))


(defclass menu-bar (menu-shell)
  ((shadow-type
    :allocation :param
    :location "GtkMenuBar::shadow"
    :accessor menu-bar-shadow-type
    :initarg :shadow-type
    :type shadow-type))
  (:metaclass container-class)
  (:alien-name "GtkMenuBar"))

(defclass menu-bar-child (menu-shell-child)
  ()
  (:metaclass child-class))
  

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

(defclass menu-child (menu-shell-child)
  ()
  (:metaclass child-class))
  

(defclass packer (container)
  ((spacing
    :allocation :param
    :accessor packer-spacing
    :initarg :spacing
    :type unsigned-int)
   (default-border-width
    :allocation :param
    :accessor packer-default-border-width
    :initarg :default-border-width
    :type unsigned-int)
   (default-pad-x
    :allocation :param
    :accessor packer-default-pad-x
    :initarg :default-pad-x
    :type unsigned-int)
   (default-pad-y
    :allocation :param
    :accessor packer-default-pad-y
    :initarg :default-pad-y
    :type unsigned-int)
   (default-ipad-x
    :allocation :param
    :accessor packer-default-ipad-x
    :initarg :default-ipad-x
    :type unsigned-int)
   (default-ipad-y
    :allocation :param
    :accessor packer-default-ipad-y
    :initarg :default-ipad-y
    :type unsigned-int))
  (:metaclass container-class)
  (:alien-name "GtkPacker"))

(defclass packer-child (container-child)
  ((side
    :allocation :arg
    :accessor packer-child-side
    :initarg :side
    :type side-type)
   (anchor
    :allocation :arg
    :accessor packer-child-anchor
    :initarg :anchor
    :type anchor-type)
   (expand
    :allocation :arg
    :accessor packer-child-expand-p
    :initarg :expand
    :type boolean)
   (fill-x
    :allocation :arg
    :accessor packer-child-fill-x-p
    :initarg :fill-x
    :type boolean)
   (fill-y
    :allocation :arg
    :accessor packer-child-fill-y-p
    :initarg :fill-y    
    :type boolean)
   (use-default
    :allocation :arg
    :accessor packer-child-use-default
    :initarg :default
    :type boolean)
   (border-width
    :allocation :arg
    :accessor packer-child-border-width
    :initarg :default    
    :type unsigned-int)
   (pad-x
    :allocation :arg
    :accessor packer-child-pad-x
    :initarg :pad-x
    :type unsigned-int)
   (pad-y
    :allocation :arg
    :accessor packer-child-pad-y
    :initarg :pad-y
    :type unsigned-int)
   (ipad-x
    :allocation :arg
    :accessor packer-child-ipad-x
    :initarg :ipad-x
    :type unsigned-int)
   (ipad-y
    :allocation :arg
    :accessor packer-child-ipad-y
    :initarg :ipad-y
    :type unsigned-int)
   (position
    :allocation :arg
    :accessor packer-child-position
    :initarg :iposition
    :type long))
  (:metaclass child-class))


;(defclass socket (container))


(defclass table (container)
  ((rows
    :allocation :param
    :location "GtkTable::n_rows"
    :accessor table-rows
    :initarg :rows
    :type unsigned-int)
   (columns
    :allocation :param
    :location "GtkTable::n_columns"
    :accessor table-columns
    :initarg :columns
    :type unsigned-int)
   (row-spacing
    :allocation :param
    :accessor table-default-row-spacing
    :initarg :row-spacing
    :type unsigned-int)
   (column-spacing
    :allocation :param
    :accessor table-default-column-spacing
    :initarg :column-spacing
    :type unsigned-int)
   (homogeneous
    :allocation :param
    :accessor table-homogeneous-p
    :initarg :homogeneous
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkTable"))

(defclass table-child (container-child)
  ((left-attach
    :allocation :arg
    :accessor table-child-left-attach
    :initarg :left-attach
    :type unsigned-int)
   (right-attach
    :allocation :arg
    :accessor table-child-right-attach
    :initarg :right-attach
    :type unsigned-int)
   (top-attach
    :allocation :arg
    :accessor table-child-top-attach
    :initarg :top-attach
    :type unsigned-int)
   (bottom-attach
    :allocation :arg
    :accessor table-child-bottom-attach
    :initarg :bottom-attach
    :type unsigned-int)
   (x-options
    :allocation :arg
    :accessor table-child-x-options
    :initarg :x-options
    :type attach-options)
   (y-options
    :allocation :arg
    :accessor table-child-y-options
    :initarg :y-options
    :type attach-options)
   (x-padding
    :allocation :arg
    :accessor table-child-x-padding
    :initarg :x-padding
    :type unsigned-int)
   (y-padding
    :allocation :arg
    :accessor table-child-y-padding
    :initarg :y-padding
    :type unsigned-int)
   
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
  ((orientation
    :allocation :param
    :accessor toolbar-orientation
    :initarg :orientation
    :type orientation)
   (toolbar-style
    :allocation :param
    :accessor toolbar-style
    :initarg :toolbar-style
    :type toolbar-style)
   (space-size
    :allocation :param
    :accessor toolbar-space-size
    :initarg :space-size
    :type unsigned-int)
   (space-style
    :allocation :param
    :accessor toolbar-space-style
    :initarg :space-style
    :type toolbar-space-style)
   (relief
    :allocation :param
    :accessor toolbar-relief
    :initarg :relief
    :type relief-style)
   (tooltips
    :allocation :virtual
    :location ("gtk_toolbar_get_tooltips" "gtk_toolbar_set_tooltips")
    :accessor toolbar-tooltips-p
    :initarg :tooltips
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkToolbar"))

(defclass toolbar-child (container-child)
  ()
  (:metaclass child-class))


;; Deprecated
(defclass tree (container)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((selection-mode         :type selection-mode)
;    (view-mode              :type tree-view-mode)
;    (view-lines             :type boolean)
;    (root-tree              :read-only t :type tree)))
  (:metaclass container-class)
  (:alien-name "GtkTree"))


(defclass calendar (widget)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkCalendar"))


(defclass drawing-area (widget)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkDrawingArea"))

; (defclass curve drawing-area
;   :slots
;   ((curve-type             :read-method :arg :type curve-type)
;    (min-x                  :access-method :arg :type single-float)
;    (max-x                  :access-method :arg :type single-float)
;    (min-y                  :access-method :arg :type single-float)
;    (max-y                  :access-method :arg :type single-float)))

(defclass editable (widget)
  ((position
    :allocation :param
    :location "GtkEditable::text_position"
    :accessor ediatable-position
    :initarg :position
    :type int)
   (editable
    :allocation :param
    :accessor editable-editable-p
    :initarg :editable
    :type boolean)
   (text
    :allocation :virtual
    :location editable-text
    :initarg :text
    :type string))
  (:metaclass widget-class)
  (:alien-name "GtkEditable"))

  
(defclass entry (editable)
  ((max-length
    :allocation :param
    :accessor entry-max-length
    :initarg :max-length
    :type unsigned-int)
   (visible
    :allocation :param
    :location "GtkEntry::visibility"
    :accessor entry-visible-p
    :initarg :visible
    :type boolean))
  (:metaclass widget-class)
  (:alien-name "GtkEntry"))
  

(defclass combo (hbox)
  ((entry
    :allocation :virtual
    :location "gtk_combo_get_entry"
    :reader combo-entry
    :type entry)
   (use-arrows
    :allocation :virtual
    :location ("gtk_combo_get_use_arrows" "gtk_combo_set_use_arrows")
    :accessor combo-use-arrows-p
    :initarg :use-arrows
    :type boolean)
   (use-arrows-always
    :allocation :virtual
    :location
    ("gtk_combo_get_use_arrows_always" "gtk_combo_set_use_arrows_always")
    :accessor combo-use-arrows-always-p
    :initarg :use-arrows-always
    :type boolean)
   (case-sensitive
    :allocation :virtual
    :location ("gtk_combo_get_case_sensitive" "gtk_combo_set_case_sensitive")
    :accessor combo-case-sensitive-p
    :initarg :case-sensitive
    :type boolean))
  (:metaclass widget-class)
  (:alien-name "GtkCombo"))
  

(defclass spin-button (entry)
  ((adjustment
    :allocation :param
    :accessor spin-button-adjustment
    :initarg :adjustment
    :type adjustment)
   (climb-rate
    :allocation :param
    :accessor spin-button-climb-rate
    :initarg :climb-rate
    :type single-float)
   (digits
    :allocation :param
    :accessor spin-button-digits
    :initarg :digits
    :type unsigned-int)
   (snap-to-ticks
    :allocation :param
    :accessor spin-button-snap-to-ticks-p
    :initarg :snap-to-ticks
    :type boolean)
   (numeric
    :allocation :param
    :accessor spin-button-numeric-p
    :initarg :numeric
    :type boolean)
   (wrap
    :allocation :param
    :accessor spin-button-wrap-p
    :initarg :wrap
    :type boolean)
   (update-policy
    :allocation :param
    :accessor spin-button-update-policy
    :initarg :update-policy
    :type spin-button-update-policy)
   (shadow-type
    :allocation :param
    :accessor spin-button-shadow-type
    :initarg :shadow-type
    :type shadow-type)
   (value
    :allocation :param
    :accessor spin-button-value
    :initarg :value
    :type single-float))
  (:metaclass widget-class)
  (:alien-name "GtkSpinButton"))
  

;; Deprecated
; (defclass text editable
;   :slots
;   ((hadjustment            :access-method :arg :type adjustment)
;    (vadjustment            :access-method :arg :type adjustment)
;    (line-wrap              :read-method :arg :type boolean)
;    (word-wrap              :read-method :arg :type boolean)
;    ;; slots not accessible through the arg mechanism
;    (point                  :type unsigned-int)
;    (length                 :read-only t :type unsigned-int)))

(defclass ruler (widget)
  ((lower
    :allocation :param
    :accessor ruler-lower
    :initarg :lower
    :type single-float)
   (upper
    :allocation :param
    :accessor ruler-upper
    :initarg :upper
    :type single-float)
   (position
    :allocation :param
    :accessor ruler-position
    :initarg :position
    :type single-float)
   (max-size
    :allocation :param
    :accessor ruler-max-size
    :initarg :max-size
    :type single-float)
   (metric
    :allocation :virtual
    :location (nil "gtk_ruler_set_metric")
    :accessor ruler-metric
    :initarg :metric
    :type metric-type))
  (:metaclass widget-class)
  (:alien-name "GtkRuler"))


(defclass hruler (ruler)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkHRuler"))


(defclass vruler (ruler)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkVRuler"))


(defclass range (widget)
  ((update-policy
    :allocation :param
    :accessor range-update-policy
    :initarg :update-policy
    :type update-type)
   (adjustment
    :allocation :virtual
    :location ("gtk_range_get_adjustment" "gtk_range_set_adjustment")
    :accessor ruler-adjustment
    :initarg :adjustment
    :type adjustment))
  (:metaclass widget-class)
  (:alien-name "GtkRange"))


(defclass scale (range)
  ((digits
    :allocation :param
    :accessor scale-digits
    :initarg :digits
    :type int)
   (draw-value
    :allocation :param
    :accessor scale-draw-value-p
    :initarg :draw-value
    :type boolean)
   (value-position
    :allocation :param
    :location "GtkScale::value_pos"
    :accessor scale-value-position
    :initarg :value-position
    :type position-type)
   (value-width
    :allocation :virtual
    :location "gtk_scale_get_value_width"
    :reader ruler-value-width
    :type int))
  (:metaclass widget-class)
  (:alien-name "GtkScale"))


(defclass hscale (scale)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkHScale"))


(defclass vscale (scale)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkVScale"))


(defclass scrollbar (range)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkScrollbar"))


(defclass hscrollbar (scrollbar)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkHScrollbar"))


(defclass vscrollbar (scrollbar)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkVScrollbar"))


(defclass separator (widget)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkSeparator"))


(defclass hseparator (separator)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkHSeparator"))


(defclass vseparator (separator)
  ()
  (:metaclass widget-class)
  (:alien-name "GtkVSeparator"))


; (defclass preview widget
;   :slots
;   ((expand                 :read-method :arg :type boolean)))

(defclass progress (widget)
  ((activity-mode
    :allocation :param
    :accessor progress-activity-mode-p
    :initarg :activity-mode
    :type boolean)   
   (show-text
    :allocation :param
    :accessor progress-show-text-p
    :initarg :show-text
    :type boolean)
   (text-xalign
    :allocation :param
    :accessor progress-text-xalign
    :initarg :text-xalign
    :type single-float)
   (text-yalign
    :allocation :param
    :accessor progress-text-yalign
    :initarg :text-yalign
    :type single-float)
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
   (text
    :allocation :virtual
    :location ("gtk_progress_get_current_text")
    :reader progress-text
    :type string))
  (:metaclass widget-class)
  (:alien-name "GtkProgress"))
  

(defclass progress-bar (progress)
  ((orientation
    :allocation :param
    :accessor progress-bar-orientation
    :initarg :orientation
    :type progress-bar-orientation)
   (bar-style
    :allocation :param
    :accessor progress-bar-style
    :initarg :bar-style
    :type progress-bar-style)
   (activity-step
    :allocation :param
    :accessor progress-bar-activity-step
    :initarg :activity-step
    :type unsigned-int)
   (activity-blocks
    :allocation :param
    :accessor progress-bar-activity-blocks
    :initarg :activity-blocks
    :type unsigned-int)
   (discrete-blocks
    :allocation :param
    :accessor progress-bar-discrete-blocks
    :initarg :discrete-blocks
    :type unsigned-int)
   (fraction
    :allocation :param
    :accessor progress-bar-fraction
    :initarg :fraction
    :type single-float)
   (pulse-step
    :allocation :param
    :accessor progress-bar-pulse-step
    :initarg :pulse-step
    :type single-float)
   (text
    :allocation :virtual
    :location ("gtk_progress_get_current_text" "gtk_progress_bar_set_text")
    :accessor progress-bar-text
    :type string))
  (:metaclass widget-class)
  (:alien-name "GtkProgressBar"))

; (defclass item-factory object)

