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

;; $Id: gtktypes.lisp,v 1.1 2000-08-14 16:44:59 espen Exp $



(in-package "GTK")


; (deftype color-type
;   (enum
;    :foreground
;    :background
;    :light
;    :dark
;    :mid
;    :text
;    :base
;    :white
;    :black))


(defclass  style (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GtkStyle"))

; (define-boxed accel-group :c-name "GtkAccelGroup")


(deftype (accel-group "GtkAccelGroup") () 'pointer)

(deftype accel-entry () 'pointer)
(deftype radio-button-group () 'pointer)
(deftype radio-menu-item-group () 'pointer)
; (define-boxed ctree-node :c-name "GtkCTreeNode")


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
  

; (define-class tooltips data
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((delay                  :type unsigned-int)))


;; Forward declaration, the real definition is in gtkwidget.lisp
(defclass widget (object)
  ()
  (:metaclass object-class)
  (:alien-name "GtkWidget"))


(defclass misc (widget)
  ((xalign
    :allocation :arg
    :accessor misc-xalign
    :initarg :xalign
    :type single-float)
   (yalign
    :allocation :arg
    :accessor misc-yalign
    :initarg :yalign
    :type single-float)
   (xpad
    :allocation :arg
    :accessor misc-xpad
    :initarg :xpad
    :type int)
   (ypad
    :allocation :arg
    :accessor misc-ypad
    :initarg :ypad
    :type int))
  (:metaclass widget-class)
  (:alien-name "GtkMisc"))


(defclass label (misc)
  ((label
    :allocation :arg
    :accessor label-label
    :initarg :label
    :type string)
   (pattern
    :allocation :arg
    :accessor label-pattern
    :initarg :pattern
    :type string)
   (justify
    :allocation :arg
    :accessor label-justify
    :initarg :justify
    :type justification)
   (wrap
    :allocation :arg
    :accessor label-line-wrap-p
    :initarg :wrap
    :type boolean))
  (:metaclass widget-class)
  (:alien-name "GtkLabel"))


(defclass accel-label (label)
  ((widget
    :allocation :arg
    :location "GtkAccelLabel::accel_widget"
    :accessor accel-widget
    :initarg :widget
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
    :allocation :arg
    :accessor tips-query-emit-always-p
    :initarg :emit-always
    :type boolean)
   (caller
    :allocation :arg
    :accessor tips-query-caller
    :initarg :caller
    :type widget)
   (label-inactive
    :allocation :arg
    :accessor tips-query-label-inactive
    :initarg :label-inactive
    :type string)
   (label-no-tip
    :allocation :arg
    :accessor tips-query-label-no-tip
    :initarg :label-no-tip
    :type string))
  (:metaclass widget-class)
  (:alien-name "GtkTipsQuery"))


(defclass arrow (misc)
  ((arrow-type
    :allocation :arg
    :accessor arrow-arrow-type
    :initarg :arrow-type
    :type arrow-type)
   (shadow-type
    :allocation :arg
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
  ()
  (:metaclass widget-class)
  (:alien-name "GtkPixmap"))


;; Forward declaration, the real definition is in gtkcontainer.lisp
(defclass container (widget)
  ()
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
    :initarg :child
    :type widget))
  (:metaclass container-class)
  (:alien-name "GtkBin"))

(defclass bin-child (container-child))


(defclass alignment (bin)
  ((xalign
    :allocation :arg
    :accessor alignment-xalign
    :initarg :xalign
    :type single-float)
   (yalign
    :allocation :arg
    :accessor alignment-yalign
    :initarg :yalign
    :type single-float)
   (xscale
    :allocation :arg
    :accessor alignment-xscale
    :initarg :xscale
    :type single-float)
   (yscale
    :allocation :arg
    :accessor alignment-yscale
    :initarg :yscale
    :type single-float))
  (:metaclass container-class)
  (:alien-name "GtkAlignment"))

(defclass alignment-child (bin-child))


(defclass frame (bin)
  ((label
    :allocation :arg
    :accessor frame-label
    :initarg :label
    :type string)
   (label-xalign
    :allocation :arg
    :accessor frame-label-xalign
    :initarg :label-xalign
    :type single-float)
   (label-yalign
    :allocation :arg
    :accessor frame-label-yalign
    :initarg :label-yalign
    :type single-float)
   (shadow-type
    :allocation :arg
    :location "GtkFrame::shadow"
    :accessor frame-shadow-type
    :initarg :shadow-type
    :type shadow-type))
  (:metaclass container-class)
  (:alien-name "GtkFrame"))

(defclass frame-child (bin-child))
  

; (defclass aspect-frame (frame)
;   ((xalign
;     :allocation :arg
;     :accessor aspect-frame-xalign
;     :initarg :xalign
;     :type single-float)
;    (yalign
;     :allocation :arg
;     :accessor aspect-frame-yalign
;     :initarg :yalign
;     :type single-float)
;    (ratio
;     :allocation :arg
;     :accessor aspect-frame-ratio
;     :initarg :ratio
;     :type single-float)
;    (obey-child
;     :allocation :arg
;     :accessor aspect-frame-obey-child-p
;     :initarg :obey-child
;     :type boolean))
;   (:metaclass container-class)
;   (:alien-name "GtkAspectFrame"))

; (defclass aspect-frame-child (aspect-child))


(defclass button (bin)
  ((label
    :allocation :arg
    :accessor button-label
    :initarg :label
    :type string)
   (relief
    :allocation :arg
    :accessor button-relief
    :initarg :relief
    :type relief-style))
  (:metaclass container-class)
  (:alien-name "GtkButton"))

(defclass button-child (bin-child))
  

(defclass toggle-button (button)
  ((active
    :allocation :arg
    :accessor toggle-button-active-p
    :initarg :active
    :type boolean)
   (draw-indicator
    :allocation :arg
    :accessor toggle-button-draw-indicator-p
    :initarg :draw-indicator
    :type boolean))
  (:metaclass container-class)
  (:alien-name "GtkToggleButton"))

(defclass toggle-button-child (button-child))


(defclass check-button (toggle-button)
  ()
  (:metaclass container-class)
  (:alien-name "GtkCheckButton"))

(defclass check-button-child (toggle-button-child))


(defclass radio-button (check-button)
  ((group
    :allocation :arg
    :initarg :group
;    :access :write-only
    :type pointer)) ;radio-button-group))
  (:metaclass container-class)
  (:alien-name "GtkRadioButton"))

(defclass radio-button-child (check-button-child))
  

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

(defclass option-menu-child (button-child))


(defclass item (bin)
  ()
  (:metaclass container-class)
  (:alien-name "GtkOptionMenu"))

(defclass item-child (bin-child))


(defclass menu-item (item)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((label                  :write-only t :access-method :lisp :type string)
;    (submenu                :write-method :lisp :type menu-item)
;    (placement              :write-only t :type submenu-placement)
;    (toggle-indicator       :c-reader "gtk_menu_item_get_show_toggle"
; 		           :write-method :lisp :type boolean)
;    (submenu-indicator      :c-reader "gtk_menu_item_get_show_submenu"
; 		           :write-method :lisp :type boolean)))
  (:metaclass container-class)
  (:alien-name "GtkMenuItem"))
  
(defclass menu-item-child (item-child))

  
(defclass check-menu-item (menu-item)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((active :type boolean)
;    (toggle-indicator       :c-writer "gtk_check_menu_item_set_show_toggle"
; 			   :c-reader "gtk_check_menu_item_get_show_toggle"
; 			   :type boolean)))
  (:metaclass container-class)
  (:alien-name "GtkCheckMenuItem"))

(defclass check-menu-item-child (menu-item-child))

(defclass radio-menu-item (check-menu-item)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((group                  :c-reader "gtk_radio_menu_item_group"
; 			   :type radio-menu-item-group)))
  (:metaclass container-class)
  (:alien-name "GtkRadioMenuItem"))

(defclass radio-menu-item-child (check-menu-item-child))


(defclass tearoff-menu-item (menu-item)
  ()
  (:metaclass container-class)
  (:alien-name "GtkTearoffMenuItem"))

(defclass tearoff-menu-item-child (menu-item-child))

(defclass list-item (item)
  ()
  (:metaclass container-class)
  (:alien-name "GtkListItem"))

(defclass list-item-child (item-child))
  

(defclass tree-item (item)
  ()
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((subtree                :write-method :lisp :type tree-item)))
  (:metaclass container-class)
  (:alien-name "GtkTreeItem"))

(defclass tree-item-child (item-child))


(defclass window (bin)
  ((type
    :allocation :arg
    :accessor window-type
    :initarg :type
    :type window-type)
   (title
    :allocation :arg
    :accessor window-title
    :initarg :title
    :type string)
   (auto-shrink
    :allocation :arg
    :accessor window-auto-shrink-p
    :initarg :auto-shrink
    :type boolean)
   (allow-shrink
    :allocation :arg
    :accessor window-allow-shrink-p
    :initarg :allow-shrink
    :type boolean)
   (allow-grow
    :allocation :arg
    :accessor window-allow-grow-p
    :initarg :allow-grow
    :type boolean)
   (modal
    :allocation :arg
    :accessor window-modal-p
    :initarg :modal
    :type boolean)
   (position
    :allocation :arg
    :location "GtkWindow::window_position"
    :accessor window-position
    :initarg :position
    :type window-position)
   (default-width
    :allocation :arg
    :accessor window-default-width
    :initarg :default-width
    :type int)
   (default-height
    :allocation :arg
    :accessor window-default-height
    :initarg :default-height
    :type int))
  (:metaclass container-class)
  (:alien-name "GtkWindow"))

(defclass window-child (bin-child))


; (defclass color-selection-dialog window
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((colorsel               :read-only t :type widget)
;    (main-vbox              :read-only t :type widget)
;    (ok-button              :read-only t :type widget)
;    (reset-button           :read-only t :type widget)
;    (cancel-button          :read-only t :type widget)
;    (help-button            :read-only t :type widget)))

; (defclass dialog window
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((action-area            :read-only t :type widget)
;    (vbox                   :read-only t :type widget)))

; (defclass input-dialog dialog)

; (defclass file-selection window
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((filename               :type string)
;    (action-area            :read-only t :type widget)
;    (ok-button              :read-only t :type widget)
;    (cancel-button          :read-only t :type widget)))

; (defclass plug window)

; (defclass event-box bin)

; (defclass handle-box bin
;   :slots
;   ((shadow-type            :read-method :arg :arg-name "shadow"
; 			   :type shadow-type)
;    (handle-position        :read-method :arg :type position-type)
;    (snap-edge              :read-method :arg :type position-type)))

(defclass scrolled-window (bin)
  ((hadjustment
    :allocation :arg
    :accessor scrolled-window-hadjustment
    :initarg :hadjustment
    :type adjustment)   
   (vadjustment
    :allocation :arg
    :accessor scrolled-window-vadjustment
    :initarg :vadjustment
    :type adjustment)
   (hscrollbar-policy
    :allocation :arg
    :accessor scrolled-window-hscrollbar-policy
    :initarg :hscrollbar-policy
    :type policy-type)
   (vscrollbar-policy
    :allocation :arg
    :accessor scrolled-window-vscrollbar-policy
    :initarg :vscrollbar-policy
    :type policy-type)
   (placement
    :allocation :arg
    :location "GtkScrolledWindow::window_placement"
    :accessor scrolled-window-placement
    :initarg :placement
    :type corner-type)
   (shadow-type
    :allocation :arg
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

(defclass scrolled-window-child (bin-child))



; (defclass viewport bin
;   :slots
;   ((hadjustment            :read-method :arg :type adjustment)
;    (vadjustment            :read-method :arg :type adjustment)
;    (shadow-type            :read-method :arg :type shadow-type)))

(defclass box (container)
  ((spacing
    :allocation :arg
    :accessor box-spacing
    :initarg :spacing
    :type int)
   (homogeneous
    :allocation :arg
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
    :type int)
   (layout
    :allocation :virtual
    :location ("gtk_button_box_get_layout" "gtk_button_box_set_layout")
    :accessor button-box-layout
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



; (defclass color-selection vbox
;   :slots
;   ((policy                 :c-writer "gtk_color_selection_set_update_policy"
; 			   :read-method :arg :type update-type)
;    (use-opacity            :c-writer "gtk_color_selection_set_opacity"
; 			   :read-method :arg :type boolean)
;    ;; slots not accessible through the arg mechanism
;    (color                  :access-method :lisp)))

; (defclass gamma-curve vbox)

(defclass hbox (box)
  ()
  (:metaclass container-class)
  (:alien-name "GtkHBox"))

(defclass hbox-child (box-child)
  ()
  (:metaclass child-class))



; (defclass statusbar hbox)

; (defclass clist container
;   :c-name "GtkCList"
;   :c-prefix "gtk_clist_"
;   :slots
;   ((n-columns              :read-only t :initarg t :access-method :arg
; 			   :type unsigned-int)
;    (shadow-type            :read-method :arg :type shadow-type)
;    (selection-mode         :read-method :arg :type selection-mode)
;    (row-height             :read-method :arg :type unsigned-int)
;    (reorderable            :read-method :arg :type boolean)
;    (titles-visible         :write-method :lisp :type boolean)
;    (titles-active          :access-method :arg :type boolean)
;    (use-drag-icons         :read-method :arg :type boolean)
;    (sort-type              :read-method :arg :type sort-type)
;    ;; slots not accessible through the arg mechanism
;    (hadjustment            :type adjustment)
;    (vadjustment            :type adjustment)
;    (sort-column            :type int)
;    (focus-row              :reader %clist-focus-row :read-only t :type int)
;    (n-rows                 :read-only t :type int)))

; (defclass ctree clist
;   :c-name "GtkCTree"
;   :c-prefix "gtk_ctree_"
;   :slots
;   ((n-columns              :read-only t :initarg t :access-method :arg
; 			   :type unsigned-int)
;    (tree-column            :read-only t :initarg t :access-method :arg
; 			   :type unsigned-int)
;    (indent                 :read-method :arg :type unsigned-int)
;    (spacing                :read-method :arg :type unsigned-int)
;    (show-stub              :read-method :arg :type boolean)
;    (line-style             :read-method :arg :type ctree-line-style)
;    (expander-style         :read-method :arg :type ctree-expander-style)))

; (defclass fixed container)

; (defclass notebook container
;   :slots
;   ((show-tabs              :read-method :arg :type boolean)
;    (show-border            :read-method :arg :type boolean)
;    (scrollable             :read-method :arg :type boolean)
;    (enable-popup           :access-method :arg :type boolean)
;    (homogeneous            :c-writer "gtk_notebook_set_homogeneous_tabs"
; 			   :read-method :arg :type boolean)
;    (current-page           :c-writer "gtk_notebook_set_page" :type int)
;    (tab-pos                :read-method :arg :type position-type)
;    (tab-border             :read-method :arg :type unsigned-int)
;    (tab-hborder            :read-method :arg :type unsigned-int)
;    (tab-vborder            :read-method :arg :type unsigned-int))
;   :child-slots
;   ((tab-label              :access-method :arg :type string)
;    (menu-label             :access-method :arg :type string)
;    (tab-fill               :access-method :arg :type boolean)
;    (tab-pack               :access-method :arg :type boolean)
;    (position               :access-method :arg :type int)))

; (defclass font-selection notebook)

; (defclass paned container
;   :constructor nil
;   :slots
;   ((handle-size            :read-method :arg :type unsigned-int)
;    (gutter-size            :read-method :arg :type unsigned-int)
;    ;; slots not accessible through the arg mechanism
;    (position               :write-only t :type int)))

; (defclass hpaned paned)

; (defclass vpaned paned)

; (defclass layout container
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((hadjustment            :type adjustment)
;    (vadjustment            :type adjustment)
;    (bin-window             :read-only t :type gdk:window)))

; (defclass list-widget container
;   :c-name "GtkList"
;   :slots
;   ((selection-mode         :read-method :arg :accessor list-selection-mode
; 			   :c-writer "gtk_list_set_selection_mode"
; 			   :type selection-mode)))

; (defclass menu-shell container :constructor nil)

; (defclass menu-bar menu-shell
;   :slots
;   ((shadow-type            :read-method :arg :arg-name "shadow"
; 			   :type shadow-type)))

; (defclass menu menu-shell
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((accel-group            :type accel-group)
;    (tearoff-state          :write-only t :type boolean)
;    (title                  :write-only t :type string)))

; (defclass packer container
;   :slots
;   ((spacing                :read-method :arg :type unsigned-int)
;    (default-border-width   :read-method :arg :type unsigned-int)
;    (default-pad-x          :access-method :arg :type unsigned-int)
;    (default-pad-y          :access-method :arg :type unsigned-int)
;    (default-ipad-x         :access-method :arg :type unsigned-int)
;    (default-ipad-y         :access-method :arg :type unsigned-int))
;   :child-slots
;   ((side                   :access-method :arg :type side-type)
;    (anchor                 :access-method :arg :type anchor-type)
;    (expand                 :access-method :arg :type boolean)
;    (fill-x                 :access-method :arg :type boolean)
;    (fill-y                 :access-method :arg :type boolean)
;    (use-default            :access-method :arg :type boolean)
;    (border-width           :access-method :arg :type unsigned-int)
;    (pad-x                  :access-method :arg :type unsigned-int)
;    (pad-y                  :access-method :arg :type unsigned-int)
;    (ipad-x                 :access-method :arg :type unsigned-int)
;    (ipad-y                 :access-method :arg :type unsigned-int)
;    (position               :access-method :arg :type long)))

; (defclass socket container)

; (defclass table container
;   :slots
;   ((rows                   :access-method :arg :arg-name "n_rows"
; 			   :type unsigned-int)
;    (columns                :access-method :arg :arg-name "n_columns"
; 			   :type unsigned-int)
;    (row-spacing            :c-writer "gtk_table_set_row_spacings"
; 			   :accessor table-row-spacings
; 			   :read-method :arg :type unsigned-int)
;    (column-spacing         :c-writer "gtk_table_set_col_spacings"
; 			   :accessor table-column-spacings
; 			   :read-method :arg  :type unsigned-int)
;    (homogeneous            :read-method :arg :type boolean))
;   :child-slots
;   ((left-attach            :access-method :arg :type unsigned-int)
;    (right-attach           :access-method :arg :type unsigned-int)
;    (top-attach             :access-method :arg :type unsigned-int)
;    (bottom-attach          :access-method :arg :type unsigned-int)
;    (x-options              :access-method :arg :type attach-options)
;    (y-options              :access-method :arg :type attach-options)
;    (x-padding              :access-method :arg :type unsigned-int)
;    (y-padding              :access-method :arg :type unsigned-int)
;    ;; Slots added for convenience sake
;    (x-expand               :access-method :lisp :type boolean)
;    (y-expand               :access-method :lisp :type boolean)
;    (x-shrink               :access-method :lisp :type boolean)
;    (y-shrink               :access-method :lisp :type boolean)
;    (x-fill                 :access-method :lisp :type boolean)
;    (y-fill                 :access-method :lisp :type boolean)))

; (defclass toolbar container
;   :slots
;   ((orientation            :read-method :arg :type orientation)
;    (toolbar-style          :accessor toolbar-style
;                            :c-writer "gtk_toolbar_set_style"
; 			   :read-method :arg :type toolbar-style)
;    (space-size             :read-method :arg :type unsigned-int)
;    (space-style            :read-method :arg :type toolbar-space-style)
;    (relief                 :c-writer "gtk_toolbar_set_button_relief"
; 			   :read-method :arg :type relief-style)
;    ;; slots not accessible through the arg mechanism
;    (tooltips               :write-only t :type boolean)))

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


; (defclass drawing-area widget)

; (defclass curve drawing-area
;   :slots
;   ((curve-type             :read-method :arg :type curve-type)
;    (min-x                  :access-method :arg :type single-float)
;    (max-x                  :access-method :arg :type single-float)
;    (min-y                  :access-method :arg :type single-float)
;    (max-y                  :access-method :arg :type single-float)))

; (defclass editable widget
;   :slots
;   ((position               :type int)
;    (editable               :read-method :arg :type boolean)
;    ;; slots not accessible through the arg mechanism
;    (text                   :access-method :lisp :type string)))

; (defclass entry editable
;   :slots
;   ((max-length             :read-method :arg :type unsigned-int)
;    (visibility             :read-method :arg :accessor entry-visible-p
; 			   :type boolean)
;    ;; slots not accessible through the arg mechanism
;    (text                   :type string)))

; (defclass combo hbox
;   :slots
;   ;; slots not accessible through the arg mechanism
;   ((entry                  :read-only t :type entry)
;    (use-arrows             :type boolean)
;    (use-arrows-always      :type boolean)
;    (case-sensitive         :type boolean)))

; (defclass spin-button entry
;   :slots
;   ((adjustment             :access-method :arg :type adjustment)
;    (climb-rate             :access-method :arg :type single-float)
;    (digits                 :access-method :arg :type unsigned-int)
;    (snap-to-ticks          :read-method :arg :type boolean)
;    (numeric                :read-method :arg :type boolean)
;    (wrap                   :read-method :arg :type boolean)
;    (update-policy          :read-method :arg :type spin-button-update-policy)
;    (shadow-type            :read-method :arg :type shadow-type)
;    (value                  :read-method :arg :type single-float)))

; (defclass text editable
;   :slots
;   ((hadjustment            :access-method :arg :type adjustment)
;    (vadjustment            :access-method :arg :type adjustment)
;    (line-wrap              :read-method :arg :type boolean)
;    (word-wrap              :read-method :arg :type boolean)
;    ;; slots not accessible through the arg mechanism
;    (point                  :type unsigned-int)
;    (length                 :read-only t :type unsigned-int)))

; (defclass ruler widget
;   :constructor nil
;   :slots
;   ((lower                  :access-method :arg :type single-float)
;    (upper                  :access-method :arg :type single-float)
;    (position               :access-method :arg :type single-float)
;    (max-size               :access-method :arg :type single-float)
;    ;; slots not accessible through the arg mechanism
;    (:metric                :write-only t :type metric-type)))

; (defclass hruler ruler)

; (defclass vruler ruler)

; (defclass range widget
;   :slots
;   ((update-policy          :read-method :arg :type update-type)
;    ;; slots not accessible through the arg mechanism
;    (adjustment             :type adjustment)))

; (defclass scale range
;   :constructor nil
;   :slots
;   ((digits                 :read-method :arg :type unsigned-int)
;    (draw-value             :read-method :arg :type boolean)
;    (value-pos              :read-method :arg :type position-type)
;    ;; slots not accessible through the arg mechanism
;    (value-width            :read-only t :type int)))

; (defclass hscale scale)

; (defclass vscale scale)

; (defclass scrollbar range :constructor nil)

; (defclass hscrollbar scrollbar)

; (defclass vscrollbar scrollbar)

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

; (defclass progress widget
;   :slots
;   ((activity-mode          :read-method :arg :type boolean)
;    (show-text              :read-method :arg :type boolean)
;    (text-xalign            :access-method :arg :type single-float)
;    (text-yalign            :access-method :arg :type single-float)
;    ;; slots not accessible through the arg mechanism
;    (format-string          :type string)
;    (adjustment             :type adjustment)
;    (percentage             :c-reader "gtk_progress_get_current_percentage"
; 			   :type single-float)
;    (value                  :type single-float)
;    (text                   :c-reader "gtk_progress_get_current_text"
; 			   :read-only t :type string)))

; (defclass progress-bar progress
;   :slots
;   ((adjustment             :c-writer "gtk_progress_set_adjustment"
; 			   :read-method :arg :type adjustment)
;    (orientation            :read-method :arg :type progress-bar-orientation)
;    (bar-style              :read-method :arg :accessor progress-bar-style
; 			   :type progress-bar-style)
;    (activity-step          :read-method :arg :type unsigned-int)
;    (activity-blocks        :read-method :arg :type unsigned-int)
;    (discrete-blocks        :read-method :arg :type unsigned-int)))

; (defclass item-factory object)

