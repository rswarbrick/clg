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

;; $Id: gtk.lisp,v 1.4 2001-01-28 14:25:48 espen Exp $


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

(defmethod initialize-instance ((pixmap pixmap) &rest initargs
				&key source mask)
  (declare (ignore initargs))
  (call-next-method)
  (if (typep source 'gdk:pixmap)
      (pixmap-set pixmap source mask)
    (multiple-value-bind (source mask) (gdk:pixmap-create source)
      (pixmap-set pixmap source mask))))

(defun pixmap-new (source &optional mask)
  (make-instance 'pixmap :source source :mask mask))

(define-foreign pixmap-set () nil
  (pixmap pixmap)
  (source gdk:pixmap)
  (mask (or null gdk:bitmap)))

(defun (setf pixmap-source) (source pixmap)
  (if (typep source 'gdk:pixmap)
      (pixmap-set pixmap source (pixmap-mask pixmap))
    (multiple-value-bind (source mask) (gdk:pixmap-create source)
      (pixmap-set pixmap source mask)))
  source)

(defun (setf pixmap-mask) (mask pixmap)
  (pixmap-set pixmap (pixmap-source pixmap) mask)
  mask)
    
(define-foreign ("gtk_pixmap_get" pixmap-source) () nil
  (pixmap pixmap)
  (val gdk:pixmap :out)
  (nil null))

(define-foreign ("gtk_pixmap_get" pixmap-mask) () nil
  (pixmap pixmap)
  (nil null)
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

(defmethod (setf button-label) ((label string) (button check-button))
  (call-next-method)
  (setf (misc-xalign (bin-child button)) 0.0)
  label)



;;; Radio button

(define-foreign %radio-button-new-with-label-from-widget () radio-button
  (widget (or null radio-button))
  (label string))

(define-foreign %radio-button-new-from-widget () radio-button
  (widget (or null radio-button)))

(defun radio-button-new (&optional label group-with)
  (if label
    (%radio-button-new-with-label-from-widget group-with label))
  (%radio-button-new-from-widget group-with))

(define-foreign ("gtk_radio_button_group" %radio-button-get-group) () pointer
  (radio-button radio-button))

(define-foreign %radio-button-set-group () nil
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

(define-foreign option-menu-new () option-menu)

(define-foreign %option-menu-set-menu () nil
  (option-menu option-menu)
  (menu widget))

(define-foreign %option-menu-remove-menu () nil
  (option-menu option-menu))

(defun (setf option-menu-menu) (menu option-menu)
  (if (not menu)
      (%option-menu-remove-menu option-menu)
    (%option-menu-set-menu option-menu menu))
  menu)
    


;;; Item

(define-foreign item-select () nil
  (item item))

(define-foreign item-deselect () nil
  (item item))

(define-foreign item-toggle () nil
  (item item))



;;; Menu item

(define-foreign %menu-item-new () menu-item)

(define-foreign %menu-item-new-with-label () menu-item
  (label string))

(defun menu-item-new (&optional label)
  (if label
      (%menu-item-new-with-label label)
    (%menu-item-new)))

(defun (setf menu-item-label) (label menu-item)
  (make-instance 'accel-label
   :label label :xalign 0.0 :yalign 0.5 :accel-widget menu-item
   :visible t :parent menu-item)
  label)

(define-foreign %menu-item-set-submenu () nil
  (menu-item menu-item)
  (submenu menu))

(define-foreign %menu-item-remove-submenu () nil
  (menu-item menu-item))

(defun (setf menu-item-submenu) (submenu menu-item)
  (if (not submenu)
      (%menu-item-remove-submenu menu-item)
    (%menu-item-set-submenu menu-item submenu))
  submenu)

(define-foreign %menu-item-configure () nil
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

(define-foreign menu-item-select () nil
  (menu-item menu-item))

(define-foreign menu-item-deselect () nil
  (menu-item menu-item))

(define-foreign menu-item-activate () nil
  (menu-item menu-item))

(define-foreign menu-item-right-justify () nil
  (menu-item menu-item))



;;; Check menu item

(define-foreign %check-menu-item-new
    () check-menu-item)

(define-foreign %check-menu-item-new-with-label () check-menu-item
  (label string))

(defun check-menu-item-new (&optional label)
  (if label
      (%check-menu-item-new-with-label label)
    (%check-menu-item-new)))

(define-foreign check-menu-item-toggled () nil
  (check-menu-item check-menu-item))



;;; Radio menu item

(define-foreign %radio-menu-item-new () radio-menu-item
  (group pointer))

(define-foreign %radio-menu-item-new-with-label () radio-menu-item
  (group pointer)
  (label string))

(define-foreign
    ("gtk_radio_menu_item_group" %radio-menu-item-get-group) () pointer
  (radio-menu-item radio-menu-item))

(define-foreign %radio-menu-item-set-group () nil
  (radio-menu-item radio-menu-item)
  (group pointer))

(defun radio-menu-item-new (&optional label group-with)
  (let ((group
	 (if group-with
	     (%radio-menu-item-get-group group-with)
	   (make-pointer 0))))
    (if label
	(%radio-menu-item-new-with-label group label)
      (%radio-menu-item-new group))))

(defun radio-menu-item-add-to-group (item1 item2)
  "Add ITEM1 to the group which ITEM2 belongs to."
  (%radio-menu-item-set-group item1 (%radio-menu-item-get-group item2)))

(defmethod initialize-instance ((item radio-menu-item)
				&rest initargs &key group)
  (call-next-method)
  (when group
    (radio-menu-item-add-to-group item group)))
  


;;; Tearoff menu item

(define-foreign tearoff-menu-item-new () tearoff-menu-item)



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

;; gtkglue.c
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



;;; Dialog

(define-foreign dialog-new () dialog)


;;; Color selection dialog

(define-foreign color-selection-dialog-new () color-selection-dialog
  (title string))


;;; Input dialog

(define-foreign input-dialog-new () dialog)



;;; File selection

(define-foreign file-selection-new () file-selection
  (title string))

(define-foreign file-selection-complete () nil
  (file-selection file-selection)
  (pattern string))

(define-foreign file-selection-show-fileop-buttons () nil
  (file-selection file-selection))

(define-foreign file-selection-hide-fileop-buttons () nil
  (file-selection file-selection))



;;; Handle box

(define-foreign handle-box-new () handle-box)



;;; Scrolled window

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



;;; Viewport

(define-foreign viewport-new () viewport
  (hadjustment adjustment)
  (vadjustment adjustment))
  


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
		  button-box-get-default-child-size) () nil
  (min-width int :out)
  (min-height int :out))

(define-foreign ("gtk_button_box_set_child_size_default"
		 button-box-set-default-child-size) () nil
  (min-width int)
  (min-height int))

(define-foreign ("gtk_button_box_get_child_ipadding_default"
		  button-box-get-default-child-ipadding) () nil
  (ipad-x int :out)
  (ipad-y int :out))


(define-foreign ("gtk_button_box_get_child_ipadding_default"
		 button-box-set-default-child-ipadding) () nil
  (ipad-x int)
  (ipad-y int))



;;; HButton box

(define-foreign hbutton-box-new () hbutton-box)

(define-foreign ("gtk_hbutton_box_get_spacing_default"
		  hbutton-box-default-spacing) () int)

(define-foreign %hbutton-box-set-spacing-default () nil
  (spacing int))

(defun (setf hbutton-box-default-spacing) (spacing)
  (%hbutton-box-set-spacing-default spacing))
  
(define-foreign ("gtk_hbutton_box_get_layout_default"
		  hbutton-box-default-layout) () button-box-style)

(define-foreign %hbutton-box-set-layout-default () nil
  (layout button-box-style))

(defun (setf hbutton-box-default-layout) (layout)
  (%hbutton-box-set-layout-default layout))



;;; VButton Box

(define-foreign vbutton-box-new () vbutton-box)

(define-foreign ("gtk_vbutton_box_get_spacing_default"
		  vbutton-box-default-spacing) () int)

(define-foreign %vbutton-box-set-spacing-default () nil
  (spacing int))

(defun (setf vbutton-box-default-spacing) (spacing)
  (%vbutton-box-set-spacing-default spacing))
  
(define-foreign ("gtk_vbutton_box_get_layout_default"
		  vbutton-box-default-layout) () button-box-style)

(define-foreign %vbutton-box-set-layout-default () nil
  (layout button-box-style))

(defun (setf vbutton-box-default-layout) (layout)
  (%vbutton-box-set-layout-default layout))



;;; VBox

(define-foreign vbox-new () vbox
  (homogeneous boolean)
  (spacing int))



;;; Color selection

(define-foreign color-selection-new () color-selection)

(define-foreign %color-selection-get-color () nil
  (colorsel color-selection)
  (color pointer))

(defun color-selection-color (colorsel)
  (let ((color (allocate-memory (* (size-of 'double-float) 4))))
    (%color-selection-get-color colorsel color)
    (funcall (get-from-alien-function '(vector double-float 4)) color)))

(define-foreign %color-selection-set-color () nil
  (colorsel color-selection)
  (color (vector double-float 4)))

(defun (setf color-selection-color) (color colorsel)
  (%color-selection-set-color colorsel color)
  color)

(define-foreign %color-selection-get-old-color () nil
  (colorsel color-selection)
  (color pointer))

(defun color-selection-old-color (colorsel)
  (let ((color (allocate-memory (* (size-of 'double-float) 4))))
    (%color-selection-get-old-color colorsel color)
    (funcall (get-from-alien-function '(vector double-float 4)) color)))

(define-foreign %color-selection-set-old-color () nil
  (colorsel color-selection)
  (color (vector double-float 4)))

(defun (setf color-selection-old-color) (color colorsel)
  (%color-selection-set-old-color colorsel color)
  color)

(define-foreign %color-selection-get-palette-color () boolean
  (colorsel color-selection)
  (x int)
  (y int)
  (color (vector double-float 4) :out))

(defun color-selection-palette-color (colorsel x y)
  (multiple-value-bind (color-set-p color)
      (%color-selection-get-palette-color colorsel x y)
    (and color-set-p color)))

(define-foreign %color-selection-set-palette-color () nil
  (colorsel color-selection)
  (x int)
  (y int)
  (color (vector double-float 4)))

(define-foreign %color-selection-unset-palette-color () nil
  (colorsel color-selection)
  (x int)
  (y int))

(defun (setf color-selection-palette-color) (color colorsel x y)
  (if color
      (%color-selection-set-palette-color colorsel x y color)
    (%color-selection-unset-palette-color colorsel x y))
  color)

(define-foreign ("gtk_color_selection_is_adjusting"
		 color-selection-is-adjusting-p) () boolean
  (colorsel color-selection))



;;; Gamma curve

;(define-foreign gamma-curve-new () gamma-curve)



;;; HBox

(define-foreign hbox-new () hbox
  (homogeneous boolean)
  (spacing int))



;;; Combo

(define-foreign combo-new () combo)

(define-foreign combo-set-value-in-list () nil
  (combo combo)
  (val boolean)
  (ok-if-empty boolean))

; (define-foreign ("gtk_combo_set_item_string" (setf combo-item-string)) () nil
;   (combo combo)
;   (item item)
;   (item-value string))

(define-foreign %combo-set-popdown-strings () nil
  (combo combo)
  (strings (glist string)))

(defun (setf combo-popdown-strings) (strings combo)
  (%combo-set-popdown-strings combo strings)
  strings)

(define-foreign combo-disable-activate () nil
  (combo combo))



;;; Statusbar

(define-foreign statusbar-new () statusbar)

(define-foreign
    ("gtk_statusbar_get_context_id" statusbar-context-id) () unsigned-int
  (statusbar statusbar)
  (context-description string))

(define-foreign statusbar-push () unsigned-int
  (statusbar statusbar)
  (context-id unsigned-int)  
  (text string))

(define-foreign statusbar-pop () nil
  (statusbar statusbar)
  (context-id unsigned-int))

(define-foreign statusbar-remove () nil
  (statusbar statusbar)
  (context-id unsigned-int)
  (message-id unsigned-int))



;;; Fixed

(define-foreign fixed-new () fixed)

(define-foreign fixed-put () nil
  (fixed fixed)
  (widget widget)
  (x (signed 16))
  (y (signed 16)))

(define-foreign fixed-move () nil
  (fixed fixed)
  (widget widget)
  (x (signed 16))
  (y (signed 16)))



;;; Notebook

(define-foreign notebook-new () notebook)

(define-foreign ("gtk_notebook_insert_page_menu" notebook-insert-page)
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
  
(define-foreign notebook-remove-page () nil
  (notebook notebook)
  (page-num int))

; (defun notebook-current-page-num (notebook)
;   (let ((page-num (notebook-current-page notebook)))
;     (if (= page-num -1)
; 	nil
;       page-num)))

(define-foreign ("gtk_notebook_get_nth_page" notebook-nth-page-child) () widget
  (notebook notebook)
  (page-num int))

(defun notebook-page-child (notebook)
  (notebook-nth-page-child notebook (notebook-page notebook)))

(define-foreign %notebook-page-num () int
  (notebook notebook)
  (child widget))

(defun notebook-child-num (notebook child)
  (let ((page-num (%notebook-page-num notebook child)))
    (if (= page-num -1)
	nil
      page-num)))

(define-foreign notebook-next-page () nil
  (notebook notebook))

(define-foreign notebook-prev-page () nil
  (notebook notebook))

(define-foreign notebook-popup-enable () nil
  (notebook notebook))

(define-foreign notebook-popup-disable () nil
  (notebook notebook))

(define-foreign
    ("gtk_notebook_get_tab_label" notebook-tab-label) (notebook ref) widget
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget))

(define-foreign %notebook-set-tab-label () nil
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
   
(define-foreign
    ("gtk_notebook_get_menu_label" notebook-menu-label) (notebook ref) widget
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget))

(define-foreign %notebook-set-menu-label () nil
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

(define-foreign notebook-query-tab-label-packing (notebook ref) nil
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget)
  (expand boolean :out)
  (fill boolean :out)
  (pack-type pack-type :out))

(define-foreign
    notebook-set-tab-label-packing (notebook ref expand fill pack-type) nil
  (notebook notebook)
  ((if (typep ref 'widget)
       ref
     (notebook-nth-page-child notebook ref))
   widget)
  (expand boolean)
  (fill boolean)
  (pack-type pack-type))

(define-foreign notebook-reorder-child () nil
  (notebook notebook)
  (child widget)
  (position int))



;;; Font selection




;;; Paned

(define-foreign paned-pack1 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))

(define-foreign paned-pack2 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))

;; gtkglue.c
(define-foreign paned-child1 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))

;; gtkglue.c
(define-foreign paned-child2 () widget
  (paned paned)
  (resize boolean :out)
  (shrink boolean :out))

(defun (setf paned-child1) (child paned)
  (paned-pack1 paned child nil t))

(defun (setf paned-child2) (child paned)
  (paned-pack2 paned child t t))


(define-foreign vpaned-new () vpaned)

(define-foreign hpaned-new () hpaned)



;;; Layout

(define-foreign layout-new (&optional hadjustment vadjustment) layout
  (hadjustment (or null adjustment))
  (vadjustment (or null adjustment)))

(define-foreign layout-put () nil
  (layout layout)
  (widget widget)
  (x int)
  (y int))

(define-foreign layout-move () nil
  (layout layout)
  (widget widget)
  (x int)
  (y int))

(define-foreign layout-set-size () nil
  (layout layout)
  (width int)
  (height int))

;; gtkglue.c
(define-foreign layout-get-size () nil
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

(define-foreign layout-freeze () nil
  (layout layout))

(define-foreign layout-thaw () nil
  (layout layout))



;;; List

; (define-foreign list-new () list-widget)

; (define-foreign list-insert-items () nil
;   (list list-widget)
;   (items (list list-item))
;   (position int))

; (define-foreign list-append-items () nil
;   (list list-widget)
;   (items (glist list-item)))

; (define-foreign list-prepend-items () nil
;   (list list-widget)
;   (items (glist list-item)))

; (define-foreign %list-remove-items () nil
;   (list list-widget)
;   (items (glist list-item)))

; (define-foreign %list-remove-items-no-unref () nil
;   (list list-widget)
;   (items (glist list-item)))

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

; ;; gtkglue.c
; (define-foreign list-selection () (glist list-item)
;   (list list-widget))



;;; Menu shell

(define-foreign menu-shell-insert () nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  (position int))

(defun menu-shell-append (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item -1))

(defun menu-shell-prepend (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item 0))

(define-foreign menu-shell-deactivate () nil
  (menu-shell menu-shell))

(define-foreign menu-shell-select-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item))

(define-foreign menu-shell-deselect () nil
  (menu-shell menu-shell))

(define-foreign menu-shell-activate-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  (fore-deactivate boolean))



; ;;; Menu bar

(define-foreign menu-bar-new () menu-bar)

; (define-foreign menu-bar-insert () nil
;   (menu-bar menu-bar)
;   (menu menu)
;   (position int))

; (defun menu-bar-append (menu-bar menu)
;   (menu-bar-insert menu-bar menu -1))

; (defun menu-bar-prepend (menu-bar menu)
;   (menu-bar-insert menu-bar menu 0))



; ;;; Menu

(define-foreign menu-new () menu)

; (defun menu-insert (menu menu-item position)
;   (menu-shell-insert menu menu-item position))

; (defun menu-append (menu menu-item)
;   (menu-shell-append menu menu-item))

; (defun menu-prepend (menu menu-item)
;   (menu-shell-prepend menu menu-item))

;(defun menu-popup ...)

(define-foreign menu-reposition () nil
  (menu menu))

(define-foreign menu-popdown () nil
  (menu menu))

(define-foreign ("gtk_menu_get_active" menu-active) () widget
  (menu menu))

(define-foreign %menu-set-active () nil
  (menu menu)
  (index unsigned-int))

(defun (setf menu-active) (menu index)
  (%menu-set-active menu index))
  
;(defun menu-attach-to-widget ...)

(define-foreign menu-detach () nil
  (menu menu))

(define-foreign ("gtk_menu_get_attach_widget" menu-attach-widget) () widget
  (menu menu))

(define-foreign menu-reorder-child () nil
  (menu menu)
  (menu-item menu-item)
  (position int))



;;; Packer

(define-foreign packer-new () packer)

(define-foreign packer-add
    (packer child side anchor
     &key
     options
     (border-width (packer-default-border-width packer))
     (pad-x (packer-default-pad-x packer))
     (pad-y (packer-default-pad-y packer))
     (ipad-x (packer-default-ipad-x packer))
     (ipad-y (packer-default-ipad-y packer))) nil
  (packer packer)
  (child widget)
  (side side-type)
  (anchor anchor-type)
  (options packer-options)
  (border-width unsigned-int)
  (pad-x unsigned-int)
  (pad-y unsigned-int)
  (ipad-x unsigned-int)
  (ipad-y unsigned-int))

(define-foreign packer-set-child-packing () nil
  (packer packer)
  (child widget)
  (side side-type)
  (anchor anchor-type)
  (options packer-options)
  (border-width unsigned-int)
  (pad-x unsigned-int)
  (pad-y unsigned-int)
  (ipad-x unsigned-int)
  (ipad-y unsigned-int))

(define-foreign packer-reorder-child () nil
  (packer packer)
  (child widget)
  (position int))



;;; Table

(define-foreign table-new () table
  (rows unsigned-int)
  (columns unsigned-int)
  (homogeneous boolean))

(define-foreign table-resize () nil
  (table table)
  (rows unsigned-int)
  (columns unsigned-int))

(define-foreign table-attach (table child left right top bottom
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

(define-foreign %table-set-row-spacing () nil
  (table table)
  (row unsigned-int)
  (spacing unsigned-int))

(defun (setf table-row-spacing) (spacing table row)
  (%table-set-row-spacing table row spacing)
  spacing)

;; gtkglue.c
(define-foreign table-row-spacing (table row) unsigned-int
  (table table)
  ((progn
     (assert (and (>= row 0) (< row (table-rows table))))
     row) unsigned-int))

(define-foreign %table-set-col-spacing () nil
  (table table)
  (col unsigned-int)
  (spacing unsigned-int))

(defun (setf table-column-spacing) (spacing table column)
  (%table-set-col-spacing table column spacing)
  spacing)

;; gtkglue.c
(define-foreign table-column-spacing (table col) unsigned-int
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

(define-foreign toolbar-new () toolbar
  (orientation orientation)
  (style toolbar-style))

;; gtkglue.c
(define-foreign toolbar-num-children () int
  (toolbar toolbar))

(defun %toolbar-position-num (toolbar position)
  (case position
    (:prepend 0)
    (:append (toolbar-num-children toolbar))
    (t
     (assert (and (>= position 0) (< position (toolbar-num-children toolbar))))
     position)))

(define-foreign %toolbar-insert-element () widget
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

; ;; gtkglue.c
; (define-foreign ("gtk_drawing_area_get_size" drawing-area-size) () nil
;   (drawing-area drawing-area)
;   (width int :out)
;   (height int :out))



; ;;; Curve



; ;;; Editable

(define-foreign editable-select-region (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(define-foreign editable-insert-text
    (editable text &optional (position 0)) nil
  (editable editable)
  (text string)
  ((length text) int)
  ((or position -1) int :in-out))

(defun editable-append-text (editable text)
  (editable-insert-text editable text nil))

(defun editable-prepend-text (editable text)
  (editable-insert-text editable text 0))

(define-foreign editable-delete-text (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(define-foreign ("gtk_editable_get_chars" editable-text)
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

(define-foreign editable-cut-clipboard () nil
  (editable editable))

(define-foreign editable-copy-clipboard () nil
  (editable editable))

(define-foreign editable-paste-clipboard () nil
  (editable editable))

; (define-foreign editable-claim-selection () nil
;   (editable editable)
;   (claim boolean)
;   (time unsigned-int))

(define-foreign editable-delete-selection () nil
  (editable editable))

; (define-foreign editable-changed () nil
;   (editable editable))



;;; Entry

(define-foreign %entry-new() entry)

(define-foreign %entry-new-with-max-length () entry
  (max (unsigned 16)))

(defun entry-new (&optional max)
  (if max
      (%entry-new-with-max-length max)
    (%entry-new)))


;;; Spin button

(define-foreign spin-button-new () spin-button
  (adjustment adjustment)
  (climb-rate single-float)
  (digits unsigned-int))

(defun spin-button-value-as-int (spin-button)
  (round (spin-button-value spin-button)))

(define-foreign spin-button-spin () nil
  (spin-button spin-button)
  (direction spin-type)
  (increment single-float))

(define-foreign spin-button-update () nil
  (spin-button spin-button))



; ;;; Ruler

(define-foreign ruler-set-range () nil
  (ruler ruler)
  (lower single-float)
  (upper single-float)
  (position single-float)
  (max-size single-float))

(define-foreign ruler-draw-ticks () nil
  (ruler ruler))

(define-foreign ruler-draw-pos () nil
  (ruler ruler))

(define-foreign hruler-new () hruler)

(define-foreign vruler-new () vruler)


;;; Range

(define-foreign range-draw-background () nil
  (range range))

(define-foreign range-clear-background () nil
  (range range))

(define-foreign range-draw-trough () nil
  (range range))

(define-foreign range-draw-slider () nil
  (range range))

(define-foreign range-draw-step-forw () nil
  (range range))

(define-foreign range-slider-update () nil
  (range range))

(define-foreign range-trough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(define-foreign range-default-hslider-update () nil
  (range range))

(define-foreign range-default-vslider-update () nil
  (range range))

(define-foreign range-default-htrough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(define-foreign range-default-vtrough-click () int
  (range range)
  (x int)
  (y int)
  (jump-perc single-float :out))

(define-foreign range-default-hmotion () int
  (range range)
  (x-delta int)
  (y-delta int))

(define-foreign range-default-vmotion () int
  (range range)
  (x-delta int)
  (y-delta int))



;;; Scale

(define-foreign scale-draw-value () nil
  (scale scale))

(define-foreign hscale-new () hscale
  (adjustment adjustment))

(define-foreign vscale-new () hscale
  (adjustment adjustment))



;;; Scrollbar

(define-foreign hscrollbar-new () hscrollbar
  (adjustment adjustment))

(define-foreign vscrollbar-new () vscrollbar
  (adjustment adjustment))



;;; Separator

(define-foreign vseparator-new () vseparator)

(define-foreign hseparator-new () hseparator)



;;; Preview



;;; Progress

(define-foreign progress-configure () adjustment
  (progress progress)
  (value single-float)
  (min single-float)
  (max single-float))

(define-foreign ("gtk_progress_get_text_from_value"
		  progress-text-from-value) () string
  (progress progress))

(define-foreign ("gtk_progress_get_percentage_from_value"
		  progress-percentage-from-value) () single-float
  (progress progress))



;;; Progress bar

(define-foreign progress-bar-new () progress-bar)

(define-foreign progress-bar-pulse () nil
  (progress-bar progress-bar))



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

(define-foreign tooltips-new () tooltips)

(define-foreign tooltips-enable () nil
  (tooltips tooltips))

(define-foreign tooltips-disable () nil
  (tooltips tooltips))

(define-foreign tooltips-set-tip () nil
  (tooltips tooltips)
  (widget widget)
  (tip-text string)
  (tip-private string))

(define-foreign tooltips-set-colors (tooltips background foreground) nil
  (tooltips tooltips)
  ((gdk:ensure-color background) gdk:color)
  ((gdk:ensure-color foreground) gdk:color))

(define-foreign tooltips-force-window () nil
  (tooltips tooltips))



;;; Rc

(define-foreign rc-add-default-file (filename) nil
  ((namestring (truename filename)) string))

(define-foreign rc-parse (filename) nil
  ((namestring (truename filename)) string))

(define-foreign rc-parse-string () nil
  (rc-string string))

(define-foreign rc-reparse-all () nil)

(define-foreign rc-get-style () style
  (widget widget))



;;; Accelerator Groups

(define-foreign accel-group-new () accel-group)

(define-foreign accel-group-get-default () accel-group)

(deftype-method alien-ref accel-group (type-spec)
  (declare (ignore type-spec))
  '%accel-group-ref)

(deftype-method alien-unref accel-group (type-spec)
  (declare (ignore type-spec))
  '%accel-group-unref)

(define-foreign %accel-group-ref () accel-group
  (accel-group (or accel-group pointer)))

(define-foreign %accel-group-unref () nil
  (accel-group (or accel-group pointer)))

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

(define-foreign %style-get-color () gdk:color
  (style style)
  (color-type color-type)
  (state-type state-type))

(define-foreign %style-set-color () gdk:color
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

(define-foreign style-get-gc () gdk:gc
  (style style)
  (color-type color-type)
  (state-type state-type))


(define-foreign draw-hline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x1 int)
  (x2 int)
  (y int))

(define-foreign draw-vline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (y1 int)
  (y2 int)
  (x int))

(define-foreign draw-shadow () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (define-foreign draw-polygon () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (points (vector gdk:point))
;   ((length points) int)
;   (fill boolean))

(define-foreign draw-arrow () nil
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
  
(define-foreign draw-diamond () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (define-foreign draw-oval () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

(define-foreign draw-string () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (string string))

(define-foreign draw-box () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-flat-box () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-check () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-option () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int))

; (define-foreign draw-cross () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

; (define-foreign draw-ramp () nil
;   (style style)
;   (window gdk:window)
;   (state state-type)
;   (shadow shadow-type)
;   (arrow arrow-type)
;   (x int)
;   (y int)
;   (width int)
;   (height int))

(define-foreign draw-tab () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-shadow-gap () nil
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

(define-foreign draw-box-gap () nil
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

(define-foreign draw-extension () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-focus () nil
  (style style)
  (window gdk:window)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign draw-slider () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(define-foreign draw-handle () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(define-foreign draw-handle () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (shadow shadow-type)
  (x int)
  (y int)
  (width int)
  (height int)
  (orientation orientation))

(define-foreign paint-hline () nil
  (style style)
  (window gdk:window)
  (state state-type)
  (x1 int)
  (x2 int)
  (y int))
