;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000-2002 Espen S. Johnsen <espen@users.sourceforge.net>
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

;; $Id: gtkwidget.lisp,v 1.11 2004-12-17 00:27:01 espen Exp $

(in-package "GTK")


(defmethod shared-initialize ((widget widget) names &rest initargs &key parent)
  (remf initargs :parent)
  (prog1
      (apply #'call-next-method widget names initargs)
    (when parent
      (when (slot-boundp widget 'parent)
	(container-remove (widget-parent widget) widget))
      (let ((parent-widget (first (mklist parent)))
	    (args (rest (mklist parent))))
	(apply #'container-add parent-widget widget args)))))

(defmethod shared-initialize :after ((widget widget) names &rest initargs
				     &key show-all all-visible)
  (declare (ignore initargs names))
  (when (or all-visible show-all)
    (widget-show-all widget)))


(defmethod slot-unbound ((class gobject-class) (object widget) slot)
  (cond
   ((and (eq slot 'child-slots) (slot-value object 'parent))
    (with-slots (parent child-slots) object
      (setf
       child-slots
       (make-instance
	(gethash (class-of parent) *container-to-child-class-mappings*)
	:parent parent :child object))))
   (t (call-next-method))))


(defun child-slot-value (widget slot)
  (slot-value (widget-child-slots widget) slot))

(defun (setf child-slot-value) (value widget slot)
  (setf (slot-value (widget-child-slots widget) slot) value))

(defmacro with-child-slots (slots widget &body body)
  `(with-slots ,slots (widget-child-slots ,widget)
     ,@body))


(defmacro widget-destroyed (place)
  `(setf ,place nil))


;;; Bindings

(defbinding widget-destroy () nil
  (widget widget))

(defbinding widget-unparent () nil
  (widget widget))

(defbinding widget-show () nil
  (widget widget))

(defbinding widget-show-now () nil
  (widget widget))

(defbinding widget-hide () nil
  (widget widget))

(defbinding widget-show-all () nil
  (widget widget))

(defbinding widget-hide-all () nil
  (widget widget))

(defbinding widget-map () nil
  (widget widget))

(defbinding widget-unmap () nil
  (widget widget))

(defbinding widget-realize () nil
  (widget widget))

(defbinding widget-unrealize () nil
  (widget widget))

(defbinding widget-queue-draw () nil
  (widget widget))

(defbinding widget-queue-resize () nil
  (widget widget))

(defbinding widget-size-request () nil
  (widget widget)
  (requisition requisition))

(defbinding widget-get-child-requisition () nil
  (widget widget)
  (requisition requisition))

(defbinding widget-size-allocate () nil
  (widget widget)
  (allocation allocation))


(defbinding widget-add-accelerator
    (widget signal accel-group key modifiers flags) nil
  (widget widget)
  ((name-to-string signal) string)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags))

(defbinding widget-remove-accelerator
    (widget accel-group key modifiers) nil
  (widget widget)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding (widget-set-accelerator-path "gtk_widget_set_accel_path") () nil
  (widget widget)
  (accel-path string)
  (accel-group accel-group))
  

(defbinding widget-event () int
  (widget widget)
  (event gdk:event))

(defbinding widget-activate () boolean
  (widget widget))

(defbinding widget-reparent () nil
  (widget widget)
  (new-parent widget))

(defbinding %widget-intersect () boolean
  (widget widget)
  (area gdk:rectangle)
  (intersection (or null gdk:rectangle)))

(defun widget-intersection (widget area)
  (let ((intersection (make-instance 'gdk:rectangle)))
    (when (%widget-intersect widget area intersection)
      intersection)))

(defun widget-intersect-p (widget area)
  (%widget-intersect widget area nil))

;; (defbinding (widget-is-focus-p "gtk_widget_is_focus") () boolean
;;   (widget widget))

(defbinding widget-grab-focus () nil
  (widget widget))

(defbinding widget-grab-default () nil
  (widget widget))

(defbinding widget-add-events () nil
  (widget widget)
  (events gdk:event-mask))

(defbinding widget-get-toplevel () widget
  (widget widget))

(defbinding widget-get-ancestor (widget type) widget
  (widget widget)
  ((find-type-number type) type-number))

(defbinding widget-get-pointer () nil
  (widget widget)
  (x int :out)
  (y int :out))

(defbinding (widget-is-ancestor-p "gtk_widget_is_ancestor") () boolean
  (widget widget)
  (ancestor widget))

(defbinding widget-translate-coordinates () boolean
  (src-widget widget)
  (dest-widget widget)
  (src-x int) (src-y int)
  (set-x int :out) (dest-y int :out))

(defun widget-hide-on-delete (widget)
  "Utility function; intended to be connected to the DELETE-EVENT
signal on a GtkWindow. The function calls WIDGET-HIDE on its
argument, then returns T. If connected to DELETE-EVENT, the
result is that clicking the close button for a window (on the window
frame, top right corner usually) will hide but not destroy the
window. By default, GTK+ destroys windows when DELETE-EVENT is
received."
  (widget-hide widget)
  t)
  
(defbinding widget-ensure-style () nil
  (widget widget))

(defbinding widget-reset-rc-styles () nil
  (widget widget))

(defbinding widget-push-colormap () nil
  (colormap gdk:colormap))

(defbinding widget-pop-colormap () nil)

(defbinding widget-set-default-colormap () nil
  (colormap gdk:colormap))

(defbinding widget-get-default-style () style)

(defbinding widget-get-default-colormap () gdk:colormap)

(defbinding widget-get-default-visual () gdk:visual)

(defbinding widget-get-default-direction () text-direction)

(defbinding widget-set-default-direction () nil
  (direction  text-direction))

(defbinding widget-shape-combine-mask () nil
  (widget widget)
  (shape-mask gdk:bitmap)
  (x-offset int)
  (y-offset int))

(defbinding widget-path () nil
  (widget widget)
  (path-length int :out)
  (path string :out)
  (reverse-path string :out))

(defbinding widget-class-path () nil
  (widget widget)
  (path-length int :out)
  (path string :out)
  (reverse-path string :out))

(defbinding widget-modify-style () nil
  (widget widget)
  (style rc-style))

(defbinding widget-modify-style () rc-style
  (widget widget))

(defbinding (widget-modify-foreground "gtk_widget_modify_fg") () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding (widget-modify-background "gtk_widget_modify_bg") () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-text () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-base () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-font () nil
  (widget widget)
  (state state-type)
  (font-desc pango:font-description))

(defbinding widget-create-pango-context () pango:context
  (widget widget))

(defbinding widget-get-pango-context () pango:context
  (widget widget))

(defbinding widget-create-pango-layout (widget &optional text) pango:layout
  (widget widget)
  (text (or string null)))

(defbinding widget-render-icon () gdk:pixbuf
  (widget widget)
  (stock-id string)
  (size icon-size)
  (detail string))

(defbinding widget-push-composite-child () nil)

(defbinding widget-pop-composite-child () nil)

(defbinding widget-queue-draw-area () nil
  (widget widget)
  (x int) (y int) (width int) (height int))

(defbinding widget-reset-shapes () nil
  (widget widget))

(defbinding widget-set-double-buffered () nil
  (widget widget)
  (double-buffered boolean))

(defbinding widget-set-redraw-on-allocate () nil
  (widget widget)
  (redraw-on-allocate boolean))

(defbinding widget-set-scroll-adjustments () boolean
  (widget widget)
  (hadjustment adjustment)
  (vadjustment adjustment))

(defbinding widget-mnemonic-activate () boolean
  (widget widget)
  (group-cycling boolean))

(defbinding widget-region-intersect () pointer ;gdk:region
  (widget widget)
  (region pointer)) ;gdk:region))

(defbinding widget-send-expose () int
  (widget widget)
  (event gdk:event))

(defbinding widget-get-accessible () atk:object
  (widget widget))

(defbinding widget-child-focus () boolean
  (widget widget)
  (direction direction-type))

(defbinding widget-child-notify () nil
  (widget widget)
  (child-property string))

(defbinding widget-freeze-child-notify () nil
  (widget widget))

(defbinding %widget-get-size-request () nil
  (widget widget)
  (width int :out)
  (height int :out))

(defun widget-get-size-request (widget)
  (multiple-value-bind (width height) (%widget-get-size-request widget)
     (values (unless (= width -1) width) (unless (= height -1) height))))

(defbinding widget-set-size-request (widget width height) nil
  (widget widget)
  ((or width -1) int)
  ((or height -1) int))

(defbinding widget-thaw-child-notify () nil
  (widget widget))


;;; Additional bindings and functions

(defbinding (widget-mapped-p "gtk_widget_mapped_p") () boolean
  (widget widget))

(defbinding widget-get-size-allocation () nil
  (widget widget)
  (width int :out)
  (height int :out))

(defbinding get-event-widget () widget
  (event gdk:event))

(defun (setf widget-cursor) (cursor-type widget)
  (let ((cursor (make-instance 'cursor  :type cursor-type)))
    (gdk:window-set-cursor (widget-window widget) cursor)))
