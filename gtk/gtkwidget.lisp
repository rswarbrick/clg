;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000-2001 Espen S. Johnsen <espen@users.sourceforge.net>
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

;; $Id: gtkwidget.lisp,v 1.5 2001-10-21 23:22:04 espen Exp $

(in-package "GTK")


(defmethod initialize-instance ((widget widget) &rest initargs &key parent)
  (declare (ignore initargs))
  (call-next-method)
  (when parent
    (let ((parent-widget (first (mklist parent)))
	  (args (rest (mklist parent))))
      (apply #'container-add parent-widget widget args))))

(defmethod initialize-instance :after ((widget widget) &rest initargs
				       &key show-all)
  (declare (ignore initargs))
  (when show-all
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

(defbinding widget-accelerator-signal
    (widget accel-group key modifiers) unsigned-int
  (widget widget)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding widget-lock-accelerators () nil
  (widget widget))

(defbinding widget-unlock-accelerators () nil
  (widget widget))

(defbinding (widget-accelerators-locked-p "gtk_widget_accelerators_locked")
    () boolean
  (widget widget))

(defbinding widget-event () int
  (widget widget)
  (event gdk:event))

(defbinding get-event-widget () widget
  (event gdk:event))

(defbinding widget-activate () boolean
  (widget widget))

(defbinding widget-set-scroll-adjustments () boolean
  (widget widget)
  (hadjustment adjustment)
  (vadjustment adjustment))

(defbinding widget-reparent () nil
  (widget widget)
  (new-parent widget))

; (defbinding widget-popup () nil
;   (widget widget)
;   (x int)
;   (y int))

(defbinding widget-grab-focus () nil
  (widget widget))

(defbinding widget-grab-default () nil
  (widget widget))

(defbinding grab-add () nil
  (widget widget))

(defbinding grab-get-current () widget)

(defbinding grab-remove () nil
  (widget widget))

(defbinding widget-allocation () nil
  (widget widget)
  (width int :out)
  (height int :out))

(defbinding widget-add-events () nil
  (widget widget)
  (events gdk:event-mask))

(defbinding (widget-toplevel "gtk_widget_get_toplevel") () widget
  (widget widget))

(defbinding (widget-ancestor "gtk_widget_get_ancestor") (widget type) widget
  (widget widget)
  ((find-type-number type) type-number))

(defbinding (widget-pointer "gtk_widget_get_pointer") () nil
  (widget widget)
  (x int :out)
  (y int :out))

(defbinding (widget-is-ancestor-p "gtk_widget_is_ancestor") () boolean
  (widget widget)
  (ancestor widget))

(defbinding widget-ensure-style () nil
  (widget widget))

(defbinding widget-reset-rc-styles () nil
  (widget widget))

(defun (setf widget-cursor) (cursor-type widget)
  (let ((cursor (gdk:cursor-new cursor-type))
	(window (widget-window widget)))
    (gdk:window-set-cursor window cursor)
    ;(gdk:cursor-destroy cursor)
    ))

;; Push/pop pairs, to change default values upon a widget's creation.
;; This will override the values that got set by the
;; widget-set-default-* functions.

(defbinding widget-push-colormap () nil
  (colormap gdk:colormap))

(defbinding widget-push-composite-child () nil)

(defbinding widget-pop-colormap () nil)

(defbinding widget-pop-composite-child () nil)


;; Set certain default values to be used at widget creation time.

(defbinding widget-set-default-colormap () nil
  (colormap gdk:colormap))

(defbinding widget-get-default-style () style)

(defbinding widget-get-default-colormap () gdk:colormap)

(defbinding widget-shape-combine-mask () nil
  (widget widget)
  (shape-mask gdk:bitmap)
  (x-offset int)
  (y-offset int))

;; defined in gtkglue.c
(defbinding widget-mapped-p () boolean
  (widget widget))

