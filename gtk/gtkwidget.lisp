;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: gtkwidget.lisp,v 1.1 2000-08-14 16:45:02 espen Exp $

(in-package "GTK")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass widget (object)
    ((child-slots
      :allocation :instance
      :accessor widget-child-slots
      :type container-child)
     (name
      :allocation :arg
      :accessor widget-name
      :initarg :name
      :type string)
     (parent
      :allocation :arg
      :accessor widget-parent
;     :initarg :parent
      :type container)
     (x
      :allocation :arg
      :accessor widget-x-position
      :initarg :x
      :type int)
     (y
      :allocation :arg
      :accessor widget-y-position
      :initarg :y
      :type int)
     (width
      :allocation :arg
      :accessor widget-width
      :initarg :width
      :type int)
     (height
      :allocation :arg
      :accessor widget-height
      :initarg :height
      :type int)
     (visible
      :allocation :arg
      :accessor widget-visible-p
      :initarg :visible
      :type boolean)
     (sensitive
      :allocation :arg
      :accessor widget-sensitive-p
      :initarg :sensitive
      :type boolean)
     (app-paintable
      :allocation :arg
      :reader widget-app-paintable-p
;     :access :read-only
      :type boolean)
     (can-focus
      :allocation :arg
      :accessor widget-can-focus-p
      :initarg :can-focus
      :type boolean)
     (has-focus
      :allocation :arg
      :accessor widget-has-focus-p
      :initarg :has-focus
      :type boolean)
     (can-default
      :allocation :arg
      :accessor widget-can-default-p
      :initarg :can-default
      :type boolean)
     (has-default
      :allocation :arg
      :accessor widget-has-default-p
      :initarg :has-default
      :type boolean)
     (receives-default
      :allocation :arg
      :accessor widget-receives-default-p
      :initarg :receives-default
      :type boolean)
     (composite-child
      :allocation :arg
      :accessor widget-composite-child-p
      :initarg :composite-child
      :type boolean)
;    (style
;     :allocation :arg
;     :accessor widget-style
;     :initarg :style
;     :type style)
     (events
      :allocation :arg
      :accessor widget-events
      :initarg :events
      :type gdk:event-mask)
     (extension-events
      :allocation :arg
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
    (:alien-name "GtkWidget")))


(defmethod initialize-instance ((widget widget) &rest initargs &key parent)
  (declare (ignore initargs))
  (cond
   ((consp parent)
    (with-slots ((container parent) child-slots) widget
      (setf
       container (car parent)
       child-slots
       (apply
	#'make-instance
	(slot-value (class-of container) 'child-class)
	:parent container :child widget (cdr parent)))))
   (parent
    (setf (slot-value widget 'parent) parent)))
    (call-next-method))


(defmethod slot-unbound ((class object-class) (object widget) slot)
  (cond
   ((and (eq slot 'child-slots) (slot-value object 'parent))
    (with-slots (parent child-slots) object
      (setf
       child-slots
       (make-instance
	(slot-value (class-of parent) 'child-class)
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

(define-foreign widget-destroy () nil
  (widget widget))

(define-foreign widget-unparent () nil
  (widget widget))

(define-foreign widget-show () nil
  (widget widget))

(define-foreign widget-show-now () nil
  (widget widget))

(define-foreign widget-hide () nil
  (widget widget))

(define-foreign widget-show-all () nil
  (widget widget))

(define-foreign widget-hide-all () nil
  (widget widget))

(define-foreign widget-map () nil
  (widget widget))

(define-foreign widget-unmap () nil
  (widget widget))

(define-foreign widget-realize () nil
  (widget widget))

(define-foreign widget-unrealize () nil
  (widget widget))

(define-foreign widget-add-accelerator
    (widget signal accel-group key modifiers flags) nil
  (widget widget)
  ((name-to-string signal) string)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags))

(define-foreign widget-remove-accelerator
    (widget accel-group key modifiers) nil
  (widget widget)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign widget-accelerator-signal
    (widget accel-group key modifiers) unsigned-int
  (widget widget)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(define-foreign widget-lock-accelerators () nil
  (widget widget))

(define-foreign widget-unlock-accelerators () nil
  (widget widget))

(define-foreign
    ("gtk_widget_accelerators_locked" widget-accelerators-locked-p) () boolean
  (widget widget))

(define-foreign widget-event () int
  (widget widget)
  (event gdk:event))

(define-foreign widget-activate () boolean
  (widget widget))

(define-foreign widget-set-scroll-adjustments () boolean
  (widget widget)
  (hadjustment adjustment)
  (vadjustment adjustment))

(define-foreign widget-reparent () nil
  (widget widget)
  (new-parent widget))

(define-foreign widget-popup () nil
  (widget widget)
  (x int)
  (y int))

(define-foreign widget-grab-focus () nil
  (widget widget))

(define-foreign widget-grab-default () nil
  (widget widget))

;; cl-gtk.c
(define-foreign widget-allocation () nil
  (widget widget)
  (width int :out)
  (height int :out))


(define-foreign widget-set-uposition (widget &key (x t) (y t)) nil
  (widget widget)
  ((case x
     ((t) -2)
     ((nil) -1)
     (otherwise x)) int)
  ((case y
     ((t) -2)
     ((nil) -1)
     (otherwise y)) int))

(define-foreign widget-add-events () nil
  (widget widget)
  (events gdk:event-mask))

(define-foreign ("gtk_widget_get_toplevel" widget-toplevel) () widget
  (widget widget))

(define-foreign ("gtk_widget_get_ancestor"
		  widget-ancestor) (widget type) widget
  (widget widget)
  ((find-type-number type) type-number))

; (define-foreign ("gtk_widget_get_colormap" widget-colormap) () gdk:colormap
;   (widget widget))

; (define-foreign ("gtk_widget_get_visual" widget-visual) () gdk:visual
;   (widget widget))

(define-foreign ("gtk_widget_get_pointer" widget-pointer) () nil
  (widget widget)
  (x int :out)
  (y int :out))

(define-foreign ("gtk_widget_is_ancestor" widget-is-ancestor-p) () boolean
  (widget widget)
  (ancestor widget))

(define-foreign widget-set-rc-style () nil
  (widget widget))

(define-foreign widget-ensure-style () nil
  (widget widget))

(define-foreign widget-restore-default-style () nil
  (widget widget))

(define-foreign widget-reset-rc-styles () nil
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

(define-foreign widget-push-style () nil
  (style style))

(define-foreign widget-push-colormap () nil
  (colormap gdk:colormap))

; (define-foreign widget-push-visual () nil
;   (visual gdk:visual))

(define-foreign widget-push-composite-child () nil)

(define-foreign widget-pop-style () nil)

(define-foreign widget-pop-colormap () nil)

;(define-foreign widget-pop-visual () nil)

(define-foreign widget-pop-composite-child () nil)


;; Set certain default values to be used at widget creation time.

(define-foreign widget-set-default-style () nil
  (style style))

(define-foreign widget-set-default-colormap () nil
  (colormap gdk:colormap))

; (define-foreign widget-set-default-visual () nil
;   (visual gdk:visual))

(define-foreign widget-get-default-style () style)

(define-foreign widget-get-default-colormap () gdk:colormap)

(define-foreign widget-get-default-visual () gdk:visual)

(define-foreign widget-shape-combine-mask () nil
  (widget widget)
  (shape-mask gdk:bitmap)
  (x-offset int)
  (y-offset int))

;; cl-gtk.c
(define-foreign widget-mapped-p () boolean
  (widget widget))

