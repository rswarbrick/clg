;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gdktypes.lisp,v 1.2 2001-02-11 20:28:07 espen Exp $

(in-package "GDK")


(defclass color (alien-structure)
  ((pixel
    :allocation :alien
    :type unsigned-long)
   (red
    :allocation :alien
    :accessor color-red
    :type unsigned-short)
   (green
    :allocation :alien
    :accessor color-grenn
    :type unsigned-short)
   (blue
    :allocation :alien
    :accessor color-blue
    :type unsigned-short))
  (:metaclass alien-class)
  (:alien-name "GdkColor"))


(defclass visual (static-structure)
  ()
  (:metaclass alien-class)
  (:alien-name "GdkVisual"))


(defclass colormap (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkColormap"))


(defclass drawable (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkDrawable"))


(defclass window (drawable)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkWindow")
  (:type-init "gdk_window_object_get_type"))


(defclass pixmap (drawable)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkPixmap"))

;; Bitmaps is not defined as a propper type in gdk, only as an alias for
;; GdkDrawable, so we have to define it this way as a workaround
(defclass bitmap (alien-object)
  ()
  (:metaclass alien-class))


; (defclass geometry (alien-structure)
;   ((min-width
;     :allocation :alien
;     :accessor geometry-min-width
;     :initarg :min-width
;     :type int)  
;    (min-height
;     :allocation :alien
;     :accessor geometry-min-height
;     :initarg :min-heigth
;     :type int)
;    (max-width
;     :allocation :alien
;     :accessor geometry-max-width
;     :initarg :max-width
;     :type int)  
;    (max-height
;     :allocation :alien
;     :accessor geometry-max-height
;     :initarg :max-heigth
;     :type int)
;    (base-width
;     :allocation :alien
;     :accessor geometry-base-width
;     :initarg :base-width
;     :type int)
;    (base-height
;     :allocation :alien
;     :accessor geometry-base-height
;     :initarg :base-heigth
;     :type int)
;    (width-inc
;     :allocation :alien
;     :accessor geometry-width-inc
;     :initarg :width-inc
;     :type int)   
;    (height-inc
;     :allocation :alien
;     :accessor geometry-height-inc
;     :initarg :heigth-inc
;     :type int)
;    (min-aspect
;     :allocation :alien
;     :accessor geometry-min-aspect
;     :initarg :min-aspect
;     :type double-float)
;    (max-aspect
;     :allocation :alien
;     :accessor geometry-max-aspect
;     :initarg :max-aspect
;     :type double-float))
;   (:metaclass alien-class))
  

(defclass image (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkImage"))


(defclass gc (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkGC"))


(defclass font (alien-object)
  ()
  (:metaclass alien-class)
  (:alien-name "GdkFont"))


(defclass cursor (alien-object)
  ((type
    :allocation :alien
    :accessor cursor-type
    :initarg :type
    :type cursor-type))
  (:metaclass alien-class))


(defclass drag-context (gobject)
  ()
  (:metaclass gobject-class)
  (:alien-name "GdkDragContext"))


(defclass device (alien-structure)
  ()
  (:metaclass alien-class))

(defclass event (alien-structure)
  ((window
    :allocation :alien
    :offset #.(size-of 'pointer)
    :accessor event-window
    :initarg :window
    :type window)
   (send-event
    :allocation :alien
    :accessor event-send-event
    :initarg :send-event
    :type (boolean 8))
   (%align :allocation :alien :offset 2 :type (unsigned 8)))
  (:metaclass alien-class)
  (:alien-name "GdkEvent"))

(defclass timed-event (event)
  ((time
    :allocation :alien
    :accessor event-time
    :initarg :time
    :type (unsigned 32)))
  (:metaclass alien-class))
  
(defclass delete-event (event)
  ()
  (:metaclass alien-class))

(defclass destroy-event (event)
  ()
  (:metaclass alien-class))

(defclass expose-event (event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type int)
   (width
    :allocation :alien
    :accessor event-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor event-height
    :initarg :height
    :type int)
   (count
    :allocation :alien
    :accessor event-count
    :initarg :count
    :type int))
  (:metaclass alien-class))

(defclass motion-notify-event (timed-event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type double-float)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type double-float)
   (state
    :allocation :alien
    :offset #.(size-of 'pointer)
    :accessor event-state
    :initarg :state
    :type unsigned-int)
   (is-hint
    :allocation :alien
    :accessor event-is-hint
    :initarg :is-hint
    :type (signed 16) ; should it be (boolean 16)?
    )
   (device
    :allocation :alien
    :offset 2
    :accessor event-device
    :initarg :device
    :type device)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float))
  (:metaclass alien-class))

(defclass button-press-event (timed-event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type double-float)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type double-float)
   (state
    :allocation :alien
    :offset #.(size-of 'pointer)
    :accessor event-state
    :initarg :state
    :type modifier-type)
   (button
    :allocation :alien
    :accessor event-button
    :initarg :button
    :type unsigned-int)
   (device
    :allocation :alien
    :accessor event-device
    :initarg :device
    :type device)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float))
  (:metaclass alien-class))

(defclass 2-button-press-event (button-press-event)
  ()
  (:metaclass alien-class))

(defclass 3-button-press-event (button-press-event)
  ()
  (:metaclass alien-class))

(defclass button-release-event (button-press-event)
  ()
  (:metaclass alien-class))

(defclass key-press-event (event)
  ()
  (:metaclass alien-class))

(defclass key-release-event (event)
  ()
  (:metaclass alien-class))

(defclass enter-notify-event (event)
  ()
  (:metaclass alien-class))

(defclass leave-notify-event (event)
  ()
  (:metaclass alien-class))

(defclass focus-change-event (event)
  ()
  (:metaclass alien-class))

(defclass configure-event (event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type int)
   (width
    :allocation :alien
    :accessor event-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor event-height
    :initarg :height
    :type int))
  (:metaclass alien-class))

(defclass map-event (event)
  ()
  (:metaclass alien-class))

(defclass unmap-event (event)
  ()
  (:metaclass alien-class))

(defclass property-notify-event (event)
  ()
  (:metaclass alien-class))

(defclass selection-clear-event (event)
  ()
  (:metaclass alien-class))

(defclass selection-request-event (event)
  ()
  (:metaclass alien-class))

(defclass selection-notify-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-enter-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-leave-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-motion-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-status-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-start-event (event)
  ()
  (:metaclass alien-class))

(defclass drag-finished-event (event)
  ()
  (:metaclass alien-class))

(defclass client-event (event)
  ()
  (:metaclass alien-class))

(defclass visibility-notify-event (event)
  ((state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type visibility-state))
  (:metaclass alien-class))

(defclass no-expose-event (event)
  ()
  (:metaclass alien-class))

(defclass scroll-event (timed-event)
  ()
  (:metaclass alien-class))

