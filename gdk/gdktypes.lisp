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

;; $Id: gdktypes.lisp,v 1.1 2000-08-14 16:44:41 espen Exp $

(in-package "GDK")


(defclass color (alien-object)
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

; (defclass bitmap (drawable))
(deftype bitmap () 'pointer)

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


(defclass event (alien-structure)
  ((type
    :allocation :alien
;    :accessor event-type
    :type event-type)
   (window
    :allocation :alien
    :accessor event-window
    :initarg :window
    :type window)
   (send-event
    :allocation :alien
    :accessor event-send-event
    :initarg :send-event
    :type (signed 8)))
  (:metaclass alien-class)
  (:alien-name "GdkEvent"))


(defclass expose-event (event)
  ()
  (:metaclass alien-class))


(defclass delete-event (event)
  ()
  (:metaclass alien-class))




;(define-boxed device-key)
;(define-boxed device-info)
;(define-boxed time-coord)
;(define-boxed ic)
;(define-boxed ic-attr)

