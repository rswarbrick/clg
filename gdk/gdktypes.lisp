;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gdktypes.lisp,v 1.10 2004-12-26 11:47:24 espen Exp $

(in-package "GDK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string
			    (pkg-config:pkg-variable "gtk+-2.0" "libdir")
			    "/libgdk-x11-2.0.so") :prefix "gdk_")
  (init-types-in-library #.(concatenate 'string
			    (pkg-config:pkg-variable "gtk+-2.0" "libdir")
			    "/libgdk-x11-2.0.so") :prefix "_gdk_")
  (init-types-in-library #.(concatenate 'string
			    (pkg-config:pkg-variable "gtk+-2.0" "libdir")
			    "/libgdk_pixbuf-2.0.so") :prefix "gdk_"))


(defclass color (boxed)
  ((pixel
    :allocation :alien
    :type unsigned-long)
   (red
    :allocation :alien
    :accessor color-red
    :type unsigned-short)
   (green
    :allocation :alien
    :accessor color-green
    :type unsigned-short)
   (blue
    :allocation :alien
    :accessor color-blue
    :type unsigned-short))
  (:metaclass boxed-class)
  (:alien-name "GdkColor"))


(deftype point () '(vector int 2))
(deftype segment () '(vector int 4))
(deftype trapezoid () '(vector double-float 6))



;; Could this just as well have been a vector?
(defclass rectangle (boxed)
  ((x
    :allocation :alien
    :accessor rectangle-x
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor rectangle-y
    :initarg :y
    :type int)
   (width
    :allocation :alien
    :accessor rectangle-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor rectangle-height
    :initarg :height
    :type int))
  (:metaclass boxed-class)
  (:alien-name "GdkRectangle"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-types-by-introspection "Gdk"
    ("GdkFunction" :type gc-function)
    ("GdkWMDecoration" :type wm-decoration)
    ("GdkWMFunction" :type wm-function)
    ("GdkGC" :type gc)
    ("GdkGCX11" :type gc-x11)
    ("GdkGCValuesMask" :type gc-values-mask)
    ("GdkDrawableImplX11" :ignore t)
    ("GdkWindowImplX11" :ignore t)
    ("GdkPixmapImplX11" :ignore t)
    ("GdkGCX11" :ignore t)
    ("GdkColor" :ignore t)
    ("GdkEvent" :ignore t)
    ("GdkRectngle" :ignore t)
    ("GdkFont" :ignore t) ; deprecated

    ("GdkDrawable"
     :slots
     ((display
       :allocation :virtual
       :getter "gdk_drawable_get_display"
       :reader drawable-display
       :type display)
      (screen
       :allocation :virtual
       :getter "gdk_drawable_get_screen"
       :reader drawable-screen
       :type screen)
      (visual
       :allocation :virtual
       :getter "gdk_drawable_get_visual"
       :reader drawable-visual
       :type visual)
      (colormap
       :allocation :virtual
       :getter "gdk_drawable_get_colormap"
       :setter "gdk_drawable_set_colormap"
       :unbound nil
       :accessor drawable-colormap
       :initarg :colormap
       :type colormap)
      (depth
       :allocation :virtual
       :getter "gdk_drawable_get_depth"
       :reader drawable-depth
       :type int)
      (with 
       :allocation :virtual
       :getter drawable-width)
      (height
       :allocation :virtual
       :getter drawable-height)))

    ("GdkWindow"
     :slots
     ((state
       :allocation :virtual
       :getter "gdk_window_get_state"
       :reader window-state
       :type window-state)
      (parent
       :allocation :virtual
       :getter "gdk_window_get_parent"
       :reader window-parent
       :type window)
      (toplevel
       :allocation :virtual
       :getter "gdk_window_get_toplevel"
       :reader window-toplevel
       :type window)
      (children
       :allocation :virtual
       :getter "gdk_window_get_children"
       :reader window-children
       :type (glist window))
      (events
       :allocation :virtual
       :getter "gdk_window_get_events"
       :setter "gdk_window_set_events"
       :accessor window-events
       :type event-mask)
      (group
       :allocation :virtual
       :getter "gdk_window_get_group"
       :setter "gdk_window_set_group"
       :unbound nil
       :accessor window-group
       :type window)

      ))
))


(deftype bitmap () 'pixmap)

(defclass cursor (struct)
  ((type
    :allocation :alien
    :reader cursor-type
    :type cursor-type)
   (ref-count
    :allocation :alien
    :type unsigned-int)
   (display
    :allocation :virtual
    :getter "gdk_cursor_get_display"
    :reader cursor-display
    :type display))
  (:metaclass struct-class))

(defclass device (struct)
  ()
  (:metaclass struct-class))

(defclass geometry (struct)
  ((min-width 
    :allocation :alien
    :accessor geometry-min-width
    :initarg :min-width
    :type int)
   (min-height 
    :allocation :alien
    :accessor geometry-min-height
    :initarg :min-height
    :type int)
   (max-width 
    :allocation :alien
    :accessor geometry-max-width
    :initarg :max-width
    :type int)
   (max-height 
    :allocation :alien
    :accessor geometry-max-height
    :initarg :max-height
    :type int)
   (base-width 
    :allocation :alien
    :accessor geometry-base-width
    :initarg :base-width
    :type int)
   (base-height 
    :allocation :alien
    :accessor geometry-base-height
    :initarg :base-height
    :type int)
   (width-inc
    :allocation :alien
    :accessor geometry-width-inc
    :initarg :width-inc
    :type int)
   (height-inc
    :allocation :alien
    :accessor geometry-height-inc
    :initarg :height-inc
    :type int)
   (min-aspect
    :allocation :alien
    :accessor geometry-min-aspect
    :initarg :min-aspect
    :type double-float)
   (max-aspect
    :allocation :alien
    :accessor geometry-max-aspect
    :initarg :max-aspect
    :type double-float)
   (gravity
    :allocation :alien
    :accessor geometry-gravity
    :initarg :gravity
    :type gravity))
  (:metaclass struct-class))
