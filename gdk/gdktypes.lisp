;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2006 Espen S. Johnsen <espen@users.sf.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; $Id: gdktypes.lisp,v 1.25 2006-07-06 13:05:59 espen Exp $

(in-package "GDK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string
			    (pkg-config:pkg-variable "gtk+-2.0" "libdir")
			    "/libgdk-x11-2.0.so") :prefix ("gdk_" "_gdk_"))
  (init-types-in-library #.(concatenate 'string
			    (pkg-config:pkg-variable "gtk+-2.0" "libdir")
			    "/libgdk_pixbuf-2.0.so") :prefix "gdk_"))


(defclass color (boxed)
  ((pixel
    :allocation :alien
    :type (unsigned 32))
   (red
    :allocation :alien
    :accessor color-red
    :type (unsigned 16))
   (green
    :allocation :alien
    :accessor color-green
    :type (unsigned 16))
   (blue
    :allocation :alien
    :accessor color-blue
    :type (unsigned 16)))
  (:metaclass boxed-class)
  (:packed t))


(deftype point () '(vector int 2))
(deftype segment () '(vector int 4))
(deftype trapezoid () '(vector double-float 6))
(deftype atom () 'unsigned-int)


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
  (:metaclass boxed-class))

(defclass region (struct)
  ()
  (:metaclass struct-class)
  (:ref %region-copy)
  (:unref %region-destroy))


(register-type 'event-mask '|gdk_event_mask_get_type|)
(define-flags-type event-mask
  (:exposure 2)
  :pointer-motion
  :pointer-motion-hint
  :button-motion
  :button1-motion
  :button2-motion
  :button3-motion
  :button-press
  :button-release
  :key-press
  :key-release
  :enter-notify
  :leave-notify
  :focus-change
  :structure
  :property-change
  :visibility-notify
  :proximity-in
  :proximity-out
  :substructure
  :scroll
  (:all-events #x3FFFFE))

(register-type 'event-mask '|gdk_modifier_type_get_type|)
(define-flags-type modifier-type
  :shift :lock :control :mod1 :mod2 :mod3 :mod4 :mod5 
  :button1 :button2 :button3 :button4 :button5
  (:release #.(ash 1 30)))


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
  ("GdkRectangle" :ignore t)
  ("GdkCursor" :ignore t)
  ("GdkFont" :ignore t) ; deprecated
  ("GdkEventMask" :ignore t) ; manually defined
  ("GdkModifierType" :ignore t) ; manually defined

  ("GdkDisplay"
   :slots
   ((name
     :allocation :virtual
     :getter "gdk_display_get_name"
     :reader display-name
     :type (copy-of string))
    (screens
     :allocation :virtual
     :getter display-screens)
    (devices
     :allocation :virtual
     :getter "gdk_display_list_devices"
     :reader display-devices
     :type (copy-of (glist device)))))

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
    #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
    (type-hint
     :allocation :virtual
     :getter "gdk_window_get_type_hint"
     :setter "gdk_window_set_type_hint"
     :accessor window-type-hint
     :type window-type-hint)
    #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
    (type-hint
     :allocation :virtual
     :getter "gdk_window_get_type_hint"
     :accessor window-type-hint
     :type window-type-hint)
    (decorations
     :allocation :virtual
     :getter %window-decoration-getter
     :setter "gdk_window_set_decoration"
     :boundp %window-decoration-boundp
     :accessor window-decorations
     :type wm-decoration))))


(deftype bitmap () 'pixmap)

(defclass cursor (boxed)
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
  (:metaclass boxed-class)
  (:ref %cursor-ref)
  (:unref %cursor-unref))  


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

(deftype native-window () '(unsigned 32))
