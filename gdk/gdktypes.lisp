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

;; $Id: gdktypes.lisp,v 1.3 2001-05-11 16:17:21 espen Exp $

(in-package "GDK")

(init-types-in-library "/opt/gnome/lib/libgdk-x11-1.3.so")

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
    :accessor color-grenn
    :type unsigned-short)
   (blue
    :allocation :alien
    :accessor color-blue
    :type unsigned-short))
  (:metaclass boxed-class))



(define-types-by-introspection "Gdk"
  ("GdkFunction" :type gc-function)
  ("GdkGC" :type gc)
  ("GdkDrawableImplX11" :ignore t)
  ("GdkWindowImplX11" :ignore t)
  ("GdkPixmapImplX11" :ignore t)
  ("GdkGCX11" :ignore t)
  ("GdkColor" :ignore t)
  ("GdkEvent" :ignore t))


(deftype bitmap () 'pixmap)

(defclass cursor (struct)
  ((type
    :allocation :alien
    :accessor cursor-type
    :initarg :type
    :type cursor-type))
  (:metaclass proxy-class)
  (:copy %cursor-copy)
  (:free %cursor-free))

(defclass device (struct)
  ()
  (:metaclass proxy-class))

