;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2004 Espen S. Johnsen <espen@users.sourceforge.net>
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

;; $Id: gtktext.lisp,v 1.1 2004-12-04 18:19:59 espen Exp $


(in-package "GTK")

;;; Text View

(defbinding text-view-scroll-to-mark () nil
  (text-view text-view)
  (mark text-mark)
  (within-margin boolean)
  (use-align boolean)
  (xalign double-float)
  (yalign double-float))

(defbinding text-view-scroll-to-iter () nil
  (text-view text-view)
  (iter text-iter)
  (within-margin boolean)
  (use-align boolean)
  (xalign double-float)
  (yalign double-float))

(defbinding text-view-scroll-mark-onscreen () nil
  (text-view text-view)
  (mark text-mark))

(defbinding text-view-move-mark-onscreen () nil
  (text-view text-view)
  (mark text-mark))

(defbinding text-view-place-cursor-onscreen () nil
  (text-view text-view))

(defbinding text-view-get-visible-rect 
    (text-view &optional (rect (make-instance 'gdk:rectangle))) nil
  (text-view text-view)
  (rect gdk:rectangle :return))

(defbinding text-view-get-iter-location 
    (text-view iter &optional (location (make-instance 'gdk:rectangle))) nil
  (text-view text-view)
  (iter text-iter)
  (location gdk:rectangle :return))

(defbinding text-view-get-line-at-y 
    (text-view y &optional (iter (make-instance 'text-iter))) nil
  (text-view text-view)
  (iter text-iter :return)
  (y int)
  (line-top int :out))

(defbinding text-view-get-line-yrange () nil
  (text-view text-view)
  (iter text-iter)
  (y int :out)
  (height int :out))

(defbinding text-view-get-iter-at-location 
    (text-view x y &optional (iter (make-instance 'text-iter))) nil
  (text-view text-view)
  (iter text-iter :return)
  (x int)
  (y int))

(defbinding text-view-buffer-to-window-coords () nil
  (text-view text-view)
  (win text-window-type)
  (buffer-x int) 
  (buffer-y int)
  (window-x int :out)
  (window-y int :out))

(defbinding text-view-window-to-buffer-coords () nil
  (text-view text-view)
  (win text-window-type)
  (window-x int)
  (window-y int)
  (buffer-x int :out) 
  (buffer-y int :out))

(defbinding text-view-get-window () gdk:window
  (text-view text-view)
  (win text-window-type))

(defbinding text-view-get-window-type () text-window-type
  (text-view text-view)
  (window gdk:window))

(defbinding %text-view-set-border-window-size () nil
  (text-view text-view)
  (win text-window-type)
  (size int))

(defun (setf text-view-border-window-size) (size text-view win)
  (%text-view-set-border-window-size text-view win size)
  size)

(defbinding (text-view-border-window-size 
	     "gtk_text_view_get_border_window_size") () int
  (text-view text-view)
  (win text-window-type))

(defbinding text-view-forward-display-line () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-backward-display-line () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-forward-display-line-end () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-backward-display-line-start () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding (text-view-starts-display-line-p 
	     "gtk_text_view_starts_display_line") () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-move-visually () boolean
  (text-view text-view)
  (iter text-iter)
  (count int))

(defbinding text-view-add-child-at-anchor () nil
  (text-view text-view)
  (child widget)
  (anchor text-child-anchor))

(defbinding text-view-add-child-in-window () nil
  (text-view text-view)
  (child widget)
  (win text-window-type)
  (x int)
  (y int))

(defbinding text-view-move-child () nil
  (text-view text-view)
  (child widget)
  (x int)
  (y int))


;; Text Child Anchor

(defbinding (text-view-child-anchor-deleted-p
	     "gtk_text_child_anchor_get_deleted") () boolean
  (anchor text-child-anchor))
