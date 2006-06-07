;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2004-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: pixbuf.lisp,v 1.5 2006-06-07 13:18:20 espen Exp $


(in-package "GDK")

(defbinding pixbuf-get-option () (copy-of string)
  (pixbuf pixbuf)
  (key string))

(defbinding %pixbuf-new-from-file () (referenced pixbuf)
  (filename pathname)
  (nil gerror :out))

(defbinding %pixbuf-new-from-file-at-size () (referenced pixbuf)
  (filename pathname)
  (width int)
  (height int)
  (nil gerror :out))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding %pixbuf-new-from-file-at-scale () (referenced pixbuf)
  (filename pathname)
  (width int)
  (height int)
  (preserve-aspect-ratio boolean)
  (nil gerror :out))

(defun pixbuf-load (filename &key width height size (preserve-aspect-ratio t))
  #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
  (unless preserve-aspect-ratio 
    (warn ":preserve-aspect-ratio not supported with this version of Gtk"))

  (multiple-value-bind (pixbuf gerror)
      (cond
       (size 
	#?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	(%pixbuf-new-from-file-at-size filename size size)
	#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	(%pixbuf-new-from-file-at-scale filename size size preserve-aspect-ratio))
       ((and width height)
	#?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	(%pixbuf-new-from-file-at-size filename width height)
	#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	(%pixbuf-new-from-file-at-scale filename width height preserve-aspect-ratio))
       ((or width height)
	(error "Both :width and :height must be specified"))
       (t (%pixbuf-new-from-file filename)))
    (if gerror
	(signal-gerror gerror)
      pixbuf)))


;; (defbinding pixbuf-get-file-info () (copy-of pixbuf-format)
;;   (filename pathname)
;;   (width int :out)
;;   (height int :out))

(defbinding %pixbuf-savev () boolean
  (pixbuf pixbuf)
  (filename pathname)
  (type string)
  (keys strings)
  (values string)
  (nil gerror :out))

(defun pixbuf-save (pixbuf filename type &rest options)
  (let ((keys (make-array 0 :adjustable t :fill-pointer t))
	(values (make-array 0 :adjustable t :fill-pointer t)))
    (loop 
     as (key value . rest) = options then rest
     do (vector-push-extend (string-downcase key) keys)
        (vector-push-extend 
	 (etypecase value 
	   (string value)
	   (symbol (string-downcase value))
	   (number (format nil "~A" value)))
	 values))
    (multiple-value-bind (ok-p gerror)
	(%pixbuf-savev pixbuf filename type keys values)
      (unless ok-p
	(signal-gerror gerror)))))

(defbinding pixbuf-new-from-xpm-data () (referenced pixbuf)
  (data (vector string)))

(defbinding %pixbuf-new-subpixbuf () pixbuf ;; or (referenced pixbuf)?
  (pixbuf pixbuf)
  (x int) (y int) (width int) (height int))

(defbinding %pixbuf-copy () (referenced pixbuf)
  (pixbuf pixbuf))

(defun copy-pixbuf (pixbuf &optional x y width height)
  (if (and (not x) (not y) (not width) (not height))
      (%pixbuf-copy pixbuf)
    (%pixbuf-new-subpixbuf pixbuf x y width height)))

(defbinding %pixbuf-get-from-drawable () (referenced pixbuf)
  (dest (or null pixbuf))
  (drawable drawable)
  (colormap (or null colormap))
  (src-x int)
  (src-y int)
  (dest-x int)
  (dest-y int)
  (width int)
  (height int))

(defun pixbuf-get-from-drawable (drawable &key (src-x 0) (src-y 0) (dest-x 0) (dest-y 0) width height colormap dest)
  (unless (or (and width height) (not (typep drawable 'window)))
    (error "Width and height must be specified for windows"))
  (or
   (%pixbuf-get-from-drawable dest drawable
    (cond
     (colormap)
     ((slot-boundp drawable 'colormap) nil)
     ((colormap-get-system)))
    src-x src-y dest-x dest-y (or width -1) (or height -1))
   (error "Couldn't get pixbuf from drawable")))


;;; Utilities

(defbinding pixbuf-add-alpha 
    (pixbuf &optional substitute-color (red 255) (green 255) (blue 255))
    (referenced pixbuf)
  (pixbuf pixbuf)
  (substitute-color boolean)
  (red (unsigned 8))
  (green (unsigned 8))
  (blue (unsigned 8)))
