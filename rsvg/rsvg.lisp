;; Common Lisp bindings for librsvg
;; Copyright 2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: rsvg.lisp,v 1.9 2008-10-08 18:24:01 espen Exp $

(in-package "RSVG")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library rsvg "librsvg-2" :prefix "rsvg_")

  (define-types-by-introspection "Rsvg"
    ("RsvgError" :ignore t)))


(defbinding init () nil)
(defbinding term () nil)

(defbinding (set-default-dpi "rsvg_set_default_dpi_x_y") (dpi-x &optional (dpi-y dpi-x)) nil
  (dpi-x double-float)
  (dpi-y double-float))


(defbinding handle-write () boolean
  (handle handle)
  (data (vector (integer 8)))
  ((length data) int)
  (nil gerror-signal :out))

(defbinding handle-close () boolean
  (handle handle)
  (nil gerror-signal :out))

(defbinding (handle-get-pixbuf "rsvg_handle_get_pixbuf_sub") (handle &optional id) boolean
  (handle handle)
  (id (or null string)))


(defbinding %handle-new-from-data () pointer
  (data string)
  ((1- (utf8-length data)) int)
  (nil gerror-signal :out))

(defbinding %handle-new-from-file () pointer
  (filename pathname)
  (nil gerror-signal :out))

(defmethod allocate-foreign ((handle handle) &key data filename)
  (cond 
   (filename (%handle-new-from-file filename))
   (data (%handle-new-from-data data))
   (t (call-next-method))))

(defmacro with-handle ((handle &rest args) &body body)
  `(let ((,handle (make-instance 'handle ,@args)))
     (unwind-protect
	  (progn ,@body)
       (handle-close ,handle))))

;;; Cairo interface

(defbinding (render-cairo "rsvg_handle_render_cairo_sub") (handle cr &optional id) nil
  (handle handle)
  (cr cairo:context)
  (id (or null string)))

(defun image-surface-create-from-svg (filename &key width height (format :argb32)id)
  (with-handle (handle :filename filename)
    (multiple-value-bind (width height)
	(cond
	  ((and width height) (values width height))
	  (width 
	   (let ((ratio (/ (handle-height handle) (handle-width handle))))
	     (values width (truncate (* width ratio)))))
	  (height 
	   (let ((ratio (/ (handle-width handle) (handle-height handle))))
	     (values (truncate (* height ratio)) height)))
	  (t (values (handle-width handle) (handle-height handle))))
      (let ((image (make-instance 'cairo:image-surface 
		    :width width :height height :format format)))
	(cairo:with-surface (image cr)
	  (cairo:scale cr (/ width (handle-width handle)) (/ height (handle-height handle)))
	  (render-cairo handle cr id))
	image))))
