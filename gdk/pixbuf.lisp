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

;; $Id: pixbuf.lisp,v 1.9 2008-12-10 03:01:34 espen Exp $


(in-package "GDK")

(defbinding %pixbuf-new () pointer
  colorspace 
  (has-alpha boolean)
  (bits-per-sample int)
  (width int)
  (height int))

(defbinding %pixbuf-new-from-data () pointer
  (data pointer)
  colorspace 	    
  (has-alpha boolean)
  (bits-per-sample int)
  (width int)
  (height int)
  (rowstride int)
  (nil null)
  (nil null))

(defbinding %pixbuf-new-from-xpm-data () pointer
  (data (vector string)))

(defbinding %pixbuf-new-from-file () pointer
  (filename pathname)
  (nil (or null gerror) :out))

(defbinding %pixbuf-new-from-file-at-size () pointer
  (filename pathname)
  (width int)
  (height int)
  (nil (or null gerror) :out))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding %pixbuf-new-from-file-at-scale () pointer
  (filename pathname)
  (width int)
  (height int)
  (preserve-aspect-ratio boolean)
  (nil (or null gerror) :out))

(defun %pixbuf-load (filename width height preserve-aspect-p)
  (multiple-value-bind (location gerror)
      (cond
	((and width height)
	 (%pixbuf-new-from-file-at-size filename width height))
	((or width height)
	 #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	 (error "Both :width and :height must be specified")
	 #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
	 (%pixbuf-new-from-file-at-scale filename 
	  (or width -1) (or height -1) preserve-aspect-p))
	(t (%pixbuf-new-from-file filename)))
    (if gerror
	(signal-gerror gerror)
      location)))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.14.0")
(progn
  (defbinding %pixbuf-new-from-stream () pointer
    gio:input-stream-designator
    (nil (or null gio:cancellable))
    (nil (or null gerror) :out))

  (defbinding %pixbuf-new-from-stream-at-scale () pointer
    gio:input-stream-designator
    (width int)
    (height int)
    (preserve-aspect-ratio boolean)
    (nil (or null gio:cancellable))
    (nil (or null gerror) :out))

  (defun %load-from-stream (stream width height preserve-aspect-p)
    (multiple-value-bind (location gerror)
	(cond
	  ((or width height)
	   (%pixbuf-new-from-stream-at-scale stream
	    (or width -1) (or height -1) preserve-aspect-p))
	  (t (%pixbuf-new-from-stream stream)))
      (if gerror
	  (signal-gerror gerror)
	location))))

(defmethod allocate-foreign ((pixbuf pixbuf) &key source (bits-per-sample 8)
			     (colorspace :rgb) (has-alpha t) width height 
			     (preserve-aspect-ratio t) destroy stride)
  (cond
   ((not source) 
    (%pixbuf-new colorspace has-alpha bits-per-sample width height))
   ((typep source 'pointer)
    ;; TODO: destory
    (%pixbuf-new-from-data source colorspace has-alpha bits-per-sample width height (or stride (* width (if has-alpha 4 3)))))
   ((and (vectorp source) (stringp (aref source 0)))
    (%pixbuf-new-from-xpm-data source))
   ((typep source 'vector)

    )
   ((or (pathnamep source) (stringp source))
    #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
    (unless preserve-aspect-ratio 
      (warn ":preserve-aspect-ratio not supported with this version of Gtk"))
    (%pixbuf-load source width height preserve-aspect-ratio))
   #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.14.0")
   ((typep source 'gio:input-stream-designator)
    (%load-from-stream source width height preserve-aspect-ratio))
   ((call-next-method))))


(defbinding (pixbuf-subpixbuf "gdk_pixbuf_new_subpixbuf") () 
    (or null (referenced pixbuf))
  pixbuf (src-x int) (src-y int) (width int) (height int))

(defbinding pixbuf-copy () (or null (referenced pixbuf))
  pixbuf)


(defbinding pixbuf-get-option () (copy-of string)
  (pixbuf pixbuf)
  (key string))

(defun pixbuf-load (filename &key width height size (preserve-aspect-ratio t))
  (make-instance 'pixbuf :source filename 
   :width (or size width) :height (or size height)
   :preserve-aspect-ratio preserve-aspect-ratio))


;; (defbinding pixbuf-get-file-info () (copy-of pixbuf-format)
;;   (filename pathname)
;;   (width int :out)
;;   (height int :out))

(defbinding %pixbuf-savev () boolean
  (pixbuf pixbuf)
  (filename pathname)
  (type string)
  (keys strings)
  (values strings)
  (nil (or null gerror) :out))

(defun %pixbuf-save-options (options)
  (let ((keys (make-array 0 :adjustable t :fill-pointer t))
	(values (make-array 0 :adjustable t :fill-pointer t)))
    (loop 
     for (key value) on options by #'cddr
     do (vector-push-extend (string-downcase key) keys)
        (vector-push-extend 
	 (etypecase value 
	   (string value)
	   (symbol (string-downcase value))
	   (number (format nil "~A" value)))
	 values))
    (values keys values)))

(defgeneric pixbuf-save (pixbuf dest type &rest options))

(defmethod pixbuf-save (pixbuf (filename string) type &rest options)
  (multiple-value-bind (ok-p gerror)
      (multiple-value-call #'%pixbuf-savev 
       pixbuf filename (string-downcase type) 
       (%pixbuf-save-options options))
    (unless ok-p
      (signal-gerror gerror))))

(defmethod pixbuf-save (pixbuf (pathname pathname) type &rest options)
  (apply #'pixbuf-save pixbuf (namestring (translate-logical-pathname pathname))
   type options))

(define-callback stream-write-func boolean 
    ((data pointer) (length gsize) (gerror pointer) (stream-id pointer-data))
  (block stream-write
    (handler-case
	(let ((stream (find-user-data stream-id)))
	  (write-sequence
	   (map-c-vector 'vector 'identity data '(unsigned-byte 8) length)
	   stream))
      (serious-condition (condition)
	(gerror-set-in-callback gerror (file-error-domain) 
	 (enum-int :failed 'file-error-enum) (princ-to-string condition))
	(return-from stream-write nil)))
    t))

(defbinding %pixbuf-save-to-callbackv (pixbuf stream type keys values) boolean
  (pixbuf pixbuf)
  (stream-write-func callback)
  (stream pointer-data)
  (type string)
  (keys strings)
  (values strings)
  (nil (or null gerror) :out))

(defmethod pixbuf-save (pixbuf (stream stream) type &rest options)
  (let ((stream-id (register-user-data stream)))
    (unwind-protect
	 (multiple-value-bind (ok-p gerror)
	     (multiple-value-call #'%pixbuf-save-to-callbackv
	       pixbuf stream-id (string-downcase type) 
	       (%pixbuf-save-options options))
	   (unless ok-p
	     (signal-gerror gerror)))
      (destroy-user-data stream-id))))


;; (defbinding pixbuf-new-from-xpm-data () (referenced pixbuf)
;;   (data (vector string)))

(defbinding %pixbuf-new-subpixbuf () pixbuf ;; or (referenced pixbuf)?
  (pixbuf pixbuf)
  (x int) (y int) (width int) (height int))

(defbinding %pixbuf-copy () (referenced pixbuf)
  (pixbuf pixbuf))

(defun copy-pixbuf (pixbuf &optional x y width height)
  (if (and (not x) (not y) (not width) (not height))
      (%pixbuf-copy pixbuf)
    (%pixbuf-new-subpixbuf pixbuf x y width height)))

(defbinding %pixbuf-get-from-drawable () (or null (referenced pixbuf))
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


;;; Pixbuf Loader

(defbinding %pixbuf-loader-new-with-type () pointer
  (type string)
  (nil gerror-signal :out))

(defbinding %pixbuf-loader-new-with-mime-type () pointer
  (mime-type string)
  (nil gerror-signal :out))

(defmethod allocate-foreign ((loader pixbuf-loader) &key type mime-type)
  (cond
   ((and type mime-type) 
    (error "Only one of the keyword arguments :TYPE and :MIME-TYPE can be specified"))
   (type (%pixbuf-loader-new-with-type type))
   (mime-type (%pixbuf-loader-new-with-mime-type mime-type))
   ((call-next-method))))

(defbinding pixbuf-loader-write (loader buffer &optional (length (length buffer))) boolean
  (loader pixbuf-loader)
  (buffer (unboxed-vector (unsigned-byte 8)))
  (length integer)  
  (nil gerror-signal :out))

(defbinding pixbuf-loader-close () boolean
  (loader pixbuf-loader)
  (nil gerror-signal :out))

(defbinding pixbuf-loader-get-pixbuf () (or null pixbuf)
  (loader pixbuf-loader))

(defbinding pixbuf-loader-get-animation () (or null pixbuf-animation)
  (loader pixbuf-loader))

(defbinding pixbuf-loader-set-size () nil
  (loader pixbuf-loader)
  (width integer)
  (height integer))


;;; Utilities

(defbinding pixbuf-add-alpha 
    (pixbuf &optional substitute-color (red 255) (green 255) (blue 255))
    (referenced pixbuf)
  (pixbuf pixbuf)
  (substitute-color boolean)
  (red (unsigned 8))
  (green (unsigned 8))
  (blue (unsigned 8)))

;; The purpose of this function is to be able to share pixel data
;; between GdkPixbufs and Cairo image surfaces.
#+nil
(defun pixbuf-swap-rgb (pixbuf)
  (assert (= (pixbuf-bits-per-sample pixbuf) 8))
  (assert (= (pixbuf-n-channels pixbuf) 4))
  (assert (pixbuf-has-alpha-p pixbuf))
  (let ((pixels (pixbuf-pixels pixbuf))
	(stride (pixbuf-rowstride pixbuf))
	(n-channels (pixbuf-n-channels pixbuf)))
    (loop for y from 0 below (pixbuf-height pixbuf) do
     (let ((row-offset (* y stride)))
       (loop for x from 0 below (pixbuf-width pixbuf) do
        (let* ((offset (+ row-offset (* n-channels x)))
	       (p0 (ref-uint-8 pixels offset))
	       (p2 (ref-uint-8 pixels (+ offset 2))))
	  (setf (ref-uint-8 pixels offset) p2)
	  (setf (ref-uint-8 pixels (+ offset 2)) p0)))))))

(defbinding (pixbuf-swap-rgb "clg_gdk_pixbuf_swap_rgb") () nil
  pixbuf)
