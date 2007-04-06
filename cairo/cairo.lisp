;; Common Lisp bindings for Cairo
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

;; $Id: cairo.lisp,v 1.12 2007-04-06 14:12:09 espen Exp $

(in-package "CAIRO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-enum-type surface-format :argb32 :rgb24 :a8 :a1)
  (define-enum-type content :color :alpha :color-alpha)
  (define-enum-type svg-version :svg-1.1 :svg-1.2)

  (define-enum-type status
    :success :no-memory :invalid-restore :invalid-pop-group
    :no-current-point :invalid-matrix :invalid-status :null-pointer
    :invalid-string :invalid-path-data :read-error :write-error
    :surface-finished :surface-type-mismatch :pattern-type-mismatch
    :invalid-content :invalid-format :invalid-visual :file-not-found
    :invalid-dash)

  (define-enum-type fill-rule :winding :even-odd)
  (define-enum-type line-cap :butt :round :square)
  (define-enum-type line-join :miter :round :bevel)    
  (define-enum-type font-slant :normal :itaic :oblique)
  (define-enum-type font-weight :normal :bold)

  (define-enum-type operator
   :clear :source :over :in :out :atop :dest :dest-over 
   :dest-in :dest-out :dest-atop :xor :add :saturate)

  (define-enum-type antialias :default :none :gray :subpixel)
  (define-enum-type extend :none :repeat :reflect)
  (define-enum-type filter :fast :good :best :nearest :bilinear :gaussian)
  (define-enum-type subpixel-order :default :rgb :bgr :vrgb :vbgr)
  (define-enum-type hint-style :default :none :slight :medium :full)
  (define-enum-type hint-metrics :default :off :on)


  (define-enum-type surface-type 
    image-surface pdf-surface ps-surface xlib-surface xcb-surface 
    glitz-surface quartz-surface win32-surface beos-surface 
    directfb-surface)

  (defclass surface-class (proxy-class)
    ())

  (defmethod validate-superclass ((class surface-class) (super standard-class))
    (subtypep (class-name super) 'surface))

  (defclass glyph (struct)
    ((index 
      :allocation :alien 
      :initarg :index 
      :accessor glyph-index 
      :type unsigned-long)
     (x     
      :allocation :alien 
      :initarg :x 
      :accessor glyph-x 
      :type double-float)
     (y     
      :allocation :alien 
      :initarg :y 
      :accessor glyph-y
      :type double-float))
    (:metaclass struct-class))

  (defclass font-face (ref-counted-object)
    ()
    (:metaclass proxy-class)
    (:ref %font-face-reference)
    (:unref %font-face-destroy))

  (defclass font-options (ref-counted-object)
    ((antialias
      :allocation :virtual 
      :getter "font_options_get_antialias"
      :setter "font_options_set_antialias"
      :accessor font-options-antialias
      :type antialias)
     (subpixel-order
      :allocation :virtual 
      :getter "font_options_get_subpixel_order"
      :setter "font_options_set_subpixel_order"
      :accessor font-options-subpixel-order
      :type subpixel-order)
     (hint-style
      :allocation :virtual 
      :getter "font_options_get_hint_style"
      :setter "font_options_set_hint_style"
      :accessor font-options-hint-style
      :type hint-style)
     (hint-metrics
      :allocation :virtual 
      :getter "font_options_get_hint_metrics"
      :setter "font_options_set_hint_metrics"
      :accessor font-options-hint-metrics
      :type hint-metrics))
    (:metaclass proxy-class)    
    (:ref %font-options-reference)
    (:unref %font-options-destroy))

  (defclass scaled-font (ref-counted-object)
    ()
    (:metaclass proxy-class)
    (:ref %scaled-font-reference)
    (:unref %scaled-font-destroy))

  (defclass matrix (struct)
    ((xx :allocation :alien :initarg :xx :initform 1.0
	 :accessor matrix-xx :type double-float)
     (yx :allocation :alien  :initarg :yx :initform 0.0
	 :accessor matrix-yx :type double-float)
     (xy :allocation :alien  :initarg :xy :initform 1.0
	 :accessor matrix-xy :type double-float)
     (yy :allocation :alien  :initarg :yy :initform 0.0
	 :accessor matrix-yy :type double-float)
     (x0 :allocation :alien  :initarg :x0 :initform 0.0
	 :accessor matrix-x0 :type double-float)
     (y0 :allocation :alien  :initarg :y0 :initform 0.0
	 :accessor matrix-y0 :type double-float))
    (:metaclass struct-class))


  (defclass text-extents (struct)
    ((x-bearing :allocation :alien :reader text-extents-x-bearing :type double-float)
     (y-bearing :allocation :alien :reader text-extents-y-bearing :type double-float)
     (width :allocation :alien :reader text-extents-width :type double-float)
     (height :allocation :alien :reader text-extents-height :type double-float)
     (x-advance :allocation :alien :reader text-extents-x-advance :type double-float)
     (y-advance :allocation :alien :reader text-extents-y-advance :type double-float))
    (:metaclass struct-class))

  (defclass pattern (ref-counted-object)
    ((extend
      :allocation :virtual 
      :getter "cairo_pattern_get_extend"
      :setter "cairo_pattern_set_extend"
      :accessor pattern-extend
      :type extend)
     (filter
      :allocation :virtual 
      :getter "cairo_pattern_get_filter"
      :setter "cairo_pattern_set_filter"
      :accessor pattern-filter
      :type filter)
     (matrix
      :allocation :virtual 
      :getter "cairo_pattern_get_matrix"
      :setter "cairo_pattern_set_matrix"
      :accessor pattern-matrix
      :type matrix))
    (:metaclass proxy-class)
    (:ref %pattern-reference)
    (:unref %pattern-destroy))


  (defclass surface (ref-counted-object)
    ((content 
      :allocation :virtual 
      :getter "cairo_surface_get_content"
      :reader surface-content
      :type content))
    (:metaclass surface-class))

  (defclass image-surface (surface)
    ((data
      :allocation :virtual 
      :getter "cairo_image_surface_get_data"
      :reader surface-data
      :type pointer)
     (format
      :allocation :virtual 
      :getter "cairo_image_surface_get_format"
      :reader surface-format
      :type surface-format)
     (width
      :allocation :virtual 
      :getter "cairo_image_surface_get_width"
      :reader surface-width
      :type int)
     (height
      :allocation :virtual 
      :getter "cairo_image_surface_get_height"
      :reader surface-height
      :type int)
     (stride
      :allocation :virtual 
      :getter "cairo_image_surface_get_stride"
      :reader surface-height
      :type int))
    (:metaclass surface-class))

  (defclass xlib-surface (surface)
    ((width
      :allocation :virtual 
      :getter "cairo_xlib_surface_get_width"
      :reader surface-width
      :type int)
     (height
      :allocation :virtual 
      :getter "cairo_xlib_surface_get_height"
      :reader surface-height
      :type int))
    (:metaclass surface-class))

  (defclass pdf-surface (surface)
    ()
    (:metaclass surface-class))
  
  (defclass ps-surface (surface)
    ()
    (:metaclass surface-class))
    
  (defclass svg-surface (surface)
    ()
    (:metaclass surface-class))


  (defclass context (ref-counted-object)
    ((target
      :allocation :virtual 
      :getter "cairo_get_target"
      :reader target
      :type surface)
     (source
      :allocation :virtual 
      :getter "cairo_get_source"
      :setter "cairo_set_source"
      :accessor source
      :type pattern)
     (antialias
      :allocation :virtual 
      :getter "cairo_get_antialias"
      :setter "cairo_set_antialias"
      :accessor antialias
      :type antialias)
     (tolerance
      :allocation :virtual 
      :getter "cairo_get_tolerance"
      :setter "cairo_set_tolerance"
      :accessor tolerance
      :type double-float)
     (fill-rule
      :allocation :virtual 
      :getter "cairo_get_fill_rule"
      :setter "cairo_set_fill_rule"
      :accessor fill-rule
      :type fill-rule)
     (line-width
      :allocation :virtual 
      :getter "cairo_get_line_width"
      :setter "cairo_set_line_width"
      :accessor line-width
      :type double-float)
     (line-cap
      :allocation :virtual 
      :getter "cairo_get_line_cap"
      :setter "cairo_set_line_cap"
      :accessor line-cap
      :type line-cap)
     (line-join
      :allocation :virtual 
      :getter "cairo_get_line_join"
      :setter "cairo_set_line_join"
      :accessor line-join
      :type line-join)
     (miter-limit
      :allocation :virtual 
      :getter "cairo_get_miter_limit"
      :setter "cairo_set_miter_limit"
      :accessor miter-limit
      :type double-float)
     (font-matrix
      :allocation :virtual 
      :getter "cairo_get_font_matrix"
      :setter "cairo_set_font_matrix"
      :accessor font-matrix
      :type matrix)
     (font-options
      :allocation :virtual 
      :getter "cairo_get_font_options"
      :setter "cairo_set_font_options"
      :accessor font-options
      :type font-options)
     (font-face
      :allocation :virtual 
      :getter "cairo_get_font_face"
      :setter "cairo_set_font_face"
      :accessor font-face
      :type font-face)
     (operator
      :allocation :virtual 
      :getter "cairo_get_operator"
      :setter "cairo_set_operator"
      :accessor operator
      :type operator)
     (matrix
      :allocation :virtual 
      :getter matrix
      :setter "cairo_set_matrix"
      :writer (setf matrix)
      :type matrix)
     )
    (:metaclass proxy-class)
    (:ref %reference)
    (:unref %destroy))


;;   (defclass path (proxy)
;;     ()
;;     (:metaclass proxy-class))

  )


;;; Cairo context

(defmethod allocate-foreign ((context context) &key target)
  (%create-context target))

(defbinding (%create-context "cairo_create") () pointer
  (target surface))

(defbinding %reference () pointer
  (location pointer))

(defbinding %destroy () nil
  (location pointer))

(defbinding (save-context "cairo_save") () nil
  (cr context))

(defbinding (restore-context "cairo_restore") () nil
  (cr context))

(defmacro with-context ((cr &optional var) &body body)
  (let ((context (or var (make-symbol "CONTEXT"))))
    `(let ((,context ,cr))
       (save-context ,context)
       (unwind-protect
	   (progn ,@body)
	 (restore-context ,context)))))

(defbinding status () status
  (cr context))

(defun ensure-color-component (component)
  (etypecase component
    (float component)
    (integer (/ component 256.0))))

(defbinding (set-source-color "cairo_set_source_rgba") (cr red green blue &optional (alpha 1.0)) nil
  (cr context)
  ((ensure-color-component red) double-float)
  ((ensure-color-component green) double-float)
  ((ensure-color-component blue) double-float)
  ((ensure-color-component alpha) double-float))

(defbinding set-source-surface (cr surface &optional (x 0.0) (y 0.0)) nil
  (cr context)
  (surface surface)
  (x double-float)
  (y double-float))

(defun set-source (cr source)
  (etypecase source
    (pattern (setf (source cr) source))
    (surface (set-source-surface cr source))
    (null (set-source-color cr 0.0 0.0 0.0))
    (list (apply #'set-source-color cr source))
    (vector (apply #'set-source-color cr (coerce source 'list)))))

(defbinding set-dash (cr dashes &optional (offset 0.0)) nil
  (cr context)
  (dashes (vector double-float))
  ((length dashes) int)
  (offset double-float))

(defbinding (paint "cairo_paint_with_alpha") (cr &optional (alpha 1.0)) nil
  (cr context)
  (alpha double-float))

(defbinding mask () nil
  (cr context)
  (pattern pattern))

(defbinding mask-surface () nil
  (cr context)
  (surface surface)
  (surface-x double-float)
  (surface-y double-float))

(defmacro defoperator (name &optional clip-p)
  (let ((iname (intern (format nil "%~A" name)))
	(pname (intern (format nil "%~A-PRESERVE" name))))
    `(progn
       (defbinding ,iname () nil
	 (cr context))
       (defbinding ,pname () nil
	 (cr context))
       (defun ,name (cr &optional preserve)
	 (if preserve
	     (,pname cr)
	   (,iname cr)))
       ,(unless clip-p
	  (let ((tname (intern (format nil "IN-~A-P" name)))
		(ename (intern (format nil "~A-EXTENTS" name))))
	    `(progn
	       (defbinding ,tname () boolean
		 (cr context)
		 (x double-float)
		 (y double-float))
	       (defbinding ,ename () nil
		 (cr context)
		 (x1 double-float :out)
		 (y1 double-float :out)
		 (x2 double-float :out)
		 (y2 double-float :out))))))))

(defoperator clip t)
(defoperator stroke)
(defoperator fill)

(defbinding reset-clip () nil
  (cr context))

(defbinding copy-page () nil
  (cr context))

(defbinding show-page () nil
  (cr context))


;;; Paths

(defbinding get-current-point () nil
  (cr context)
  (x double-float :out)
  (y double-float :out))

(defbinding new-path () nil
  (cr context))

#?(pkg-exists-p "cairo" :atleast-version "1.2")
(defbinding new-sub-path () nil
  (cr context))

(defbinding close-path () nil
  (cr context))

(defmacro defpath (name args &optional relative-p)
  (flet ((def (name type)
	   `(progn
	      ,(when (eq type 'optimized-double-float)
		 `(declaim (ftype (function (context ,@(loop repeat (length args) collect 'double-float))) ,(first name))))
	      (defbinding ,name () nil
		(cr context)
		,@(mapcar #'(lambda (arg) (list arg type)) args)))))

  `(progn
     ,(def name 'double-float)
     ,(let ((name (intern (format nil "FAST-~A" name)))
	    (cname (gffi::default-alien-fname name)))
	(def (list name cname) 'optimized-double-float))
     ,@(when relative-p
	 (let* ((rel-name (intern (format nil "REL-~A" name)))
		(fast-rel-name (intern (format nil "FAST-REL-~A" name)))
		(cname (gffi::default-alien-fname rel-name)))
	   (list
	    (def rel-name 'double-float)
	    (def (list fast-rel-name cname) 'optimized-double-float)))))))


(defpath arc (xc yc radius angle1 angle2))
(defpath arc-negative (xc yc radius angle1 angle2))
(defpath curve-to (x1 y1 x2 y2 x3 y3) t)
(defpath line-to (x y) t)
(defpath move-to (x y) t)
(defpath rectangle (x y width height))

(defun circle (cr x y radius &optional negative-p)
  (move-to cr radius 0.0d0)
  (if negative-p
      (arc-negative cr x y radius (* pi 2) 0.0d0)
    (arc cr x y radius 0.0d0 (* pi 2)))
  (close-path cr))


(defbinding glyph-path (cr glyphs) nil
  (cr context)
  (glyphs (vector glyph))
  ((length glyphs) int))

(defbinding text-path () nil
  (cr context)
  (text string))



;;; Patterns

(defbinding (pattern-add-color-stop "cairo_pattern_add_color_stop_rgba")
    (pattern offset red green blue &optional (alpha 1.0)) nil
  (pattern pattern)
  (offset double-float)
  ((ensure-color-component red) double-float)
  ((ensure-color-component green) double-float)
  ((ensure-color-component blue) double-float)
  ((ensure-color-component alpha) double-float))

(defbinding (pattern-create "cairo_pattern_create_rgba")
    (red green blue &optional (alpha 1.0)) pattern   
  ((ensure-color-component red) double-float)
  ((ensure-color-component green) double-float)
  ((ensure-color-component blue) double-float)
  ((ensure-color-component alpha) double-float))

(defbinding pattern-create-for-surface () pattern
  (surface surface))

(defbinding pattern-create-linear () pattern
  (x0 double-float)
  (y0 double-float)
  (x1 double-float)
  (y1 double-float))

(defbinding pattern-create-radial () pattern
  (cx0 double-float)
  (cy0 double-float)
  (radius0 double-float)
  (cx1 double-float)
  (cy1 double-float)
  (radius1 double-float))

(defbinding %pattern-reference () pointer
  (location pointer))

(defbinding %pattern-destroy () nil
  (location pointer))

(defbinding pattern-status () status
  (pattern pattern))



;;; Transformations

(defbinding translate () nil
  (cr context)
  (tx double-float)
  (ty double-float))

(defbinding scale (cr sx &optional (sy sx)) nil
  (cr context)
  (sx double-float)
  (sy double-float))

(defun scale-to-device (cr &optional keep-rotation-p)
  (if keep-rotation-p
      (multiple-value-call #'scale cr (device-to-user-distance cr 1.0))
    (multiple-value-bind (x y) 
	(multiple-value-call #'user-to-device cr (get-current-point cr))
;      (identity-matrix cr)
      (setf (matrix cr) (matrix-init-identity))
      (translate cr x y))))

(defbinding rotate () nil
  (cr context)
  (angle double-float))

(defbinding transform () nil
  (cr context)
  (matrix matrix))

(defbinding (matrix "cairo_get_matrix") () nil
  (cr context)
  ((make-instance 'matrix) matrix :in/return))

(defbinding identity-matrix () nil
  (cr context))

(defbinding user-to-device () nil
  (cr context)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding user-to-device-distance (cr dx &optional (dy dx)) nil
  (cr context)
  (dx double-float :in/out)
  (dy double-float :in/out))

(defbinding device-to-user () nil
  (cr context)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding device-to-user-distance (cr dx &optional (dy dx)) nil
  (cr context)
  (dx double-float :in/out)
  (dy double-float :in/out))


;;; Text

(defbinding select-font-face () nil
  (cr context)
  (family string)
  (slant font-slant)
  (weight font-weight))

(defbinding set-font-size () nil
  (cr context)
  (size double-float))

(defbinding show-text () nil
  (cr context)
  (text string))

(defbinding show-glyphs () nil
  (cr context)
  (glyphs (vector glyph))
  ((length glyphs) int))

(defbinding font-extents () boolean
  (cr context))

(defbinding text-extents (cr text &optional (extents (make-instance 'text-extents))) nil
  (cr context)
  (text string)
  (extents text-extents :in/return))

(defbinding glyph-extents (cr glyphs &optional (extents (make-instance 'text-extents))) nil
  (cr context)
  (glyphs (vector glyph))
  ((length glyphs) int)
  (extents text-extents :in/return))


;;; Fonts

(defbinding %font-face-reference () pointer
  (location pointer))

(defbinding %font-face-destroy () nil
  (location pointer))

(defbinding font-face-status () status
  (font-face font-face))



;;; Scaled Fonts

(defbinding %scaled-font-reference () pointer
  (location pointer))

(defbinding %scaled-font-destroy () nil
  (location pointer))

(defbinding scaled-font-status () status
  (scaled-font scaled-font))

(defbinding scaled-font-extents (scaled-font &optional (extents (make-instance 'text-extents))) nil
  (scaled-font scaled-font)
  (extents text-extents :in/return))

(defbinding scaled-font-glyph-extents (scaled-font glyphs &optional (extents (make-instance 'text-extents))) nil
  (scaled-font scaled-font)
  (glyphs (vector glyph))
  ((length glyphs) int)
  (extents text-extents :in/return))

(defbinding %scaled-font-create () pointer
  (font-face font-face)
  (font-matrix matrix)
  (ctm matrix)
  (options font-options))

(defmethod allocate-foreign ((scaled-font scaled-font) &key font-face font-matrix cmt options)
  (%scaled-font-create font-face font-matrix cmt options))



;;; Font Options


(defbinding %font-options-copy () nil
  (location pointer))

(defbinding %font-options-destroy () nil
  (location pointer))

(defbinding font-options-status () status
  (font-options font-options))

(defbinding %font-options-create () pointer)

(defmethod allocate-foreign ((font-options font-options) &rest initargs)
  (declare (ignore initargs))
  (%font-options-create))

(defbinding font-options-merge () nil
  (options1 font-options :in/return)
  (options2 font-options))

(defbinding font-options-hash () unsigned-int
  (options font-options))

(defbinding font-options-equal-p () boolean
  (options1 font-options)
  (options2 font-options))



;;; Surfaces

(defmethod make-proxy-instance :around ((class surface-class) location 
					&rest initargs)
  (let ((class (find-class (%surface-get-type location))))
    (apply #'call-next-method class location initargs)))

(defbinding %surface-get-type () surface-type
  (location pointer))

(defbinding %surface-reference () pointer
  (location pointer))

(defbinding %surface-destroy () nil
  (location pointer))

(defmethod reference-function ((class surface-class))
  (declare (ignore class))
  #'%surface-reference)

(defmethod unreference-function ((class surface-class))
  (declare (ignore class))
  #'%surface-destroy)

(defbinding %surface-set-user-data (surface key data-id) status
  (surface pointer)
  ((quark-intern key) pointer-data)
  (data-id pointer-data)
  (user-data-destroy-callback callback))

(defmethod (setf user-data) (data (surface surface) key)
  (%surface-set-user-data (foreign-location surface) key (register-user-data data))
  data)

(defbinding %surface-get-user-data () pointer-data
  (surface surface)
  (key pointer-data))

(defmethod user-data ((surface surface) key)
  (find-user-data (%surface-get-user-data surface (quark-intern key))))

(defbinding surface-create-similar () surface
  (other surface)
  (format surface-format )
  (width int)
  (height int))

(defbinding surface-finish () nil
  (surface surface))

(defbinding surface-flush () nil
  (surface surface))

(defbinding surface-get-font-options () nil
  (surface surface)
  ((make-instance 'font-options) font-options :in/return))

(defbinding surface-set-device-offset () nil
  (surface surface)
  (x-offset double-float)
  (y-offset double-float))

(defbinding surface-status () status
  (surface surface))

(defbinding %surface-mark-dirty () nil
  (surface surface))

(defbinding %surface-mark-dirty-rectangle () nil
  (surface surface)
  (x int)
  (y int)
  (width int)
  (height int))

(defun surface-mark-dirty (surface &optional x y width height)
  (if x
      (%surface-mark-dirty-rectangle surface x y width height)
    (%surface-mark-dirty surface)))

(defbinding surface-set-fallback-resolution () nil
  (surface surface)
  (x-pixels-per-inch double-float)
  (y-pixels-per-inch double-float))

(define-callback stream-write-func status 
    ((stream-id pointer-data) (data pointer) (length unsigned-int))
  (let ((stream (find-user-data stream-id)))
    (typecase stream
      (stream
       (map-c-vector 'nil #'(lambda (octet) (write-byte octet stream))
	data '(unsigned-byte 8) length))
      ((or symbol function)
       (funcall stream 
	(map-c-vector 'vector #'identity data '(unsigned-byte 8) length)))))
  :success)


;; Image Surface

;; Should data be automatically freed when the surface is GCed?
(defmethod allocate-foreign ((surface image-surface) 
			     &key filename width height stride format data)
  (cond
   (filename (%image-surface-create-from-png filename))
   ((not data) (%image-surface-create format width height))
   (t
    (%image-surface-create-for-data data format width height 
     (or 
      stride
      (let ((element-size (cdr (assoc format '((:argb32 . 4) (:rgb24 . 4) (:a8 . 1) (:a1 1/8))))))
	(ceiling (* width element-size))))))))


(defbinding %image-surface-create () pointer
  (format surface-format)
  (width int)
  (hegit int))

(defbinding %image-surface-create-for-data () pointer
  (data pointer)
  (format surface-format)
  (width int)
  (hegit int)
  (stride int))

(defbinding %image-surface-create-from-png () pointer
  (filename pathname))

(defbinding surface-write-to-png () status
  (surface surface)
  (filename pathname))


;;; PDF Surface

(defmethod allocate-foreign ((surface pdf-surface)
			     &key filename stream width height)
  (cond
   ((and filename stream)
    (error "Only one of the arguments :filename and :stream may be specified"))
   (filename (%pdf-surface-create filename width height))
   (stream 
    (let* ((stream-id (register-user-data stream))
	   (location (%pdf-surface-create-for-stream stream-id width height)))
      (%surface-set-user-data location 'stream stream-id)
      location))))


(defbinding %pdf-surface-create () pointer
  (filename pathname)
  (width double-float)
  (height double-float))

(defbinding %pdf-surface-create-for-stream (stream width height) pointer
  (stream-write-func callback)
  (stream pointer-data)
  (width double-float)
  (height double-float))

(defbinding pdf-surface-set-size () nil
  (surface pdf-surface)
  (width double-float)
  (height double-float))


;;; PS Surface

(defmethod allocate-foreign ((surface ps-surface) 
			     &key filename stream width height)
  (cond
   ((and filename stream)
    (error "Only one of the arguments :filename and :stream may be specified"))
   (filename (%ps-surface-create filename width height))
   (stream 
    (let* ((stream-id (register-user-data stream))
	   (location (%ps-surface-create-for-stream stream-id width height)))
      (%surface-set-user-data location 'stream stream-id)
      location))))

(defbinding %ps-surface-create () pointer
  (filename pathname)
  (width double-float)
  (height double-float))

(defbinding %ps-surface-create-for-stream (stream width height) pointer
  (stream-write-func callback)
  (stream pointer-data)
  (width double-float)
  (height double-float))

(defbinding ps-surface-set-size () nil
  (surface ps-surface)
  (width double-float)
  (height double-float))

(defbinding ps-surface-dsc-begin-setup () nil
  (surface ps-surface))

(defbinding ps-surface-dsc-begin-page-setup () nil
  (surface ps-surface))

(defbinding ps-surface-dsc-comment () nil
  (surface ps-surface)
  (comment string))


;;; SVG Surface

(defmethod allocate-foreign ((surface svg-surface) 
			     &key filename stream width height)
  (cond
   ((and filename stream)
    (error "Only one of the arguments :filename and :stream may be specified"))
   (filename (%svg-surface-create filename width height))
   (stream 
    (let* ((stream-id (register-user-data stream))
	   (location (%svg-surface-create-for-stream stream-id width height)))
      (%surface-set-user-data location 'stream stream-id)
      location))))

(defbinding %svg-surface-create () pointer
  (filename pathname)
  (width double-float)
  (height double-float))

(defbinding %svg-surface-create-for-stream (stream width height) pointer
  (stream-write-func callback)
  (stream pointer-data)
  (width double-float)
  (height double-float))

(defbinding svg-surface-restrict-to-version () nil
  (surface svg-surface)
  (version svg-version))



;;; Matrices

(defbinding matrix-init () nil
  (matrix matrix :in/return)
  (xx double-float) (yx double-float) 
  (xy double-float) (yy double-float) 
  (x0 double-float) (y0 double-float))

(defbinding matrix-init-identity (&optional (matrix (make-instance 'matrix))) nil
  (matrix matrix :in/return))

(defbinding matrix-init-translate () nil
  (matrix matrix :in/return)
  (tx double-float)
  (ty double-float))

(defbinding matrix-init-scale () nil
  (matrix matrix :in/return)
  (sx double-float)
  (sy double-float))

(defbinding matrix-init-rotate () nil
  (matrix matrix :in/return)
  (radians double-float))

(defbinding matrix-translate () nil
  (matrix matrix :in/return)
  (tx double-float)
  (ty double-float))

(defbinding matrix-scale () nil
  (matrix matrix :in/return)
  (sx double-float)
  (sy double-float))

(defbinding matrix-rotate () nil
  (matrix matrix :in/return)
  (radians double-float))

(defbinding matrix-invert () nil
  (matrix matrix :in/return))

(defbinding matrix-multiply () nil
  (result matrix :out)
  (a matrix)
  (b matrix))

(defbinding matrix-transform-distance () nil
  (matrix matrix :in/return)
  (dx double-float)
  (dy double-float))

(defbinding matrix-transform-point () nil
  (matrix matrix :in/return)
  (x double-float)
  (y double-float))



