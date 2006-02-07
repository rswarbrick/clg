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

;; $Id: cairo.lisp,v 1.3 2006-02-07 17:04:40 espen Exp $

(in-package "CAIRO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-enum-type surface-format :argb32 :rgb24 :a8 :a1)

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

  (defclass font-face (proxy)
    ()
    (:metaclass proxy-class))

  (defclass font-options (proxy)
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
    (:metaclass proxy-class))

  (defclass scaled-font (proxy)
    ()
    (:metaclass proxy-class))

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

  (defclass pattern (proxy)
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
    (:metaclass proxy-class))

  (defclass context (proxy)
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
    (:metaclass proxy-class))

  (defclass surface (proxy)
    ()
    (:metaclass proxy-class))

  (defclass image-surface (surface)
    ((width
      :allocation :virtual 
      :getter "cairo_image_surface_get_width"
      :reader surface-width
      :type int)
     (height
      :allocation :virtual 
      :getter "cairo_image_surface_get_height"
      :reader surface-height
      :type int))
    (:metaclass proxy-class))

;;   (defclass path (proxy)
;;     ()
;;     (:metaclass proxy-class))

)


;;; Cairo context

(defbinding %reference () nil
  (location pointer))

(defbinding %destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'context))) location)
  (%reference location))

(defmethod unreference-foreign ((class (eql (find-class 'context))) location)
  (%destroy location))

(defbinding (save-context "cairo_save") () nil
  (cr context))

(defbinding (restore-context "cairo_restore") () nil
  (cr context))

(defmacro with-context ((cr) &body body)
  (let ((context (make-symbol "CONTEXT")))
    `(let ((,context ,cr))
       (save-context ,context)
       (unwind-protect
	   (progn ,@body)
	 (restore-context ,context)))))

(defbinding status () status
  (cr context))

(defbinding (set-source-color "cairo_set_source_rgba") (cr red green blue &optional (alpha 1.0)) nil
  (cr context)
  (red double-float)
  (green double-float)
  (blue double-float)
  (alpha double-float))

(defbinding set-source-surface () nil
  (cr context)
  (surface surface)
  (x double-float)
  (y double-float))

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
	       (defbinding ,ename () boolean
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

(defbinding close-path () nil
  (cr context))

(defbinding arc () nil
  (cr context)
  (xc double-float)
  (yc double-float)
  (radius double-float)
  (angle1 double-float)
  (angle2 double-float))

(defbinding arc-negative () nil
  (cr context)
  (xc double-float)
  (yc double-float)
  (radius double-float)
  (angle1 double-float)
  (angle2 double-float))

(defun circle (cr x y radius)
  (arc cr x y radius 0.0 (* pi 2)))

(defmacro defpath (name &rest args)
  (let ((relname (intern (format nil "REL-~A" name))))
    `(progn
       (defbinding ,name () nil
	 (cr context)
	 ,@args)
       (defbinding ,relname () nil
	 (cr context)
	 ,@args))))

(defpath curve-to
  (x1 double-float)
  (y1 double-float)
  (x2 double-float)
  (y2 double-float)
  (x3 double-float)
  (y3 double-float))

(defpath line-to
  (x double-float)
  (y double-float))

(defpath move-to
  (x double-float)
  (y double-float))

(defbinding rectangle () nil
  (cr context)
  (x double-float)
  (y double-float)
  (width double-float)
  (height double-float))

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
  (red double-float)
  (green double-float)
  (blue double-float)
  (alpha double-float))

(defbinding (pattern-create "cairo_pattern_create_rgba")
    (red green blue &optional (alpha 1.0)) pattern   
  (red double-float)
  (green double-float)
  (blue double-float)
  (alpha double-float))

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

(defbinding %pattern-reference () nil
  (location pointer))

(defbinding %pattern-destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'pattern))) location)
  (%pattern-reference location))

(defmethod unreference-foreign ((class (eql (find-class 'pattern))) location)
  (%pattern-destroy location))

(defbinding pattern-status () status
  (pattern pattern))



;;; Transformations

(defbinding translate () nil
  (cr context)
  (tx double-float)
  (ty double-float))

(defbinding scale () nil
  (cr context)
  (sx double-float)
  (sy double-float))

(defbinding rotate () nil
  (cr context)
  (angle double-float))

(defbinding transform () nil
  (cr context)
  (matrix matrix))

(defbinding (matrix "cairo_get_matrix") () nil
  (cr context)
  ((make-instance 'matrix) matrix :return))

(defbinding identity-matrix () nil
  (cr context))

(defbinding user-to-device () nil
  (cr context)
  (x double-float :in-out)
  (y double-float :in-out))

(defbinding user-to-device-distance () nil
  (cr context)
  (dx double-float :in-out)
  (dy double-float :in-out))

(defbinding device-to-user () nil
  (cr context)
  (x double-float :in-out)
  (y double-float :in-out))

(defbinding device-to-user-distance () nil
  (cr context)
  (dx double-float :in-out)
  (dy double-float :in-out))


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
  (extents text-extents :return))

(defbinding glyph-extents (cr glyphs &optional (extents (make-instance 'text-extents))) nil
  (cr context)
  (glyphs (vector glyph))
  ((length glyphs) int)
  (extents text-extents :return))


;;; Fonts

(defbinding %font-face-reference () nil
  (location pointer))

(defbinding %font-face-destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'font-face))) location)
  (%font-face-reference location))

(defmethod unreference-foreign ((class (eql (find-class 'font-face))) location)
  (%font-face-destroy location))

(defbinding font-face-status () status
  (font-face font-face))



;;; Scaled Fonts

(defbinding %scaled-font-reference () nil
  (location pointer))

(defbinding %scaled-font-destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'scaled-font))) location)
  (%scaled-font-reference location))

(defmethod unreference-foreign ((class (eql (find-class 'scaled-font))) location)
  (%scaled-font-destroy location))

(defbinding scaled-font-status () status
  (scaled-font scaled-font))

(defbinding scaled-font-extents (scaled-font &optional (extents (make-instance 'text-extents))) nil
  (scaled-font scaled-font)
  (extents text-extents :return))

(defbinding scaled-font-glyph-extents (scaled-font glyphs &optional (extents (make-instance 'text-extents))) nil
  (scaled-font scaled-font)
  (glyphs (vector glyph))
  ((length glyphs) int)
  (extents text-extents :return))

(defbinding %scaled-font-create () pointer
  (font-face font-face)
  (font-matrix matrix)
  (ctm matrix)
  (options font-options))

(defmethod initialize-instance ((scaled-font scaled-font) &key font-face font-matrix cmt options)
  (setf
   (slot-value scaled-font 'location)
   (%scaled-font-create font-face font-matrix cmt options))
  (call-next-method))



;;; Font Options


(defbinding %font-options-copy () nil
  (location pointer))

(defbinding %font-options-destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'font-options))) location)
  (%font-options-copy location))

(defmethod unreference-foreign ((class (eql (find-class 'font-options))) location)
  (%font-options-destroy location))

(defbinding font-options-status () status
  (font-options font-options))

(defbinding %font-options-create () pointer)

(defmethod initialize-instance ((font-options font-options) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value font-options 'location) (%font-options-create))
  (call-next-method))

(defbinding font-options-merge () nil
  (options1 font-options :return)
  (options2 font-options))

(defbinding font-options-hash () unsigned-int
  (options font-options))

(defbinding font-options-equal-p () boolean
  (options1 font-options)
  (options2 font-options))



;;; Surfaces

(defbinding %surface-reference () nil
  (location pointer))

(defbinding %surface-destroy () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'surface))) location)
  (%surface-reference location))

(defmethod unreference-foreign ((class (eql (find-class 'surface))) location)
  (%surface-destroy location))

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
  ((make-instance 'font-options) font-options :return))

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



;; Image Surface

;; Should data be automatically freed when the surface is GCed?
(defmethod initialize-instance ((surface image-surface) 
				&key width height stride format data)
  (setf 
   (slot-value surface 'location)
   (if (not data)
       (%image-surface-create format width height)
     (%image-surface-create-for-data data format width height 
      (or 
       stride
       (let ((element-size (cdr (assoc format '((:argb32 . 4) (:rgb24 . 4) (:a8 . 1) (:a1 1/8))))))
	 (ceiling (* width element-size)))))))
  (call-next-method))


(defbinding %image-surface-create () image-surface
  (format surface-format)
  (width int)
  (hegit int))

(defbinding %image-surface-create-for-data () image-surface
  (data pointer)
  (format surface-format)
  (width int)
  (hegit int)
  (stride int))



;;; PNG Surface

(defbinding image-surface-create-from-png (filename) image-surface
  ((truename filename) pathname))




;;; Matrices

(defbinding matrix-init () nil
  (matrix matrix :return)
  (xx double-float) (yx double-float) 
  (xy double-float) (yy double-float) 
  (x0 double-float) (y0 double-float))

(defbinding matrix-init-identity () nil
  (matrix matrix :return))

(defbinding matrix-init-translate () nil
  (matrix matrix :return)
  (tx double-float)
  (ty double-float))

(defbinding matrix-init-scale () nil
  (matrix matrix :return)
  (sx double-float)
  (sy double-float))

(defbinding matrix-init-rotate () nil
  (matrix matrix :return)
  (radians double-float))

(defbinding matrix-translate () nil
  (matrix matrix :return)
  (tx double-float)
  (ty double-float))

(defbinding matrix-scale () nil
  (matrix matrix :return)
  (sx double-float)
  (sy double-float))

(defbinding matrix-rotate () nil
  (matrix matrix :return)
  (radians double-float))

(defbinding matrix-invert () nil
  (matrix matrix :return))

(defbinding matrix-multiply () nil
  (result matrix :out)
  (a matrix)
  (b matrix))

(defbinding matrix-transform-distance () nil
  (matrix matrix :return)
  (dx double-float)
  (dy double-float))

(defbinding matrix-transform-point () nil
  (matrix matrix :return)
  (x double-float)
  (y double-float))



