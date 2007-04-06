;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2001-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: pango.lisp,v 1.13 2007-04-06 14:51:26 espen Exp $

(in-package "PANGO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string 
			    (pkg-variable "pango" "libdir")
		            "/libpango-1.0." asdf:*dso-extension*) 
			 :prefix "pango_")
  (init-types-in-library #.(concatenate 'string 
			    (pkg-variable "pango" "libdir")
		            "/libpangoxft-1.0." asdf:*dso-extension*) 
			 :prefix "pango_xft")
  (init-types-in-library #.(concatenate 'string 
			    (pkg-variable "pango" "libdir")
			    "/libpangoft2-1.0." asdf:*dso-extension*) 
			 :prefix "pango_fc")
  (init-types-in-library #.(concatenate 'string 
			    (pkg-variable "pango" "libdir")
			    "/libpangocairo-1.0." asdf:*dso-extension*) 
			 :prefix "pango_cairo"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-types-by-introspection "Pango"))
  
(defclass font-description (boxed)
  ((family
    :allocation :virtual
    :initarg :family
    :getter "pango_font_description_get_family"
    :setter "pango_font_description_set_family"
    :boundp %font-description-family-boundp
    :makunbound %font-description-family-makunbound
    :accessor font-description-family
    :type string)
   (style
    :allocation :virtual
    :initarg :style
    :getter "pango_font_description_get_style"
    :setter "pango_font_description_set_style"
    :boundp %font-description-style-boundp
    :makunbound %font-description-style-makunbound
    :accessor font-description-style
    :type style)
   (variant
   :allocation :virtual
   :initarg :variant
   :getter "pango_font_description_get_variant"
   :setter "pango_font_description_set_variant"
   :boundp %font-description-variant-boundp
   :makunbound %font-description-variant-makunbound
   :accessor font-description-variant
   :type variant)
   (weight
   :allocation :virtual
   :initarg :weight
   :getter "pango_font_description_get_weight"
   :setter "pango_font_description_set_weight"
   :boundp %font-description-weight-boundp
   :makunbound %font-description-weight-makunbound
   :accessor font-description-weight
   :type weight)
   (stretch
   :allocation :virtual
   :initarg :stretch
   :getter "pango_font_description_get_stretch"
   :setter "pango_font_description_set_stretch"
   :boundp %font-description-stretch-boundp
   :makunbound %font-description-stretch-makbound
   :accessor font-description-stretch
   :type stretch)
   (size
   :allocation :virtual
   :initarg :size
   :setter (setf font-description-size)
   :getter "pango_font_description_get_size"
   :boundp %font-description-size-boundp
   :makunbound %font-description-size-makunbound
   :reader font-description-size
   :type integer)
   #?(pkg-exists-p "pango" :atleast-version "1.8.0")
   (absolute-size-p
   :allocation :virtual
   :getter "pango_font_description_get_size_is_absolute"
   :boundp %font-description-size-boundp
   :reader font-description-size-is-absolute-p
   :type boolean))
  (:metaclass boxed-class))

(defclass layout (gobject)
  ((context
    :allocation :virtual
    :getter "pango_layout_get_context"
    :reader layout-context
    :type context)
   (text
    :allocation :virtual
    :initarg text
    :getter "pango_layout_get_text"
    :setter %layout-set-text
    :accessor layout-text
    :type string)
   (attributes 
    :allocation :virtual
    :initarg :attributes
    :getter "pango_layout_get_attributes"
    :setter "pango_layout_set_attributes"
    :accessor layout-attributes
    :type attr-list)
   (font-description 
    :allocation :virtual
    :initarg :font-description
    :getter "pango_layout_get_font_description"
    :setter "pango_layout_set_font_description"
    :accessor layout-font-description
    :type font-description)
   (width
    :allocation :virtual
    :initarg :width
    :getter "pango_layout_get_width"
    :setter "pango_layout_set_width"
    :accessor layout-width
    :type int)
   (wrap
    :allocation :virtual
    :initarg :wrap
    :getter "pango_layout_get_wrap"
    :setter "pango_layout_set_wrap"
    :accessor layout-wrap
    :type wrap-mode)
   (ellipsize
    :allocation :virtual
    :initarg :ellipsize
    :getter "pango_layout_get_ellipsize"
    :setter "pango_layout_set_ellipsize"
    :accessor layout-ellipsize
    :type ellipsize-mode)
   (indent
    :allocation :virtual
    :initarg :indent
    :getter "pango_layout_get_indent"
    :setter "pango_layout_set_indetn"
    :accessor layout-indent
    :type int)
   (spacing
    :allocation :virtual
    :initarg :spacing
    :getter "pango_layout_get_spacing"
    :setter "pango_layout_set_spacing"
    :accessor layout-spacing
    :type int)
   (justify
    :allocation :virtual
    :initarg :justify
    :getter "pango_layout_get_justify"
    :setter "pango_layout_set_justify"
    :accessor layout-justify-p
    :type boolean)
   (auto-dir
    :allocation :virtual
    :initarg :auto-dir
    :getter "pango_layout_get_auto_dir"
    :setter "pango_layout_set_auto_dir"
    :accessor layout-auto-dir-p
    :type boolean)
   (alignment
    :allocation :virtual
    :initarg :alignment
    :getter "pango_layout_get_alignment"
    :setter "pango_layout_set_alignment"
    :accessor layout-alignment
    :type alignment)
   (tab-array
    :allocation :virtual
    :initarg :tab-array
    :getter "pango_layout_tab_array"
    :setter "pango_layout_tab-array"
    :accessor layout-tab-array
    :type tab-array)
   (single-paragraph
    :allocation :virtual
    :initarg :single-paragraph
    :getter "pango_layout_get_single_paragraph_mode"
    :setter "pango_layout_set_single_paragraph_mode"
    :accessor layout-single-paragraph-p
    :type boolean))
  (:metaclass gobject-class))

(defclass cairo-font-map (interface)
  ((resolution
    :allocation :virtual
    :getter "pango_cairo_font_map_get_resolution"
    :setter "pango_cairo_font_map_set_resolution"
    :accessor cairo-font-map-resolution
    :type double-float))
  (:metaclass interface-class))


;;;; Font description

(defmethod initialize-instance ((desc font-description) &key absolute-size)
  (call-next-method)
  (when absolute-size
    (setf (font-description-size desc t) absolute-size)))

(defbinding %font-description-new () pointer)

(defmethod allocate-foreign ((desc font-description) &rest initargs)
  (declare (ignore initargs))
  (%font-description-new))

(defbinding %font-description-get-set-fields () font-mask
  (desc font-description))

(defun %font-description-family-boundp (desc)
  (find :family (%font-description-get-set-fields desc)))

(defun %font-description-style-boundp (desc)
  (find :style (%font-description-get-set-fields desc)))

(defun %font-description-variant-boundp (desc)
  (find :variant (%font-description-get-set-fields desc)))

(defun %font-description-weight-boundp (desc)
  (find :weight (%font-description-get-set-fields desc)))

(defun %font-description-stretch-boundp (desc)
  (find :stretch (%font-description-get-set-fields desc)))

(defun %font-description-size-boundp (desc)
  (find :size (%font-description-get-set-fields desc)))

(defbinding %font-description-unset-fields () nil
  (desc font-description)
  (mask font-mask))

(defun %font-description-family-makunbound (desc)
  (%font-description-unset-fields desc :family))

(defun %font-description-style-makunbound (desc)
  (%font-description-unset-fields desc :style))

(defun %font-description-variant-makunbound (desc)
  (%font-description-unset-fields desc :variant))

(defun %font-description-weight-makunbound (desc)
  (%font-description-unset-fields desc :weight))

(defun %font-description-stretch-makunbound (desc)
  (%font-description-unset-fields desc :stretch))

(defun %font-description-size-makunbound (desc)
  (%font-description-unset-fields desc :size))

(defbinding %font-description-set-size () nil
  (desc font-description)
  (size int))

#?(pkg-exists-p "pango" :atleast-version "1.8.0")
(defbinding %font-description-set-absolute-size () nil
  (desc font-description)
  (size double-float))

(defun (setf font-description-size) (size desc &optional absolute-p)
  (if absolute-p
      #?(pkg-exists-p "pango" :atleast-version "1.8.0")
      (%font-description-set-absolute-size desc size)
      #?-(pkg-exists-p "pango" :atleast-version "1.8.0")
      (error "Setting of absolute font size requires at least Pango 1.8.0")
    (%font-description-set-size desc size)))

(defbinding font-description-merge (desc merge-desc &optional replace-p) nil
  (desc font-description)
  (merge-desc font-description)
  (replace-p boolean))

(defbinding font-description-better-match () boolean
  (desc font-description)
  (old-math font-description)
  (new-math font-description))

(defbinding font-description-from-string () font-description
  (desc string))

(defbinding font-description-to-string () string
  (desc font-description))


;;;; Layout

(defmethod initialize-instance ((layout layout) &key markup)
  (call-next-method)
  (when markup
    (layout-set-markup layout markup)))

(defmethod allocate-foreign ((layout layout) &key context)
  (etypecase context
    (context (%layout-new context))
    (cairo:context (%cairo-create-layout context))))

(defbinding %layout-new () pointer
  (context context))

(defbinding layout-copy () (referenced layout)
  (layout layout))

(defbinding layout-context-changed () nil
  (layout layout))

(defbinding %layout-set-text () nil
  (layout layout)
  (text string)
  (-1 int))

(defbinding layout-set-markup () nil
  (layout layout)
  (markup string)
  (-1 int))



;;; Cairo Rendering

(defbinding (cairo-create-font-map "pango_cairo_font_map_new") 
    () (referenced font-map))

(defbinding cairo-font-map-get-default () font-map)

(defbinding cairo-font-map-create-context () (referenced context)
  (font-map cairo-font-map))

(defbinding (cairo-context-resolution "pango_cairo_context_get_resolution")
    () double-float
  (context context))

(defbinding %cairo-context-set-resolution () nil
  (context context)
  (dpi double-float))

(defun (setf cairo-context-resolution) (dpi context)
  (%cairo-context-set-resolution context dpi))

(defbinding (cairo-context-font-options "pango_cairo_context_get_font_options")
    () cairo:font-options
  (context context))

(defbinding %cairo-context-set-font-options () nil
  (context context)
  (font-options cairo:font-options))

(defun (setf cairo-context-font-options) (font-options context)
  (%cairo-context-set-font-options context font-options))

(defbinding %cairo-create-layout () pointer
  (cr cairo:context))

(defbinding cairo-update-layout () nil
  (cr cairo:context)
  (layout layout))

(defbinding cairo-show-glyph-string () nil
  (cr cairo:context)
  (font font)
  (glyphs glyph-string))

(defbinding cairo-show-layout-line () nil
  (cr cairo:context)
  (line layout-line))

(defbinding cairo-show-layout () nil
  (cr cairo:context)
  (layout layout))

(defbinding cairo-show-error-underline () nil
  (cr cairo:context)
  (x double-float)
  (y double-float)
  (width double-float)
  (height double-float))

(defbinding cairo-glyph-string-path () nil
  (cr cairo:context)
  (font font)
  (glyphs glyph-string))

(defbinding cairo-layout-line-path () nil
  (cr cairo:context)
  (line layout-line))

(defbinding cairo-layout-path () nil
  (cr cairo:context)
  (layout layout))

(defbinding cairo-error-underline-path () nil
  (cr cairo:context)
  (x double-float)
  (y double-float)
  (width double-float)
  (height double-float))

