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

;; $Id: rsvg.lisp,v 1.3 2006-02-09 22:33:13 espen Exp $

(in-package "RSVG")


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass dimension-data (struct)
    ((width
      :allocation :alien 
      :initarg :width
      :accessor dimension-data-width
      :type int)
     (height
      :allocation :alien 
      :initarg :height
      :accessor dimension-data-height
      :type int)
     (em
      :allocation :alien 
      :initarg :em
      :accessor dimension-data-em
      :type double-float)
     (ex
      :allocation :alien 
      :initarg :ex
      :accessor dimension-data-ex
      :type double-float))
    (:metaclass struct-class))


  (defclass handle (proxy)
    ((base-uri
      :allocation :virtual 
      :getter "rsvg_handle_get_base_uri"
      :setter "rsvg_handle_set_base_uri"
      :accessor handle-base-uri
      :type string)
     (dimensions
      :allocation :virtual 
      :getter handle-get-dimensions
      :reader handle-dimensions
      :type dimension-data)
     (title
      :allocation :virtual 
      :getter "rsvg_handle_get_title"
      :reader handle-title
      :type string)
     (description
      :allocation :virtual 
      :getter "rsvg_handle_get_desc"
      :reader handle-description
      :type string)
     (metadata
      :allocation :virtual 
      :getter "rsvg_handle_get_metadata"
      :reader handle-metadata
      :type string))
    (:metaclass proxy-class))

)

(defbinding init () nil)
(defbinding term () nil)

(defbinding set-default-dpi () nil
  (dpi-x double-float)
  (dpi-y double-float))

(defbinding handle-set-dpi () nil
  (handle handle)
  (dpi-x double-float)
  (dpi-y double-float))


(defbinding handle-get-dimensions (handle &optional (dimensions (make-instance 'dimension-data))) nil
  (handle handle)
  (dimensions dimension-data :return))



(defbinding handle-close () boolean
  (handle handle)
  (nil gerror :out))

(defbinding %handle-new () pointer)

(defbinding %handle-new-from-file () pointer
  (filename pathname)
  (nil gerror :out))

(defmethod allocate-foreign ((handle handle) &key filename)
  (multiple-value-bind (location gerror)
      (cond 
       (filename (%handle-new-from-file filename))
       (t (%handle-new)))
    (if gerror 
	(signal-gerror gerror)
      location)))


(defbinding %handle-free () nil
  (location pointer))

(defmethod unreference-foreign ((class (eql (find-class 'handle))) location)
  (%handle-free location))




;;; Cairo interface

(defbinding cairo-render () nil
  (cr cairo:context)
  (handle handle))