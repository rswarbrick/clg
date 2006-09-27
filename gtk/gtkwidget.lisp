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

;; $Id: gtkwidget.lisp,v 1.25 2006-09-27 08:44:44 espen Exp $

(in-package "GTK")


#-debug-ref-counting
(defmethod print-object ((widget widget) stream)
  (if (and 
       (proxy-valid-p widget) 
       (slot-boundp widget 'name) (not (zerop (length (widget-name widget)))))
      (print-unreadable-object (widget stream :type t :identity nil)
        (format stream "~S at 0x~X" 
	 (widget-name widget) (pointer-address (foreign-location widget))))
    (call-next-method)))

(defmethod shared-initialize ((widget widget) names &key (visible nil visible-p))
  (declare (ignore names))
  (when (and visible-p (not visible)) ; widget explicit set as not visible
    (setf (user-data widget 'hidden-p) t)
    (signal-connect widget 'show 
     #'(lambda () 
	 (unset-user-data widget 'hidden-p))
     :remove t))
  (call-next-method))

(defmethod shared-initialize :after ((widget widget) names &key parent visible)
  (declare (ignore names))
  (when visible
    (widget-show widget))
  (when parent
    (when (slot-boundp widget 'parent)
      (container-remove (widget-parent widget) widget))
    (destructuring-bind (parent &rest args)  (mklist parent)
      (apply #'container-add parent widget args))))

(defmethod slot-unbound ((class gobject-class) (object widget) 
			 (slot (eql 'child-properties)))
  (cond
   ((slot-boundp object 'parent)
    (with-slots (parent child-properties) object
      (setf child-properties
       (make-instance 
        (gethash (class-of parent) *container-to-child-class-mappings*)
	:parent parent :child object))))
   ((call-next-method))))


(defmethod compute-signal-function ((widget widget) signal function object)
  (declare (ignore signal))
  (if (eq object :parent)
      #'(lambda (&rest args)
	  (if (slot-boundp widget 'parent)
	      (apply function (widget-parent widget) (rest args))
	    ;; Delay until parent is set
	    (signal-connect widget 'parent-set
	     #'(lambda (old-parent)
		 (declare (ignore old-parent))
		 (let ((*signal-stop-emission* 
			#'(lambda ()
			    (warn "Ignoring emission stop in delayed signal handler"))))
		   (apply function (widget-parent widget) (rest args))))
	     :remove t)
;	    (warn "Widget has no parent -- ignoring signal")
	    ))
    (call-next-method)))
      
(defun child-property-value (widget slot)
  (slot-value (widget-child-properties widget) slot))

(defun (setf child-property-value) (value widget slot)
  (setf (slot-value (widget-child-properties widget) slot) value))

(defmacro with-child-properties (slots widget &body body)
  `(with-slots ,slots (widget-child-properties ,widget)
     ,@body))


;;; Bindings

(defbinding widget-destroy () nil
  (widget widget))

(defbinding widget-unparent () nil
  (widget widget))

(defbinding widget-show () nil
  (widget widget))

(defbinding widget-show-now () nil
  (widget widget))

(defbinding widget-hide () nil
  (widget widget))

(defun widget-hidden-p (widget)
  "Return T if WIDGET has been explicit hidden during construction."
  (user-data widget 'hidden-p))

(defbinding widget-show-all () nil
  (widget widget))

(defbinding widget-hide-all () nil
  (widget widget))

(defbinding widget-map () nil
  (widget widget))

(defbinding widget-unmap () nil
  (widget widget))

(defbinding widget-realize () nil
  (widget widget))

(defbinding widget-unrealize () nil
  (widget widget))

(defbinding widget-queue-draw () nil
  (widget widget))

(defbinding widget-queue-resize () nil
  (widget widget))

(defbinding widget-queue-resize-no-redraw () nil
  (widget widget))

(defbinding widget-size-request
    (widget &optional (requisition (make-instance 'requisition))) nil
  (widget widget)
  (requisition requisition :in/return))

(defbinding widget-get-child-requisition 
    (widget &optional (requisition (make-instance 'requisition))) nil
  (widget widget)
  (requisition requisition :in/return))

(defbinding widget-size-allocate () nil
  (widget widget)
  (allocation allocation))

(defbinding widget-add-accelerator
    (widget signal accel-group key modifiers flags) nil
  (widget widget)
  ((signal-name-to-string signal) string)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags))

(defbinding widget-remove-accelerator
    (widget accel-group key modifiers) nil
  (widget widget)
  (accel-group accel-group)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding widget-set-accel-path () nil
  (widget widget)
  (accel-path string)
  (accel-group accel-group))

(defbinding widget-list-accel-closures () (glist pointer)
  (widget widget))

(defbinding widget-can-activate-accel-p () boolean
  (widget widget)
  (signal-id unsigned-int))

(defbinding widget-event () boolean
  (widget widget)
  (event gdk:event))

(defbinding widget-activate () boolean
  (widget widget))

(defbinding widget-reparent () nil
  (widget widget)
  (new-parent widget))

(defbinding %widget-intersect () boolean
  (widget widget)
  (area gdk:rectangle)
  (intersection (or null gdk:rectangle)))

(defun widget-intersection (widget area)
  (let ((intersection (make-instance 'gdk:rectangle)))
    (when (%widget-intersect widget area intersection)
      intersection)))

(defun widget-intersect-p (widget area)
  (%widget-intersect widget area nil))

(defbinding widget-grab-focus () nil
  (widget widget))

(defbinding widget-grab-default () nil
  (widget widget))

(defbinding widget-add-events () nil
  (widget widget)
  (events gdk:event-mask))

(defbinding widget-get-toplevel () widget
  (widget widget))

(defbinding widget-get-ancestor (widget type) widget
  (widget widget)
  ((find-type-number type) type-number))

(defbinding widget-get-pointer () nil
  (widget widget)
  (x int :out)
  (y int :out))

(defbinding widget-is-ancestor-p () boolean
  (widget widget)
  (ancestor widget))

(defbinding widget-translate-coordinates () boolean
  (src-widget widget)
  (dest-widget widget)
  (src-x int) (src-y int)
  (set-x int :out) (dest-y int :out))

(defun widget-hide-on-delete (widget)
  "Utility function; intended to be connected to the DELETE-EVENT
signal on a WINDOW. The function calls WIDGET-HIDE on its
argument, then returns T. If connected to DELETE-EVENT, the
result is that clicking the close button for a window (on the window
frame, top right corner usually) will hide but not destroy the
window. By default, GTK+ destroys windows when DELETE-EVENT is
received."
  (widget-hide widget)
  t)
  
(defbinding widget-ensure-style () nil
  (widget widget))

(defbinding widget-reset-rc-styles () nil
  (widget widget))

(defbinding widget-push-colormap () nil
  (colormap gdk:colormap))

(defbinding widget-pop-colormap () nil)

(defbinding %widget-set-default-colormap () nil
  (colormap gdk:colormap))

(defun (setf widget-default-colormap) (colormap)
  (%widget-set-default-colormap colormap)
  colormap)

(defbinding (widget-default-style "gtk_widget_get_default_style") () style)

(defbinding (widget-default-colromap "gtk_widget_get_default_colormap") 
    () gdk:colormap)

(defbinding (widget-default-visual "gtk_widget_get_default_visual") 
    () gdk:visual)

(defbinding (widget-default-direction "gtk_widget_get_default_direction")
    () text-direction)

(defbinding %widget-set-default-direction () nil
  (direction text-direction))

(defun (setf widget-default-direction) (direction)
  (%widget-set-default-direction direction)
  direction)

(defbinding widget-shape-combine-mask () nil
  (widget widget)
  (shape-mask (or null gdk:bitmap))
  (x-offset int)
  (y-offset int))

(defun widget-path (widget)
  (let ((subpath (list (if (and 
			    (slot-boundp widget 'name) 
			    (not (zerop (length (widget-name widget)))))
			   (widget-name widget)
			 (type-of widget)))))
    (if (slot-boundp widget 'parent)
	(nconc (widget-path (widget-parent widget)) subpath)
      subpath)))

(defun widget-class-path (widget)
    (let ((subpath (list (type-of widget))))
  (if (slot-boundp widget 'parent)
      (nconc (widget-class-path (widget-parent widget)) subpath)
    subpath)))


(defun widget-path-lookup (path &optional (root (nreverse (window-list-toplevels))) (error-p t))
  (let ((component (first path)))
    (loop
     for widget in (mklist root)
     do (when (or
	       (and 
		(stringp component) (slot-boundp widget 'name) 
		(string= component (widget-name widget)))
	       (and
		(symbolp component) (typep widget component)))
	  (cond
	   ((endp (rest path)) (return widget))
	   ((typep widget 'container)
	    (let ((descendant (widget-path-lookup (rest path) (container-children widget) nil)))
	      (when descendant
		(return descendant))))))))
  (when error-p
    (error "Widget not found: ~A" path)))


(defun widget-find (name &optional (root (nreverse (window-list-toplevels))) (error-p t))
  "Search for a widget with the given name. ROOT should be a container
widget or a list of containers."
  (loop
   for widget in (mklist root)
   do (cond
       ((and (slot-boundp widget 'name) (string= name (widget-name widget)))
	(return-from widget-find widget))
       ((typep widget 'container)
	(let ((descendant (widget-find name (container-children widget) nil)))
	  (when descendant
	    (return-from widget-find descendant))))))
  (when error-p
    (error "Widget not found: ~A" name)))


(defbinding widget-modify-style () nil
  (widget widget)
  (style rc-style))

(defbinding widget-get-modifier-style () rc-style
  (widget widget))

(defbinding widget-modify-fg () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-bg () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-text () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-base () nil
  (widget widget)
  (state state-type)
  (color gdk:color))

(defbinding widget-modify-font (widget font-desc) nil
  (widget widget)
  ((etypecase font-desc
     (pango:font-description font-desc)
     (string (pango:font-description-from-string font-desc)))
   pango:font-description))

(defbinding widget-create-pango-context () pango:context
  (widget widget))

(defbinding widget-get-pango-context () pango:context
  (widget widget))

(defbinding widget-create-pango-layout (widget &optional text) pango:layout
  (widget widget)
  (text (or string null)))

(defbinding widget-render-icon (widget stock-id &optional size detail) 
    gdk:pixbuf
  (widget widget)
  (stock-id string)
  ((or size -1) (or icon-size int))
  (detail (or null string)))

(defbinding widget-push-composite-child () nil)

(defbinding widget-pop-composite-child () nil)

(defbinding widget-queue-draw-area () nil
  (widget widget)
  (x int) (y int) (width int) (height int))

(defbinding widget-reset-shapes () nil
  (widget widget))

;; (defbinding widget-set-double-buffered () nil
;;   (widget widget)
;;   (double-buffered boolean))

;; (defbinding widget-set-redraw-on-allocate () nil
;;   (widget widget)
;;   (redraw-on-allocate boolean))

(defbinding widget-set-scroll-adjustments () boolean
  (widget widget)
  (hadjustment (or null adjustment))
  (vadjustment (or null adjustment)))

(defbinding widget-mnemonic-activate () boolean
  (widget widget)
  (group-cycling boolean))

(defbinding widget-class-find-style-property (class name) param
  ((type-class-peek class) pointer)
  (name string))

(defbinding widget-class-list-style-properties (class)
    (vector (copy-of param) n-properties)
  ((type-class-peek class) pointer)
  (n-properties unsigned-int :out))

(defbinding widget-region-intersect () pointer ;gdk:region
  (widget widget)
  (region pointer)) ;gdk:region))

(defbinding widget-send-expose () boolean
  (widget widget)
  (event gdk:event))

(defbinding %widget-style-get-property () nil
  (widget widget)
  (name string)
  (value gvalue))

(defun style-property-value (widget style)
  (let* ((name (string-downcase style))
	 (param (widget-class-find-style-property (class-of widget) name)))
    (if (not param)
	(error "~A has no such style property: ~A" widget style)
      (with-gvalue (gvalue (param-value-type param))
        (%widget-style-get-property widget (string-downcase style) gvalue)))))

(defbinding widget-get-accessible () atk:object
  (widget widget))

(defbinding widget-child-focus () boolean
  (widget widget)
  (direction direction-type))

(defbinding widget-child-notify () nil
  (widget widget)
  (child-property string))

(defbinding widget-freeze-child-notify () nil
  (widget widget))

(defbinding widget-get-clipboard () clipboard
  (widget widget)
  (selection int #|gdk:atom|#))

(defbinding widget-get-display () gdk:display
  (widget widget))

(defbinding widget-get-root-window () gdk:window
  (widget widget))

(defbinding widget-get-screen () gdk:screen
  (widget widget))

(defbinding widget-has-screen-p () boolean
  (widget widget))

(defbinding %widget-get-size-request () nil
  (widget widget)
  (width int :out)
  (height int :out))

(defun widget-get-size-request (widget)
  (multiple-value-bind (width height) (%widget-get-size-request widget)
     (values (unless (= width -1) width) (unless (= height -1) height))))

(defbinding widget-set-size-request (widget width height) nil
  (widget widget)
  ((or width -1) int)
  ((or height -1) int))

(defbinding widget-thaw-child-notify () nil
  (widget widget))

(defbinding widget-list-mnemonic-labels () (glist widget)
  (widget widget))

(defbinding widget-add-mnemonic-label () nil
  (widget widget)
  (label widget))

(defbinding widget-remove-mnemonic-label () nil
  (widget widget)
  (label widget))


;;; Additional bindings and functions

(defbinding (widget-mapped-p "gtk_widget_mapped_p") () boolean
  (widget widget))

(defbinding widget-get-size-allocation () nil
  (widget widget)
  (width int :out)
  (height int :out))

(defbinding get-event-widget () widget
  (event gdk:event))

(defun (setf widget-cursor) (cursor-type widget)
  (warn "(SETF WIDGET-CURSOR) is deprecated, use WIDGET-SET-CURSOR instead")
  (widget-set-cursor widget cursor-type))

(defun widget-set-cursor (widget cursor &rest args)
  (gdk:window-set-cursor (widget-window widget) 
   (apply #'gdk:ensure-cursor cursor args)))

(defbinding %widget-get-parent-window () gdk:window
  (widget widget))

(defun %widget-parent-window (widget)
  (when (slot-boundp widget 'parent)
    (%widget-get-parent-window widget)))
