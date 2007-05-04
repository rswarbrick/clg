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

;; $Id: gdk.lisp,v 1.36 2007-05-04 07:28:25 espen Exp $


(in-package "GDK")

;;; Initialization
 
(defbinding (gdk-init "gdk_parse_args") () nil
  "Initializes the library without opening the display."
  (nil null)
  (nil null))



;;; Display

#-debug-ref-counting
(defmethod print-object ((display display) stream)
  (if (and (proxy-valid-p display) (slot-boundp display 'name))
      (print-unreadable-object (display stream :type t :identity nil)
        (format stream "~S at 0x~X" 
	 (display-name display) (pointer-address (foreign-location display))))
    (call-next-method)))

(defbinding %display-open () display
  (display-name (or null string)))

(defun display-open (&optional display-name)
  (let ((display (or
		  (%display-open display-name)
		  (error "Opening display failed: ~A" display-name))))
    (unless (display-get-default)
      (display-set-default display))
    display))

(defbinding %display-get-n-screens () int
  (display display))

(defbinding %display-get-screen () screen
  (display display)
  (screen-num int))

(defun display-screens (&optional (display (display-get-default)))
  (loop
   for i from 0 below (%display-get-n-screens display)
   collect (%display-get-screen display i)))

(defbinding display-get-default-screen 
    (&optional (display (display-get-default))) screen
  (display display))

(defbinding display-beep (&optional (display (display-get-default))) nil
  (display display))

(defbinding display-sync (&optional (display (display-get-default))) nil
  (display display))

(defbinding display-flush (&optional (display (display-get-default))) nil
  (display display))

(defbinding display-close (&optional (display (display-get-default))) nil
  (display display))

(defbinding display-get-event
    (&optional (display (display-get-default))) event
  (display display))

(defbinding display-peek-event
    (&optional (display (display-get-default))) event
  (display display))

(defbinding display-put-event
    (event &optional (display (display-get-default))) event
  (display display)
  (event event))

(defbinding (display-connection-number "clg_gdk_connection_number")
    (&optional (display (display-get-default))) int
  (display display))

(defun find-display (name)
  (find name (list-displays) :key #'display-name :test #'string=))

(defun ensure-display (display)
  (etypecase display
    (null (display-get-default))
    (display display)
    (string 
     (or
      (find display (list-displays) :key #'display-name :test #'string=)
      (display-open display)))))


;;; Display manager

(defbinding display-get-default () display)

(defbinding (display-set-default "gdk_display_manager_set_default_display")
    (display) nil
  ((display-manager) display-manager)
  (display display))

(defbinding (list-displays "gdk_display_manager_list_displays") ()
    (gslist (static display))
  ((display-manager) display-manager))

;; The only purpose of exporting this is to make it possible for
;; applications to connect to the display-opened signal
(defbinding (display-manager "gdk_display_manager_get") () display-manager)

(defbinding display-get-core-pointer 
    (&optional (display (display-get-default))) device
  (display display))


;;; Primitive graphics structures (points, rectangles and regions)

(defbinding %rectangle-intersect () boolean
  (src1 rectangle)
  (src2 rectangle)
  (dest rectangle))

(defun rectangle-intersect (src1 src2 &optional (dest (make-instance 'rectangle)))
  "Calculates the intersection of two rectangles. It is allowed for DEST to be the same as either SRC1 or SRC2. DEST is returned if the to rectangles intersect, otherwise NIL" 
  (when (%rectangle-intersect src1 src2 dest)
    dest))

(defbinding rectangle-union (src1 src2 &optional (dest (make-instance 'rectangle))) nil
  "Calculates the union of two rectangles. The union of rectangles SRC1 and SRC2 is the smallest rectangle which includes both SRC1 and SRC2 within it. It is allowed for DEST to be the same as either SRC1 or SRC2." 
  (src1 rectangle)
  (src2 rectangle)
  (dest rectangle :in/return))

(defun ensure-rectangle (rectangle)
  (etypecase rectangle 
    (rectangle rectangle)
    (vector (make-instance 'rectangle 
	     :x (aref rectangle 0) :y (aref rectangle 1)
	     :width (aref rectangle 2) :height (aref rectangle 3)))))


(defbinding %region-new () pointer)

(defbinding %region-polygon () pointer
  (points (vector (inlined point)))
  (n-points int)
  (fill-rule fill-rule))

(defbinding %region-rectangle () pointer
  (rectangle rectangle))

(defbinding %region-copy () pointer
  (location pointer))

(defbinding %region-destroy () nil
  (location pointer))

(defmethod allocate-foreign ((region region) &key rectangle polygon fill-rule)
  (cond
   ((and rectangle polygon) 
    (error "Only one of the keyword arguments :RECTANGLE and :POLYGON can be specified"))
   (rectangle (%region-rectangle (ensure-rectangle rectangle)))
   (polygon (%region-polygon polygon (length polygon) fill-rule))
   ((%region-new))))

(defun ensure-region (region)
  (etypecase region 
    (region region)
    ((or rectangle vector) 
     (make-instance 'region :rectangle (ensure-rectangle region)))
    (list
     (make-instance 'region :polygon region))))

(defbinding region-get-clipbox (region &optional (rectangle (make-instance 'rectangle))) nil
  (region region)
  (rectangle rectangle :in/return))

(defbinding %region-get-rectangles () nil
  (region region)
  (rectangles pointer :out)
  (n-rectangles int :out))

(defun region-get-rectangles (region)
  "Obtains the area covered by the region as a list of rectangles."
  (multiple-value-bind (location length) (%region-get-rectangles region)
    (prog1
	(map-c-vector 'list #'identity location '(inlined rectangle) length :get)
      (deallocate-memory location))))

(defbinding region-empty-p () boolean
  (region region))

(defbinding region-equal-p () boolean
  (region1 region)
  (region2 region))

(defbinding region-point-in-p () boolean
  (region region)
  (x int)
  (y int))

(defbinding region-rect-in (region rectangle) overlap-type
  (region region)
  ((ensure-rectangle rectangle) rectangle))

(defbinding region-offset () nil
  (region region)
  (dx int)
  (dy int))

(defbinding region-shrink () nil
  (region region)
  (dx int)
  (dy int))

(defbinding region-intersect (source1 source2) nil
  ((ensure-region source1) region :return)
  ((ensure-region source2) region))

(defbinding region-union (source1 source2) nil
  ((ensure-region source1) region :return)
  ((ensure-region source2) region))

(defbinding region-subtract (source1 source2) nil
  ((ensure-region source1) region :return)
  ((ensure-region source2) region))

(defbinding region-xor (source1 source2) nil
  ((ensure-region source1) region :return)
  ((ensure-region source2) region))


;;; Events

(defbinding (events-pending-p "gdk_events_pending") () boolean)

(defbinding event-get () event)

(defbinding event-peek () event)

(defbinding event-get-graphics-expose () event
  (window window))

(defbinding event-put () event
  (event event))

;(defbinding event-handler-set () ...)

(defbinding set-show-events () nil
  (show-events boolean))

(defbinding get-show-events () boolean)


;;; Miscellaneous functions

(defbinding screen-width () int
  (screen screen))

(defbinding screen-height () int
  (screen screen))

(defbinding screen-width-mm () int
  (screen screen))

(defbinding screen-height-mm () int
  (screen screen))


(defbinding pointer-grab 
    (window &key owner-events events confine-to cursor time) grab-status
  (window window)
  (owner-events boolean)
  (events event-mask)
  (confine-to (or null window))
  (cursor (or null cursor))
  ((or time 0) (unsigned 32)))

(defbinding (pointer-ungrab "gdk_display_pointer_ungrab")
    (&optional time (display (display-get-default))) nil
  (display display)
  ((or time 0) (unsigned 32)))

(defbinding (pointer-is-grabbed-p "gdk_display_pointer_is_grabbed") 
    (&optional (display (display-get-default))) boolean
  (display display))

(defbinding keyboard-grab (window &key owner-events time) grab-status
  (window window)
  (owner-events boolean)
  ((or time 0) (unsigned 32)))

(defbinding (keyboard-ungrab "gdk_display_keyboard_ungrab")
    (&optional time (display (display-get-default))) nil
  (display display)
  ((or time 0) (unsigned 32)))



(defbinding atom-intern (atom-name &optional only-if-exists) atom
  ((string atom-name) string)
  (only-if-exists boolean))

(defbinding atom-name () string
  (atom atom))



;;; Visuals

(defbinding visual-get-best-depth () int)

(defbinding visual-get-best-type () visual-type)

(defbinding visual-get-system () visual)


(defbinding (%visual-get-best-with-nothing "gdk_visual_get_best") () visual)

(defbinding %visual-get-best-with-depth () visual
  (depth int))

(defbinding %visual-get-best-with-type () visual
  (type visual-type))

(defbinding %visual-get-best-with-both () visual
  (depth int)
  (type visual-type))

(defun visual-get-best (&key depth type)
  (cond
   ((and depth type) (%visual-get-best-with-both depth type))
   (depth (%visual-get-best-with-depth depth))
   (type (%visual-get-best-with-type type))
   (t (%visual-get-best-with-nothing))))

;(defbinding query-depths ..)

;(defbinding query-visual-types ..)

(defbinding list-visuals () (glist visual))


;;; Windows

(defbinding window-destroy () nil
  (window window))

(defbinding window-at-pointer () window
  (x int :out)
  (y int :out))

(defbinding window-show () nil
  (window window))

(defbinding window-show-unraised () nil
  (window window))

(defbinding window-hide () nil
  (window window))

(defbinding window-is-visible-p () boolean
  (window window))

(defbinding window-is-viewable-p () boolean
  (window window))

(defbinding window-withdraw () nil
  (window window))

(defbinding window-iconify () nil
  (window window))

(defbinding window-deiconify () nil
  (window window))

(defbinding window-stick () nil
  (window window))

(defbinding window-unstick () nil
  (window window))

(defbinding window-maximize () nil
  (window window))

(defbinding window-unmaximize () nil
  (window window))

(defbinding window-fullscreen () nil
  (window window))

(defbinding window-unfullscreen () nil
  (window window))

(defbinding window-set-keep-above () nil
  (window window)
  (setting boolean))

(defbinding window-set-keep-below () nil
  (window window)
  (setting boolean))

(defbinding window-move () nil
  (window window)
  (x int)
  (y int))

(defbinding window-resize () nil
  (window window)
  (width int)
  (height int))

(defbinding window-move-resize () nil
  (window window)
  (x int)
  (y int)
  (width int)
  (height int))

(defbinding window-scroll () nil
  (window window)
  (dx int)
  (dy int))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(defbinding window-move-region (window region dx dy) nil
  (window window)
  ((ensure-region region) region)
  (dx int)
  (dy int))

(defbinding window-reparent () nil
  (window window)
  (new-parent window)
  (x int)
  (y int))

(defbinding window-clear () nil
  (window window))

(defbinding %window-clear-area () nil
  (window window)
  (x int) (y int) (width int) (height int))

(defbinding %window-clear-area-e () nil
  (window window)
  (x int) (y int) (width int) (height int))

(defun window-clear-area (window x y width height &optional expose)
  (if expose
      (%window-clear-area-e window x y width height)
    (%window-clear-area window x y width height)))

(defbinding window-raise () nil
  (window window))

(defbinding window-lower () nil
  (window window))

(defbinding window-focus () nil
  (window window)
  (timestamp unsigned-int))

(defbinding window-register-dnd () nil
  (window window))

(defbinding window-begin-resize-drag () nil
  (window window)
  (edge window-edge)
  (button int)
  (root-x int)
  (root-y int)
  (timestamp unsigned-int))

(defbinding window-begin-move-drag () nil
  (window window)
  (button int)
  (root-x int)
  (root-y int)
  (timestamp unsigned-int))

;; Probably not needed
;; (defbinding window-constrain-size () nil ..

(defbinding window-begin-paint-region (window region) nil
  (window window)
  ((ensure-region region) region))

(defbinding window-end-paint () nil
  (window window))

(defmacro with-window-paint ((window region) &body body)
  `(progn
     (window-begin-paint-region ,window ,region)
     (unwind-protect 
	 (progn ,@body)
       (window-end-paint ,window))))

;; TODO: create wrapper function and use gdk_window_invalidate_maybe_recurse 
;; if last arg is a function
(defbinding window-invalidate-region (window region invalidate-children-p) nil
  (window window)
  ((ensure-region region) region)
  (invalidate-children-p boolean))

(defbinding window-get-update-area () region
  (window window))

(defbinding window-freeze-updates () nil
  (window window))

(defbinding window-thaw-updates () nil
  (window window))

(defbinding window-process-all-updates () nil)

(defbinding window-process-updates () nil
  (window window)
  (update-children-p boolean))

(defbinding window-set-debug-updates () nil
  (enable-p boolean))

(defbinding window-enable-synchronized-configure () nil
  (window window))
  
(defbinding window-configure-finished () nil
  (window window))

;; Deprecated, use gobject user data mechanism
(defbinding window-set-user-data () nil
  (window window)
  (user-data pointer))

(defbinding window-set-override-redirect () nil
  (window window)
  (override-redirect-p boolean))

(defbinding window-set-accept-focus () nil
  (window window)
  (accept-focus-p boolean))

(defbinding window-set-focus-on-map () nil
  (window window)
  (focus-on-map-p boolean))

;; Added if needed
; (defbinding window-add-filter () nil
; (defbinding window-remove-filter () nil

;; New code should use window-shape-combine
(defbinding window-shape-combine-mask () nil
  (window window)
  (shape-mask bitmap)
  (offset-x int)
  (offset-y int))

(defbinding %window-shape-combine-region () nil
  (window window)
  (region (or null region))
  (offset-x int)
  (offset-y int))

(defun window-shape-combine (window shape offset-x offset-y)
  (etypecase shape
    (null (%window-shape-combine-region window nil 0 0))
    (region (%window-shape-combine-region window shape offset-x offset-y))
    (bitmap (window-shape-combine-mask window shape offset-x offset-y))))

(defbinding window-set-child-shapes () nil
  (window window))

(defbinding window-merge-child-shapes () nil
  (window window))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
(progn
  (defbinding %window-input-shape-combine-mask () nil
    (window window)
    (shape-mask bitmap)
    (x int)
    (y int))

  (defbinding %window-input-shape-combine-region () nil
    (window window)
    (region (or null region))
    (x int)
    (y int))
  
  (defun window-input-shape-combine (window shape x y)
    (etypecase shape
      (null (%window-input-shape-combine-region window nil 0 0))
      (region (%window-input-shape-combine-region window shape x y))
      (bitmap (%window-input-shape-combine-mask window shape x y))))

  (defbinding window-set-child-input-shapes () nil
    (window window))
  
  (defbinding window-merge-child-input-shapes () nil
    (window window)))

(defbinding window-set-static-gravities () boolean
  (window window)
  (use-static-p boolean))

(defbinding window-set-title () nil
  (window window)
  (title string))

(defbinding window-set-background () nil
  (window window)
  (color color))

(defbinding window-set-back-pixmap (window pixmap &optional parent-relative-p) nil
  (window window)
  (pixmap (or null pixmap))
  (parent-relative-p boolean))

(defbinding window-set-cursor () nil
  (window window)
  (cursor (or null cursor)))

(defbinding window-get-geometry () nil
  (window window)
  (x int :out)
  (y int :out)
  (width int :out)
  (height int :out)
  (depth int :out))

;(defbinding window-set-geometry-hints () nil

(defbinding window-set-icon-list () nil
  (window window)
  (icons (glist pixbufs)))

(defbinding window-set-skip-taskbar-hint () nil
  (window window)
  (skip-taskbar-p boolean))

(defbinding window-set-skip-pager-hint () nil
  (window window)
  (skip-pager-p boolean))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(defbinding window-set-urgency-hint () nil
  (window window)
  (urgent-p boolean))

(defbinding window-get-position () nil
  (window window)
  (x int :out)
  (y int :out))

(defbinding window-get-root-origin () nil
  (window window)
  (x int :out)
  (y int :out))

(defbinding window-get-frame-extents (window &optional (extents (make-instance 'rect))) nil
  (window window)
  (extents rectangle :in/return))

(defbinding window-get-origin () nil ; this may not work as
  (window window)                    ; an int is actually returned
  (x int :out)
  (y int :out))

(defbinding window-get-pointer () window
  (window window)
  (x int :out)
  (y int :out)
  (mask modifier-type :out))

;(defbinding window-set-icon () nil

(defbinding window-set-icon-name () nil
  (window window)
  (icon-name string))

(defbinding window-set-transient-for () nil
  (window window)
  (parent window))

(defbinding window-set-role () nil
  (window window)
  (role string))

(defbinding %window-get-decorations () boolean
  (window window)
  (decorations wm-decoration :out))

(defun %window-decorations-getter (window)
  (nth-value 1 (%window-get-decorations window)))

(defun %window-decorations-boundp (window)
  (%window-get-decorations window))

(defbinding %window-get-toplevels () (glist window))

(defun window-get-toplevels (&optional screen)
  (if screen
      (error "Not implemented")
    (%window-get-toplevels)))

(defbinding %get-default-root-window () window)

(defun get-root-window (&optional display)
  (if display
      (error "Not implemented")
    (%get-default-root-window)))



;;; Drag and Drop

;; Destination side

(defbinding drag-status () nil
  (context drag-context)
  (action drag-action)
  (time (unsigned 32)))






;;

(defbinding rgb-init () nil)




;;; Cursor

(defmethod allocate-foreign ((cursor cursor) &key source mask fg bg 
			     (x 0) (y 0) (display (display-get-default)))
  (etypecase source
    (keyword (%cursor-new-for-display display source))
    (pixbuf (%cursor-new-from-pixbuf display source x y))
    (pixmap (%cursor-new-from-pixmap source mask 
	     (or fg (ensure-color #(0.0 0.0 0.0)))
	     (or bg (ensure-color #(1.0 1.0 1.0))) x y))
    (pathname (%cursor-new-from-pixbuf display (pixbuf-load source) x y))))

(defun ensure-cursor (cursor &rest args)
  (if (typep cursor 'cursor)
      cursor
    (apply #'make-instance 'cursor :source cursor args)))

(defbinding %cursor-new-for-display () pointer
  (display display)
  (cursor-type cursor-type))

(defbinding %cursor-new-from-pixmap () pointer
  (source pixmap)
  (mask bitmap)
  (foreground color)
  (background color)
  (x int) (y int))

(defbinding %cursor-new-from-pixbuf () pointer
  (display display)
  (pixbuf pixbuf)
  (x int) (y int))

(defbinding %cursor-ref () pointer
  (location pointer))

(defbinding %cursor-unref () nil
  (location pointer))


;;; Pixmaps

(defbinding %pixmap-new () pointer
  (window (or null window))
  (width int)
  (height int)
  (depth int))

(defmethod allocate-foreign ((pximap pixmap) &key width height depth window)
  (%pixmap-new window width height depth))

(defun pixmap-new (width height depth &key window)
  (warn "PIXMAP-NEW is deprecated, use (make-instance 'pixmap ...) instead")
  (make-instance 'pixmap :width width :height height :depth depth :window window))

(defbinding %pixmap-colormap-create-from-xpm () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (filename pathname))

(defbinding %pixmap-colormap-create-from-xpm-d () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (data (vector string)))

;; Deprecated, use pixbufs instead
(defun pixmap-create (source &key color window colormap)
  (let ((window
	 (if (not (or window colormap))
	     (get-root-window)
	   window)))
    (multiple-value-bind (pixmap mask)
        (etypecase source
	  ((or string pathname)
	   (%pixmap-colormap-create-from-xpm window colormap color  source))
 	  ((vector string)
	   (%pixmap-colormap-create-from-xpm-d window colormap color source)))
      (values pixmap mask))))


;;; Color

(defbinding colormap-get-system () colormap)

(defbinding %color-copy () pointer
  (location pointer))

(defmethod allocate-foreign ((color color)  &rest initargs)
  (declare (ignore color initargs))
  ;; Color structs are allocated as memory chunks by gdk, and since
  ;; there is no gdk_color_new we have to use this hack to get a new
  ;; color chunk
  (with-memory (location #.(foreign-size (find-class 'color)))
    (%color-copy location)))

(defun %scale-value (value)
  (etypecase value
    (integer value)
    (float (truncate (* value 65535)))))

(defmethod initialize-instance ((color color) &key (red 0.0) (green 0.0) (blue 0.0))
  (call-next-method)
  (with-slots ((%red red) (%green green) (%blue blue)) color
    (setf
     %red (%scale-value red)
     %green (%scale-value green)
     %blue (%scale-value blue))))

(defbinding %color-parse () boolean
  (spec string)
  (color color :in/return))

(defun color-parse (spec &optional (color (make-instance 'color)))
  (multiple-value-bind (succeeded-p color) (%color-parse spec color)
    (if succeeded-p
	color
      (error "Parsing color specification ~S failed." spec))))

(defun ensure-color (color)
  (etypecase color
    (null nil)
    (color color)
    (string (color-parse color))
    (vector
     (make-instance 'color 
      :red (svref color 0) :green (svref color 1) :blue (svref color 2)))))


  
;;; Drawable -- all the draw- functions are deprecated and will be
;;; removed, use cairo for drawing instead.

(defbinding drawable-get-size () nil
  (drawable drawable)
  (width int :out)
  (height int :out))

(defbinding (drawable-width "gdk_drawable_get_size") () nil
  (drawable drawable)
  (width int :out)
  (nil null))

(defbinding (drawable-height "gdk_drawable_get_size") () nil
  (drawable drawable)
  (nil null)
  (height int :out))

;; (defbinding drawable-get-clip-region () region
;;   (drawable drawable))

;; (defbinding drawable-get-visible-region () region
;;   (drawable drawable))

(defbinding draw-point () nil
  (drawable drawable) (gc gc) 
  (x int) (y int))

(defbinding %draw-points () nil
  (drawable drawable) (gc gc) 
  (points pointer)
  (n-points int))

(defbinding draw-line () nil
  (drawable drawable) (gc gc) 
  (x1 int) (y1 int)
  (x2 int) (y2 int))

(defbinding draw-pixbuf
    (drawable gc pixbuf src-x src-y dest-x dest-y &optional
     width height (dither :none) (x-dither 0) (y-dither 0)) nil
  (drawable drawable) (gc (or null gc))
  (pixbuf pixbuf)
  (src-x int) (src-y int)
  (dest-x int) (dest-y int)
  ((or width -1) int) ((or height -1) int)
  (dither rgb-dither)
  (x-dither int) (y-dither int))

(defbinding draw-rectangle () nil
  (drawable drawable) (gc gc) 
  (filled boolean)
  (x int) (y int) 
  (width int) (height int))

(defbinding draw-arc () nil
  (drawable drawable) (gc gc) 
  (filled boolean)
  (x int) (y int) 
  (width int) (height int)
  (angle1 int) (angle2 int))

(defbinding %draw-layout () nil
  (drawable drawable) (gc gc) 
  (font pango:font)
  (x int) (y int)
  (layout pango:layout))

(defbinding %draw-layout-with-colors () nil
  (drawable drawable) (gc gc) 
  (font pango:font)
  (x int) (y int)
  (layout pango:layout)
  (foreground (or null color))
  (background (or null color)))

(defun draw-layout (drawable gc font x y layout &optional foreground background)
  (if (or foreground background)
      (%draw-layout-with-colors drawable gc font x y layout foreground background)
    (%draw-layout drawable gc font x y layout)))

(defbinding draw-drawable 
    (drawable gc src src-x src-y dest-x dest-y &optional width height) nil
  (drawable drawable) (gc gc) 
  (src drawable)
  (src-x int) (src-y int)
  (dest-x int) (dest-y int)
  ((or width -1) int) ((or height -1) int))

(defbinding draw-image 
    (drawable gc image src-x src-y dest-x dest-y &optional width height) nil
  (drawable drawable) (gc gc) 
  (image image)
  (src-x int) (src-y int)
  (dest-x int) (dest-y int)
  ((or width -1) int) ((or height -1) int))

(defbinding drawable-get-image () image
  (drawable drawable)
  (x int) (y int)
  (width int) (height int))

(defbinding drawable-copy-to-image 
    (drawable src-x src-y width height &optional image dest-x dest-y) image
  (drawable drawable)
  (image (or null image))
  (src-x int) (src-y int)
  ((if image dest-x 0) int) 
  ((if image dest-y 0) int)
  (width int) (height int))


;;; Key values

(defbinding keyval-name () string
  (keyval unsigned-int))

(defbinding %keyval-from-name () unsigned-int
  (name string))

(defun keyval-from-name (name)
  "Returns the keysym value for the given key name or NIL if it is not a valid name."
  (let ((keyval (%keyval-from-name name)))
    (unless (zerop keyval)
      keyval)))

(defbinding keyval-to-upper () unsigned-int
  (keyval unsigned-int))

(defbinding keyval-to-lower () unsigned-int
  (keyval unsigned-int))

(defbinding (keyval-is-upper-p "gdk_keyval_is_upper") () boolean
  (keyval unsigned-int))

(defbinding (keyval-is-lower-p "gdk_keyval_is_lower") () boolean
  (keyval unsigned-int))

;;; Cairo interaction

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(progn
  (defbinding cairo-create () cairo:context
    (drawable drawable))

  (defmacro with-cairo-context ((cr drawable) &body body)
    `(let ((,cr (cairo-create ,drawable)))
       (unwind-protect
	   (progn ,@body)
	 (invalidate-instance ,cr t))))

  (defbinding cairo-set-source-color () nil
    (cr cairo:context)
    (color color))

  (defbinding cairo-set-source-pixbuf () nil
    (cr cairo:context)
    (pixbuf pixbuf)
    (x double-float)
    (y double-float))
 
  (defbinding cairo-rectangle () nil
    (cr cairo:context)
    (rectangle rectangle))
 
;;   (defbinding cairo-region () nil
;;     (cr cairo:context)
;;     (region region))

  (defbinding (cairo-xlib-surface-get-window 
	       "clg_gdk_cairo_xlib_surface_get_window") () window
  (surface cairo:xlib-surface))
)



;;; Multi-threading support

#+sbcl
(progn
  (defvar *global-lock* (sb-thread:make-mutex :name "global GDK lock"))
  (let ((recursive-level 0))
    (defun threads-enter ()
      (if (eq (sb-thread:mutex-value *global-lock*) sb-thread:*current-thread*)
	  (incf recursive-level)
 	(sb-thread:get-mutex *global-lock*)))

    (defun threads-leave (&optional flush-p)
      (cond
       ((zerop recursive-level)	  
	(when flush-p
	  (display-flush))
	(sb-thread:release-mutex *global-lock*))
       (t (decf recursive-level)))))

  (define-callback %enter-fn nil ()
    (threads-enter))
  
  (define-callback %leave-fn nil ()
    (threads-leave))
  
  (defbinding threads-set-lock-functions (&optional) nil
    (%enter-fn callback)
    (%leave-fn callback))

  (defmacro with-global-lock (&body body)
    `(progn
       (threads-enter)
       (unwind-protect
	   ,@body
	 (threads-leave t)))))
