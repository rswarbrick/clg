;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gdk.lisp,v 1.22 2006-02-26 15:09:44 espen Exp $


(in-package "GDK")

;;; Initialization
 
(defbinding (gdk-init "gdk_parse_args") () nil
  "Initializes the library without opening the display."
  (nil null)
  (nil null))



;;; Display

(defbinding %display-open () display
  (display-name (or null string)))

(defun display-open (&optional display-name)
  (let ((display (%display-open display-name)))
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



;;; Display manager

(defbinding display-get-default () display)

(defbinding (display-manager "gdk_display_manager_get") () display-manager)

(defbinding (display-set-default "gdk_display_manager_set_default_display")
    (display) nil
  ((display-manager) display-manager)
  (display display))



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

(defbinding screen-width () int)
(defbinding screen-height () int)

(defbinding screen-width-mm () int)
(defbinding screen-height-mm () int)

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
    (&optional (display (display-get-default))) boolean)

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

;; komplett så langt

(defbinding window-set-user-data () nil
  (window window)
  (user-data pointer))

(defbinding window-set-override-redirect () nil
  (window window)
  (override-redirect boolean))

; (defbinding window-add-filter () nil

; (defbinding window-remove-filter () nil

(defbinding window-shape-combine-mask () nil
  (window window)
  (shape-mask bitmap)
  (offset-x int)
  (offset-y int))

(defbinding window-set-child-shapes () nil
  (window window))

(defbinding window-merge-child-shapes () nil
  (window window))


(defbinding window-set-static-gravities () boolean
  (window window)
  (use-static boolean))

; (defbinding add-client-message-filter ...

(defbinding window-set-cursor () nil
  (window window)
  (cursor (or null cursor)))

(defbinding window-get-pointer () window
  (window window)
  (x int :out)
  (y int :out)
  (mask modifier-type :out))

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

(defmethod allocate-foreign ((cursor cursor) &key type mask fg bg 
			     (x 0) (y 0) (display (display-get-default)))
  (etypecase type
    (keyword (%cursor-new-for-display display type))
    (pixbuf (%cursor-new-from-pixbuf display type x y))
    (pixmap (%cursor-new-from-pixmap type mask fg bg x y))))


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

(defmethod reference-foreign ((class (eql (find-class 'cursor))) location)
  (declare (ignore class))
  (%cursor-ref location))

(defmethod unreference-foreign ((class (eql (find-class 'cursor))) location)
  (declare (ignore class))
  (%cursor-unref location))


;;; Pixmaps

(defbinding pixmap-new (width height depth &key window) pixmap
  (width int)
  (height int)
  (depth int)
  (window (or null window)))
					
(defbinding %pixmap-colormap-create-from-xpm () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (filename string))

(defbinding %pixmap-colormap-create-from-xpm-d () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (data (vector string)))

(defun pixmap-create (source &key color window colormap)
  (let ((window
	 (if (not (or window colormap))
	     (get-root-window)
	   window)))
    (multiple-value-bind (pixmap mask)
        (etypecase source
	  ((or string pathname)
	   (%pixmap-colormap-create-from-xpm
	    window colormap color (namestring (truename source))))
 	  ((vector string)
	   (%pixmap-colormap-create-from-xpm-d window colormap color source)))
;;       (unreference-instance pixmap)
;;       (unreference-instance mask)
      (values pixmap mask))))



;;; Color

(defun %scale-value (value)
  (etypecase value
    (integer value)
    (float (truncate (* value 65535)))))

(defmethod initialize-instance ((color color) &rest initargs
				&key red green blue)
  (declare (ignore initargs))
  (call-next-method)
  (with-slots ((%red red) (%green green) (%blue blue)) color
    (setf
     %red (%scale-value red)
     %green (%scale-value green)
     %blue (%scale-value blue))))

(defun ensure-color (color)
  (etypecase color
    (null nil)
    (color color)
    (vector
     (make-instance
      'color :red (svref color 0) :green (svref color 1)
      :blue (svref color 2)))))
       

  
;;; Drawable

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

;; (defun draw-points (drawable gc &rest points)
  
;;   )

(defbinding draw-line () nil
  (drawable drawable) (gc gc) 
  (x1 int) (y1 int)
  (x2 int) (y2 int))

;; (defbinding draw-lines (drawable gc &rest points) nil
;;   (drawable drawable) (gc gc) 
;;   (points (vector point))
;;   ((length points) int))

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

;; (defbinding draw-segments (drawable gc &rest points) nil
;;   (drawable drawable) (gc gc) 
;;   (segments (vector segments))
;;   ((length segments) int))

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

;; (defbinding draw-polygon (drawable gc &rest points) nil
;;   (drawable drawable) (gc gc) 
;;   (points (vector point))
;;   ((length points) int))

;; (defbinding draw-trapezoid (drawable gc &rest points) nil
;;   (drawable drawable) (gc gc) 
;;   (points (vector point))
;;   ((length points) int))

;; (defbinding %draw-layout-line () nil
;;   (drawable drawable) (gc gc) 
;;   (font pango:font)
;;   (x int) (y int)
;;   (line pango:layout-line))

;; (defbinding %draw-layout-line-with-colors () nil
;;   (drawable drawable) (gc gc) 
;;   (font pango:font)
;;   (x int) (y int)
;;   (line pango:layout-line)
;;   (foreground (or null color))
;;   (background (or null color)))

;; (defun draw-layout-line (drawable gc font x y line &optional foreground background)
;;   (if (or foreground background)
;;       (%draw-layout-line-with-colors drawable gc font x y line foreground background)
;;     (%draw-layout-line drawable gc font x y line)))

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

#+gtk2.8
(progn
  (defbinding cairo-create () cairo:context
    (drawable drawable))

  (defmacro with-cairo-context ((cr drawable) &body body)
    `(let ((,cr (cairo-create ,drawable)))
       (unwind-protect
	   (progn ,@body)
	 (unreference-foreign 'cairo:context (foreign-location ,cr))
	 (invalidate-instance ,cr))))

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
)
