;; Common Lisp bindings for GTK+ v1.2.x
;; Copyright (C) 1999 Espen S. Johnsen <espejohn@online.no>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; $Id: gdk.lisp,v 1.1 2000-08-14 16:44:39 espen Exp $


(in-package "GDK")


;;; Events

; (defmethod initialize-instance ((event event) &rest initargs &key)
;   (declare (ignore initargs))
;   (call-next-method)
;   )

(defun find-event-class (event-type)
  (find-class
   (ecase event-type
     (:expose 'expose-event)
     (:delete 'delete-event))))

(deftype-method alien-copier event (type-spec)
  (declare (ignore type-spec))
  '%event-copy)

(deftype-method alien-deallocator event (type-spec)
  (declare (ignore type-spec))
  '%event-free)

(deftype-method translate-from-alien
    event (type-spec location &optional (alloc :dynamic))
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (let ((event-class
	      (find-event-class
	       (funcall (get-reader-function 'event-type) location 0))))
	 ,(ecase alloc
	    (:dynamic '(ensure-alien-instance event-class location))
	    (:static '(ensure-alien-instance event-class location :static t))
	    (:copy '(ensure-alien-instance
		     event-class (%event-copy location))))))))


(define-foreign event-poll-fd () int)

(define-foreign ("gdk_events_pending" events-pending-p) () boolean)

(define-foreign event-get () event)

(define-foreign event-peek () event)

(define-foreign event-get-graphics-expose () event
  (window window))

(define-foreign event-put () event
  (event event))

(define-foreign %event-copy (event &optional size) pointer
  (event (or event pointer)))

(define-foreign %event-free () nil
  (event (or event pointer)))

(define-foreign event-get-time () (unsigned 32)
  (event event))

;(define-foreign event-handler-set () ...)

(define-foreign set-show-events () nil
  (show-events boolean))

;;; Misc

(define-foreign set-use-xshm () nil
  (use-xshm boolean))

(define-foreign get-show-events () boolean)

(define-foreign get-use-xshm () boolean)

(define-foreign get-display () string)

; (define-foreign time-get () (unsigned 32))

; (define-foreign timer-get () (unsigned 32))

; (define-foreign timer-set () nil
;   (milliseconds (unsigned 32)))

; (define-foreign timer-enable () nil)

; (define-foreign timer-disable () nil)

; input ...

(define-foreign pointer-grab () int
  (window window)
  (owner-events boolean)
  (event-mask event-mask)
  (confine-to (or null window))
  (cursor (or null cursor))
  (time (unsigned 32)))

(define-foreign pointer-ungrab () nil
  (time (unsigned 32)))

(define-foreign keyboard-grab () int
  (window window)
  (owner-events boolean)
  (time (unsigned 32)))

(define-foreign keyboard-ungrab () nil
  (time (unsigned 32)))

(define-foreign ("gdk_pointer_is_grabbed" pointer-is-grabbed-p) () boolean)

(define-foreign screen-width () int)
(define-foreign screen-height () int)

(define-foreign screen-width-mm () int)
(define-foreign screen-height-mm () int)

(define-foreign flush () nil)
(define-foreign beep () nil)

(define-foreign key-repeat-disable () nil)
(define-foreign key-repeat-restore () nil)



;;; Visuals

(define-foreign visual-get-best-depth () int)

(define-foreign visual-get-best-type () visual-type)

(define-foreign visual-get-system () visual)


(define-foreign
  ("gdk_visual_get_best" %visual-get-best-with-nothing) () visual)

(define-foreign %visual-get-best-with-depth () visual
  (depth int))

(define-foreign %visual-get-best-with-type () visual
  (type visual-type))

(define-foreign %visual-get-best-with-both () visual
  (depth int)
  (type visual-type))

(defun visual-get-best (&key depth type)
  (cond
   ((and depth type) (%visual-get-best-with-both depth type))
   (depth (%visual-get-best-with-depth depth))
   (type (%visual-get-best-with-type type))
   (t (%visual-get-best-with-nothing))))

;(define-foreign query-depths ..)

;(define-foreign query-visual-types ..)

(define-foreign list-visuals () (double-list visual))


;;; Windows

; (define-foreign window-new ... )

(define-foreign window-destroy () nil
  (window window))


; (define-foreign window-at-pointer () window
;   (window window)
;   (x int :in-out)
;   (y int :in-out))

(define-foreign window-show () nil
  (window window))

(define-foreign window-hide () nil
  (window window))

(define-foreign window-withdraw () nil
  (window window))

(define-foreign window-move () nil
  (window window)
  (x int)
  (y int))

(define-foreign window-resize () nil
  (window window)
  (width int)
  (height int))

(define-foreign window-move-resize () nil
  (window window)
  (x int)
  (y int)
  (width int)
  (height int))

(define-foreign window-reparent () nil
  (window window)
  (new-parent window)
  (x int)
  (y int))

(define-foreign window-clear () nil
  (window window))

(unexport
 '(window-clear-area-no-e window-clear-area-e))

(define-foreign ("gdk_window_clear_area" window-clear-area-no-e) () nil
  (window window)
  (x int) (y int) (width int) (height int))

(define-foreign window-clear-area-e () nil
  (window window)
  (x int) (y int) (width int) (height int))

(defun window-clear-area (window x y width height &optional expose)
  (if expose
      (window-clear-area-e window x y width height)
    (window-clear-area-no-e window x y width height)))

; (define-foreign window-copy-area () nil
;   (window window)
;   (gc gc)
;   (x int)
;   (y int)
;   (source-window window)
;   (source-x int)
;   (source-y int)
;   (width int)
;   (height int))

(define-foreign window-raise () nil
  (window window))

(define-foreign window-lower () nil
  (window window))

; (define-foreign window-set-user-data () nil

(define-foreign window-set-override-redirect () nil
  (window window)
  (override-redirect boolean))

; (define-foreign window-add-filter () nil

; (define-foreign window-remove-filter () nil

(define-foreign window-shape-combine-mask () nil
  (window window)
  (shape-mask bitmap)
  (offset-x int)
  (offset-y int))

(define-foreign window-set-child-shapes () nil
  (window window))

(define-foreign window-merge-child-shapes () nil
  (window window))

(define-foreign ("gdk_window_is_visible" window-is-visible-p) () boolean
  (window window))

(define-foreign ("gdk_window_is_viewable" window-is-viewable-p) () boolean
  (window window))

(define-foreign window-set-static-gravities () boolean
  (window window)
  (use-static boolean))

; (define-foreign add-client-message-filter ...


;;; Drag and Drop

(define-foreign drag-context-new () drag-context)

(define-foreign drag-context-ref () nil
  (context drag-context))

(define-foreign drag-context-unref () nil
  (context drag-context))

;; Destination side

(define-foreign drag-status () nil
  (context drag-context)
  (action drag-action)
  (time (unsigned 32)))




(define-foreign window-set-cursor () nil
  (window window)
  (cursor cursor))

(define-foreign window-get-pointer () window
  (window window)
  (x int :out)
  (y int :out)
  (mask modifier-type :out))

(define-foreign get-root-window () window)



;;

(define-foreign rgb-init () nil)




;;; Cursor

(deftype-method alien-ref cursor (type-spec)
  (declare (ignore type-spec))
  '%cursor-ref)

(deftype-method alien-unref cursor (type-spec)
  (declare (ignore type-spec))
  '%cursor-unref)


(define-foreign cursor-new () cursor
  (cursor-type cursor-type))

(define-foreign cursor-new-from-pixmap () cursor
  (source pixmap)
  (mask bitmap)
  (foreground color)
  (background color)
  (x int) (y int))

(define-foreign %cursor-ref () pointer
  (cursor (or cursor pointer)))

(define-foreign %cursor-unref () nil
  (cursor (or cursor pointer)))



;;; Pixmaps

(define-foreign pixmap-new (width height depth &key window) pixmap
  (width int)
  (height int)
  (depth int)
  (window (or null window)))
					

(define-foreign %pixmap-colormap-create-from-xpm () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (filename string))

(define-foreign pixmap-colormap-create-from-xpm-d () pixmap
  (window (or null window))
  (colormap (or null colormap))
  (mask bitmap :out)
  (color (or null color))
  (data pointer))

; (defun pixmap-create (source &key color window colormap)
;   (let ((window
; 	 (if (not (or window colormap))
; 	     (get-root-window)
; 	   window)))
;     (multiple-value-bind (pixmap bitmap)
;         (typecase source
; 	  ((or string pathname)
; 	   (pixmap-colormap-create-from-xpm
; 	    window colormap color (namestring (truename source))))
; 	  (t
; 	   (with-array (data :initial-contents source :free-contents t)
; 	     (pixmap-colormap-create-from-xpm-d window colormap color data))))
;       (if color
; 	  (progn
; 	    (bitmap-unref bitmap)
; 	    pixmap)
; 	(values pixmap bitmap)))))
    


;;; Color

(defun %scale-value (value)
  (etypecase value
    (integer value)
    (float (truncate (* value 65535)))))

(defmethod initialize-instance ((color color) &rest initargs
				&key (colors #(0 0 0)) red green blue)
  (declare (ignore initargs))
  (call-next-method)
  (with-slots ((%red red) (%green green) (%blue blue)) color
    (setf
     %red (%scale-value (or red (svref colors 0)))
     %green (%scale-value (or green (svref colors 1)))
     %blue (%scale-value (or blue (svref colors 2))))))


(defun ensure-color (color)
  (etypecase color
    (null nil)
    (color color)
    (vector (make-instance 'color :colors color))))
       

  
;;; Fonts

(define-foreign font-load () font
  (font-name string))

(defun ensure-font (font)
  (etypecase font
    (null nil)
    (font font)
    (string (font-load font))))

(define-foreign fontset-load () font
  (fontset-name string))

(define-foreign font-ref () font
  (font font))

(define-foreign font-unref () nil
  (font font))

(defun font-maybe-unref (font1 font2)
  (unless (eq font1 font2)
    (font-unref font1)))

(define-foreign font-id () int
  (font font))

(define-foreign ("gdk_font_equal" font-equalp) () boolean
  (font-a font)
  (font-b font))

(define-foreign string-width () int
  (font font)
  (string string))

(define-foreign text-width
    (font text &aux (length (length text))) int
  (font font)
  (text string)
  (length int))

; (define-foreign ("gdk_text_width_wc" text-width-wc)
;     (font text &aux (length (length text))) int
;   (font font)
;   (text string)
;   (length int))

(define-foreign char-width () int
  (font font)
  (char char))

; (define-foreign ("gdk_char_width_wc" char-width-wc) () int
;   (font font)
;   (char char))


(define-foreign string-measure () int
  (font font)
  (string string))

(define-foreign text-measure
    (font text &aux (length (length text))) int
  (font font)
  (text string)
  (length int))

(define-foreign char-measure () int
  (font font)
  (char char))

(define-foreign string-height () int
  (font font)
  (string string))

(define-foreign text-height
    (font text &aux (length (length text))) int
  (font font)
  (text string)
  (length int))

(define-foreign char-height () int
  (font font)
  (char char))


;;; Drawing functions

(define-foreign draw-rectangle () nil
  (drawable (or window pixmap bitmap))
  (gc gc) (filled boolean)
  (x int) (y int) (width int) (height int))


;;; Key values

(define-foreign keyval-name () string
  (keyval unsigned-int))

(define-foreign keyval-from-name () unsigned-int
  (name string))

(define-foreign keyval-to-upper () unsigned-int
  (keyval unsigned-int))

(define-foreign keyval-to-lower ()unsigned-int
  (keyval unsigned-int))

(define-foreign ("gdk_keyval_is_upper" keyval-is-upper-p) () boolean
  (keyval unsigned-int))

(define-foreign ("gdk_keyval_is_lower" keyval-is-lower-p) () boolean
  (keyval unsigned-int))

