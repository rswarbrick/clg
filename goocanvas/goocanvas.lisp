(in-package :goocanvas)

;; http://library.gnome.org/devel/goocanvas/0.15/goocanvas-GooCanvas-Types.html

(defbinding %canvas-points-ref () pointer
  (location pointer))

(defbinding %canvas-points-unref () nil
  (location pointer))

(defbinding %canvas-points-new () pointer
  (num-points int))

(defmethod allocate-foreign ((canvas-points canvas-points) &key points)
  (unless (evenp (length points))
    (error "Can't make canvas points with an odd number of numbers."))
  (let* ((length (length points))
         (ptr (%canvas-points-new (/ length 2)))
         (pptr (ref-pointer ptr))
         (offset 0))
    ;; Use MAP, since it deals with any type of sequence.
    (map nil
         (lambda (x)
           (funcall (writer-function 'double-float) x pptr offset)
           (incf offset (size-of 'double-float)))
         points)
    ptr))

(defmacro with-canvas-points (points sym &body body)
  "Runs the code of BODY with allocated GooCanvasPoints corresponding to (the
runtime values bound to the symbol) POINTS. The CANVAS-POINTS object is bound to
SYM."
  `(let ((,sym (make-instance 'canvas-points :points ,points)))
     (unwind-protect (progn ,@body)
       (invalidate-instance ,sym t))))


;;http://library.gnome.org/devel/goocanvas/0.15/GooCanvas.html

(defbinding canvas-get-root-item () canvas-item
  (canvas canvas))

(defbinding canvas-set-root-item () nil
  (canvas canvas)
  (canvas-item canvas-item))

(defbinding canvas-get-root-item-model () canvas-item-model
  (canvas canvas))

(defbinding canvas-set-root-item-model () nil
  (canvas canvas)
  (item-model canvas-item-model))

(defbinding canvas-get-static-root-item () canvas-item
  (canvas canvas))

(defbinding canvas-set-static-root-item () nil
  (canvas canvas)
  (item canvas-item))

(defbinding canvas-get-bounds () nil
  (canvas canvas)
  (left double-float :out)
  (top double-float :out)
  (right double-float :out)
  (bottom double-float :out))

(defbinding canvas-set-bounds () nil
  (canvas canvas)
  (left double-float)
  (top double-float)
  (right double-float)
  (bottom double-float))

(defbinding canvas-get-item () canvas-item
  (canvas canvas)
  (model canvas-item-model))

(defbinding canvas-get-item-at () canvas-item
  (canvas canvas)
  (x double-float)
  (y double-float)
  (pointer-event? boolean))

(defbinding canvas-get-items-at () (gslist canvas-item)
  (canvas canvas)
  (x double-float)
  (y double-float)
  (pointer-event? boolean))

(defbinding canvas-get-items-in-area () (gslist canvas-item)
  (canvas canvas)
  (area canvas-bounds)
  (inside-area boolean)
  (allow-overlaps boolean)
  (include-containers boolean))

(defbinding canvas-scroll-to () nil
  (canvas canvas)
  (left double-float)
  (top double-float))

(defbinding canvas-render () nil
  (canvas canvas)
  (cr cairo:context)
  (bounds canvas-bounds)
  (scale double-float))

(defbinding canvas-convert-to-pixels () nil
  (canvas canvas)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding canvas-convert-from-pixels () nil
  (canvas canvas)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding canvas-convert-to-item-space () nil
  (canvas canvas)
  (item canvas-item)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding canvas-convert-from-item-space () nil
  (canvas canvas)
  (item canvas-item)
  (x double-float :in/out)
  (y double-float :in/out))

(defbinding canvas-convert-bounds-to-item-space () nil
  (canvas canvas)
  (item canvas-item)
  (bounds canvas-bounds))

(defbinding canvas-pointer-grab () gdk:grab-status
  (canvas canvas)
  (item canvas-item)
  (event-mask gdk:event-mask)
  (cursor gdk:cursor)
  (time unsigned-int))

(defbinding canvas-pointer-ungrab () nil
  (canvas canvas)
  (item canvas-item)
  (time unsigned-int))

(defbinding canvas-grab-focus () nil
  (canvas canvas)
  (item canvas-item))

(defbinding canvas-keyboard-grab () gdk:grab-status
  (canvas canvas)
  (item canvas-item)
  (owner-events boolean)
  (time unsigned-int))

(defbinding canvas-keyboard-ungrab () nil
  (canvas canvas)
  (item canvas-item)
  (time unsigned-int))

(defbinding canvas-create-cairo-context () cairo:context
  (canvas canvas))

(defbinding canvas-unregister-item () nil
  (canvas canvas)
  (model canvas-item-model))

(defbinding canvas-register-widget-item () nil
  (canvas canvas)
  (widget-item canvas-widget))

(defbinding canvas-unregister-widget-item () nil
  (canvas canvas)
  (widget-item canvas-widget))

(defbinding canvas-update () nil
  (canvas canvas))

(defbinding canvas-request-update () nil
  (canvas canvas))

(defbinding canvas-request-redraw () nil
  (canvas canvas)
  (bounds canvas-bounds))

(defbinding canvas-request-item-redraw () nil
  (canvas canvas)
  (bounds canvas-bounds)
  (static? boolean))

(defbinding canvas-get-default-line-width () double-float
  (canvas canvas))

;;http://library.gnome.org/devel/goocanvas/0.15/goocanvas-goocanvasitem.html

(defbinding canvas-item-get-n-children () int
  (item canvas-item))

(defbinding canvas-item-get-child () canvas-item
  (item canvas-item)
  (child-num int))

(defbinding canvas-item-find-child () int
  (item canvas-item)
  (child canvas-item))

(defbinding canvas-item-add-child () nil
  (item canvas-item)
  (child canvas-item)
  (position int))

(defbinding canvas-item-move-child () nil
  (item canvas-item)
  (old-position int)
  (new-position int))

(defbinding canvas-item-remove-child () nil
  (item canvas-item)
  (child-num int))

;; TODO: Child properties?

(defbinding canvas-item-get-canvas () canvas
  (item canvas-item))

(defbinding canvas-item-set-canvas () nil
  (item canvas-item)
  (canvas canvas))

(defbinding canvas-item-remove () nil
  (item canvas-item))

(defbinding canvas-item-is-container () boolean
  (item canvas-item))

(defbinding canvas-item-raise () nil
  (item canvas-item)
  (above canvas-item))

(defbinding canvas-item-lower () nil
  (item canvas-item)
  (below canvas-item))

;; TODO: This is icky. Should be able to do better...
(defbinding canvas-item-get-transform () boolean
  (item canvas-item)
  (transform cairo:matrix :in/out))

(defbinding canvas-item-set-transform () nil
  (item canvas-item)
  (transform cairo:matrix :in))

(defbinding canvas-item-get-simple-transform () boolean
  (item canvas-item)
  (x double-float :out) (y double-float :out)
  (scale double-float :out) (rotation double-float :out))

(defbinding canvas-item-set-simple-transform () nil
  (item canvas-item)
  (x double-float) (y double-float)
  (scale double-float) (rotation double-float))

(defbinding canvas-item-translate () nil
  (item canvas-item)
  (tx double-float)
  (ty double-float))

(defbinding canvas-item-scale () nil
  (item canvas-item)
  (sx double-float)
  (sy double-float))

(defbinding canvas-item-rotate () nil
  (canvas-item canvas-item)
  (degrees double-float)
  (cx double-float)
  (cy double-float))

(defbinding canvas-item-skew-x () nil
  (item canvas-item)
  (degrees double-float)
  (cx double-float)
  (cy double-float))

(defbinding canvas-item-skew-y () nil
  (item canvas-item)
  (degrees double-float)
  (cx double-float)
  (cy double-float))

(defbinding canvas-item-get-style () canvas-style
  (item canvas-item))

(defbinding canvas-item-set-style () nil
  (item canvas-item)
  (style canvas-style))

(defbinding canvas-item-animate () nil
  (item canvas-item)
  (x double-float) (y double-float) (scale double-float)
  (degrees double-float) (absolute double-float) (absolute boolean)
  (duration int) (step-time int) (type canvas-animate-type))

(defbinding canvas-item-stop-animation () nil
  (item canvas-item))

(defbinding canvas-item-get-bounds () nil
  (item canvas-item)
  (bounds canvas-bounds :out))

(defbinding canvas-item-get-items-at () (gslist canvas-item)
  (item canvas-item)
  (x double-float) (y double-float)
  (cr cairo:context) (pointer-event? boolean) (parent-visible? boolean)
  (found-items (gslist canvas-item)))

(defbinding canvas-item-is-visible () boolean
  (item canvas-item))

(defbinding canvas-item-get-model () canvas-item-model
  (item canvas-item))

(defbinding canvas-item-set-model () nil
  (item canvas-item)
  (model canvas-item-model))

(defbinding canvas-item-request-update () nil
  (item canvas-item))

(defbinding canvas-item-ensure-updated () nil
  (item canvas-item))

(defbinding canvas-item-update () nil
  (item canvas-item)
  (entire-tree boolean)
  (cr cairo:context)
  (bounds canvas-bounds))

(defbinding canvas-item-paint () nil
  (item canvas-item)
  (cr cairo:context)
  (bounds canvas-bounds)
  (scale double-float))

(defbinding canvas-item-get-requested-area () boolean
  (item canvas-item)
  (cr cairo:context)
  (requested-area canvas-bounds))

(defbinding canvas-item-get-requested-height () double-float
  (item canvas-item)
  (cr cairo:context)
  (width double-float))

(defbinding canvas-item-allocate-area () nil
  (item canvas-item)
  (cr cairo:context)
  (requested-area canvas-bounds)
  (allocated-area canvas-bounds)
  (x-offset double-float)
  (y-offset double-float))

(defbinding canvas-item-get-is-static () boolean
  (item canvas-item))

(defbinding canvas-item-set-is-static () nil
  (item canvas-item)
  (static? boolean))

;;http://library.gnome.org/devel/goocanvas/0.15/GooCanvasPolyline.html

(defun make-canvas-polyline (points &rest attributes)
  "Create a GooCanvasPolyline, with initial points given by POINTS-LIST, which
should be an even length sequence of double floats. ATTRIBUTES are passed to
MAKE-INSTANCE when creating the polyline."
  (let ((p (make-instance 'canvas-points :points points))
        (line (apply #'make-instance 'canvas-polyline attributes)))
    (setf (slot-value line 'points) p)
    line))
