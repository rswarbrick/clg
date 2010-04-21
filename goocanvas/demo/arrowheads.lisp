(in-package :goocanvas-demo)

(defvar *label-text*
  "This demo allows you to edit arrowhead shapes.  Drag the little boxes
to change the shape of the line and its arrowhead.  You can see the
arrows at their normal scale on the right hand side of the window.")

(defvar *left* 50.0)
(defvar *right* 350.0)
(defvar *middle* 150.0)

(defgeneric update-view-item (item model))
(defmethod update-view-item (item model)
  (declare (ignore item model))
  (values))

(defclass arrow-model ()
  ((line-width :initform 2 :accessor line-width)
   (tip-length :initform 4 :accessor tip-length)
   (arrow-length :initform 5 :accessor arrow-length)
   (arrow-width :initform 4 :accessor arrow-width)
   (view :initarg :view)))

(defgeneric update-views (model-or-view))
(defmethod update-views ((model arrow-model))
  (update-views (slot-value model 'view)))

(defclass drag-box () ((rect) (coord-fun)))

(defun make-drag-box (root coord-fun &optional callback model)
  "Create a drag box with parent ROOT. When updating, it calls COORD-FUN with an
ARROW-MODEL to get two values, the X and Y coordinates of the correct
position. When CALLBACK is non-nil, when the box is dragged, CALLBACK is called
with arguments (EVENT MODEL)."
  (let ((drag (make-instance 'drag-box)))
    (setf (slot-value drag 'rect) (create-drag-box root)
          (slot-value drag 'coord-fun) coord-fun)
    (when callback
      (glib:signal-connect (slot-value drag 'rect) 'gdk:motion-notify-event
                           (lambda (drag event)
                             (declare (ignore drag))
                             (funcall callback event model))))
    drag))

(defmethod update-view-item ((drag drag-box) model)
  (with-slots (x y) (slot-value drag 'rect)
    (multiple-value-bind (cx cy) (funcall (slot-value drag 'coord-fun) model)
      (setf x (- cx 5) y (- cy 5)))))

(defclass dimension ()
  ((line) (text) (coord-fun)))

(defun make-dimension (root anchor coord-fun)
  "Create a 'dimension' widget, with text anchored like ANCHOR. COORD-FUN should
take a model and return the list (X1 Y1 X2 Y2 XT YT TEXT) for positions of
endpoints of the arrow and of the text."
  (let ((dim (make-instance 'dimension)))
    (setf (slot-value dim 'line)
          (make-canvas-polyline nil
                                :fill-color "black"
                                :start-arrow t
                                :end-arrow t
                                :parent root)
          (slot-value dim 'text)
          (make-instance 'canvas-text
                         :anchor anchor
                         :fill-color "black"
                         :font "Sans 12"
                         :parent root)
          (slot-value dim 'coord-fun) coord-fun)
    dim))

(defun set-dimension (arrow text x1 y1 x2 y2 tx ty dim)
  (with-canvas-points (list x1 y1 x2 y2) p
    (setf (slot-value arrow 'points) p))
  (with-slots (text x y) text
    (setf text (format nil "~D" dim) x tx y ty)))

(defmethod update-view-item ((dim dimension) model)
  (with-slots (line text coord-fun) dim
    (apply #'set-dimension line text (funcall coord-fun model))))

(defmacro make-dim-coord-fun ((&rest coord-exprs) &rest slots-used)
  `(lambda (model) (with-slots ,slots-used model (list ,@coord-exprs))))

(defmacro make-coord-fun (xexpr yexpr &rest slots-used)
  `(lambda (model)
     (with-slots ,slots-used model
       (values ,xexpr ,yexpr))))

(defclass info-text () ((text) (update)))

(defun make-info-text (root x y update-fun)
  "Make an info text instance, placed at (X,Y). When the view gets updated,
UPDATE-FUN gets called with the argument MODEL and should return the text to
use."
  (let ((txt (make-instance 'info-text)))
    (with-slots (text update) txt
      (setf text
            (make-instance 'canvas-text
                           :x x :y y :anchor :nw
                           :fill-color "black" :font "Sans 14"
                           :parent root)
            update update-fun))
    txt))

(defmethod update-view-item ((info info-text) model)
  (setf (slot-value (slot-value info 'text) 'text)
        (funcall (slot-value info 'update) model)))

(defmacro make-text-update-fun (name slot)
  `(lambda (model) (format nil "~A: ~D" ,name (slot-value model ',slot))))

(defclass sample-arrow () ((arrow)))

(defun make-sample-arrow (root x1 y1 x2 y2)
  (let ((a (make-instance 'sample-arrow)))
    (setf (slot-value a 'arrow)
          (make-canvas-polyline (list x1 y1 x2 y2)
                                :line-width 2 :fill-color "black"
                                :start-arrow t
                                :end-arrow t
                                :parent root))
    a))

(defmethod update-view-item ((sa sample-arrow) model)
  (with-slots (line-width arrow-tip-length arrow-length arrow-width)
      (slot-value sa 'arrow)
    (setf line-width (line-width model)
          arrow-tip-length (tip-length model)
          arrow-length (arrow-length model)
          arrow-width (arrow-width model))))

(defclass arrow-view ()
  ((big-arrow :accessor big-arrow)
   (outline :accessor outline)
   (items :accessor items :initform nil)
   (model)))

(defun make-arrow-view (model root)
  (let ((view (make-instance 'arrow-view)))
    (setf (big-arrow view)
          (make-canvas-polyline (list *left* *middle* *right* *middle*)
                                :stroke-color "mediumseagreen"
                                :end-arrow t
                                :parent root)
          (outline view)
          (make-canvas-polyline nil
                                :stroke-color "black"
                                :line-width 2.0
                                :line-cap :round
                                :line-join :round
                                :parent root)
          (slot-value view 'model) model
          (slot-value model 'view) view)

    ;; Drag boxes
    (mapc
     (lambda (coord-fun callback)
       (push (make-drag-box root coord-fun callback model)
             (items view)))
     (list (make-coord-fun *left* (- *middle* (* 10 (/ line-width 2)))
                           line-width)
           (make-coord-fun (- *right* (* 10 tip-length line-width)) *middle*
                           tip-length line-width)
           (make-coord-fun (- *right* (* 10 arrow-length line-width))
                           (- *middle* (* 10 arrow-width (/ line-width 2)))
                           arrow-length arrow-width line-width))
     (list #'width-drag-box-move #'tip-length-drag-box-move
           #'arrow-lw-drag-box-move))

    ;; Dimension arrows
    (mapc (lambda (anchor coord-fun)
            (push (make-dimension root anchor coord-fun) (items view)))
          '(:e :n :n :w)
          (list
           (make-dim-coord-fun
            ((- *left* 10) (- *middle* (* 10 (/ line-width 2)))
             (- *left* 10) (+ *middle* (* 10 (/ line-width 2)))
             (- *left* 15) *middle* line-width) line-width)
           (make-dim-coord-fun
            ((- *right* (* 10 tip-length line-width))
             (+ *middle* (* 10 arrow-width(/ line-width 2)) 10)
             *right* (+ *middle* (* 10 arrow-width (/ line-width 2)) 10)
             (- *right* (* 10 tip-length (/ line-width 2)))
             (+ *middle* (* 10 arrow-width(/ line-width 2)) 15) tip-length)
            line-width tip-length arrow-width)
           (make-dim-coord-fun
            ((- *right* (* 10 arrow-length line-width))
             (+ *middle* (* 10 arrow-width(/ line-width 2)) 35)
             *right* (+ *middle* (* 10 arrow-width (/ line-width 2)) 35)
             (- *right* (* 10 arrow-length (/ line-width 2)))
             (+ *middle* (* 10 arrow-width(/ line-width 2)) 40) arrow-length)
            arrow-length line-width arrow-width)
           (make-dim-coord-fun
            ((+ *right* 10) (- *middle* (* 10 arrow-width (/ line-width 2)))
             (+ *right* 10) (+ *middle* (* 10 arrow-width (/ line-width 2)))
             (+ *right* 15) *middle* arrow-width)
            arrow-width line-width)))

    ;; Info text
    (mapc (lambda (y update-fun)
            (push (make-info-text root *left* y update-fun) (items view)))
          '(260 280 300 320)
          (list (make-text-update-fun "line-width" line-width)
                (make-text-update-fun "arrow-tip-length" tip-length)
                (make-text-update-fun "arrow-length" arrow-length)
                (make-text-update-fun "arrow-width" arrow-width)))

    ;; Sample arrows
    (mapc (lambda (coords)
            (push (apply #'make-sample-arrow root coords) (items view)))
          (list
           (list (+ 100 *right*) 30 (+ 100 *right*) (- *middle* 30))
           (list (+ 70 *right*) *middle* (+ 130 *right*) *middle*)
           (list (+ 70 *right*) (+ 30 *middle*)
                 (+ 130 *right*) (+ 120 *middle*))))))

(defmethod update-views ((view arrow-view))
  (with-slots (model) view
    (let ((lw (line-width model)) (tl (tip-length model))
          (al (arrow-length model)) (aw (arrow-width model)))
      ;; Big arrow
      (with-slots (line-width arrow-tip-length arrow-length arrow-width)
          (slot-value view 'big-arrow)
        (setf line-width (* 10 lw) arrow-tip-length tl
              arrow-length al arrow-width aw))

      ;; Outline
      (with-canvas-points (outline-coords lw tl al aw) ps
        (setf (slot-value (slot-value view 'outline) 'points) ps))

      ;; Everything else
      (dolist (x (slot-value view 'items)) (update-view-item x model)))))

(defun create-canvas-arrowhead ()
  (let* ((canvas (make-instance 'canvas))
         (root (canvas-get-root-item canvas))
         (vbox (make-instance 'gtk:v-box :border-width 4 :spacing 4))
         (model (make-instance 'arrow-model :view canvas)))

    (gtk:widget-set-size-request canvas 500 350)
    (canvas-set-bounds canvas 0 0 500 350)

    (make-arrow-view model root)
    (update-views model)

    (make-canvas-polyline (list (+ 50 *right*) 0 (+ 50 *right*) 1000)
                          :fill-color "black" :line-width 2 :parent root)

    (gtk:box-pack
     vbox (gtk:create-label *label-text*) :expand nil :fill nil :padding 0)
    (gtk:box-pack
     vbox
     (make-instance 'gtk:alignment :xscale 0 :yscale 0
                    :child (make-instance
                            'gtk:frame :shadow-type :etched-in :child canvas))
     :padding 0)
    vbox))

(defun create-drag-box (root)
  (let ((rect (make-instance 'canvas-rect
                             :parent root
                             :x 0 :y 0 :width 10 :height 10
                             :fill-color "black"
                             :stroke-color "black"
                             :line-width 1)))
    (gtk:signal-connect rect 'gdk:enter-notify-event #'drag-box-entered)
    (gtk:signal-connect rect 'gdk:leave-notify-event #'drag-box-left)
    (gtk:signal-connect rect 'gdk:button-press-event #'drag-box-grab)
    (gtk:signal-connect rect 'gdk:button-release-event #'drag-box-release)
    rect))

(defun drag-box-entered (box event)
  (declare (ignore event))
  (setf (slot-value box 'fill-color) "red"))

(defun drag-box-left (box event)
  (declare (ignore event))
  (setf (slot-value box 'fill-color) "black"))

(defun drag-box-grab (box event)
  (canvas-pointer-grab (canvas-item-get-canvas box)
                       box
                       '(:pointer-motion :pointer-motion-hint :button-release)
                       (gdk:ensure-cursor :fleur)
                       (gdk:event-time event)))

(defun drag-box-release (box event)
  (canvas-pointer-ungrab (canvas-item-get-canvas box)
                         box
                         (gdk:event-time event)))

(defun width-drag-box-move (event model)
  (when (member :button1 (gdk:event-state event))
    (let ((width (floor (- *middle* (gdk:event-y event)) 5)))
      (unless (< width 0)
        (setf (line-width model) width)
        (update-views model)
        t))))

(defun tip-length-drag-box-move (event model)
  (when (member :button1 (gdk:event-state event))
    (let ((tl (floor (- *right* (gdk:event-x event))
                     (* 10 (line-width model)))))
      (when (<= 0 tl 30)
        (setf (tip-length model) tl)
        (update-views model)
        t))))

(defun arrow-lw-drag-box-move (event model)
  (when (member :button1 (gdk:event-state event))
    (let ((al (floor (- *right* (gdk:event-x event))
                     (* 10 (line-width model))))
          (aw (floor (- *middle* (gdk:event-y event))
                     (* 5 (line-width model))))
          (changed nil))
      (when (<= 0 al 30) (setf (arrow-length model) al changed t))
      (when (<= 0 aw) (setf (arrow-width model) aw changed t))
      (when changed
        (update-views model)
        t))))

(defun outline-coords (lw tl al aw)
  (list (- *right* (* 10 tl lw))
        (- *middle* (* 10 (/ lw 2)))
        (- *right* (* 10 al lw))
        (- *middle* (* 10 aw (/ lw 2)))
        *right*
        *middle*
        (- *right* (* 10 al lw))
        (+ *middle* (* 10 aw (/ lw 2)))
        (- *right* (* 10 tl lw))
        (+ *middle* (* 10 (/ lw 2)))))

(register-demo
 (make-instance 'demo :name "Arrowheads" :setup 'create-canvas-arrowhead))
