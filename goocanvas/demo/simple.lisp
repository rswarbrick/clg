(in-package :goocanvas-demo)

(defun simple-demo ()
  (gtk:clg-init)
  (gtk:within-main-loop
    (make-instance
     'gtk:window
     :title "Goocanvas Demo: Simple"
     :default-height 600 :default-width 640
     :visible t :show-children t

     :signal (list 'gdk::delete-event #'(lambda (e) (declare (ignore e)) nil))

     :child (make-instance
             'gtk:scrolled-window
             :shadow-type :etched-in
             :child (simple-demo-canvas)))))

(defun simple-demo-canvas ()
  (let* ((c (make-instance 'canvas))
         (root (canvas-get-root-item c)))
    (gtk:widget-set-size-request c 600 450)
    (canvas-set-bounds c 0 0 1000 1000)
    (let ((rect (make-instance 'canvas-rect
                               :x 100 :y 100 :width 400 :height 400
                               :line-width 10
                               :radius-x 20
                               :radius-y 10
                               :stroke-color "yellow"
                               :fill-color "red"
                               :parent root))
          (text (make-instance 'canvas-text
                               :x 300 :y 300 :text "Hello World"
                               :font "Sans 24" :anchor :center :parent root)))
      (glib:signal-connect rect
                           'gdk:button-press-event
                           (lambda (i u) (declare (ignore i u))
                                   (format t "Button press!~%")
                                   (force-output)
                                   nil))
      (canvas-item-rotate text 45 300 300))
    c))

(register-demo
 (make-instance 'demo :name "Simple" :setup #'simple-demo-canvas))
