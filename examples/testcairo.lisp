;; This file contains the cairo C snippets translated to lisp 
;; See http://cairographics.org/samples/

#+sbcl(require :gtk)
#+sbcl(require :cairo)
#+(or cmu clisp)(asdf:oos 'asdf:load-op :gtk)
#+(or cmu clisp)(asdf:oos 'asdf:load-op :cairo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (pkg-config:pkg-exists-p "librsvg-2.0" :atleast-version "2.14.0")
    (warn "SVG tests disabled as the required version of librsvg is not available.")))

#?(pkg-config:pkg-exists-p "librsvg-2.0" :atleast-version "2.14.0")
#+sbcl(require :rsvg)
#+(or cmu clisp)(asdf:oos 'asdf:load-op :rsvg)

(defpackage "TESTCAIRO"
  (:use "COMMON-LISP" "GTK")
  (:export "CREATE-TESTS"))

(in-package "TESTCAIRO")

(declaim (inline deg-to-rad))
(defun deg-to-rad (deg)
  (* deg (/ pi 180.0)))

(declaim (inline rad-to-deg))
(defun rad-to-deg (rad)
  (/ (* rad 180.0) pi))


(defvar *snippets* ())


(defmacro define-snippet (name (cr) &body body)
  (let ((widget (make-symbol "WIDGET"))
	(window (make-symbol "WINDOW"))
	(event (make-symbol "EVENT")))
    `(let ((,window nil))
       (pushnew ',name *snippets*)
       (defun ,name ()
	 (if (not ,window)
	     (let ((,widget (make-instance 'drawing-area)))
	       (setq ,window
	        (make-instance 'window 
		 :width-request 300 :height-request 300 
		 :title ,(string-downcase name)
		 :visible t :child ,widget))
	       (signal-connect ,window 'destroy 
                #'(lambda () (setq ,window nil)))
	       (signal-connect ,widget 'expose-event
		#'(lambda (,event)
		    (declare (ignore ,event))
		    (gdk:with-cairo-context (,cr (widget-window ,widget))
		      (multiple-value-bind (width height) 
			  (widget-get-size-allocation ,widget)
			(cairo:scale ,cr width height))
		      (setf (cairo:line-width ,cr) 0.04)
		      ,@body)))
	       (widget-show-all ,window))
	   (widget-destroy ,window))))))


     

(defun arc-helper-lines (cr xc yc radius angle1 angle2)
  (cairo:set-source-color cr 1.0 0.2 0.2 0.6)
  (cairo:arc cr xc yc 0.05 0 (deg-to-rad 360.0))
  (cairo:fill cr)
  (setf (cairo:line-width cr) 0.03)
  (cairo:move-to cr xc yc)
  (cairo:rel-line-to cr (* radius (cos angle1)) (* radius (sin angle1)))
  (cairo:stroke cr)
  (cairo:move-to cr xc yc)
  (cairo:rel-line-to cr (* radius (cos angle2)) (* radius (sin angle2)))
  (cairo:stroke cr))

(define-snippet arc (cr)
  (let ((xc 0.5)
	(yc 0.5)
	(radius 0.4)
	(angle1 (deg-to-rad 45.0))
	(angle2 (deg-to-rad 180.0)))

    (cairo:with-context (cr)
      (setf (cairo:line-cap cr) :round)
      (cairo:arc cr xc yc radius angle1 angle2)
      (cairo:stroke cr))

    (arc-helper-lines cr xc yc radius angle1 angle2)))

(define-snippet arc-negative (cr)
  (let ((xc 0.5)
	(yc 0.5)
	(radius 0.4)
	(angle1 (deg-to-rad 45.0))
	(angle2 (deg-to-rad 180.0)))

    (cairo:with-context (cr)
      (setf (cairo:line-cap cr) :round)
      (cairo:arc-negative cr xc yc radius angle1 angle2)
      (cairo:stroke cr))

    (arc-helper-lines cr xc yc radius angle1 angle2)))


(define-snippet clip (cr)
  (cairo:circle cr 0.5 0.5 0.3)
  (cairo:clip cr)

  (cairo:new-path cr) ; current path is not consumed by cairo:clip
  (cairo:rectangle cr 0 0 1 1)
  (cairo:fill cr)
  (cairo:set-source-color cr 0.0 1.0 0.0)
  (cairo:move-to cr 0.0 0.0)
  (cairo:line-to cr 1.0 1.0)
  (cairo:move-to cr 1.0 0.0)
  (cairo:line-to cr 0.0 1.0)
  (cairo:stroke cr))


(define-snippet clip-image (cr)
  (cairo:circle cr 0.5 0.5 0.3)
  (cairo:clip cr)
  (cairo:new-path cr)

  (let ((image (make-instance 'cairo:image-surface 
		:filename #p"clg:examples;romedalen.png")))
	       
    (let ((width (cairo:surface-width image))
	  (height (cairo:surface-height image)))
      (cairo:scale cr (/ 1.0 width) (/ 1.0 height)))

    (cairo:set-source-surface cr image 0 0)
    (cairo:paint cr)))

(define-snippet clip-rectangle (cr)
  (cairo:new-path cr)
  (cairo:move-to cr 0.25 0.25)
  (cairo:line-to cr 0.25 0.75)
  (cairo:line-to cr 0.75 0.75)
  (cairo:line-to cr 0.75 0.25)
  (cairo:line-to cr 0.25 0.25)
  (cairo:close-path cr)

  (cairo:clip cr)

  (cairo:move-to cr 0.0 0.0)
  (cairo:line-to cr 1.0 1.0)
  (cairo:stroke cr))


(defun %curve-rectangle (cr x0 y0 width height radius)
  (unless (and (zerop width) (zerop height))
    (let ((x1 (+ x0 width))
	  (y1 (+ y0 height)))
      (cond
       ((and (< (* 0.5 width) radius) (< (* 0.5 height) radius))
	(cairo:move-to cr x0 (* 0.5 (+ y0 y1)))
	(cairo:curve-to cr x0 y0 x0 y0 (* 0.5 (+ x0 x1)) y0)
	(cairo:curve-to cr x1 y0 x1 y0 x1 (* 0.5 (+ y0 y1)))
	(cairo:curve-to cr x1 y1 x1 y1 (* 0.5 (+ x0 x1)) y1)
	(cairo:curve-to cr x0 y1 x0 y1 x0 (* 0.5 (+ y0 y1))))
       ((< (* 0.5 width) radius)
	(cairo:move-to  cr x0 (+ y0 radius))
	(cairo:curve-to cr x0 y0 x0 y0 (* 0.5 (+ x0 x1)) y0)
	(cairo:curve-to cr x1 y0 x1 y0 x1 (+ y0 radius))
	(cairo:line-to cr x1  (- y1 radius))
	(cairo:curve-to cr x1 y1 x1 y1 (* 0.5 (+ x0 x1)) y1)
	(cairo:curve-to cr x0 y1 x0 y1 x0 (- y1 radius)))
       ((< (* 0.5 height) radius)
	(cairo:move-to cr x0 (* 0.5 (+ y0 y1)))
	(cairo:curve-to cr x0  y0 x0  y0 (+ x0 radius) y0)
	(cairo:line-to cr (- x1 radius) y0)
	(cairo:curve-to cr x1 y0 x1 y0 x1 (* 0.5 (+ y0 y1)))
	(cairo:curve-to cr x1 y1 x1 y1 (- x1 radius) y1)
	(cairo:line-to cr (+ x0 radius) y1)
	(cairo:curve-to cr x0 y1 x0 y1 x0 (* 0.5 (+ y0 y1))))
       (t
	(cairo:move-to cr x0 (+ y0 radius))
	(cairo:curve-to cr x0 y0 x0 y0 (+ x0 radius) y0)
	(cairo:line-to cr (- x1 radius) y0)
	(cairo:curve-to cr x1 y0 x1 y0 x1 (+ y0 radius))
	(cairo:line-to cr x1 (- y1 radius))
	(cairo:curve-to cr x1 y1 x1 y1 (- x1 radius) y1)
	(cairo:line-to cr (+ x0 radius) y1)
	(cairo:curve-to cr x0 y1 x0 y1 x0 (- y1 radius))))
      (cairo:close-path cr))))

(define-snippet curve-rectangle (cr)
  (let ((x0 0.1)
	(y0 0.1)
	(width 0.8)
	(height 0.8)
	(radius 0.4))
    (%curve-rectangle cr x0 y0 width height radius)
    (cairo:set-source-color cr 0.5 0.5 1.0)
    (cairo:fill cr t)
    (cairo:set-source-color cr 0.5 0.0 0.0 0.5)
    (cairo:stroke cr)))


(define-snippet curve-to (cr)
  (let ((x 0.1) (y 0.5)
	(x1 0.4) (y1 0.9)
	(x2 0.6) (y2 0.1)
	(x3 0.9) (y3 0.5))

    (cairo:move-to cr x y)
    (cairo:curve-to cr x1 y1 x2 y2 x3 y3)

    (cairo:stroke cr)

    (cairo:set-source-color cr 1.0 0.2 0.2 0.6)
    (setf (cairo:line-width cr) 0.03)
    (cairo:move-to cr x y)
    (cairo:line-to cr x1 y1)
    (cairo:move-to cr x2 y2)
    (cairo:line-to cr x3 y3)
    (cairo:stroke cr)))


(define-snippet dash (cr)
  (let ((dashes #(0.20 0.05 0.05 0.05))
	(offset -0.2))
    (cairo:set-dash cr dashes offset)
    (cairo:move-to cr 0.5 0.1)
    (cairo:line-to cr 0.9 0.9)
    (cairo:rel-line-to cr -0.4 0.0)
    (cairo:curve-to cr 0.2 0.9 0.2 0.5 0.5 0.5)
    (cairo:stroke cr)))


(defun fill-and-stroke-common (cr)
  (cairo:move-to cr 0.5 0.1)
  (cairo:line-to cr 0.9 0.9)
  (cairo:rel-line-to cr -0.4 0.0)
  (cairo:curve-to cr 0.2 0.9 0.2 0.5 0.5 0.5)
  (cairo:close-path cr))
 

(define-snippet fill-and-stroke2 (cr)
  (fill-and-stroke-common cr)
  (cairo:move-to cr 0.25 0.1)
  (cairo:rel-line-to cr 0.2 0.2)
  (cairo:rel-line-to cr -0.2 0.2)
  (cairo:rel-line-to cr -0.2 -0.2)
  (cairo:close-path cr)

  (cairo:set-source-color cr 0.0 0.0 1.0)
  (cairo:fill cr t)
  (cairo:set-source-color cr 0.0 0.0 0.0)
  (cairo:stroke cr))


(define-snippet fill-and-stroke (cr)
  (fill-and-stroke-common cr)

  (cairo:set-source-color cr 0.0 0.0 1.0)
  (cairo:fill cr t)
  (cairo:set-source-color cr 0.0 0.0 0.0)
  (cairo:stroke cr))


(define-snippet gradient (cr)
  (let ((pattern (cairo:pattern-create-linear 0.0 0.0 0.0 1.0)))
    (cairo:pattern-add-color-stop pattern 1.0 0.0 0.0 0.0 1.0)
    (cairo:pattern-add-color-stop pattern 0.0 1.0 1.0 1.0 1.0)
    (cairo:rectangle cr 0.0 0.0 1.0 1.0)
    (setf (cairo:source cr) pattern)
    (cairo:fill cr))
  (let ((pattern (cairo:pattern-create-radial 0.45 0.4 0.1 0.4 0.4 0.5)))
    (cairo:pattern-add-color-stop pattern 0.0 1.0 1.0 1.0 1.0)
    (cairo:pattern-add-color-stop pattern 1.0 0.0 0.0 0.0 1.0)
    (setf (cairo:source cr) pattern)
    (cairo:circle cr 0.5 0.5 0.3)
    (cairo:fill cr)))


(define-snippet image (cr)
  (let ((image (make-instance 'cairo:image-surface 
		:filename #p"clg:examples;romedalen.png")))
    (cairo:translate cr 0.5 0.5)
    (cairo:rotate cr (deg-to-rad 45.0))
    (let ((width (cairo:surface-width image))
	  (height (cairo:surface-height image)))
      (cairo:scale cr (/ 1.0 width) (/ 1.0 height))  
      (cairo:translate cr (* -0.5 width) (* -0.5 height)))
    (cairo:set-source-surface cr image 0 0)
    (cairo:paint cr)))

    
(define-snippet image-pattern (cr)
  (let* ((image (make-instance 'cairo:image-surface
		 :filename #p"clg:examples;romedalen.png"))
	 (pattern (cairo:pattern-create-for-surface image)))
    (setf (cairo:pattern-extend pattern) :repeat)    
    (cairo:translate cr 0.5 0.5)
    (cairo:rotate cr (deg-to-rad 45.0))
    (cairo:scale cr (/ 1.0 (sqrt 2.0)) (/ 1.0 (sqrt 2.0)))  
    (cairo:translate cr -0.5 -0.5)
    (let ((width (cairo:surface-width image))
	  (height (cairo:surface-height image))
	  (matrix (make-instance 'cairo:matrix)))
      (cairo:matrix-init-scale matrix (* 5 width) (* 5 height))
      (setf (cairo:pattern-matrix pattern) matrix))
    (setf (cairo:source cr) pattern)
    (cairo:rectangle cr 0.0 0.0 1.0 1.0)
    (cairo:fill cr)))


#?(pkg-config:pkg-exists-p "librsvg-2.0" :atleast-version "2.13.93")
(progn
(defun snippet-set-bg-svg (cr filename)
  (let ((handle (make-instance 'rsvg:handle :filename filename)))
    (cairo:with-context (cr)
      (with-slots (rsvg:width rsvg:height) handle
        (cairo:scale cr (/ 1.0 rsvg:width) (/ 1.0 rsvg:height))
	(rsvg:render-cairo handle cr)))))

(define-snippet librsvg (cr)
  (snippet-set-bg-svg cr "clg:examples;home.svg"))


(defmacro define-operator-snippet (name operator)
  `(define-snippet ,name (cr)
     (snippet-set-bg-svg cr "clg:examples;freedesktop.svg")
     (setf (cairo:operator cr) ,operator)
     
     (cairo:set-source-color cr 1.0 0.0 0.0 0.5)
     (cairo:rectangle cr 0.2 0.2 0.5 0.5)
     (cairo:fill cr)
     
     (cairo:set-source-color cr 0.0 1.0 0.0)
     (cairo:rectangle cr 0.4 0.4 0.4 0.4)
     (cairo:fill cr)
       
     (cairo:set-source-color cr 0.0 0.0 1.0)
     (cairo:rectangle cr 0.6 0.6 0.3 0.3)
     (cairo:fill cr)))

(define-operator-snippet operator-add :add)
(define-operator-snippet operator-atop :atop)
(define-operator-snippet operator-atop-reverse :dest-atop)
(define-operator-snippet operator-in :in)
(define-operator-snippet operator-in-reverse :dest-in)
(define-operator-snippet operator-out :out)
(define-operator-snippet operator-out-reverse :dest-out)
(define-operator-snippet operator-over :over)
(define-operator-snippet operator-over-reverse :dest-over)
(define-operator-snippet operator-saturate :saturate)
(define-operator-snippet operator-xor :xor)
)

      

(define-snippet path (cr)
  (cairo:move-to cr 0.5 0.1)
  (cairo:line-to cr 0.9 0.9)
  (cairo:rel-line-to cr -0.4 0.0)
  (cairo:curve-to cr 0.2 0.9 0.2 0.5 0.5 0.5)
  (cairo:stroke cr))


(define-snippet set-line-cap (cr)
  (setf (cairo:line-width cr) 0.12)
  (setf (cairo:line-cap cr) :butt)
  (cairo:move-to cr 0.25 0.2)
  (cairo:line-to cr 0.25 0.8)
  (cairo:stroke cr)
  (setf (cairo:line-cap cr) :round)
  (cairo:move-to cr 0.5 0.2)
  (cairo:line-to cr 0.5 0.8)
  (cairo:stroke cr)
  (setf (cairo:line-cap cr) :square)
  (cairo:move-to cr 0.75 0.2)
  (cairo:line-to cr 0.75 0.8)
  (cairo:stroke cr)

  ;; draw helping lines
  (cairo:set-source-color cr 1.0 0.2 0.2)
  (setf (cairo:line-width cr) 0.01)
  (cairo:move-to cr 0.25 0.2)
  (cairo:line-to cr 0.25 0.8)
  (cairo:move-to cr 0.5 0.2) 
  (cairo:line-to cr 0.5 0.8)
  (cairo:move-to cr 0.75 0.2)
  (cairo:line-to cr 0.75 0.8)
  (cairo:stroke cr))


(define-snippet set-line-join (cr)
  (setf (cairo:line-width cr) 0.16)
  (cairo:move-to cr 0.3 0.33)
  (cairo:rel-line-to cr 0.2 -0.2)
  (cairo:rel-line-to cr 0.2 0.2)
  (setf (cairo:line-join cr) :miter) ; default
  (cairo:stroke cr)

  (cairo:move-to cr 0.3 0.63)
  (cairo:rel-line-to cr 0.2 -0.2)
  (cairo:rel-line-to cr 0.2 0.2)
  (setf (cairo:line-join cr) :bevel)
  (cairo:stroke cr)
		
  (cairo:move-to cr 0.3 0.93)
  (cairo:rel-line-to cr 0.2 -0.2)
  (cairo:rel-line-to cr 0.2 0.2)
  (setf (cairo:line-join cr) :round)
  (cairo:stroke cr))



(define-snippet text (cr)
   (cairo:select-font-face cr "Sans" :normal :bold)
;;   ;(setf (cairo:font-size cr) 0.35)
   (cairo:set-font-size cr 0.35)

   (cairo:move-to cr 0.04 0.53)
   (cairo:show-text cr "Hello")

  (cairo:move-to cr 0.27 0.65)
  (cairo:text-path cr "void")
  (cairo:set-source-color cr 0.5 0.5 1.0)
  (cairo:fill cr t)

  (cairo:set-source-color cr 0.0 0.0 0.0)
  (setf (cairo:line-width cr) 0.01)
  (cairo:stroke cr)

  ;; draw helping lines
  (cairo:set-source-color cr 1.0 0.2 0.2  0.6)
  (cairo:arc cr 0.04 0.53 0.02 0 (deg-to-rad 360.0))
  (cairo:arc cr 0.27 0.65 0.02 0 (deg-to-rad 360.0))
  (cairo:fill cr))


(define-snippet text-align-center (cr)
  (let ((text "cairo"))
    (cairo:select-font-face cr "Sans" :normal :normal)
    (cairo:set-font-size cr 0.2)

    (let* ((extents (cairo:text-extents cr text))
	   (x (- 0.5 (+ (/ (cairo:text-extents-width extents) 2) (cairo:text-extents-x-bearing extents))))
	   (y (- 0.5 (+ (/ (cairo:text-extents-height extents) 2) (cairo:text-extents-y-bearing extents)))))
      (cairo:move-to cr x y)
      (cairo:show-text cr text)

      ;; draw helping lines
      (cairo:set-source-color cr 1.0 0.2 0.2 0.6)
      (cairo:circle cr x y 0.05)
      (cairo:fill cr)
      (cairo:move-to cr 0.5 0.0)
      (cairo:rel-line-to cr 0.0 1.0)
      (cairo:move-to cr 0.0 0.5)
      (cairo:rel-line-to cr 1.0 0.0)
      (cairo:stroke cr))))

(define-snippet text-extents (cr)
  (let ((text "cairo"))
    (cairo:select-font-face cr "Sans" :normal :normal)
    (cairo:set-font-size cr 0.4)

    (let* ((extents (cairo:text-extents cr text))
	   (x 0.1)
	   (y 0.6))
      (cairo:move-to cr x y)
      (cairo:show-text cr text)

      ;; draw helping lines
      (cairo:set-source-color cr 1.0 0.2 0.2 0.6)
      (cairo:circle cr x y 0.05)
      (cairo:fill cr)
      (cairo:move-to cr x y)
      (cairo:rel-line-to cr 0 (- (cairo:text-extents-height extents)))
      (cairo:rel-line-to cr (cairo:text-extents-width extents) 0)
      (cairo:rel-line-to cr 
       (cairo:text-extents-x-bearing extents)
       (- (cairo:text-extents-y-bearing extents)))
      (cairo:stroke cr))))


(defun create-tests ()
;;   (rc-parse "clg:examples;testgtkrc2")
;;   (rc-parse "clg:examples;testgtkrc")

  (let* ((main-window (make-instance 'window
		       :title "testcairo.lisp" :name "main-window"
		       :default-width 200 :default-height 400
		       :allow-grow t :allow-shrink nil))
	 (scrolled-window (make-instance 'scrolled-window
			   :hscrollbar-policy :automatic 
			   :vscrollbar-policy :automatic
			   :border-width 10))
	 (close-button (make-instance 'button 
		        :stock "gtk-close" :can-default t
			:signal (list 'clicked #'widget-destroy 
			         :object main-window))))

    (let ((icon (gdk:pixbuf-load #p"clg:examples;gtk.png")))
      (setf 
       (window-icon main-window) 
       (gdk:pixbuf-add-alpha icon t 254 254 252)))

    ;; Main box
    (make-instance 'v-box
     :parent main-window
     :child-args '(:expand nil)
     :child (list (make-instance 'label 
		   :label (format nil "Cairo ~A" (cairo:version-string)))
		  :fill nil)
     :child (list (make-instance 'label :label (clg-version)) :fill nil)
     :child (list (make-instance 'label 			  
		   :label #-cmu
		          (format nil "~A ~A" 
			   (lisp-implementation-type)
			   #-clisp
			   (lisp-implementation-version)
			   #+clisp
			   (let ((version (lisp-implementation-version)))
			     (subseq version 0 (position #\sp version))))
		          ;; The version string in CMUCL is far too long
		          #+cmu(lisp-implementation-type))
		  :fill nil)
     :child (list scrolled-window :expand t)
     :child (make-instance 'h-separator)
     :child (make-instance 'v-box 
	     :homogeneous nil :spacing 10 :border-width 10 
	     :child close-button))

    (let ((content-box 
	   (make-instance 'v-box
	    :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
	    :children (mapcar #'(lambda (snippet)
				  (create-button (string-downcase snippet) snippet))
			      (setq *snippets* (sort *snippets* #'string<))))))
      (scrolled-window-add-with-viewport scrolled-window content-box))
    
    (widget-grab-focus close-button)
    (widget-show-all main-window)
    main-window))


(clg-init)
(rsvg:init)

;; We need to turn off floating point exceptions, because Cairo is
;; presumably using internal code which generates NaNs in some cases.
;; Thanks to Christophe Rhodes for pointing this out.
#+sbcl(sb-int:set-floating-point-modes :traps nil) 
#+cmu(ext:set-floating-point-modes :traps nil)

(within-main-loop (create-tests))
