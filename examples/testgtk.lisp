;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: testgtk.lisp,v 1.5 2004-11-08 14:16:12 espen Exp $


;;; Some of the code in this file are really outdatet, but it is
;;; still the most complete example of how to use the library


;(use-package "GTK")
(in-package "GTK")

(defmacro define-toplevel (name (window title &rest initargs) &body body)
  `(let ((,window nil))
     (defun ,name ()
       (unless ,window
	 (setq ,window (apply #'make-instance 'window :title ,title ',initargs))
	 (signal-connect ,window 'destroy #'(lambda () (setq ,window nil)))
	 ,@body)
       
       (if (not (widget-visible-p ,window))
	   (widget-show-all ,window)
	 (widget-hide ,window)))))


(defmacro define-dialog (name (dialog title &optional (class 'dialog)
			       &rest initargs)
			 &body body)
  `(let ((,dialog nil))
     (defun ,name ()
       (unless ,dialog
	 (setq ,dialog (apply #'make-instance ,class :title ,title ',initargs))
	 (signal-connect ,dialog 'destroy #'(lambda () (setq ,dialog nil)))
	 ,@body)
       
       (if (not (widget-visible-p ,dialog))
	   (widget-show ,dialog)
	 (widget-hide ,dialog)))))


(defmacro define-simple-dialog (name (dialog title &rest initargs) &body body)
  `(define-dialog ,name (,dialog ,title 'dialog ,@initargs)
    (dialog-add-button ,dialog "Close" #'widget-destroy :object t)
    ,@body))



;;; Pixmaps used in some of the tests

(defvar gtk-mini-xpm
  #("15 20 17 1"
    "       c None"
    ".      c #14121F"
    "+      c #278828"
    "@      c #9B3334"
    "#      c #284C72"
    "$      c #24692A"
    "%      c #69282E"
    "&      c #37C539"
    "*      c #1D2F4D"
    "=      c #6D7076"
    "-      c #7D8482"
    ";      c #E24A49"
    ">      c #515357"
    ",      c #9B9C9B"
    "'      c #2FA232"
    ")      c #3CE23D"
    "!      c #3B6CCB"
    "               "
    "      ***>     "
    "    >.*!!!*    "
    "   ***....#*=  "
    "  *!*.!!!**!!# "
    " .!!#*!#*!!!!# "
    " @%#!.##.*!!$& "
    " @;%*!*.#!#')) "
    " @;;@%!!*$&)'' "
    " @%.%@%$'&)$+' "
    " @;...@$'*'*)+ "
    " @;%..@$+*.')$ "
    " @;%%;;$+..$)# "
    " @;%%;@$$$'.$# "
    " %;@@;;$$+))&* "
    "  %;;;@+$&)&*  "
    "   %;;@'))+>   "
    "    %;@'&#     "
    "     >%$$      "
    "      >=       "))

(defvar book-closed-xpm
  #("16 16 6 1"
    "       c None s None"
    ".      c black"
    "X      c red"
    "o      c yellow"
    "O      c #808080"
    "#      c white"
    "                "
    "       ..       "
    "     ..XX.      "
    "   ..XXXXX.     "
    " ..XXXXXXXX.    "
    ".ooXXXXXXXXX.   "
    "..ooXXXXXXXXX.  "
    ".X.ooXXXXXXXXX. "
    ".XX.ooXXXXXX..  "
    " .XX.ooXXX..#O  "
    "  .XX.oo..##OO. "
    "   .XX..##OO..  "
    "    .X.#OO..    "
    "     ..O..      "
    "      ..        "
    "                "))

(defvar mini-page-xpm
  #("16 16 4 1"
    "       c None s None"
    ".      c black"
    "X      c white"
    "o      c #808080"
    "                "
    "   .......      "
    "   .XXXXX..     "
    "   .XoooX.X.    "
    "   .XXXXX....   "
    "   .XooooXoo.o  "
    "   .XXXXXXXX.o  "
    "   .XooooooX.o  "
    "   .XXXXXXXX.o  "
    "   .XooooooX.o  "
    "   .XXXXXXXX.o  "
    "   .XooooooX.o  "
    "   .XXXXXXXX.o  "
    "   ..........o  "
    "    oooooooooo  "
    "                "))

(defvar book-open-xpm
  #("16 16 4 1"
    "       c None s None"
    ".      c black"
    "X      c #808080"
    "o      c white"
    "                "
    "  ..            "
    " .Xo.    ...    "
    " .Xoo. ..oo.    "
    " .Xooo.Xooo...  "
    " .Xooo.oooo.X.  "
    " .Xooo.Xooo.X.  "
    " .Xooo.oooo.X.  "
    " .Xooo.Xooo.X.  "
    " .Xooo.oooo.X.  "
    "  .Xoo.Xoo..X.  "
    "   .Xo.o..ooX.  "
    "    .X..XXXXX.  "
    "    ..X.......  "
    "     ..         "
    "                "))



;;; Button box

(defun create-bbox-in-frame (class frame-label spacing width height layout)
  (declare (ignore width height))
  (make-instance 'frame
   :label frame-label
   :child (make-instance class
	   :border-width 5 :layout-style layout :spacing spacing
;	   :child-min-width width :child-min-height height
	   :child (make-instance 'button :label "OK")
	   :child (make-instance 'button :label "Cancel")
	   :child (make-instance 'button :label "Help"))))

(define-toplevel create-button-box (window "Button Boxes")
  (make-instance 'v-box
   :parent window :border-width 10 :spacing 10 :show-all t
   :child (make-instance 'frame
	   :label "Horizontal Button Boxes"
	   :child (make-instance 'v-box
		   :border-width 10 :spacing 10
		   :children (mapcar	
			      #'(lambda (args)
				  (apply #'create-bbox-in-frame 
				   'h-button-box args))
			      '(("Spread" 40 85 20 :spread) 
				("Edge" 40 85 20 :edge)
				("Start" 40 85 20 :start) 
				("End" 40 85 20 :end)))))
   :child (make-instance 'frame
	   :label "Vertical Button Boxes"
	   :child (make-instance 'h-box
		   :border-width 10 :spacing 10
		   :children (mapcar
			      #'(lambda (args)
				  (apply #'create-bbox-in-frame
				   'v-button-box args))
			      '(("Spread" 30 85 20 :spread) 
				("Edge" 30 85 20 :edge)
				("Start" 30 85 20 :start) 
				("End" 30 85 20 :end)))))))


;; Buttons

(define-simple-dialog create-buttons (dialog "Buttons")
  (let ((table (make-instance 'table
	        :n-rows 3 :n-columns 3 :homogeneous nil
		:row-spacing 5 :column-spacing 5 :border-width 10
		:parent dialog))
	  (buttons (loop
		    for n from 1 to 10
		    collect (make-instance 'button 
			     :label (format nil "button~D" (1+ n))))))

    (dotimes (column 3)
      (dotimes (row 3)
	(let ((button (nth (+ (* 3 row) column) buttons))
	      (button+1 (nth (mod (+ (* 3 row) column 1) 9) buttons)))
	  (signal-connect button 'clicked
			  #'(lambda ()
			      (if (widget-visible-p button+1)
				  (widget-hide button+1)
				(widget-show button+1))))
	  (table-attach table button column (1+ column) row (1+ row)))))
    (widget-show-all table)))


;; Calenadar

(define-simple-dialog create-calendar (dialog "Calendar")
  (make-instance 'v-box
   :parent dialog :border-width 10 :show-all t
   :child (make-instance 'calendar)))


;;; Check buttons

(define-simple-dialog create-check-buttons (dialog "Check Buttons")
  (make-instance 'v-box
   :border-width 10 :spacing 10 :parent dialog :show-all t
   :children (loop
	      for n from 1 to 3
	      collect (make-instance 'check-button
		       :label (format nil "Button~D" n)))))



;;; Color selection

(define-dialog create-color-selection (dialog "Color selection dialog" 
				       'color-selection-dialog
				       :allow-grow nil :allow-shrink nil)
  (with-slots (action-area colorsel) dialog
;;     This seg faults for some unknown reason
;;     (let ((button (make-instance 'check-button :label "Show Palette")))
;;       (dialog-add-action-widget dialog button
;;        #'(lambda () 
;;  	  (setf 
;;  	   (color-selection-has-palette-p colorsel)
;;  	   (toggle-button-active-p button)))))

    (container-add action-area 
     (create-check-button "Show Opacity" 
      #'(lambda (state)
	  (setf (color-selection-has-opacity-control-p colorsel) state))))

    (container-add action-area
     (create-check-button "Show Palette" 
      #'(lambda (state) 
	  (setf (color-selection-has-palette-p colorsel) state))))

    (signal-connect dialog :ok
     #'(lambda ()
	 (let ((color (color-selection-current-color colorsel)))
	   (format t "Selected color: ~A~%" color)
	   (setf (color-selection-current-color colorsel) color)
	   (widget-hide dialog))))

    (signal-connect dialog :cancel #'widget-destroy :object t)))


;;; Cursors

(defun clamp (n min-val max-val)
  (declare (number n min-val max-val))
  (max (min n max-val) min-val))


;; (defun set-cursor (spinner drawing-area label)
;;   (let ((cursor
;; 	 (glib:int-enum
;; 	  (logand (clamp (spin-button-value-as-int spinner) 0 152) #xFE)
;; 	  'gdk:cursor-type)))	
;;     (setf (label-text label) (string-downcase cursor))
;;     (setf (widget-cursor drawing-area) cursor)))
    

; (define-standard-dialog create-cursors "Cursors"
;   (setf (container-border-width main-box) 10)
;   (setf (box-spacing main-box) 5)
;   (let* ((hbox (hbox-new nil 0))
; 	 (label (create-label "Cursor Value : "))
; 	 (adj (adjustment-new 0 0 152 2 10 0))
; 	 (spinner (spin-button-new adj 0 0)))
;     (setf (container-border-width hbox) 5)
;     (box-pack-start main-box hbox nil t 0)
;     (setf (misc-xalign label) 0)
;     (setf (misc-yalign label) 0.5)
;     (box-pack-start hbox label nil t 0)
;     (box-pack-start hbox spinner t t 0)

;     (let ((frame (make-frame
; 		  :shadow-type :etched-in
; 		  :label-xalign 0.5
; 		  :label "Cursor Area"
; 		  :border-width 10
; 		  :parent main-box
; 		  :visible t))
; 	  (drawing-area (drawing-area-new)))
;       (setf (widget-width drawing-area) 80)
;       (setf (widget-height drawing-area) 80)
;       (container-add frame drawing-area)
;       (signal-connect
;        drawing-area 'expose-event
;        #'(lambda (event)
; 	   (declare (ignore event))
; 	   (multiple-value-bind (width height)
; 	       (drawing-area-size drawing-area)
; 	     (let* ((drawable (widget-window drawing-area))
; 		    (style (widget-style drawing-area))
; 		    (white-gc (style-get-gc style :white))
; 		    (gray-gc (style-get-gc style :background :normal))
; 		    (black-gc (style-get-gc style :black)))
; 	       (gdk:draw-rectangle
; 		drawable white-gc t 0 0 width (floor height 2))
; 	       (gdk:draw-rectangle
; 		drawable black-gc t 0 (floor height 2) width (floor height 2))
; 	       (gdk:draw-rectangle
; 		drawable gray-gc t (floor width 3) (floor height 3)
; 		(floor width 3) (floor height 3))))
; 	     t))
;       (setf (widget-events drawing-area) '(:exposure :button-press))
;       (signal-connect
;        drawing-area 'button-press-event
;        #'(lambda (event)
; 	   (when (and
; 		  (eq (gdk:event-type event) :button-press)
; 		  (or
; 		   (= (gdk:event-button event) 1)
; 		   (= (gdk:event-button event) 3)))
; 	     (spin-button-spin
; 	      spinner
; 	      (if (= (gdk:event-button event) 1)
; 		  :step-forward
; 		:step-backward)
; 	      0)
; 	     t)))
;       (widget-show drawing-area)

;     (let ((label (make-label
; 		  :visible t
; 		  :label "XXX"
; 		  :parent main-box)))
;       (setf (box-child-expand-p #|main-box|# label) nil)
;       (signal-connect
;        spinner 'changed
;        #'(lambda ()
; 	   (set-cursor spinner drawing-area label)))

;       (widget-realize drawing-area)
;       (set-cursor spinner drawing-area label)))))



;;; Dialog

(let ((dialog nil))
  (defun create-dialog ()
    (unless dialog
      (setq dialog (make-instance 'dialog 
		    :title "Dialog" :default-width 200 
		    :button "Toggle"
		    :button (list "gtk-ok" #'widget-destroy :object t)
		    :signal (list 'destroy 
			     #'(lambda () 
				 (setq dialog nil)))))

      (let ((label (make-instance 'label 
		    :label "Dialog Test" :xpad 10 :ypad 10 :visible t
		    :parent dialog)))
	(signal-connect dialog "Toggle"
	 #'(lambda ()
	     (if (widget-visible-p label)
		 (widget-hide label)
	       (widget-show label))))))

    (if (widget-visible-p dialog)
	(widget-hide dialog)
       (widget-show dialog))))


;; Entry

(define-simple-dialog create-entry (dialog "Entry")
  (let ((main (make-instance 'v-box 
	       :border-width 10 :spacing 10 :parent dialog)))

    (let ((entry (make-instance 'entry :text "hello world" :parent main)))
      (editable-select-region entry 0 5) ; this has no effect when 
					 ; entry is editable
;;     (editable-insert-text entry "great " 6)
;;     (editable-delete-text entry 6 12)
      
      (let ((combo (make-instance 'combo-box-entry 
		    :parent main
		    :content '("item0"
			       "item1 item1"
			       "item2 item2 item2"
			       "item3 item3 item3 item3"
			       "item4 item4 item4 item4 item4"
			       "item5 item5 item5 item5 item5 item5"
			       "item6 item6 item6 item6 item6"
			       "item7 item7 item7 item7"
			       "item8 item8 item8"
			       "item9 item9"))))
	(with-slots (child) combo 
	  (setf (editable-text child) "hello world")
	  (editable-select-region child 0)))

      (flet ((create-check-button (label slot)
	       (make-instance 'check-button
	        :label label :active t :parent main
		:signal (list 'toggled
			      #'(lambda (button)
				  (setf (slot-value entry slot)
					(toggle-button-active-p button)))
			      :object t))))
      
	(create-check-button "Editable" 'editable)
	(create-check-button "Visible" 'visibility)
	(create-check-button "Sensitive" 'sensitive)))
    (widget-show-all main)))



;; File chooser dialog

(define-dialog create-file-chooser (dialog "File Chooser" 'file-chooser-dialog)
  (dialog-add-button dialog "gtk-cancel" #'widget-destroy :object t)
  (dialog-add-button dialog "gtk-ok" 
   #'(lambda ()
       (format t "Selected file: ~A~%" (file-chooser-filename dialog))
       (widget-destroy dialog))))



;;; Handle box

;; (defun create-handle-box-toolbar ()
;;   (let ((toolbar (toolbar-new :horizontal :both)))
;;     (toolbar-append-item
;;      toolbar "Horizontal" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Horizontal toolbar layout"
;;      :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

;;     (toolbar-append-item
;;      toolbar "Vertical" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Vertical toolbar layout"
;;      :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

;;     (toolbar-append-space toolbar)
    
;;     (toolbar-append-item
;;      toolbar "Icons" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Only show toolbar icons"
;;      :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
;;     (toolbar-append-item
;;      toolbar "Text" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Only show toolbar text"
;;      :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
;;     (toolbar-append-item
;;      toolbar "Both" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Show toolbar icons and text"
;;      :callback #'(lambda () (setf (toolbar-style toolbar) :both)))

;;     (toolbar-append-space toolbar)

;;     (toolbar-append-item
;;      toolbar "Small" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Use small spaces"
;;      :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
;;     (toolbar-append-item
;;      toolbar "Big" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Use big spaces"
;;      :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
;;     (toolbar-append-space toolbar)

;;     (toolbar-append-item
;;      toolbar "Enable" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Enable tooltips"
;;      :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

;;     (toolbar-append-item
;;      toolbar "Disable" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Disable tooltips"
;;      :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

;;     (toolbar-append-space toolbar)

;;     (toolbar-append-item
;;      toolbar "Borders" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Show borders"
;;      :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
;;     (toolbar-append-item
;;      toolbar "Borderless" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Hide borders"
;;      :callback #'(lambda () (setf (toolbar-relief toolbar) :none)))

;;     toolbar))


;; (defun handle-box-child-signal (handle-box child action)
;;   (format t "~S: child ~S ~A~%" handle-box child action))


;; (define-test-window create-handle-box "Handle Box Test"
;;   (setf (window-allow-grow-p window) t)
;;   (setf (window-allow-shrink-p window) t)
;;   (setf (window-auto-shrink-p window) nil)
;;   (setf (container-border-width window) 20)
;;   (let ((v-box (v-box-new nil 0)))
;;     (container-add window v-box)

;;     (container-add v-box (create-label "Above"))
;;     (container-add v-box (hseparator-new))

;;     (let ((hbox (hbox-new nil 10)))
;;       (container-add v-box hbox)
      
;;       (let ((handle-box (handle-box-new)))
;; 	(box-pack-start hbox handle-box nil nil 0)
;; 	(signal-connect
;; 	 handle-box 'child-attached
;; 	 #'(lambda (child)
;; 	     (handle-box-child-signal handle-box child "attached")))
;; 	(signal-connect
;; 	 handle-box 'child-detached
;; 	 #'(lambda (child)
;; 	     (handle-box-child-signal handle-box child "detached")))
;; 	(container-add handle-box (create-handle-box-toolbar)))

;;       (let ((handle-box (handle-box-new)))
;; 	(box-pack-start hbox handle-box nil nil 0)
;; 	(signal-connect
;; 	 handle-box 'child-attached
;; 	 #'(lambda (child)
;; 	     (handle-box-child-signal handle-box child "attached")))
;; 	(signal-connect
;; 	 handle-box 'child-detached
;; 	 #'(lambda (child)
;; 	     (handle-box-child-signal handle-box child "detached")))

;; 	(let ((handle-box2 (handle-box-new)))
;; 	  (container-add handle-box handle-box2)
;; 	  (signal-connect
;; 	   handle-box2 'child-attached
;; 	   #'(lambda (child)
;; 	       (handle-box-child-signal handle-box child "attached")))
;; 	  (signal-connect
;; 	   handle-box2 'child-detached
;; 	   #'(lambda (child)
;; 	       (handle-box-child-signal handle-box child "detached")))
;; 	  (container-add handle-box2 (create-label "Foo!")))))
    
;;     (container-add v-box (hseparator-new))
;;     (container-add v-box (create-label "Below"))))

;;; Image

(define-toplevel create-image (window "Image")
  (make-instance 'image :file #p"clg:examples;gtk.png" :parent window))


;;; Labels
      
(define-toplevel create-labels (window "Labels" :border-width 5 :resizable nil)
  (flet ((create-label-in-frame (frame-label label-text &rest args)
	   (list 
	    (make-instance 'frame
	     :label frame-label
	     :child (apply #'make-instance 'label :label label-text :xpad 5 :ypad 5 args))
	    :fill nil :expand nil)))
    (make-instance 'h-box
     :spacing 5 :parent window
     :child-args '(:fill nil :expand nil)
     :child (make-instance 'v-box
             :spacing 5
	     :child (create-label-in-frame "Normal Label" "This is a Normal label")
	     :child (create-label-in-frame "Multi-line Label"
"This is a Multi-line label.
Second line
Third line")
	     :child (create-label-in-frame "Left Justified Label"
"This is a Left-Justified
Multi-line.
Third line"
                      :justify :left)
	     :child (create-label-in-frame "Right Justified Label"
"This is a Right-Justified
Multi-line.
Third line"
                     :justify :right))
     :child (make-instance 'v-box
	     :spacing 5
	     :child (create-label-in-frame "Line wrapped label"
"This is an example of a line-wrapped label.  It should not be taking up the entire             width allocated to it, but automatically wraps the words to fit.  The time has come, for all good men, to come to the aid of their party.  The sixth sheik's six sheep's sick.
     It supports multiple paragraphs correctly, and  correctly   adds many          extra  spaces. "
                      :wrap t)

	     :child (create-label-in-frame "Filled, wrapped label"
"This is an example of a line-wrapped, filled label.  It should be taking up the entire              width allocated to it.  Here is a seneance to prove my point.  Here is another sentence. Here comes the sun, do de do de do.
    This is a new paragraph.
    This is another newer, longer, better paragraph.  It is coming to an end, unfortunately."
                      :justify :fill :wrap t)

	     :child (create-label-in-frame "Underlined label"
"This label is underlined!
This one is underlined (こんにちは) in quite a funky fashion"
                      :justify :left
	              :pattern  "_________________________ _ _________ _ _____ _ __ __  ___ ____ _____")))))


;;; Layout

;; (defun layout-expose (layout event)
;;   (with-slots (window x-offset y-offset) layout
;;     (with-slots (x y width height) event
;;       (let ((imin (truncate (+ x-offset x) 10))
;; 	    (imax (truncate (+ x-offset x width 9) 10))
;; 	    (jmin (truncate (+ y-offset y) 10))
;; 	    (jmax (truncate (+ y-offset y height 9) 10)))
;; 	(declare (fixnum imin imax jmin jmax))
;; 	(gdk:window-clear-area window x y width height)

;; 	(let ((window (layout-bin-window layout))
;; 	      (gc (style-get-gc (widget-style layout) :black)))
;; 	  (do ((i imin (1+ i)))
;; 	      ((= i imax))
;; 	    (declare (fixnum i))
;; 	    (do ((j jmin (1+ j)))
;; 		((= j jmax))
;; 	      (declare (fixnum j))
;; 	      (unless (zerop (mod (+ i j) 2))
;; 		(gdk:draw-rectangle
;; 		 window gc t
;; 		 (- (* 10 i) x-offset) (- (* 10 j) y-offset)
;; 		 (1+ (mod i 10)) (1+ (mod j 10))))))))))
;;   t)


(define-toplevel create-layout (window "Layout" :default-width 200
				                :default-height 200)
  (let ((layout (make-instance 'layout
		 :parent (make-instance 'scrolled-window :parent window)
		 :width 1600 :height 128000 :events '(:exposure-mask)
;; 		 :signal (list 'expose-event #'layout-expose :object t)
		 )))

    (with-slots (hadjustment vadjustment) layout
      (setf
       (adjustment-step-increment hadjustment) 10.0
       (adjustment-step-increment vadjustment) 10.0))

    (dotimes (i 16)
      (dotimes (j 16)
	(let ((text (format nil "Button ~D, ~D" i j)))
	  (make-instance (if (not (zerop (mod (+ i j) 2)))
			     'button
			   'label)
	   :label text :parent (list layout :x (* j 100) :y (* i 100))))))

    (loop
     for i from 16 below 1280
     do (let ((text (format nil "Button ~D, ~D" i 0)))
	  (make-instance (if (not (zerop (mod i 2)))
			     'button
			   'label)
	   :label text :parent (list layout :x 0 :y (* i 100)))))))



;;; List    
    
;; (define-standard-dialog create-list "List"
;;   (let ((scrolled-window (scrolled-window-new))
;;         (list (list-new)))
;;     (setf (container-border-width scrolled-window) 5)
;;     (setf (scrolled-window-scrollbar-policy scrolled-window) :automatic)
;;     (box-pack-start main-box scrolled-window t t 0)
;;     (setf (widget-height scrolled-window) 300)

;;     (setf (list-selection-mode list) :extended)
;;     (scrolled-window-add-with-viewport scrolled-window list)
;;     (setf
;;      (container-focus-vadjustment list)
;;      (scrolled-window-vadjustment scrolled-window))
;;     (setf
;;      (container-focus-hadjustment list)
;;      (scrolled-window-hadjustment scrolled-window))
    
;;     (with-open-file (file "clg:examples;gtktypes.lisp")
;;       (labels ((read-file ()
;; 	         (let ((line (read-line file nil nil)))
;; 		   (when line
;; 		     (container-add list (list-item-new line))
;; 		     (read-file)))))
;; 	(read-file)))

;;     (let ((hbox (hbox-new t 5)))
;;       (setf (container-border-width hbox) 5)
;;       (box-pack-start main-box hbox nil t 0)

;;       (let ((button (button-new "Insert Row"))
;; 	    (i 0))
;; 	(box-pack-start hbox button t t 0)
;; 	(signal-connect
;; 	 button 'clicked
;; 	 #'(lambda ()
;; 	     (let ((item
;; 		    (list-item-new (format nil "added item ~A" (incf i)))))
;; 	       (widget-show item)
;; 	       (container-add list item)))))
	
;;       (let ((button (button-new "Clear List")))
;; 	(box-pack-start hbox button t t 0)
;; 	(signal-connect
;; 	 button 'clicked #'(lambda () (list-clear-items list 0 -1))))

;;       (let ((button (button-new "Remove Selection")))
;; 	(box-pack-start hbox button t t 0)
;; 	(signal-connect
;; 	 button 'clicked
;; 	 #'(lambda ()
;; 	     (let ((selection (list-selection list)))
;; 	       (if (eq (list-selection-mode list) :extended)
;; 		   (let ((item (or
;; 				(container-focus-child list)
;; 				(first selection))))
;; 		     (when item
;; 		       (let* ((children (container-children list))
;; 			      (sel-row
;; 			       (or
;; 				(find-if
;; 				 #'(lambda (item)
;; 				     (eq (widget-state item) :selected))
;; 				 (member item children))
;; 				(find-if
;; 				 #'(lambda (item)
;; 				     (eq (widget-state item) :selected))
;; 				 (member item (reverse children))))))
;; 			 (list-remove-items list selection)
;; 			 (when sel-row
;; 			   (list-select-child list sel-row)))))
;; 		 (list-remove-items list selection)))))
;; 	(box-pack-start hbox button t t 0)))

;;     (let ((cbox (hbox-new nil 0)))
;;       (box-pack-start main-box cbox nil t 0)

;;       (let ((hbox (hbox-new nil 5))
;; 	    (option-menu
;; 	     (create-option-menu
;; 	      `(("Single"
;; 		 ,#'(lambda () (setf (list-selection-mode list) :single)))
;; 		("Browse"
;; 		 ,#'(lambda () (setf (list-selection-mode list) :browse)))
;; 		("Multiple"
;; 		 ,#'(lambda () (setf (list-selection-mode list) :multiple)))
;; 		("Extended"
;; 		 ,#'(lambda () (setf (list-selection-mode list) :extended))))
;; 	      3)))

;; 	(setf (container-border-width hbox) 5)
;; 	(box-pack-start cbox hbox t nil 0)
;; 	(box-pack-start hbox (create-label "Selection Mode :") nil t 0)
;; 	(box-pack-start hbox option-menu nil t 0)))))



;; Menus

(defun create-menu (depth tearoff)
  (unless (zerop depth)
    (let ((menu (make-instance 'menu)))
      (when tearoff
	(let ((menu-item (make-instance 'tearoff-menu-item)))
	  (menu-shell-append menu menu-item)))
      (let ((group nil))
	(dotimes (i 5)
	  (let ((menu-item
		 (make-instance 'radio-menu-item
		  :label (format nil "item ~2D - ~D" depth (1+ i)))))
 	    (if group
 		(radio-menu-item-add-to-group menu-item group)
 	      (setq group menu-item))
	    (unless (zerop (mod depth 2))
	      (setf (check-menu-item-active-p menu-item) t))
	    (menu-shell-append menu menu-item)
	    (when (= i 3)
	      (setf (widget-sensitive-p menu-item) nil))
	    (setf (menu-item-submenu menu-item) (create-menu (1- depth) t)))))
      menu)))


(define-simple-dialog create-menus (dialog "Menus" :default-width 200)
  (let* ((main (make-instance 'v-box :parent dialog))
;	 (accel-group (make-instance 'accel-group))
	 (menubar (make-instance 'menu-bar :parent (list main :expand nil))))
;    (accel-group-attach accel-group window)

    (let ((menu-item (make-instance 'menu-item 
		      :label (format nil "test~%line2"))))
      (setf (menu-item-submenu menu-item) (create-menu 2 t))
      (menu-shell-append menubar menu-item))

    (let ((menu-item (make-instance 'menu-item :label "foo")))
      (setf (menu-item-submenu menu-item) (create-menu 3 t))
      (menu-shell-append menubar menu-item))

    (let ((menu-item (make-instance 'menu-item :label "bar")))
      (setf (menu-item-submenu menu-item) (create-menu 4 t))
      (setf (menu-item-right-justified-p menu-item) t)
      (menu-shell-append menubar menu-item))

    (make-instance 'v-box 
     :spacing 10 :border-width 10 :parent main
     :child (make-instance 'combo-box 
	     :active 3
	     :content (loop
		       for i from 1 to 5
		       collect (format nil "Item ~D" i))))
      
    (widget-show-all main)))


;;; Notebook

(defun create-notebook-page (notebook page-num)
  (let* ((title (format nil "Page ~D" page-num))
	 (page (make-instance 'frame :label title :border-width 10))
 	 (v-box (make-instance 'v-box 
 		 :homogeneous t :border-width 10 :parent page)))
     
    (make-instance 'h-box 
     :parent (list v-box :fill nil :padding 5) :homogeneous t
     :child-args '(:padding 5)
     :child (make-instance 'check-button 
	     :label "Fill Tab" :active t
	     :signal (list 'toggled
			   #'(lambda (button)
			       (setf 
				(notebook-child-tab-fill-p page)
				(toggle-button-active-p button)))
			   :object t))
     :child (make-instance 'check-button
	     :label "Expand Tab"
	     :signal (list 'toggled
			   #'(lambda (button)
			       (setf 
				(notebook-child-tab-expand-p page)
				(toggle-button-active-p button)))
			   :object t))
     :child (make-instance 'check-button
	     :label "Pack end"
	     :signal (list 'toggled
			   #'(lambda (button)
			       (setf 
				(notebook-child-tab-pack page)
				(if (toggle-button-active-p button)
				    :end
				  :start)))
			   :object t))
     :child (make-instance 'button
	     :label "Hide page"
	     :signal (list 'clicked #'(lambda () (widget-hide page)))))

    (let ((label-box (make-instance 'h-box 
		      :show-all t
		      :child-args '(:expand nil)
		      :child (make-instance 'image :pixmap book-closed-xpm)
		      :child (make-instance 'label :label title)))
	  (menu-box (make-instance 'h-box 
		     :show-all t
		     :child-args '(:expand nil)
		     :child (make-instance 'image :pixmap book-closed-xpm)
		     :child (make-instance 'label :label title))))

      (widget-show-all page)
      (notebook-append notebook page label-box menu-box))))
	

(define-simple-dialog create-notebook (dialog "Notebook")
  (let ((main (make-instance 'v-box :parent dialog)))
    (let ((notebook (make-instance 'notebook 
		     :border-width 10 :tab-pos :top :parent main)))
      (flet ((set-image (page func xpm)
	       (image-set-from-pixmap-data 
		(first (container-children (funcall func notebook page)))
		xpm)))	     
	(signal-connect notebook 'switch-page
	 #'(lambda (pointer page)
	     (declare (ignore pointer))
	     (unless (eq page (notebook-current-page-num notebook))
	       (set-image page #'notebook-menu-label book-open-xpm)
	       (set-image page #'notebook-tab-label book-open-xpm)
	     
	       (let ((curpage (notebook-current-page notebook)))
		 (when curpage
		   (set-image curpage #'notebook-menu-label book-closed-xpm)
		   (set-image curpage #'notebook-tab-label book-closed-xpm)))))))	  
      (loop for i from 1 to 5 do (create-notebook-page notebook i))

      (make-instance 'h-separator :parent (list main :expand nil :padding 10))
	
      (make-instance 'h-box 
       :spacing 5 :border-width 10
       :parent (list main :expand nil)
       :child-args '(:fill nil)
       :child (make-instance 'check-button 
	       :label "Popup menu"
	       :signal (list 'clicked
		        #'(lambda (button)
			    (if (toggle-button-active-p button)
				(notebook-popup-enable notebook)
				(notebook-popup-disable notebook)))
			:object t))
       :child (make-instance 'check-button 
	       :label "Homogeneous tabs"
	       :signal (list 'clicked
			#'(lambda (button)
			    (setf
			     (notebook-homogeneous-p notebook)
			     (toggle-button-active-p button)))
			:object t)))

      (make-instance 'h-box 
       :spacing 5 :border-width 10
       :parent (list main :expand nil)
       :child-args '(:expand nil)
       :child (make-instance 'label :label "Notebook Style: ")
       :child (let ((scrollable-p nil)) 
		;; option menu is deprecated, we should use combo-box
		(make-instance 'combo-box
		 :content '("Standard" "No tabs" "Scrollable") :active 0
		 :signal (list 'changed
			  #'(lambda (combo-box)
			      (case (combo-box-active combo-box)
				(0 
				 (setf (notebook-show-tabs-p notebook) t)
				 (when scrollable-p
				   (setq scrollable-p nil)
				   (setf (notebook-scrollable-p notebook) nil)
				   (loop repeat 10 
				    do (notebook-remove-page notebook 5))))
				(1
				 (setf (notebook-show-tabs-p notebook) nil)
				 (when scrollable-p
				   (setq scrollable-p nil)
				   (setf (notebook-scrollable-p notebook) nil)
				   (loop repeat 10 
				     do (notebook-remove-page notebook 5))))
				(2
				 (unless scrollable-p
				   (setq scrollable-p t)
				   (setf (notebook-show-tabs-p notebook) t)
				   (setf (notebook-scrollable-p notebook) t)
				   (loop for i from 6 to 15 
				    do (create-notebook-page notebook i))))))
			  :object t)))
       :child (make-instance 'button
	       :label "Show all Pages"
	       :signal (list 'clicked
			#'(lambda ()
			    (map-container nil #'widget-show notebook)))))

      (make-instance 'h-box 
       :spacing 5 :border-width 10
       :parent (list main :expand nil)
       :child (make-instance 'button 
	       :label "prev"
	       :signal (list 'clicked #'notebook-prev-page :object notebook))
       :child (make-instance 'button 
	       :label "next"
	       :signal (list 'clicked #'notebook-next-page :object notebook))
       :child (make-instance 'button 
	       :label "rotate"
	       :signal (let ((tab-pos 0))
			 (list 'clicked 
			  #'(lambda ()
			      (setq tab-pos (mod (1+ tab-pos) 4))
			      (setf
			       (notebook-tab-pos notebook)
			       (svref #(:top :right :bottom :left) tab-pos))))))))
    (widget-show-all main)))


;;; Panes

(defun toggle-resize (child)
  (let* ((paned (widget-parent child))
	 (is-child1-p (eq child (paned-child1 paned))))
    (multiple-value-bind (child resize shrink)
	(if is-child1-p
	    (paned-child1 paned)
	  (paned-child2 paned))
      (container-remove paned child)
      (if is-child1-p
	  (paned-pack1 paned child (not resize) shrink)
	(paned-pack2 paned child (not resize) shrink)))))

(defun toggle-shrink (child)
  (let* ((paned (widget-parent child))
	 (is-child1-p (eq child (paned-child1 paned))))
    (multiple-value-bind (child resize shrink)
	(if is-child1-p
	    (paned-child1 paned)
	  (paned-child2 paned))
      (container-remove paned child)
      (if is-child1-p
	  (paned-pack1 paned child resize (not shrink))
	(paned-pack2 paned child resize (not shrink))))))

(defun create-pane-options (paned frame-label label1 label2)
  (let* ((frame (make-instance 'frame :label frame-label :border-width 4))
	 (table (make-instance 'table :n-rows 3 :n-columns 2 :homogeneous t 
			              :parent frame)))

    (table-attach table (create-label label1) 0 1 0 1)
    (let ((check-button (make-instance 'check-button :label "Resize")))
      (table-attach table check-button 0 1 1 2)
      (signal-connect
       check-button 'toggled #'toggle-resize :object (paned-child1 paned)))
    (let ((check-button (make-instance 'check-button :label "Shrink")))
      (table-attach table check-button 0 1 2 3)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-shrink :object (paned-child1 paned)))

    (table-attach table (create-label label2) 1 2 0 1)
    (let ((check-button (make-instance 'check-button :label "Resize")))
      (table-attach table check-button 1 2 1 2)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-resize :object (paned-child2 paned)))
    (let ((check-button (make-instance 'check-button :label "Shrink")))
      (table-attach table check-button 1 2 2 3)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-shrink :object (paned-child2 paned)))
    frame))

(define-toplevel create-panes (window "Panes")
  (let* ((hpaned (make-instance 'h-paned
		  :child1 (make-instance 'frame
			   :width-request 60 :height-request 60
			   :shadow-type :in 
			   :child (make-instance 'buttun :label "Hi there"))
		  :child2 (make-instance 'frame			    
			   :width-request 80 :height-request 60
			   :shadow-type :in)))
	 (vpaned (make-instance 'v-paned
		  :border-width 5
		  :child1 hpaned
		  :child2 (make-instance 'frame
			   :width-request 80 :height-request 60
			   :shadow-type :in))))
    
    (make-instance 'v-box
     :parent window
     :child-args '(:expand nil)
     :child (list vpaned :expand t)
     :child (create-pane-options hpaned "Horizontal" "Left" "Right")
     :child (create-pane-options vpaned "Vertical" "Top" "Bottom"))))
  

;;; Progress bar

     


;;; Radio buttons

(define-simple-dialog create-radio-buttons (dialog "Radio buttons")
  (make-instance 'v-box
   :parent dialog :border-width 10 :spacing 10 :show-all t
   :children (create-radio-button-group '("button1" "button2" "button3") 1)))


;;; Rangle controls

(define-simple-dialog create-range-controls (dialog "Range controls")
  (let ((adjustment (adjustment-new 0.0 0.0 101.0 0.1 1.0 1.0)))
    (make-instance 'v-box
     :parent dialog :border-width 10 :spacing 10 :show-all t
     :child (make-instance 'h-scale
	     :width-request 150 :adjustment adjustment :inverted t
	     :update-policy :delayed :digits 1 :draw-value t)
     :child (make-instance 'h-scrollbar
             :adjustment adjustment :update-policy :continuous))))


;;; Reparent test

(define-simple-dialog create-reparent (dialog "Reparent")
  (let ((main (make-instance 'h-box 
	       :homogeneous t :spacing 10 :border-width 10 :parent dialog))
	(label (make-instance 'label :label "Hellow World")))

    (flet ((create-frame (title)
	     (let* ((frame (make-instance 'frame :label title :parent main))
		    (box (make-instance 'v-box 
                          :spacing 5 :border-width 5 :parent frame))
		    (button (make-instance 'button 
		             :label "switch" :parent (list box :expand nil))))
	       (signal-connect button 'clicked
	        #'(lambda ()
		    (widget-reparent label box)))
	       box)))

      (box-pack-start (create-frame "Frame 1") label nil t 0)
      (create-frame "Frame 2"))
    (widget-show-all main)))


;;; Rulers

(define-toplevel create-rulers (window "Rulers" 
				:default-width 300 :default-height 300
;;  				:events '(:pointer-motion-mask 
;;  					  :pointer-motion-hint-mask)
				)
  (setf 
   (widget-events window) 
   '(:pointer-motion-mask :pointer-motion-hint-mask))

  (let ((table (make-instance 'table :n-rows 2 :n-columns 2 :parent window)))
    (let ((ruler (make-instance 'h-ruler
		  :metric :centimeters :lower 100.0d0 :upper 0.0d0
		  :position 0.0d0 :max-size 20.0d0)))
      (signal-connect window 'motion-notify-event #'widget-event :object ruler)
      (table-attach table ruler 1 2 0 1 :y-options '(:fill)))
    (let ((ruler (make-instance 'v-ruler
	          :lower 5.0d0 :upper 15.0d0 
		  :position 0.0d0 :max-size 20.0d0)))
      (signal-connect window 'motion-notify-event #'widget-event :object ruler)
      (table-attach table ruler 0 1 1 2 :x-options '(:fill)))))



;;; Scrolled window

(define-simple-dialog create-scrolled-windows (dialog "Scrolled windows"
						      :default-width 300
						      :default-height 300)
  (let* ((scrolled-window
	  (make-instance 'scrolled-window
	   :parent dialog :border-width 10
	   :vscrollbar-policy :automatic 
	   :hscrollbar-policy :automatic))
	 (table
	  (make-instance 'table
	   :n-rows 20 :n-columns 20 :row-spacing 10 :column-spacing 10
	   :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
	   :focus-hadjustment (scrolled-window-hadjustment scrolled-window))))

      (scrolled-window-add-with-viewport scrolled-window table)
      (dotimes (i 20)
	(dotimes (j 20)
	  (let ((button
		 (make-instance 'toggle-button
		  :label (format nil "button (~D,~D)~%" i j))))
	    (table-attach table button i (1+ i) j (1+ j)))))
      (widget-show-all scrolled-window)))


;;; Shapes

;; (defun shape-create-icon (xpm-file x y px py type root-window destroy)
;;   (let* ((window
;; 	  (make-instance 'window
;; 	   :type type :x x :y y
;; 	   :events '(:button-motion :pointer-motion-hint :button-press)))
;; 	 (fixed
;; 	  (make-instance 'fixed
;; 	   :parent window :width 100 :height 100)))
      
;;     (widget-realize window)
;;     (multiple-value-bind (source mask) nil ;(gdk:pixmap-create xpm-file)
;;       (let ((pixmap (pixmap-new source mask))
;; 	    (x-offset 0)
;; 	    (y-offset 0))
;; 	(declare (fixnum x-offset y-offset))
;; 	(fixed-put fixed pixmap px py)
;; 	(widget-shape-combine-mask window mask px py)
	
;; 	(signal-connect window 'button-press-event
;; 	 #'(lambda (event)
;; 	     (when (typep event 'gdk:button-press-event)
;; 	       (setq x-offset (truncate (gdk:event-x event)))
;; 	       (setq y-offset (truncate (gdk:event-y event)))
;; 	       (grab-add window)
;; 	       (gdk:pointer-grab
;; 		(widget-window window) t
;; 		'(:button-release :button-motion :pointer-motion-hint)
;; 		nil nil 0))
;; 	     t))

;; 	(signal-connect window 'button-release-event
;; 	 #'(lambda (event)
;; 	     (declare (ignore event))
;; 	     (grab-remove window)
;; 	     (gdk:pointer-ungrab 0)
;; 	     t))
	
;; 	(signal-connect window 'motion-notify-event
;; 	 #'(lambda (event)
;; 	     (declare (ignore event))
;; 	     (multiple-value-bind (win xp yp mask)
;; 		 (gdk:window-get-pointer root-window)
;; 	       (declare (ignore mask win) (fixnum xp yp))
;; 	       (widget-set-uposition
;; 		window :x (- xp x-offset) :y (- yp y-offset)))
;; 	     t))
;; 	(signal-connect window 'destroy destroy)))
    
;;     (widget-show-all window)
;;     window))


;; (let ((modeller nil)
;;       (sheets nil)
;;       (rings nil))
;;   (defun create-shapes ()
;;     (let ((root-window (gdk:get-root-window)))
;;       (if (not modeller)
;; 	  (setq
;; 	   modeller
;; 	   (shape-create-icon
;; 	    "clg:examples;Modeller.xpm" 440 140 0 0 :popup root-window
;; 	    #'(lambda () (widget-destroyed modeller))))
;; 	(widget-destroy modeller))

;;       (if (not sheets)
;; 	  (setq
;; 	   sheets
;; 	   (shape-create-icon
;; 	    "clg:examples;FilesQueue.xpm" 580 170 0 0 :popup root-window
;; 	    #'(lambda () (widget-destroyed sheets))))
;; 	(widget-destroy sheets))

;;       (if (not rings)
;; 	  (setq
;; 	   rings
;; 	   (shape-create-icon
;; 	    "clg:examples;3DRings.xpm" 460 270 25 25 :toplevel root-window
;; 	    #'(lambda () (widget-destroyed rings))))
;; 	(widget-destroy rings)))))



;;; Spin buttons

(define-simple-dialog create-spins (dialog "Spin buttons" :has-separator nil)
  (let ((main (make-instance 'v-box 
	       :spacing 5 :border-width 10 :parent dialog)))

    (flet ((create-date-spinner (label adjustment shadow-type)
	     (declare (ignore shadow-type))
	     (make-instance 'v-box 
	      :child-args '(:expand nil)
	      :child (make-instance 'label
		      :label label :xalign 0.0 :yalign 0.5)
	      :child (make-instance 'spin-button
		      :adjustment adjustment :wrap t))))
      (make-instance 'frame 
       :label "Not accelerated" :parent main
       :child (make-instance 'h-box 
	       :border-width 10
	       :child-args '(:padding 5)
	       :child (create-date-spinner "Day : " 
		       (adjustment-new 1.0 1.0 31.0 1.0 5.0 0.0) :out)
	       :child (create-date-spinner "Month : " 
		       (adjustment-new 1.0 1.0 12.0 1.0 5.0 0.0) :etched-in)
	       :child (create-date-spinner "Year : " 
		       (adjustment-new 1998.0 0.0 2100.0 1.0 100.0 0.0) :in))))

    (let ((spinner1 (make-instance 'spin-button
		     :adjustment (adjustment-new 0.0 -10000.0 10000.0 0.5 100.0 0.0)
		      :climb-rate 1.0 :digits 2 :wrap t :width-request 100))
	  (spinner2 (make-instance 'spin-button 
		     :adjustment (adjustment-new 2.0 1.0 5.0 1.0 1.0 0.0)
		     :climb-rate 1.0 :wrap t))
	  (value-label (make-instance 'label :label "0")))
      (signal-connect (spin-button-adjustment spinner2) 'value-changed
       #'(lambda ()
	   (setf 
	    (spin-button-digits spinner1) 
	    (floor (spin-button-value spinner2)))))

      (make-instance 'frame 
       :label "Accelerated" :parent main
       :child (make-instance 'v-box 
	       :border-width 5
	       :child (list
		       (make-instance 'h-box 
			:child-args '(:padding 5)
			:child (make-instance 'v-box
				:child (make-instance 'label
					:label "Value :" 
					:xalign 0.0 :yalign 0.5)
				:child spinner1)
			:child (make-instance 'v-box
				:child (make-instance 'label 
					:label "Digits :" 
					:xalign 0.0 :yalign 0.5)
				:child spinner2))
		       :expand nil :padding 5)
	       :child (make-instance 'check-button 
		       :label "Snap to 0.5-ticks" :active t
		       :signal (list 'clicked
				#'(lambda (button)
				    (setf
				     (spin-button-snap-to-ticks-p spinner1)
				     (toggle-button-active-p button)))
				:object t))
	       :child (make-instance 'check-button
		       :label "Numeric only input mode" :active t
		       :signal (list 'clicked
				#'(lambda (button)
				    (setf
				     (spin-button-numeric-p spinner1)
				     (toggle-button-active-p button)))
				:object t))
	       :child value-label
	       :child (list
		       (make-instance 'h-box
			:child-args '(:padding 5)
			:child (make-instance 'button 
				:label "Value as Int"
				:signal (list 'clicked
					 #'(lambda ()
					     (setf
					      (label-label value-label)
					      (format nil "~D" 
					       (spin-button-value-as-int 
						spinner1))))))
			:child (make-instance 'button 
				:label "Value as Float"
				:signal (list 'clicked
					 #'(lambda ()
					     (setf
					      (label-label value-label)
					      (format nil
					       (format nil "~~,~DF" 
						(spin-button-digits spinner1))
					       (spin-button-value spinner1)))))))
		       :padding 5 :expand nil))))
    (widget-show-all main)))


;;; Statusbar

(define-toplevel create-statusbar (window "Statusbar")
  (let ((statusbar (make-instance 'statusbar :has-resize-grip t))
	(close-button (create-button '("close" :can-default t)
		       #'widget-destroy :object window))
	(counter 0))

    (signal-connect statusbar 'text-popped
     #'(lambda (context-id text)
	 (declare (ignore context-id))
	 (format nil "Popped: ~A~%" text)))
   
    (make-instance 'v-box
     :parent window
     :child (make-instance 'v-box
             :border-width 10 :spacing 10
	     :child (create-button "push something"
		     #'(lambda ()
			 (statusbar-push statusbar 1
			  (format nil "something ~D" (incf counter)))))
	     :child (create-button "pop" 
                     #'(lambda ()
			 (statusbar-pop statusbar 1)))
	     :child (create-button "steal #4" 
		     #'(lambda ()
			 (statusbar-remove statusbar 1 4)))
	     :child (create-button "dump stack")
	     :child (create-button "test contexts"))
     :child (list (make-instance 'h-separator) :expand nil)
     :child (list 
	     (make-instance 'v-box :border-width 10 :child close-button)
	     :expand nil)
     :child (list statusbar :expand nil))

    (widget-grab-focus close-button)))


;;; Idle test

;; (define-standard-dialog create-idle-test "Idle Test"
;;   (let* ((container (make-instance 'hbox :parent main-box))
;; 	 (label (make-instance 'label
;; 		 :label "count: 0" :xpad 10 :ypad 10 :parent container))
;; 	 (idle nil)
;; 	 (count 0))
;;     (declare (fixnum count))
;;     (signal-connect
;;      window 'destroy #'(lambda () (when idle (idle-remove idle))))
 
;;     (make-instance 'frame
;;      :label "Label Container" :border-width 5 :parent main-box
;;      :child
;;      (make-instance 'v-box
;;       :children
;;       (create-radio-button-group
;;        '(("Resize-Parent" :parent)
;; 	 ("Resize-Queue" :queue)
;; 	 ("Resize-Immediate" :immediate))
;;        0
;;        '(setf container-resize-mode) container)))

;;     (make-instance 'button
;;      :label "start" :can-default t :parent action-area
;;      :signals
;;      (list
;;       (list
;;        'clicked
;;        #'(lambda ()
;; 	   (unless idle
;; 	     (setq
;; 	      idle
;; 	      (idle-add
;; 	       #'(lambda ()
;; 		   (incf count)
;; 		   (setf (label-label label) (format nil "count: ~D" count))
;; 		   t))))))))
      
;;     (make-instance 'button
;;      :label "stop" :can-default t :parent action-area
;;      :signals
;;      (list
;;       (list
;;        'clicked
;;        #'(lambda ()
;; 	   (when idle
;; 	     (idle-remove idle)
;; 	     (setq idle nil))))))))
    


;;; Timeout test

;; (define-standard-dialog create-timeout-test "Timeout Test"
;;   (let ((label (make-instance 'label
;; 		:label "count: 0" :xpad 10 :ypad 10 :parent main-box))
;; 	(timer nil)
;; 	(count 0))
;;     (declare (fixnum count))
;;     (signal-connect
;;      window 'destroy #'(lambda () (when timer (timeout-remove timer))))
          
;;     (make-instance 'button
;;      :label "start" :can-default t :parent action-area
;;      :signals
;;      (list
;;       (list
;;        'clicked
;;        #'(lambda ()
;; 	   (unless timer
;; 	     (setq
;; 	      timer
;; 	      (timeout-add
;; 	       100
;; 	       #'(lambda ()
;; 		   (incf count)
;; 		   (setf (label-label label) (format nil "count: ~D" count))
;; 		   t))))))))

;;     (make-instance 'button
;;      :label "stop" :can-default t :parent action-area
;;      :signals
;;      (list
;;       (list
;;        'clicked
;;        #'(lambda ()
;; 	   (when timer
;; 	     (timeout-remove timer)
;; 	     (setq timer nil))))))))
  

;;; Toggle buttons

(define-simple-dialog create-toggle-buttons (dialog "Toggle Button")
  (make-instance 'v-box
   :border-width 10 :spacing 10 :parent dialog :show-all t
      :children (loop
	      for n from 1 to 3
	      collect (make-instance 'toggle-button
		       :label (format nil "Button~D" (1+ n))))))



;;; Toolbar test

;; TODO: style properties
(define-toplevel create-toolbar (window "Toolbar test" :resizable nil)
  (let ((toolbar (make-instance 'toolbar :parent window)))
;    (setf (toolbar-relief toolbar) :none)

    ;; Insert a stock item
    (toolbar-append toolbar "gtk-quit"
     :tooltip-text "Destroy toolbar"
     :tooltip-private-text "Toolbar/Quit"
     :callback #'(lambda () (widget-destroy window)))

    ;; Image widge as icon
    (toolbar-append toolbar "Horizontal"
     :icon (make-instance 'image :file #p"clg:examples;test.xpm")
     :tooltip-text "Horizontal toolbar layout"
     :tooltip-private-text "Toolbar/Horizontal"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

    ;; Icon from file
    (toolbar-append toolbar "Vertical"
     :icon #p"clg:examples;test.xpm"
     :tooltip-text "Vertical toolbar layout"
     :tooltip-private-text "Toolbar/Vertical"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

    (toolbar-append toolbar :space)
    
    ;; Stock icon
    (toolbar-append toolbar "Icons"
     :icon "gtk-execute"
     :tooltip-text "Only show toolbar icons"
     :tooltip-private-text "Toolbar/IconsOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
    ;; Icon from pixmap data
    (toolbar-append toolbar "Text" 
     :icon gtk-mini-xpm
     :tooltip-text "Only show toolbar text"
     :tooltip-private-text "Toolbar/TextOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
    (toolbar-append toolbar "Both"
     :tooltip-text "Show toolbar icons and text"
     :tooltip-private-text "Toolbar/Both"
     :callback #'(lambda () (setf (toolbar-style toolbar) :both)))

    (toolbar-append toolbar :space)

    (toolbar-append toolbar (make-instance 'entry)
     :tooltip-text "This is an unusable GtkEntry"
     :tooltip-private-text "Hey don't click me!")

    (toolbar-append toolbar :space)
    
;;     (toolbar-append-item
;;      toolbar "Small" ;(pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Use small spaces"
;;      :tooltip-private-text "Toolbar/Small"
;;      :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
;;     (toolbar-append-item
;;      toolbar "Big" ;(pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Use big spaces"
;;      :tooltip-private-text "Toolbar/Big"
;;      :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
;;     (toolbar-append toolbar :space)

    (toolbar-append
     toolbar "Enable"
     :tooltip-text "Enable tooltips"
     :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

    (toolbar-append
     toolbar "Disable"
     :tooltip-text "Disable tooltips"
     :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

    (toolbar-append toolbar :space)

;;     (toolbar-append-item
;;      toolbar "Borders" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Show borders"
;;      :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
;;     (toolbar-append-item
;;      toolbar
;;      "Borderless" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Hide borders"
;;      :callback #'(lambda () (setf (toolbar-relief toolbar) :none)))

;;     (toolbar-append toolbar :space)

;;     (toolbar-append-item
;;      toolbar "Empty" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Empty spaces"
;;      :callback #'(lambda () (setf (toolbar-space-style toolbar) :empty)))

;;     (toolbar-append-item
;;      toolbar "Lines" (pixmap-new "clg:examples;test.xpm")
;;      :tooltip-text "Lines in spaces"
;;      :callback #'(lambda () (setf (toolbar-space-style toolbar) :line)))
    
    ))



;;; Tooltips test

;; (define-standard-dialog create-tooltips "Tooltips"
;;   (setf
;;    (window-allow-grow-p window) t
;;    (window-allow-shrink-p window) nil
;;    (window-auto-shrink-p window) t
;;    (widget-width window) 200
;;    (container-border-width main-box) 10
;;    (box-spacing main-box) 10)

;;   (let ((tooltips (tooltips-new)))
;;     (flet ((create-button (label tip-text tip-private)
;; 	     (let ((button (make-instance 'toggle-button
;; 		    :label label :parent main-box)))
;; 	       (tooltips-set-tip tooltips button tip-text tip-private)
;; 	       button)))
;;       (create-button "button1" "This is button 1" "ContextHelp/button/1")
;;       (create-button "button2" "This is button 2. This is also a really long tooltip which probably won't fit on a single line and will therefore need to be wrapped. Hopefully the wrapping will work correctly." "ContextHelp/button/2")

;;       (let* ((toggle (create-button "Override TipSQuery Label"
;; 				    "Toggle TipsQuery view" "Hi msw! ;)"))
;; 	     (box (make-instance 'v-box
;; 		   :homogeneous nil :spacing 5 :border-width 5
;; 		   :parent (make-instance 'frame
;; 	                    :label "ToolTips Inspector"
;; 			    :label-xalign 0.5 :border-width 0
;; 			    :parent main-box)))
;; 	     (button (make-instance 'button :label "[?]" :parent box))
;; 	     (tips-query (make-instance 'tips-query
;; 			  :caller button :parent box)))

;; 	(signal-connect
;; 	 button 'clicked #'tips-query-start-query :object tips-query)
	
;; 	(signal-connect
;; 	 tips-query 'widget-entered
;; 	 #'(lambda (widget tip-text tip-private)
;; 	     (declare (ignore widget tip-private))
;; 	     (when (toggle-button-active-p toggle)
;; 	       (setf
;; 		(label-label tips-query)
;; 		(if tip-text
;; 		    "There is a Tip!"
;; 		  "There is no Tip!"))
;; 	       (signal-emit-stop tips-query 'widget-entered))))
	
;; 	(signal-connect
;; 	 tips-query 'widget-selected
;; 	 #'(lambda (widget tip-text tip-private event)
;; 	     (declare (ignore tip-text event))
;; 	     (when widget
;; 	       (format
;; 		t "Help ~S requested for ~S~%"
;; 		(or tip-private "None") (type-of widget)))
;; 	     t))

;; 	(tooltips-set-tip
;; 	 tooltips button "Start the Tooltip Inspector" "ContextHelp/buttons/?")
;; 	(tooltips-set-tip
;; 	 tooltips close-button "Push this button to close window"
;; 	 "ContextHelp/buttons/Close")))))
		  


;;; Main window
      
(defun create-main-window ()
;;   (rc-parse "clg:examples;testgtkrc2")
;;   (rc-parse "clg:examples;testgtkrc")

  (let* ((button-specs
	  '(("button box" create-button-box)
 	    ("buttons" create-buttons)
 	    ("calendar" create-calendar)
 	    ("check buttons" create-check-buttons)
;; 	    ("clist" #|create-clist|#)
 	    ("color selection" create-color-selection)
;; 	    ("ctree" #|create-ctree|#)
;; 	    ("cursors" #|create-cursors|#)
 	    ("dialog" create-dialog)
;; ;  	    ("dnd")
 	    ("entry" create-entry)
;; 	    ("event watcher")
 	    ("file chooser" create-file-chooser)
;; 	    ("font selection")
;; 	    ("handle box" create-handle-box)
 	    ("image" create-image)
;; 	    ("item factory")
 	    ("labels" create-labels)
 	    ("layout" create-layout)
;; 	    ("list" create-list)
	    ("menus" create-menus)
;; 	    ("modal window")
 	    ("notebook" create-notebook)
 	    ("panes" create-panes)
;; 	    ("progress bar" #|create-progress-bar|#)
 	    ("radio buttons" create-radio-buttons)
 	    ("range controls" create-range-controls)
;; 	    ("rc file")
 	    ("reparent" create-reparent)
 	    ("rulers" create-rulers)
;; 	    ("saved position")
 	    ("scrolled windows" create-scrolled-windows)
;; 	    ("shapes" create-shapes)
 	    ("spinbutton" create-spins)
 	    ("statusbar" create-statusbar)
;; 	    ("test idle" create-idle-test)
;; 	    ("test mainloop")
;; 	    ("test scrolling")
;; 	    ("test selection")
;; 	    ("test timeout" create-timeout-test)
;; 	    ("text" #|create-text|#)
 	    ("toggle buttons" create-toggle-buttons)
 	    ("toolbar" create-toolbar)
;; 	    ("tooltips" create-tooltips)
;; 	    ("tree" #|create-tree|#)
))
	(main-window (make-instance 'window
		      :title "testgtk.lisp" :name "main_window"
		      :default-width 200 :default-height 400
		      :allow-grow t :allow-shrink nil))
	(scrolled-window (make-instance 'scrolled-window
	                  :hscrollbar-policy :automatic 
			  :vscrollbar-policy :automatic
			  :border-width 10))
	(close-button (make-instance 'button 
		       :label "close" :can-default t
		       :signal (list 'clicked #'widget-destroy 
				     :object main-window)))) 

    ;; Main box
    (make-instance 'v-box
     :parent main-window
     :child-args '(:expand nil)
     :child (list (make-instance 'label :label (gtk-version)) :fill nil)
     :child (list (make-instance 'label :label "clg CVS version") :fill nil)
     :child (list scrolled-window :expand t)
     :child (make-instance 'h-separator)
     :child (make-instance 'v-box 
	     :homogeneous nil :spacing 10 :border-width 10 
	     :child close-button))

    (let ((content-box 
	   (make-instance 'v-box
	    :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
	    :children (mapcar #'(lambda (spec) 
				  (apply #'create-button spec))
			      button-specs))))
      (scrolled-window-add-with-viewport scrolled-window content-box))
    
    (widget-grab-focus close-button)
    (widget-show-all main-window)
    main-window))
 
(clg-init)
(create-main-window)
