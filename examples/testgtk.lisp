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

;; $Id: testgtk.lisp,v 1.2 2000-10-05 18:57:50 espen Exp $


(use-package "GTK")

(defmacro define-test-window (name title &body body)
  `(let ((window nil))
     (defun ,name ()
       (unless window
	 (setq window (window-new :toplevel))
	 (signal-connect
	  window 'destroy #'(lambda () (widget-destroyed window)))
	 (setf (window-title window) ,title)
	 (setf (container-border-width window) 0)
	 ,@body)
       
       (if (not (widget-visible-p window))
	   (widget-show-all window)
	   (widget-destroy window)))))
      

(defmacro define-test-dialog (name title &body body)
  `(let ((window nil))
     (defun ,name ()
       (unless window
	 (setq window (make-instance 'dialog))
	 (signal-connect
	  window 'destroy #'(lambda () (widget-destroyed window)))
	 (setf (window-title window) ,title)
	 (setf (container-border-width window) 0)
	 (let ((main-box (vbox-new nil 0))
	       (action-area (dialog-action-area window)))
	   (box-pack-start (dialog-main-box window) main-box t t 0)
	   ,@body))
       
       (if (not (widget-visible-p window))
	   (widget-show-all window)
	 (widget-destroy window)))))


(defmacro define-standard-dialog (name title &body body)
  `(define-test-dialog ,name ,title
     (let ((close-button (button-new "close")))
       (signal-connect close-button 'clicked #'widget-destroy :object window)
       (setf (widget-can-default-p close-button) t)
       (box-pack-start action-area close-button t t 0)
       (widget-grab-default close-button)
       ,@body)))



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
    (make-instance 'frame
     :label frame-label
     :child (make-instance class
	     :border-width 5 :layout layout :spacing spacing
	     :child-min-width width :child-min-height height
	     :children
	     (list
	      (button-new "OK")
	      (button-new "Cancel")
	      (button-new "Help")))))

(define-test-window create-button-box "Button Boxes"
  (setf (container-border-width window) 10)
  (make-instance 'vbox
   :parent window
   :children
   (list
    (list
     (make-instance 'frame
      :label  "Horizontal Button Boxes"
      :child
      (make-instance 'vbox
       :border-width 10
       :children
       (mapcar
	#'(lambda (args)
	    (list (apply #'create-bbox-in-frame 'hbutton-box args) :padding 5))
	'(("Spread" 40 85 20 :spread) ("Edge" 40 85 20 :edge)
	  ("Start" 40 85 20 :start) ("End" 40 85 20 :end)))))
     :padding 10)

    (list
     (make-instance 'frame
      :label "Vertical Button Boxes"
      :child
      (make-instance 'hbox
       :border-width 10
       :children
       (mapcar
	#'(lambda (args)
	    (list (apply #'create-bbox-in-frame 'vbutton-box args) :padding 5))
	'(("Spread" 30 85 20 :spread) ("Edge" 30 85 20 :edge)
	  ("Start" 30 85 20 :start) ("End" 30 85 20 :end)))))
     :padding 10))))


;; Buttons

(define-standard-dialog create-buttons "Buttons" 
  (let ((table (make-instance 'table
	        :rows 3 :columns 3 :homogeneous nil
		:row-spacing 5 :column-spacing 5 :border-width 10
		:parent main-box))
	(buttons (make-array 0 :adjustable t :fill-pointer t)))
    (dotimes (n 9)
      (vector-push-extend
       (button-new (format nil "button~D" (1+ n)))  buttons))
    (dotimes (column 3)
      (dotimes (row 3)
	(let ((button (aref buttons (+ (* 3 row) column)))
	      (button+1 (aref buttons (mod (+ (* 3 row) column 1) 9))))
	  (signal-connect button 'clicked
			  #'(lambda ()
			      (if (widget-visible-p button+1)
				  (widget-hide button+1)
				(widget-show button+1))))
	  (table-attach table button column (1+ column) row (1+ row)))))))


;; Calenadar

(define-standard-dialog create-calendar "Calendar"
  (setf (container-border-width main-box) 10)
  (make-instance 'calendar :parent main-box))


;;; Check buttons

(define-standard-dialog create-check-buttons "Check Buttons"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (dotimes (n 3)
    (make-instance 'check-button
     :label (format nil "Button~D" (1+ n))
     :parent main-box)))



;;; Color selection

(let ((color-dialog nil))
  (defun create-color-selection ()
    (unless color-dialog
      (setq
       color-dialog
       (make-instance 'color-selection-dialog
	:title "Color selection dialog" :position :mouse
	:allow-grow nil :allow-shrink nil
	:signals
	(list (list 'destroy #'(lambda () (widget-destroyed color-dialog))))))

      (with-slots (main-box colorsel) color-dialog
        (make-instance 'hbutton-box
	 :border-width 10 :layout :edge :visible t
	 :children
	 (list
	  (create-check-button
	   "Show Opacity" '(setf color-selection-use-opacity-p) nil colorsel)
	  (create-check-button
	   "Show Palette" '(setf color-selection-use-palette-p) nil colorsel))
	 :parent main-box)
	
	(signal-connect
	 (color-selection-dialog-ok-button color-dialog) 'clicked
	 #'(lambda ()
	     (let ((color (color-selection-color colorsel)))
	       (format t "Selected color: ~A~%" color)
	       (setf (color-selection-color colorsel) color))))
	(signal-connect
	 (color-selection-dialog-cancel-button color-dialog) 'clicked
	 #'widget-destroy :object color-dialog)))
       
    (if (not (widget-visible-p color-dialog))
	(widget-show color-dialog)
      (widget-destroy color-dialog))))




;;; Cursors

(defun clamp (n min-val max-val)
  (declare (number n min-val max-val))
  (max (min n max-val) min-val))


; (defun set-cursor (spinner drawing-area label)
;   (let ((cursor
; 	 (glib:int-enum
; 	  (logand (clamp (spin-button-value-as-int spinner) 0 152) #xFE)
; 	  'gdk:cursor-type)))	
;     (setf (label-text label) (string-downcase (symbol-name cursor)))
;     (setf (widget-cursor drawing-area) cursor)))
    

; (define-standard-dialog create-cursors "Cursors"
;   (setf (container-border-width main-box) 10)
;   (setf (box-spacing main-box) 5)
;   (let* ((hbox (hbox-new nil 0))
; 	 (label (label-new "Cursor Value : "))
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

(define-test-dialog create-dialog "Dialog"
  (setf (widget-width window) 200)
  (setf (widget-height window) 110)
      
  (let ((button (button-new "OK")))
    (signal-connect button 'clicked #'(lambda () (widget-destroy window)))
    (setf (widget-can-default-p button) t)
    (box-pack-start action-area button t t 0)
    (widget-grab-default button)
    (widget-show button))
  
  (let ((button (button-new "Toggle"))
	(label nil))
    (signal-connect
     button 'clicked
     #'(lambda ()
	 (if (not label)
	     (progn
	       (setq label (label-new "Dialog Test"))
	       (signal-connect label 'destroy #'widget-destroy :object label)
	       (setf (misc-xpad label) 10)
	       (setf (misc-ypad label) 10)
	       (box-pack-start main-box label t t 0)
	       (widget-show label))
	   (progn
	     (widget-destroy label)
	     (setq label nil)))))
    (setf (widget-can-default-p button) t)
    (box-pack-start action-area button t t 0)
    (widget-grab-default button)
    (widget-show button)))



;; Entry

(define-standard-dialog create-entry "Entry"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (let ((entry (make-instance 'entry :text "hello world" :parent main-box)))
    (editable-select-region entry 0 5)

    (let ((combo (make-instance 'combo :parent main-box)))
      (setf
       (combo-popdown-strings combo)
       '("item0"
 	 "item1 item1"
 	 "item2 item2 item2"
 	 "item3 item3 item3 item3"
 	 "item4 item4 item4 item4 item4"
 	 "item5 item5 item5 item5 item5 item5"
 	 "item6 item6 item6 item6 item6"
 	 "item7 item7 item7 item7"
 	 "item8 item8 item8"
 	 "item9 item9"))
      (let ((entry (combo-entry combo)))
	(setf (editable-text entry) "hello world")
	(editable-select-region entry 0)))

    (flet ((create-check-button (label slot)
	     (let ((button
		    (make-instance 'check-button
	             :label label :active t
		     :parent (list main-box :expand nil))))
	       (signal-connect button 'toggled
		#'(lambda ()
		    (setf
		     (slot-value entry slot)
		     (toggle-button-active-p button)))))))
      
      (create-check-button "Editable" 'editable)
      (create-check-button "Visible" 'visible)
      (create-check-button "Sensitive" 'sensitive))))



;; File selecetion dialog

(let ((filesel nil))
  (defun create-file-selection ()
    (unless filesel
      (setq filesel (file-selection-new "file selection dialog"))
      (file-selection-hide-fileop-buttons filesel)
      (setf (window-position filesel) :mouse)
      (signal-connect
       filesel 'destroy #'(lambda () (widget-destroyed filesel)))
      (signal-connect
       (file-selection-ok-button filesel) 'clicked
       #'(lambda ()
	   (format
	    t "Selected file: ~A~%" (file-selection-filename filesel))
	   (widget-destroy filesel)))
      (signal-connect
       (file-selection-cancel-button filesel) 'clicked
       #'widget-destroy :object filesel)

      (let ((button (button-new "Hide Fileops")))
	(signal-connect
	 button 'clicked
	 #'file-selection-hide-fileop-buttons :object filesel)
	(box-pack-start (file-selection-action-area filesel) button nil nil 0)
	(widget-show button))

      (let ((button (button-new "Show Fileops")))
	(signal-connect
	 button 'clicked
	 #'file-selection-show-fileop-buttons :object filesel)
	(box-pack-start (file-selection-action-area filesel) button nil nil 0)
	(widget-show button)))

    (if (not (widget-visible-p filesel))
	(widget-show-all filesel)
      (widget-destroy filesel))))



;;; Handle box

(defun create-handle-box-toolbar ()
  (let ((toolbar (toolbar-new :horizontal :both)))
    (toolbar-append-item
     toolbar "Horizontal" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Horizontal toolbar layout"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

    (toolbar-append-item
     toolbar "Vertical" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Vertical toolbar layout"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

    (toolbar-append-space toolbar)
    
    (toolbar-append-item
     toolbar "Icons" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Only show toolbar icons"
     :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
    (toolbar-append-item
     toolbar "Text" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Only show toolbar text"
     :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
    (toolbar-append-item
     toolbar "Both" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Show toolbar icons and text"
     :callback #'(lambda () (setf (toolbar-style toolbar) :both)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Small" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Use small spaces"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
    (toolbar-append-item
     toolbar "Big" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Use big spaces"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Enable" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Enable tooltips"
     :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

    (toolbar-append-item
     toolbar "Disable" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Disable tooltips"
     :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Borders" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Show borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
    (toolbar-append-item
     toolbar "Borderless" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Hide borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :none)))

    toolbar))


(defun handle-box-child-signal (handle-box child action)
  (format t "~S: child ~S ~A~%" handle-box child action))


(define-test-window create-handle-box "Handle Box Test"
  (setf (window-allow-grow-p window) t)
  (setf (window-allow-shrink-p window) t)
  (setf (window-auto-shrink-p window) nil)
  (setf (container-border-width window) 20)
  (let ((vbox (vbox-new nil 0)))
    (container-add window vbox)

    (container-add vbox (label-new "Above"))
    (container-add vbox (hseparator-new))

    (let ((hbox (hbox-new nil 10)))
      (container-add vbox hbox)
      
      (let ((handle-box (handle-box-new)))
	(box-pack-start hbox handle-box nil nil 0)
	(signal-connect
	 handle-box 'child-attached
	 #'(lambda (child)
	     (handle-box-child-signal handle-box child "attached")))
	(signal-connect
	 handle-box 'child-detached
	 #'(lambda (child)
	     (handle-box-child-signal handle-box child "detached")))
	(container-add handle-box (create-handle-box-toolbar)))

      (let ((handle-box (handle-box-new)))
	(box-pack-start hbox handle-box nil nil 0)
	(signal-connect
	 handle-box 'child-attached
	 #'(lambda (child)
	     (handle-box-child-signal handle-box child "attached")))
	(signal-connect
	 handle-box 'child-detached
	 #'(lambda (child)
	     (handle-box-child-signal handle-box child "detached")))

	(let ((handle-box2 (handle-box-new)))
	  (container-add handle-box handle-box2)
	  (signal-connect
	   handle-box2 'child-attached
	   #'(lambda (child)
	       (handle-box-child-signal handle-box child "attached")))
	  (signal-connect
	   handle-box2 'child-detached
	   #'(lambda (child)
	       (handle-box-child-signal handle-box child "detached")))
	  (container-add handle-box2 (label-new "Foo!")))))
    
    (container-add vbox (hseparator-new))
    (container-add vbox (label-new "Below"))))



;;; Labels
      
(define-test-window create-labels "Labels"
  (setf (container-border-width window) 5)
  (flet ((create-label-in-frame (frame-label label-text &rest args)
	   (list 
	    (make-instance 'frame
	     :label frame-label
	     :child
	     (apply #'make-instance 'label :label label-text args))
	    :fill nil :expand nil)))
    (make-instance 'hbox
     :spacing 5
     :parent window
     :children
     (list
      (list
       (make-instance 'vbox
        :spacing 5
	:children
	(list
	 (create-label-in-frame "Normal Label" "This is a Normal label")
	 (create-label-in-frame "Multi-line Label"
"This is a Multi-line label.
Second line
Third line")
	 (create-label-in-frame "Left Justified Label"
"This is a Left-Justified
Multi-line.
Third line"
          :justify :left)
	 (create-label-in-frame "Right Justified Label"
"This is a Right-Justified
Multi-line.
Third line"
          :justify :right)))
       :fill nil :expand nil)

      (list
       (make-instance 'vbox
        :spacing 5
	:children
	(list      
	 (create-label-in-frame "Line wrapped label"
"This is an example of a line-wrapped label.  It should not be taking up the entire             width allocated to it, but automatically wraps the words to fit.  The time has come, for all good men, to come to the aid of their party.  The sixth sheik's six sheep's sick.
     It supports multiple paragraphs correctly, and  correctly   adds many          extra  spaces. "
          :wrap t)
	 (create-label-in-frame "Filled, wrapped label"
"This is an example of a line-wrapped, filled label.  It should be taking up the entire              width allocated to it.  Here is a seneance to prove my point.  Here is another sentence. Here comes the sun, do de do de do.
    This is a new paragraph.
    This is another newer, longer, better paragraph.  It is coming to an end, unfortunately."
          :justify :fill :wrap t)
	 (create-label-in-frame "Underlined label"
"This label is underlined!
This one is underlined (こんにちは) in quite a funky fashion"
          :justify :left
	  :pattern  "_________________________ _ _________ _ _____ _ __ __  ___ ____ _____")))
       :fill nil :expand nil)))))



;;; Layout

(defun layout-expose-handler (layout event)
  (with-slots (window x-offset y-offset) layout
    (with-slots (x y width height) event
      (let ((imin (truncate (+ x-offset x) 10))
	    (imax (truncate (+ x-offset x width 9) 10))
	    (jmin (truncate (+ y-offset y) 10))
	    (jmax (truncate (+ y-offset y height 9) 10)))
	(declare (fixnum imin imax jmin jmax))
	(gdk:window-clear-area window x y width height)

	(let ((window (layout-bin-window layout))
	      (gc (style-get-gc (widget-style layout) :black)))
	  (do ((i imin (1+ i)))
	      ((= i imax))
	    (declare (fixnum i))
	    (do ((j jmin (1+ j)))
		((= j jmax))
	      (declare (fixnum j))
	      (unless (zerop (mod (+ i j) 2))
		(gdk:draw-rectangle
		 window gc t
		 (- (* 10 i) x-offset) (- (* 10 j) y-offset)
		 (1+ (mod i 10)) (1+ (mod j 10))))))))))
  t)


(define-test-window create-layout "Layout"
  (setf (widget-width window) 200)
  (setf (widget-height window) 200)
  (let ((layout (make-instance 'layout
		 :parent (make-instance 'scrolled-window :parent window)
		 :x-size 1600 :y-size 128000
		 :events '(:exposure))))

    (with-slots (hadjustment vadjustment) layout
      (setf
       (adjustment-step-increment hadjustment) 10.0
       (adjustment-step-increment vadjustment) 10.0))
    (signal-connect layout 'expose-event #'layout-expose-handler :object t)

    (dotimes (i 16)
      (dotimes (j 16)
	(let* ((text (format nil "Button ~D, ~D" i j))
	       (button (if (not (zerop (mod (+ i j) 2)))
			   (button-new text)
			 (label-new text))))
	  (layout-put layout button (* j 100) (* i 100)))))

    (do ((i 16 (1+ i)))
	((= i 1280))
      (declare (fixnum i))
      (let* ((text (format nil "Button ~D, ~D" i 0))
	     (button (if (not (zerop (mod i 2)))
			 (button-new text)
		       (label-new text))))
	(layout-put layout button 0 (* i 100))))))



;;; List    
    
(define-standard-dialog create-list "List"
  (let ((scrolled-window (scrolled-window-new))
        (list (list-new)))
    (setf (container-border-width scrolled-window) 5)
    (setf (scrolled-window-scrollbar-policy scrolled-window) :automatic)
    (box-pack-start main-box scrolled-window t t 0)
    (setf (widget-height scrolled-window) 300)

    (setf (list-selection-mode list) :extended)
    (scrolled-window-add-with-viewport scrolled-window list)
    (setf
     (container-focus-vadjustment list)
     (scrolled-window-vadjustment scrolled-window))
    (setf
     (container-focus-hadjustment list)
     (scrolled-window-hadjustment scrolled-window))
    
    (with-open-file (file "clg:examples;gtktypes.lisp")
      (labels ((read-file ()
	         (let ((line (read-line file nil nil)))
		   (when line
		     (container-add list (list-item-new line))
		     (read-file)))))
	(read-file)))

    (let ((hbox (hbox-new t 5)))
      (setf (container-border-width hbox) 5)
      (box-pack-start main-box hbox nil t 0)

      (let ((button (button-new "Insert Row"))
	    (i 0))
	(box-pack-start hbox button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (let ((item
		    (list-item-new (format nil "added item ~A" (incf i)))))
	       (widget-show item)
	       (container-add list item)))))
	
      (let ((button (button-new "Clear List")))
	(box-pack-start hbox button t t 0)
	(signal-connect
	 button 'clicked #'(lambda () (list-clear-items list 0 -1))))

      (let ((button (button-new "Remove Selection")))
	(box-pack-start hbox button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (let ((selection (list-selection list)))
	       (if (eq (list-selection-mode list) :extended)
		   (let ((item (or
				(container-focus-child list)
				(first selection))))
		     (when item
		       (let* ((children (container-children list))
			      (sel-row
			       (or
				(find-if
				 #'(lambda (item)
				     (eq (widget-state item) :selected))
				 (member item children))
				(find-if
				 #'(lambda (item)
				     (eq (widget-state item) :selected))
				 (member item (reverse children))))))
			 (list-remove-items list selection)
			 (when sel-row
			   (list-select-child list sel-row)))))
		 (list-remove-items list selection)))))
	(box-pack-start hbox button t t 0)))

    (let ((cbox (hbox-new nil 0)))
      (box-pack-start main-box cbox nil t 0)

      (let ((hbox (hbox-new nil 5))
	    (option-menu
	     (create-option-menu
	      `(("Single"
		 ,#'(lambda () (setf (list-selection-mode list) :single)))
		("Browse"
		 ,#'(lambda () (setf (list-selection-mode list) :browse)))
		("Multiple"
		 ,#'(lambda () (setf (list-selection-mode list) :multiple)))
		("Extended"
		 ,#'(lambda () (setf (list-selection-mode list) :extended))))
	      3)))

	(setf (container-border-width hbox) 5)
	(box-pack-start cbox hbox t nil 0)
	(box-pack-start hbox (label-new "Selection Mode :") nil t 0)
	(box-pack-start hbox option-menu nil t 0)))))



;; Menus

(defun create-menu (depth tearoff)
  (unless (zerop depth)
    (let ((menu (menu-new)))
      (when tearoff
	(let ((menuitem (tearoff-menu-item-new)))
	  (menu-shell-append menu menuitem)
	  (widget-show menuitem)
	  ))
      (let ((group nil))
	(dotimes (i 5)
	  (let ((menuitem
		 (radio-menu-item-new
		  (format nil "item ~2D - ~D" depth (1+ i)) group)))
	    (setq group menuitem)
	    (unless (zerop (mod depth 2))
	      (setf (check-menu-item-toggle-indicator-p menuitem) t))
	    (menu-shell-append menu menuitem)
	    (widget-show menuitem)
	    (when (= i 3)
	      (setf (widget-sensitive-p menuitem) nil))
	    (setf (menu-item-submenu menuitem) (create-menu (1- depth) t)))))
      menu)))


(define-standard-dialog create-menus "Menus"
  (setf (box-spacing main-box) 0)
  (setf (container-border-width main-box) 0)
  (widget-show main-box)
  (let ((accel-group (accel-group-new))
	(menubar (menu-bar-new)))
    (accel-group-attach accel-group window)
    (box-pack-start main-box menubar nil t 0)
    (widget-show menubar)

    (let ((menuitem (menu-item-new (format nil "test~%line2"))))
      (setf (menu-item-submenu menuitem) (create-menu 2 t))
      (menu-shell-append menubar menuitem)
      (widget-show menuitem))

    (let ((menuitem (menu-item-new "foo")))
      (setf (menu-item-submenu menuitem) (create-menu 3 t))
      (menu-shell-append menubar menuitem)
      (widget-show menuitem))

    (let ((menuitem (menu-item-new "bar")))
      (setf (menu-item-submenu menuitem) (create-menu 4 t))
      (menu-item-right-justify menuitem)
      (menu-shell-append menubar menuitem)
      (widget-show menuitem))

    (let ((box2 (vbox-new nil 10))
	  (menu (create-menu 1 nil)))
      (setf (container-border-width box2) 10)
      (box-pack-start main-box box2 t t 0)
      (widget-show box2)
      
      (setf (menu-accel-group menu) accel-group)

      (let ((menuitem (check-menu-item-new "Accelerate Me")))
	(menu-shell-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F1" '() '(:visible :signal-visible)))
    
      (let ((menuitem (check-menu-item-new "Accelerator Locked")))
	(menu-shell-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F2" '() '(:visible :locked)))
    
      (let ((menuitem (check-menu-item-new "Accelerator Frozen")))
	(menu-shell-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F2" '() '(:visible))
        (widget-add-accelerator
         menuitem 'activate accel-group "F3" '() '(:visible))
        (widget-lock-accelerators menuitem))
      
      (let ((optionmenu (option-menu-new)))
	(setf (option-menu-menu optionmenu) menu)
	(setf (option-menu-history optionmenu) 3)
	(box-pack-start box2 optionmenu t t 0)
 	(widget-show optionmenu)))))


;;; Notebook

(define-standard-dialog create-notebook "Notebook"
  (multiple-value-bind (book-open book-open-mask)
      (gdk:pixmap-create book-open-xpm)
    (multiple-value-bind (book-closed book-closed-mask)
	(gdk:pixmap-create book-closed-xpm)

      (labels
	  ((create-pages (notebook i end)
	     (when (<= i end)
	       (let* ((title (format nil "Page ~D" i))
		      (child (frame-new title))
		      (vbox (vbox-new t 0))
		      (hbox (hbox-new t 0)))
		 (setf (container-border-width child) 10)
		 (setf (container-border-width vbox) 10)
		 (container-add child vbox)
		 (box-pack-start vbox hbox nil t 5)
		 
		 (let ((button (check-button-new "Fill Tab")))
		   (box-pack-start hbox button t t 5)
		   (setf (toggle-button-active-p button) t)
		   (signal-connect
		    button 'toggled
		    #'(lambda ()
			(multiple-value-bind (expand fill pack-type)
			    (notebook-query-tab-label-packing notebook child)
			  (declare (ignore fill))
			  (notebook-set-tab-label-packing
			   notebook child expand
			   (toggle-button-active-p button) pack-type)))))
		 
		 (let ((button (check-button-new "Expand Tab")))
		   (box-pack-start hbox button t t 5)
		   (signal-connect
		    button 'toggled
		    #'(lambda ()
			(multiple-value-bind (expand fill pack-type)
			    (notebook-query-tab-label-packing notebook child)
			  (declare (ignore expand))
			  (notebook-set-tab-label-packing
			   notebook child (toggle-button-active-p button)
			   fill pack-type)))))
		 
		 (let ((button (check-button-new "Pack end")))
		   (box-pack-start hbox button t t 5)
		   (signal-connect
		    button 'toggled
		    #'(lambda ()
			(multiple-value-bind (expand fill pack-type)
			    (notebook-query-tab-label-packing notebook child)
			  (declare (ignore pack-type))
			  (notebook-set-tab-label-packing
			   notebook child expand fill
			   (if (toggle-button-active-p button)
			       :end
			     :start))))))

		 (let ((button (button-new "Hide Page")))
		   (box-pack-start vbox button nil nil 5)
		   (signal-connect
		    button 'clicked #'(lambda () (widget-hide child))))

		 (widget-show-all child)
		 
		 (let ((label-box (hbox-new nil 0))
		       (menu-box (hbox-new nil 0)))
		   (box-pack-start
		    label-box (pixmap-new book-closed book-closed-mask)
		    nil t 0)
		   (box-pack-start label-box (label-new title) nil t 0)
		   (widget-show-all label-box)
		   (box-pack-start
		    menu-box (pixmap-new book-closed book-closed-mask)
		    nil t 0)
		   (box-pack-start menu-box (label-new title) nil t 0)
		   (widget-show-all menu-box)
		   (notebook-append-page notebook child label-box menu-box)))
	       
	       (create-pages notebook (1+ i) end))))

	
	(setf (container-border-width main-box) 0)
	(setf (box-spacing main-box) 0)
	
	(let ((notebook (notebook-new)))
	  (signal-connect
	   notebook 'switch-page
	   #'(lambda (pointer page)
	       (declare (ignore pointer))
	       (let ((old-page (notebook-page-child notebook)))
		 (unless (eq page old-page)
		   (pixmap-set
		    (first
		     (container-children
		      (notebook-tab-label notebook page)))
		    book-open book-open-mask)
		   (pixmap-set
		    (first
		     (container-children
		      (notebook-menu-label notebook page)))
		    book-open book-open-mask)
		   
		   (when old-page
		     (pixmap-set
		       (first
			(container-children
			 (notebook-tab-label notebook old-page)))
		       book-closed book-closed-mask)
		     (pixmap-set
		      (first
		       (container-children
			(notebook-menu-label notebook old-page)))
		      book-closed book-closed-mask))
		   ))))
	  
	  (setf (notebook-tab-pos notebook) :top)
	  (box-pack-start main-box notebook t t 0)
	  (setf (container-border-width notebook) 10)
	  
	  (widget-realize notebook)
	  (create-pages notebook 1 5)
	
	  (box-pack-start main-box (hseparator-new) nil t 10)
	
	  (let ((box2 (hbox-new nil 5)))
	    (setf (container-border-width box2) 10)
	    (box-pack-start main-box box2 nil t 0)
	  
	    (let ((button (check-button-new "popup menu")))
	      (box-pack-start box2 button t nil 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (if (toggle-button-active-p button)
		       (notebook-popup-enable notebook)
		     (notebook-popup-disable notebook)))))
      
	    (let ((button (check-button-new "homogeneous tabs")))
	      (box-pack-start box2 button t nil 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (setf
		    (notebook-homogeneous-p notebook)
		    (toggle-button-active-p button))))))
	
	  (let ((box2 (hbox-new nil 5)))
	    (setf (container-border-width box2) 10)
	    (box-pack-start main-box box2 nil t 0)
	  
	    (box-pack-start box2 (label-new "Notebook Style : ") nil t 0)
	  
	    (let* ((scrollable-p nil)
		   (option-menu
		    (create-option-menu
		     `(("Standard"
			,#'(lambda ()
			     (setf (notebook-show-tabs-p notebook) t)
			     (when scrollable-p
			       (setq scrollable-p nil)
			       (setf (notebook-scrollable-p notebook) nil)
			       (dotimes (n 10)
				 (notebook-remove-page notebook 5)))))
		       ("No tabs"
		       ,#'(lambda ()
			    (setf (notebook-show-tabs-p notebook) nil)
			    (when scrollable-p
			      (setq scrollable-p nil)
			      (setf (notebook-scrollable-p notebook) nil)
			      (dotimes (n 10)
				(notebook-remove-page notebook 5)))))
		       ("Scrollable"
		       ,#'(lambda ()
			    (unless scrollable-p
			      (setq scrollable-p t)
			      (setf (notebook-show-tabs-p notebook) t)
			      (setf (notebook-scrollable-p notebook) t)
			      (create-pages notebook 6 15)))))
		     0)))
	      (box-pack-start box2 option-menu nil t 0))

	    (let ((button (button-new "Show all Pages")))
	      (box-pack-start box2 button nil t 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (map-container nil #'widget-show notebook)))))

	  (let ((box2 (hbox-new nil 5)))
	    (setf (container-border-width box2) 10)
	    (box-pack-start main-box box2 nil t 0)
	    
	    (let ((button (button-new "prev")))
	      (box-pack-start box2 button t t 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (notebook-prev-page notebook))))
      
	    (let ((button (button-new "next")))
	      (box-pack-start box2 button t t 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (notebook-next-page notebook))))

	    (let ((button (button-new "rotate"))
		  (tab-pos 0))
	      (box-pack-start box2 button t t 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (setq tab-pos (mod (1+ tab-pos) 4))
		   (setf
		    (notebook-tab-pos notebook)
		    (svref #(:top :bottom :right :left) tab-pos)))))))))))



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
  (let* ((frame (make-instance 'frame
		 :label frame-label :border-width 4))
	 (table (make-instance 'table
		 :rows 3 :columns 2 :homogeneous t :parent frame)))

    (table-attach table (label-new label1) 0 1 0 1)
    (let ((check-button (check-button-new "Resize")))
      (table-attach table check-button 0 1 1 2)
      (signal-connect
       check-button 'toggled #'toggle-resize :object (paned-child1 paned)))
    (let ((check-button (check-button-new "Shrink")))
      (table-attach table check-button 0 1 2 3)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-shrink :object (paned-child1 paned)))

    (table-attach table (label-new label2) 1 2 0 1)
    (let ((check-button (check-button-new "Resize")))
      (table-attach table check-button 1 2 1 2)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-resize :object (paned-child2 paned)))
    (let ((check-button (check-button-new "Shrink")))
      (table-attach table check-button 1 2 2 3)
      (setf (toggle-button-active-p check-button) t)
      (signal-connect
       check-button 'toggled #'toggle-shrink :object (paned-child2 paned)))

    frame))

(define-test-window create-panes "Panes"
  (let* ((hpaned (make-instance 'hpaned
		  :child1 (make-instance 'frame
			   :shadow-type :in :width 60 :height 60
			   :child (button-new "Hi there"))
		  :child2 (make-instance 'frame
			   :shadow-type :in :width 80 :height 60)))
	 (vpaned (make-instance 'vpaned
		  :border-width 5
		  :child1 hpaned
		  :child2 (make-instance 'frame
			   :shadow-type :in :width 80 :height 60))))
    
    (make-instance 'vbox
     :parent window
     :children
     (list
      vpaned
      (list
       (create-pane-options hpaned "Horizontal" "Left" "Right") :expand nil)
      (list
       (create-pane-options vpaned "Vertical" "Top" "Bottom") :expand nil)))))
  


;;; Pixmap

(define-standard-dialog create-pixmap "Pixmap"
  (setf (container-border-width main-box) 10)
  (make-instance 'button
   :parent main-box
   :child (make-instance 'hbox
           :border-width 2
	   :children
	   (list
	    (pixmap-new "clg:examples;test.xpm")
	    (label-new "Pixmap test")))))



;;; Progress bar

     


;;; Radio buttons

(define-standard-dialog create-radio-buttons "Radio buttons"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)

  (map nil
   #'(lambda (button)
       (box-pack-start main-box button t t 0))
   (create-radio-button-group '("button1" "button2" "button3") 1)))


;;; Rangle controls

(define-standard-dialog create-range-controls "Range controls"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (let ((adjustment (adjustment-new 0.0 0.0 101.0 0.1 1.0 1.0)))
    (make-instance 'hscale
     :width 150 :height 30 :adjustment adjustment
     :update-policy :delayed :digits 1 :draw-value t :parent main-box)
    (make-instance 'hscrollbar
     :adjustment adjustment :update-policy :continuous :parent main-box)))



;;; Reparent test

(define-standard-dialog create-reparent "reparent"
  (let ((box2 (hbox-new nil 5))
	(label (label-new "Hellow World")))
    (setf (container-border-width box2) 10)
    (box-pack-start main-box box2 t t 0)

    (let ((frame (frame-new "Frame 1"))
	  (box3 (vbox-new nil 5))
	  (button (button-new "switch")))
      (box-pack-start box2 frame t t 0)
      
      (setf (container-border-width box3) 5)
      (container-add frame box3)
      
      (signal-connect
       button 'clicked
       #'(lambda ()
	   (widget-reparent label box3)))
      (box-pack-start box3 button nil t 0)
      
      (box-pack-start box3 label nil t 0)
      (signal-connect
       label 'parent-set
       #'(lambda (old-parent)
	   (declare (ignore old-parent)))))
    
    (let ((frame (frame-new "Frame 2"))
	  (box3 (vbox-new nil 5))
	  (button (button-new "switch")))
      (box-pack-start box2 frame t t 0)
	
      (setf (container-border-width box3) 5)
      (container-add frame box3)
      
      (signal-connect
       button 'clicked
       #'(lambda ()
	   (widget-reparent label box3)))
      (box-pack-start box3 button nil t 0))))



;;; Rulers

(define-test-window create-rulers "rulers"
  (setf (widget-width window) 300)
  (setf (widget-height window) 300)
  (setf (widget-events window) '(:pointer-motion :pointer-motion-hint))

  (let ((table (make-instance 'table
	        :rows 2 :columns 2
		:parent window)))

    (let ((ruler (make-instance 'hruler
		  :metric :centimeters
		  :lower 100.0 :upper 0.0
		  :position 0.0 :max-size 20.0)))
      (signal-connect
       window 'motion-notify-event
       #'(lambda (event) (widget-event ruler event)))
      (table-attach table ruler 1 2 0 1 :y-options '(:fill)))

    (let ((ruler (make-instance 'vruler
		  :lower 5.0 :upper 15.0
		  :position 0.0 :max-size 20.0)))
      (signal-connect
       window 'motion-notify-event
       #'(lambda (event) (widget-event ruler event)))
      (table-attach table ruler 0 1 1 2 :x-options '(:fill)))))



;;; Scrolled window

(define-standard-dialog create-scrolled-windows "Scrolled windows"
  (let* ((scrolled-window
	  (make-instance 'scrolled-window
	   :parent main-box
	   :border-width 10
	   :vscrollbar-policy :automatic
	   :hscrollbar-policy :automatic))
	 (table
	  (make-instance 'table
	   :rows 20 :columns 20 :row-spacing 10 :column-spacing 10
	   :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
	   :focus-hadjustment (scrolled-window-hadjustment scrolled-window))))

      (scrolled-window-add-with-viewport scrolled-window table)
      (dotimes (i 20)
	(dotimes (j 20)
	  (let ((button
		 (toggle-button-new (format nil "button (~D,~D)~%" i j))))
	    (table-attach table button i (1+ i) j (1+ j))))))
  
;   (let ((button (button-new "remove")))
;     (signal-connect button 'clicked #'(lambda ()))
;     (setf (widget-can-default-p button) t)
;     (box-pack-start action-area button t t 0)
;     (widget-grab-default button))

  (setf (window-default-height window) 300)
  (setf (window-default-width window) 300))



;;; Shapes

(defun shape-create-icon (xpm-file x y px py type root-window destroy)
  (let* ((window
	  (make-instance 'window
	   :type type :x x :y y
	   :events '(:button-motion :pointer-motion-hint :button-press)))
	 (fixed
	  (make-instance 'fixed
	   :parent window :width 100 :height 100)))
      
    (widget-realize window)
    (multiple-value-bind (source mask) (gdk:pixmap-create xpm-file)
      (let ((pixmap (pixmap-new source mask))
	    (x-offset 0)
	    (y-offset 0))
	(declare (fixnum x-offset y-offset))
	(fixed-put fixed pixmap px py)
	(widget-shape-combine-mask window mask px py)
	
	(signal-connect window 'button-press-event
	 #'(lambda (event)
	     (when (typep event 'gdk:button-press-event)
	       (setq x-offset (truncate (gdk:event-x event)))
	       (setq y-offset (truncate (gdk:event-y event)))
	       (grab-add window)
	       (gdk:pointer-grab
		(widget-window window) t
		'(:button-release :button-motion :pointer-motion-hint)
		nil nil 0))
	     t))

	(signal-connect window 'button-release-event
	 #'(lambda (event)
	     (declare (ignore event))
	     (grab-remove window)
	     (gdk:pointer-ungrab 0)
	     t))
	
	(signal-connect window 'motion-notify-event
	 #'(lambda (event)
	     (declare (ignore event))
	     (multiple-value-bind (win xp yp mask)
		 (gdk:window-get-pointer root-window)
	       (declare (ignore mask win) (fixnum xp yp))
	       (widget-set-uposition
		window :x (- xp x-offset) :y (- yp y-offset)))
	     t))
	(signal-connect window 'destroy destroy)))
    
    (widget-show-all window)
    window))


(let ((modeller nil)
      (sheets nil)
      (rings nil))
  (defun create-shapes ()
    (let ((root-window (gdk:get-root-window)))
      (if (not modeller)
	  (setq
	   modeller
	   (shape-create-icon
	    "clg:examples;Modeller.xpm" 440 140 0 0 :popup root-window
	    #'(lambda () (widget-destroyed modeller))))
	(widget-destroy modeller))

      (if (not sheets)
	  (setq
	   sheets
	   (shape-create-icon
	    "clg:examples;FilesQueue.xpm" 580 170 0 0 :popup root-window
	    #'(lambda () (widget-destroyed sheets))))
	(widget-destroy sheets))

      (if (not rings)
	  (setq
	   rings
	   (shape-create-icon
	    "clg:examples;3DRings.xpm" 460 270 25 25 :toplevel root-window
	    #'(lambda () (widget-destroyed rings))))
	(widget-destroy rings)))))



;;; Spin buttons

(define-test-window create-spins "Spin buttons"
  (let ((main-vbox (vbox-new nil 5)))
    (setf (container-border-width main-vbox) 10)
    (container-add window main-vbox)

    (let ((frame (frame-new "Not accelerated"))
	  (vbox (vbox-new nil 0))
	  (hbox (hbox-new nil 0)))
      (box-pack-start main-vbox frame t t 0)
      (setf (container-border-width vbox) 5)
      (container-add frame vbox)
      (box-pack-start vbox hbox t t 5)

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Day :"))
	     (spinner (spin-button-new
		       (adjustment-new 1.0 1.0 31.0 1.0 5.0 0.0) 0.0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0.0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :out)
	(box-pack-start vbox2 spinner nil t 0))
    
      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Month :"))
	     (spinner (spin-button-new
		       (adjustment-new 1.0 1.0 12.0 1.0 5.0 0.0) 0.0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0.0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :etched-in)
	(box-pack-start vbox2 spinner nil t 0))

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Year :"))
	     (spinner (spin-button-new
		       (adjustment-new 1998.0 0.0 2100.0 1.0 100.0 0.0)
		       0.0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0.0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :in)
	(box-pack-start vbox2 spinner nil t 0)))

    (let* ((frame (frame-new "Accelerated"))
	   (vbox (vbox-new nil 0))
	   (hbox (hbox-new nil 0))
	   (spinner1 (spin-button-new
		      (adjustment-new 0.0 -10000.0 10000.0 0.5 100.0 0.0)
		      1.0 2))
	   (adj (adjustment-new 2.0 1.0 5.0 1.0 1.0 0.0))
	   (spinner2 (spin-button-new adj 1.0 0)))
	  
      (box-pack-start main-vbox frame t t 0)
      (setf (container-border-width vbox) 5)
      (container-add frame vbox)
      (box-pack-start vbox hbox nil t 5)

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Value :")))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0.0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner1) t)
	(setf (widget-width spinner1) 100)
	(setf (widget-height spinner1) 0)
	(box-pack-start vbox2 spinner1 nil t 0))

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Digits :")))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0.0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner2) t)
	(signal-connect adj 'value-changed
			#'(lambda ()
			    (setf
			     (spin-button-digits spinner1)
			     (floor (spin-button-value spinner2)))))
	(box-pack-start vbox2 spinner2 nil t 0))

      (let ((button (check-button-new "Snap to 0.5-ticks")))
	(signal-connect button 'clicked
			#'(lambda ()
			    (setf
			     (spin-button-snap-to-ticks-p spinner1)
			     (toggle-button-active-p button))))
	(box-pack-start vbox button t t 0)
	(setf (toggle-button-active-p button) t))

      (let ((button (check-button-new "Numeric only input mode")))
	(signal-connect button 'clicked
			#'(lambda ()
			    (setf
			     (spin-button-numeric-p spinner1)
			     (toggle-button-active-p button))))
	(box-pack-start vbox button t t 0)
	(setf (toggle-button-active-p button) t))

      (let ((val-label (label-new "0"))
	    (hbox (hbox-new nil 0)))
	(box-pack-start vbox hbox nil t 5)
	(let ((button (button-new "Value as Int")))
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (setf
		(label-label val-label)
		(format nil "~D" (spin-button-value-as-int spinner1)))))
	  (box-pack-start hbox button t t 5))
	
	(let ((button (button-new "Value as Float")))
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (setf
		(label-label val-label)
		(format nil
			(format nil "~~,~DF" (spin-button-digits spinner1))
			(spin-button-value spinner1)))))
	  (box-pack-start hbox button t t 5))

	(box-pack-start vbox val-label t t 0)))
    
    (let ((hbox (hbox-new nil 0))
	  (button (button-new "Close")))
      (signal-connect button 'clicked #'(lambda () (widget-destroy window)))
      (box-pack-start main-vbox hbox nil t 0)
      (box-pack-start hbox button t t 5))))



;;; Statusbar

(define-test-window create-statusbar "Statusbar"
  (let ((statusbar (make-instance 'statusbar))
	(statusbar-counter 0)
	(close-button
	 (create-button '("close" :can-default t) #'widget-destroy window)))

    (signal-connect
     statusbar 'text-popped
     #'(lambda (context-id text)
	 (declare (ignore context-id))
	 (format nil "Popped: ~A~%" text)))

    (make-instance 'vbox
     :parent window
     :children
     (list
      (make-instance 'vbox
       :border-width 10 :spacing 10
       :children
       (list
	(create-button
	 "push something"
	 #'(lambda ()
	     (statusbar-push
	      statusbar 1
	      (format nil "something ~D" (incf statusbar-counter)))))
	(create-button "pop" #'statusbar-pop statusbar 1)
	(create-button "steal #4" #'statusbar-remove statusbar 1 4)
	(create-button "dump stack")
	(create-button "test contexts")))
      (list (make-instance 'hseparator) :expand nil)
      (list
       (make-instance 'vbox
        :border-width 10
	:children (list (list close-button :expand nil)))
       :expand nil)
       statusbar))
    
    (widget-grab-default close-button)))



;;; Idle test

(define-standard-dialog create-idle-test "Idle Test"
  (let* ((container (make-instance 'hbox :parent main-box))
	 (label (make-instance 'label
		 :label "count: 0" :xpad 10 :ypad 10 :parent container))
	 (idle nil)
	 (count 0))
    (declare (fixnum count))
    (signal-connect
     window 'destroy #'(lambda () (when idle (idle-remove idle))))
 
    (make-instance 'frame
     :label "Label Container" :border-width 5 :parent main-box
     :child
     (make-instance 'vbox
      :children
      (create-radio-button-group
       '(("Resize-Parent" :parent)
	 ("Resize-Queue" :queue)
	 ("Resize-Immediate" :immediate))
       0
       '(setf container-resize-mode) container)))

    (make-instance 'button
     :label "start" :can-default t :parent action-area
     :signals
     (list
      (list
       'clicked
       #'(lambda ()
	   (unless idle
	     (setq
	      idle
	      (idle-add
	       #'(lambda ()
		   (incf count)
		   (setf (label-label label) (format nil "count: ~D" count))
		   t))))))))
      
    (make-instance 'button
     :label "stop" :can-default t :parent action-area
     :signals
     (list
      (list
       'clicked
       #'(lambda ()
	   (when idle
	     (idle-remove idle)
	     (setq idle nil))))))))
    


;;; Timeout test

(define-standard-dialog create-timeout-test "Timeout Test"
  (let ((label (make-instance 'label
		:label "count: 0" :xpad 10 :ypad 10 :parent main-box))
	(timer nil)
	(count 0))
    (declare (fixnum count))
    (signal-connect
     window 'destroy #'(lambda () (when timer (timeout-remove timer))))
          
    (make-instance 'button
     :label "start" :can-default t :parent action-area
     :signals
     (list
      (list
       'clicked
       #'(lambda ()
	   (unless timer
	     (setq
	      timer
	      (timeout-add
	       100
	       #'(lambda ()
		   (incf count)
		   (setf (label-label label) (format nil "count: ~D" count))
		   t))))))))

    (make-instance 'button
     :label "stop" :can-default t :parent action-area
     :signals
     (list
      (list
       'clicked
       #'(lambda ()
	   (when timer
	     (timeout-remove timer)
	     (setq timer nil))))))))
  

;;; Toggle buttons

(define-standard-dialog create-toggle-buttons "Toggle Button"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (dotimes (n 3)
    (make-instance 'toggle-button
     :label (format nil "Button~D" (1+ n)) :parent main-box)))



;;; Toolbar test

(define-test-window create-toolbar "Toolbar test"
  (setf (window-allow-grow-p window) nil)
  (setf (window-allow-shrink-p window) t)
  (setf (window-auto-shrink-p window) t)
  (widget-realize window)

  (let ((toolbar (toolbar-new :horizontal :both)))
    (setf (toolbar-relief toolbar) :none)

    (toolbar-append-item
     toolbar "Horizontal" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Horizontal toolbar layout"
     :tooltip-private-text "Toolbar/Horizontal"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

    (toolbar-append-item
     toolbar "Vertical" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Vertical toolbar layout"
     :tooltip-private-text "Toolbar/Vertical"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

    (toolbar-append-space toolbar)
    
    (toolbar-append-item
     toolbar "Icons" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Only show toolbar icons"
     :tooltip-private-text "Toolbar/IconsOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
    (toolbar-append-item
     toolbar "Text" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Only show toolbar text"
     :tooltip-private-text "Toolbar/TextOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
    (toolbar-append-item
     toolbar "Both" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Show toolbar icons and text"
     :tooltip-private-text "Toolbar/Both"
     :callback #'(lambda () (setf (toolbar-style toolbar) :both)))

    (toolbar-append-space toolbar)

    (toolbar-append-widget
     toolbar (entry-new)
     :tooltip-text "This is an unusable GtkEntry ;)"
     :tooltip-private-text "Hey don't click me!")

    (toolbar-append-space toolbar)
    
    (toolbar-append-item
     toolbar "Small" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Use small spaces"
     :tooltip-private-text "Toolbar/Small"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
    (toolbar-append-item
     toolbar "Big" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Use big spaces"
     :tooltip-private-text "Toolbar/Big"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Enable" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Enable tooltips"
     :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

    (toolbar-append-item
     toolbar "Disable" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Disable tooltips"
     :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Borders" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Show borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
    (toolbar-append-item
     toolbar
     "Borderless" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Hide borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :none)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Empty" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Empty spaces"
     :callback #'(lambda () (setf (toolbar-space-style toolbar) :empty)))

    (toolbar-append-item
     toolbar "Lines" (pixmap-new "clg:examples;test.xpm")
     :tooltip-text "Lines in spaces"
     :callback #'(lambda () (setf (toolbar-space-style toolbar) :line)))
    
    (container-add window toolbar)))



;;; Tooltips test

(define-standard-dialog create-tooltips "Tooltips"
  (setf
   (window-allow-grow-p window) t
   (window-allow-shrink-p window) nil
   (window-auto-shrink-p window) t
   (widget-width window) 200
   (container-border-width main-box) 10
   (box-spacing main-box) 10)

  (let ((tooltips (tooltips-new)))
    (flet ((create-button (label tip-text tip-private)
	     (let ((button (make-instance 'toggle-button
		    :label label :parent main-box)))
	       (tooltips-set-tip tooltips button tip-text tip-private)
	       button)))
      (create-button "button1" "This is button 1" "ContextHelp/button/1")
      (create-button "button2" "This is button 2. This is also a really long tooltip which probably won't fit on a single line and will therefore need to be wrapped. Hopefully the wrapping will work correctly." "ContextHelp/button/2")

      (let* ((toggle (create-button "Override TipSQuery Label"
				    "Toggle TipsQuery view" "Hi msw! ;)"))
	     (box (make-instance 'vbox
		   :homogeneous nil :spacing 5 :border-width 5
		   :parent (make-instance 'frame
	                    :label "ToolTips Inspector"
			    :label-xalign 0.5 :border-width 0
			    :parent main-box)))
	     (button (make-instance 'button :label "[?]" :parent box))
	     (tips-query (make-instance 'tips-query
			  :caller button :parent box)))

	(signal-connect
	 button 'clicked #'tips-query-start-query :object tips-query)
	
	(signal-connect
	 tips-query 'widget-entered
	 #'(lambda (widget tip-text tip-private)
	     (declare (ignore widget tip-private))
	     (when (toggle-button-active-p toggle)
	       (setf
		(label-label tips-query)
		(if tip-text
		    "There is a Tip!"
		  "There is no Tip!"))
	       (signal-emit-stop tips-query 'widget-entered))))
	
	(signal-connect
	 tips-query 'widget-selected
	 #'(lambda (widget tip-text tip-private event)
	     (declare (ignore tip-text event))
	     (when widget
	       (format
		t "Help ~S requested for ~S~%"
		(or tip-private "None") (type-of widget)))
	     t))

	(tooltips-set-tip
	 tooltips button "Start the Tooltip Inspector" "ContextHelp/buttons/?")
	(tooltips-set-tip
	 tooltips close-button "Push this button to close window"
	 "ContextHelp/buttons/Close")))))
		  


;;; Main window
      
(defun create-main-window ()
  (rc-parse "clg:examples;testgtkrc2")
  (rc-parse "clg:examples;testgtkrc")

  (let* ((button-specs
	  '(("button box" create-button-box)
	    ("buttons" create-buttons)
	    ("calendar" create-calendar)
	    ("check buttons" create-check-buttons)
	    ("clist" #|create-clist|#)
	    ("color selection" create-color-selection)
	    ("ctree" #|create-ctree|#)
	    ("cursors" #|create-cursors|#)
	    ("dialog" create-dialog)
;  	    ("dnd")
	    ("entry" create-entry)
	    ("event watcher")
	    ("file selection" create-file-selection)
	    ("font selection")
	    ("gamma curve")
	    ("handle box" create-handle-box)
	    ("item factory")
	    ("labels" create-labels)
	    ("layout" create-layout)
	    ("list" create-list)
	    ("menus" create-menus)
	    ("modal window")
	    ("notebook" create-notebook)
	    ("panes" create-panes)
	    ("pixmap" create-pixmap)
	    ("preview color")
	    ("preview gray")
	    ("progress bar" #|create-progress-bar|#)
	    ("radio buttons" create-radio-buttons)
	    ("range controls" create-range-controls)
	    ("rc file")
	    ("reparent" create-reparent)
	    ("rulers" create-rulers)
	    ("saved position")
	    ("scrolled windows" create-scrolled-windows)
	    ("shapes" create-shapes)
	    ("spinbutton" create-spins)
	    ("statusbar" create-statusbar)
	    ("test idle" create-idle-test)
	    ("test mainloop")
	    ("test scrolling")
	    ("test selection")
	    ("test timeout" create-timeout-test)
	    ("text" #|create-text|#)
	    ("toggle buttons" create-toggle-buttons)
	    ("toolbar" create-toolbar)
	    ("tooltips" create-tooltips)
	    ("tree" #|create-tree|#)
	    ("WM hints")))
	 (main-window (make-instance 'window
		        :type :toplevel :title "testgtk.lisp"
			:name "main window" :x 20 :y 20 :width 200 :height 400
			:allow-grow nil :allow-shrink nil :auto-shrink nil))
	 (scrolled-window (make-instance 'scrolled-window
			   :hscrollbar-policy :automatic
			   :vscrollbar-policy :automatic
			   :border-width 10))
	 (close-button (create-button
			'("close" :can-default t)
			#'widget-destroy main-window)))

    ;; Main box
    (make-instance 'vbox
     :parent main-window
     :children
     (list 
      (list
       (make-instance 'label :label (gtk-version)) :expand nil :fill nil)
      (list
       (make-instance 'label :label "clg CVS version") :expand nil :fill nil)
      scrolled-window
      (list (make-instance 'hseparator) :expand nil)
      (list
       (make-instance 'vbox
	:homogeneous nil :spacing 10 :border-width 10
	:children (list close-button))
       :expand nil)))

    (scrolled-window-add-with-viewport
     scrolled-window
     (make-instance 'vbox
      :border-width 10
      :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
      :children
      (mapcar
       #'(lambda (spec)
	   (apply #'create-button spec))
       button-specs)))
    
    (widget-grab-default close-button)
    (widget-show-all main-window)
    main-window))
 

;(create-main-window)

