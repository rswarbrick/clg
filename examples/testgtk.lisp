;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: testgtk.lisp,v 1.16 2005-01-12 14:03:04 espen Exp $


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
    ,@body
    (dialog-add-button ,dialog "gtk-close" #'widget-destroy :object t)))



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
	   :child (make-instance 'button :stock "gtk-ok")
	   :child (make-instance 'button :stock "gtk-cancel")
	   :child (make-instance 'button :stock "gtk-help"))))

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
	  (table-attach table button column (1+ column) row (1+ row)
			:options '(:expand :fill)))))
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

(defun set-cursor (spinner drawing-area label)
  (let ((cursor
	 (glib:int-enum
	  (logand (clamp (spin-button-value-as-int spinner) 0 152) #xFE)
	  'gdk:cursor-type)))
    (setf (label-label label) (string-downcase cursor))
    (setf (widget-cursor drawing-area) cursor)))

(defun cursor-expose (drawing-area event)
  (declare (ignore event))
  (multiple-value-bind (width height)
      (widget-get-size-allocation drawing-area)
    (let* ((window (widget-window drawing-area))
	   (style (widget-style drawing-area))
	   (white-gc (style-white-gc style))
	   (gray-gc (style-bg-gc style :normal))
	   (black-gc (style-black-gc style)))
      (gdk:draw-rectangle window white-gc t 0 0 width (floor height 2))
      (gdk:draw-rectangle window black-gc t 0 (floor height 2) width 
			  (floor height 2))
      (gdk:draw-rectangle window gray-gc t (floor width 3) 
			  (floor height 3) (floor width 3) 
			  (floor height 3))))
  t)

(define-simple-dialog create-cursors (dialog "Cursors")
  (let ((spinner (make-instance 'spin-button 
		  :adjustment (adjustment-new 
			       0 0 
			       (1- (enum-int :last-cursor 'gdk:cursor-type))
			       2 10 0)))
	(drawing-area (make-instance 'drawing-area
		       :width-request 80 :height-request 80
		       :events '(:exposure-mask :button-press-mask)))
	(label (make-instance 'label :label "XXX")))

    (signal-connect drawing-area 'expose-event #'cursor-expose :object t)

    (signal-connect drawing-area 'button-press-event
     #'(lambda (event)
	 (case (gdk:event-button event)
	   (1 (spin-button-spin spinner :step-forward 0.0))
	   (3 (spin-button-spin spinner :step-backward 0.0)))
	 t))

    (signal-connect drawing-area 'scroll-event
     #'(lambda (event)
	 (case (gdk:event-direction event)
	   (:up (spin-button-spin spinner :step-forward 0.0))
	   (:down (spin-button-spin spinner :step-backward 0.0)))
	 t))

    (signal-connect spinner 'changed
     #'(lambda ()
	 (set-cursor spinner drawing-area label)))

    (make-instance 'v-box
     :parent dialog :border-width 10 :spacing 5 :show-all t
     :child (list
	     (make-instance 'h-box
	      :border-width 5
	      :child (list
		      (make-instance 'label :label "Cursor Value : ")
		      :expand nil)
	      :child spinner)
	     :expand nil)
     :child (make-instance 'frame
;	     :shadow-type :etched-in
	     :label "Cursor Area" :label-xalign 0.5 :border-width 10
	     :child drawing-area)
     :child (list label :expand nil))

    (widget-realize drawing-area)
    (set-cursor spinner drawing-area label)))


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


;; Expander

(define-simple-dialog create-expander (dialog "Expander" :resizable nil)
  (make-instance 'v-box
   :parent dialog :spacing 5 :border-width 5 :show-all t
   :child (create-label "Expander demo. Click on the triangle for details.")
   :child (make-instance 'expander
	   :label "Details"
	   :child (create-label "Details can be shown or hidden."))))


;; File chooser dialog

(define-dialog create-file-chooser (dialog "File Chooser" 'file-chooser-dialog)
  (dialog-add-button dialog "gtk-cancel" #'widget-destroy :object t)
  (dialog-add-button dialog "gtk-ok" 
   #'(lambda ()
       (if (slot-boundp dialog 'filename)	   
	   (format t "Selected file: ~A~%" (file-chooser-filename dialog))
	 (write-line "No files selected"))
       (widget-destroy dialog))))



;;; Handle box

(define-toplevel create-handle-box (window "Handle Box Test" :border-width 20)
  (make-instance 'v-box 
   :parent window
   :child (create-label "Above")
   :child (make-instance 'h-separator)
   :child (make-instance 'h-box 
	   :spacing 10
	   :child (list
		   (make-instance 'handle-box
		    :child (create-toolbar window)
		    :signal (list 'child-attached
			     #'(lambda (child)
				 (format t "~A attached~%" child)))
		    :signal (list 'child-detached
			     #'(lambda (child)
				 (format t "~A detached~%" child))))
		   :expand nil :fill :nil))
   :child (make-instance 'h-separator)
   :child (create-label "Below")))

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

(defun layout-expose (layout event)
  (when (eq (gdk:event-window event) (layout-bin-window layout))
    (with-slots (gdk:x gdk:y gdk:width gdk:height) event
      (let ((imin (truncate gdk:x 10))
	    (imax (truncate (+ gdk:x gdk:width 9) 10))
	    (jmin (truncate gdk:y 10))
	    (jmax (truncate (+ gdk:y gdk:height 9) 10)))

	(let ((window (layout-bin-window layout))
	      (gc (style-black-gc (widget-style layout))))
	  (loop
	   for i from imin below imax
	   do (loop 
	       for j from jmin below jmax
	       unless (zerop (mod (+ i j) 2))
	       do (gdk:draw-rectangle
		   window gc t (* 10 i) (* 10 j) 
		   (1+ (mod i 10)) (1+ (mod j 10)))))))))
  nil)

(define-toplevel create-layout (window "Layout" :default-width 200
				                :default-height 200)
  (let ((layout (make-instance 'layout
		 :parent (make-instance 'scrolled-window :parent window)
		 :width 1600 :height 128000 :events '(:exposure-mask)
 		 :signal (list 'expose-event #'layout-expose :object t)
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
    
(define-simple-dialog create-list (dialog "List" :default-height 400)
  (let* ((store (make-instance 'list-store 
		 :column-types '(string int boolean)
		 :column-names '(:foo :bar :baz)
		 :initial-content '(#("First" 12321 nil)
				    (:foo "Yeah" :baz t))))
	 (tree (make-instance 'tree-view :model store)))

    (loop
     with iter = (make-instance 'tree-iter)
     for i from 1 to 1000
     do (list-store-append store (vector "Test" i (zerop (mod i 3))) iter))
    
    (let ((column (make-instance 'tree-view-column :title "Column 1"))
	  (cell (make-instance 'cell-renderer-text)))
      (cell-layout-pack column cell :expand t)
      (cell-layout-add-attribute column cell 'text (column-index store :foo))
      (tree-view-append-column tree column))
    
    (let ((column (make-instance 'tree-view-column :title "Column 2"))
	  (cell (make-instance 'cell-renderer-text :background "orange")))
      (cell-layout-pack column cell :expand t)
      (cell-layout-add-attribute column cell 'text (column-index store :bar))
      (tree-view-append-column tree column))      
    
    (let ((column (make-instance 'tree-view-column :title "Column 3"))
	  (cell (make-instance 'cell-renderer-text)))
      (cell-layout-pack column cell :expand t)
      (cell-layout-add-attribute column cell 'text (column-index store :baz))
      (tree-view-append-column tree column))      

    (make-instance 'v-box
     :parent dialog :border-width 10 :spacing 10 :show-all t
     :child (list
	     (make-instance 'h-box
              :spacing 10
	      :child (make-instance 'button
		      :label "Remove Selection"
		      :signal (list 'clicked
			       #'(lambda ()
				   (let ((references
					  (mapcar
					   #'(lambda (path)
					       (make-instance 'tree-row-reference :model store :path path))					  
					   (tree-selection-get-selected-rows
					    (tree-view-selection tree)))))
				     (mapc
				      #'(lambda (reference)
					  (list-store-remove store reference))
				      references))))))
	     :expand nil)
     :child (list
	     (make-instance 'h-box
              :spacing 10
	      :child (make-instance 'check-button 
		      :label "Show Headers" :active t
		      :signal (list 'toggled
			       #'(lambda (button)
				   (setf
				    (tree-view-headers-visible-p tree)
				    (toggle-button-active-p button)))
			       :object t))
	      :child (make-instance 'check-button 
		      :label "Reorderable" :active nil
		      :signal (list 'toggled
			       #'(lambda (button)
				   (setf
				    (tree-view-reorderable-p tree)
				    (toggle-button-active-p button)))
			       :object t))
	      :child (list 
		      (make-instance 'h-box
                       :child (make-instance 'label :label "Selection Mode: ")
		       :child (make-instance 'combo-box
			       :content '("Single" "Browse" "Multiple") 
			       :active 0
			       :signal (list 'changed
				        #'(lambda (combo-box)
					    (setf 
					     (tree-selection-mode 
					      (tree-view-selection tree))
					     (svref 
					      #(:single :browse :multiple)
					      (combo-box-active combo-box))))
					:object t)))
		      :expand nil))
	     :expand nil)
     :child (make-instance 'scrolled-window 
	    :child tree :hscrollbar-policy :automatic))))


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
 		(add-to-radio-group menu-item group)
 	      (setq group menu-item))
	    (unless (zerop (mod depth 2))
	      (setf (check-menu-item-active-p menu-item) t))
	    (menu-shell-append menu menu-item)
	    (when (= i 3)
	      (setf (widget-sensitive-p menu-item) nil))
	    (let ((submenu (create-menu (1- depth) t)))
	      (when submenu
		(setf (menu-item-submenu menu-item) submenu))))))
      menu)))


(define-simple-dialog create-menus (dialog "Menus" :default-width 200)
  (let* ((main (make-instance 'v-box :parent dialog))
;	 (accel-group (make-instance 'accel-group))
	 (menubar (make-instance 'menu-bar :parent (list main :expand nil))))
;    (window-add-accel-group dialog accel-group)

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

(defun create-notebook-page (notebook page-num book-closed)
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
		      :child (make-instance 'image :pixbuf book-closed)
		      :child (make-instance 'label :label title)))
	  (menu-box (make-instance 'h-box 
		     :show-all t
		     :child-args '(:expand nil)
		     :child (make-instance 'image :pixbuf book-closed)
		     :child (make-instance 'label :label title))))

      (widget-show-all page)
      (notebook-append notebook page label-box menu-box))))
	

(define-simple-dialog create-notebook (dialog "Notebook")
  (let ((main (make-instance 'v-box :parent dialog)))
    (let ((book-open (gdk:pixbuf-new-from-xpm-data book-open-xpm))
	  (book-closed (gdk:pixbuf-new-from-xpm-data book-closed-xpm))
	  (notebook (make-instance 'notebook 
		     :border-width 10 :tab-pos :top :parent main)))
      (flet ((set-image (page func pixbuf)
	       (setf
		(image-pixbuf 
		 (first (container-children (funcall func notebook page))))
		pixbuf)))
	(signal-connect notebook 'switch-page
	 #'(lambda (pointer page)
	     (declare (ignore pointer))
	     (set-image page #'notebook-menu-label book-open)
	     (set-image page #'notebook-tab-label book-open)
	     (when (slot-boundp notebook 'current-page)
	       (let ((curpage (notebook-current-page notebook)))
		 (set-image curpage #'notebook-menu-label book-closed)
		 (set-image curpage #'notebook-tab-label book-closed))))))
      (loop for i from 1 to 5 do (create-notebook-page notebook i book-closed))

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
				    do (create-notebook-page notebook i book-closed))))))
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
  (setf (paned-child-resize-p child) (not (paned-child-resize-p child))))

(defun toggle-shrink (child)
  (setf (paned-child-shrink-p child) (not (paned-child-shrink-p child))))

(defun create-pane-options (paned frame-label label1 label2)
  (let* ((table (make-instance 'table :n-rows 3 :n-columns 2 :homogeneous t)))
    (table-attach table (create-label label1) 0 1 0 1 :options '(:expand :fill))
    (let ((check-button (make-instance 'check-button :label "Resize")))
      (table-attach table check-button 0 1 1 2 :options '(:expand :fill))
      (signal-connect check-button 'toggled 
       #'toggle-resize :object (paned-child1 paned)))
    (let ((check-button (make-instance 'check-button :label "Shrink" :active t)))
      (table-attach table check-button 0 1 2 3 :options '(:expand :fill))
      (signal-connect check-button 'toggled 
       #'toggle-shrink :object (paned-child1 paned)))

    (table-attach table (create-label label2) 1 2 0 1 :options '(:expand :fill))
    (let ((check-button (make-instance 'check-button :label "Resize" :active t)))
      (table-attach table check-button 1 2 1 2 :options '(:expand :fill))
      (signal-connect check-button 'toggled 
       #'toggle-resize :object (paned-child2 paned)))
    (let ((check-button (make-instance 'check-button :label "Shrink" :active t)))
      (table-attach table check-button 1 2 2 3 :options '(:expand :fill))
      (signal-connect check-button 'toggled
       #'toggle-shrink :object (paned-child2 paned)))
    (make-instance 'frame :label frame-label :border-width 4 :child table)))

(define-toplevel create-panes (window "Panes")
  (let* ((hpaned (make-instance 'h-paned
		  :child1 (make-instance 'frame
			   :width-request 60 :height-request 60
			   :shadow-type :in 
			   :child (make-instance 'button :label "Hi there"))
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

(define-simple-dialog create-progress-bar (dialog "Progress Bar")
  (let* ((progress (make-instance 'progress-bar :pulse-step 0.05))
	 (activity-mode-button (make-instance 'check-button 
				:label "Activity mode"))
	 (timer (timeout-add 100
		 #'(lambda ()
		     (if (toggle-button-active-p activity-mode-button)
			 (progress-bar-pulse progress)
		       (let ((fract (+ (progress-bar-fraction progress) 0.01)))
			 (setf		      
			  (progress-bar-fraction progress)
			  (if (> fract 1.0)
			      0.0
			    fract))))
		     t))))

    (make-instance 'v-box
     :parent dialog :border-width 10 :spacing 10 :show-all t
     :child progress
     :child activity-mode-button)

    (signal-connect dialog 'destroy 
     #'(lambda () (when timer (timeout-remove timer))))))


;;; Radio buttons

(define-simple-dialog create-radio-buttons (dialog "Radio buttons")
  (make-instance 'v-box
   :parent dialog :border-width 10 :spacing 10 :show-all t
   :children (make-radio-group 'radio-button
	      '((:label "button1") (:label "button2") (:label "button3"))
	      nil)))


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
	(label (make-instance 'label :label "Hello World")))

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

  (let ((table (make-instance 'table :n-rows 2 :n-columns 2 :parent window))
	(h-ruler (make-instance 'h-ruler
		  :metric :centimeters :lower 100.0d0 :upper 0.0d0
		  :position 0.0d0 :max-size 20.0d0))
	(v-ruler (make-instance 'v-ruler
	          :lower 5.0d0 :upper 15.0d0 
		  :position 0.0d0 :max-size 20.0d0)))
    (signal-connect window 'motion-notify-event
     #'(lambda (event)
	 (widget-event h-ruler event)
	 (widget-event v-ruler event)))
    (table-attach table h-ruler 1 2 0 1 :options :fill :x-options :expand)
    (table-attach table v-ruler 0 1 1 2 :options :fill :y-options :expand)))


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


;;; Size group

(define-simple-dialog create-size-group (dialog "Size Group" :resizable nil)
  (let ((size-group (make-instance 'size-group)))
    (flet ((create-frame (label rows)
	     (let ((table (make-instance 'table 
			   :n-rows (length rows) :n-columns 2 :homogeneous nil
			   :row-spacing 5 :column-spacing 10 :border-width 5)))
	       (loop
		for row in rows
		for i from 0
		do (table-attach table 
		    (create-label (first row) :xalign 0 :yalign 1)
		    0 1 i (1+ i) :x-options '(:expand :fill))
		   (let ((combo (make-instance 'combo-box 
				 :content (rest row) :active 0)))
		     (size-group-add-widget size-group combo)
		     (table-attach table combo 1 2 i (1+ i))))
	       (make-instance 'frame :label label :child table))))

      (make-instance 'v-box
       :parent dialog :border-width 5 :spacing 5 :show-all t
       :child (create-frame "Color Options"
	       '(("Foreground" "Red" "Green" "Blue")
		 ("Background" "Red" "Green" "Blue")))
       :child (create-frame "Line Options"
	       '(("Dashing" "Solid" "Dashed" "Dotted")
		 ("Line ends" "Square" "Round" "Arrow")))
       :child (create-check-button "Enable grouping"
	       #'(lambda (active)
		   (setf 
		    (size-group-mode size-group) 
		    (if active :horizontal :none)))
	       t)))))


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

(define-simple-dialog create-idle-test (dialog "Idle Test")
  (let ((label (make-instance 'label
		:label "count: 0" :xpad 10 :ypad 10))
	(idle nil)
	(count 0))
    (signal-connect dialog 'destroy 
     #'(lambda () (when idle (idle-remove idle))))
 
    (make-instance 'v-box
     :parent dialog :border-width 10 :spacing 10 :show-all t
     :child label
     :child (make-instance 'frame
	     :label "Label Container" :border-width 5
	     :child(make-instance 'v-box
		   :children (make-radio-group 'radio-button
			      '((:label "Resize-Parent" :value :parent :active t)
				(:label "Resize-Queue" :value :queue)
				(:label "Resize-Immediate" :value :immediate))
			      #'(lambda (mode)
 				  (setf 
 				   (container-resize-mode (dialog-action-area dialog)) mode))))))

    (dialog-add-button dialog "Start"
     #'(lambda ()
	 (unless idle
	   (setq idle
	    (idle-add
	     #'(lambda ()
		 (incf count)
		 (setf (label-label label) (format nil "count: ~D" count))
		 t))))))
      
    (dialog-add-button dialog "Stop"
     #'(lambda ()
	 (when idle
	   (idle-remove idle)
	   (setq idle nil))))))
    


;;; Timeout test

(define-simple-dialog create-timeout-test (dialog "Timeout Test")
  (let ((label (make-instance 'label
		:label "count: 0" :xpad 10 :ypad 10 :parent dialog :visible t))
	(timer nil)
	(count 0))
    (signal-connect dialog 'destroy 
     #'(lambda () (when timer (timeout-remove timer))))

    (dialog-add-button dialog "Start"
     #'(lambda ()
	 (unless timer
	   (setq timer
	    (timeout-add 100
	     #'(lambda ()
		 (incf count)
		 (setf (label-label label) (format nil "count: ~D" count))
		 t))))))

    (dialog-add-button dialog "Stop"
     #'(lambda ()
	 (when timer
	   (timeout-remove timer)
	   (setq timer nil))))))


;;; Text

(define-simple-dialog create-text (dialog "Text" :default-width 400
					         :default-height 400)
  (let* ((text-view (make-instance 'text-view 
		     :border-width 10 :visible t :wrap-mode :word))
	 (buffer (text-view-buffer text-view))
	 (active-tags ()))

    (text-buffer-create-tag buffer "Bold" :weight :bold)
    (text-buffer-create-tag buffer "Italic" :style :italic)
    (text-buffer-create-tag buffer "Underline" :underline :single)
    
    (flet ((create-toggle-callback (tag-name)
	     (let ((tag (text-tag-table-lookup 
			 (text-buffer-tag-table buffer) tag-name)))
	       #'(lambda (active)
		   (unless (eq (and (find tag active-tags) t) active)
		     ;; user activated
		     (if active 
			 (push tag active-tags)
		       (setq active-tags (delete tag active-tags)))
 		     (multiple-value-bind (non-zero-p start end)
 			 (text-buffer-get-selection-bounds buffer)
		       (if active 
			   (text-buffer-apply-tag buffer tag start end)
			 (text-buffer-remove-tag buffer tag start end))))))))

      (let* ((actions 
	      (make-instance 'action-group 
	       :action (create-toggle-action 
			"Bold" "gtk-bold" "Bold" "<control>B" "Bold" nil
			(create-toggle-callback "Bold"))
	       :action (create-toggle-action 
			"Italic" "gtk-italic" "Italic" "<control>I" "Italic" nil
			(create-toggle-callback "Italic"))
	       :action (create-toggle-action 
			"Underline" "gtk-underline" "Underline" "<control>U" "Underline" nil
			(create-toggle-callback "Underline"))))
	     (ui (make-instance 'ui-manager)))
      
	(ui-manager-insert-action-group ui actions)
	(ui-manager-add-ui ui 
	 '((:toolbar "ToolBar"
	    (:toolitem "Bold")
	    (:toolitem "Italic")
	    (:toolitem "Underline"))))

	;; Callback to activate/deactivate toolbar buttons when cursor
	;; is moved
	(signal-connect buffer 'mark-set
	 #'(lambda (location mark)
	     (declare (ignore mark))
	     (text-tag-table-foreach (text-buffer-tag-table buffer)
	      #'(lambda (tag)
		  (let ((active
			 (or 
			  (and
			   (text-iter-has-tag-p location tag)
			   (not (text-iter-begins-tag-p location tag)))
			  (text-iter-ends-tag-p location tag))))
		    (unless (eq active (and (find tag active-tags) t))
		      (if active 
			  (push tag active-tags)
			(setq active-tags (delete tag active-tags)))
		      (setf 
		       (toggle-action-active-p
			(action-group-get-action actions (text-tag-name tag)))
		       active)))))))

	;; Callback to apply active tags when a character is inserted
	(signal-connect buffer 'insert-text
         #'(lambda (iter &rest args)
	     (declare (ignore args))
	     (let ((before (text-buffer-get-iter-at-offset buffer 
			    (1- (text-iter-offset iter)))))
	       (loop
		for tag in active-tags
		do (text-buffer-apply-tag buffer tag before iter))))
	 :after t)
	
	(container-add dialog (ui-manager-get-widget ui "/ToolBar") :expand nil)
	(container-add dialog text-view)))))


;;; Toggle buttons

(define-simple-dialog create-toggle-buttons (dialog "Toggle Button")
  (make-instance 'v-box
   :border-width 10 :spacing 10 :parent dialog :show-all t
      :children (loop
	      for n from 1 to 3
	      collect (make-instance 'toggle-button
		       :label (format nil "Button~D" (1+ n))))))



;;; Toolbar test

(defun create-toolbar (window)
  (make-instance 'toolbar 
   :show-tooltips t :show-arrow nil

   ;; Insert a stock item
   :child (make-instance 'tool-button 
	   :stock  "gtk-quit"
	   :tip-text "Destroy toolbar"
	   :tip-private "Toolbar/Quit"
	   :signal (list 'clicked #'(lambda () (widget-destroy window))))

   :child (make-instance 'separator-tool-item)

   :child (make-instance 'tool-button
	   :label "Horizontal" :stock "gtk-go-forward"
	   :tip-text "Horizontal toolbar layout"
	   :tip-private "Toolbar/Horizontal"
	   :signal (list 'clicked 
		    #'(lambda (toolbar) 
			(setf (toolbar-orientation toolbar) :horizontal))
		    :object :parent))

   :child (make-instance 'tool-button
	   :label "Vertical" :stock "gtk-go-down"
	   :tip-text "Vertical toolbar layout"
	   :tip-private "Toolbar/Vertical"
	   :signal (list 'clicked 
		    #'(lambda (toolbar) 
			(setf (toolbar-orientation toolbar) :vertical))
		    :object :parent))

   :child (make-instance 'separator-tool-item)

   :children (make-radio-group 'radio-tool-button
	      '((:label "Icons" :stock "gtk-justify-left"
		 :tip-text "Only show toolbar icons"
	         :tip-private "Toolbar/IconsOnly"
		 :value :icons)
		(:label "Both" :stock "gtk-justify-center"
		 :tip-text "Show toolbar icons and text"
		 :tip-private "Toolbar/Both"
		 :value :both :active t)
		(:label "Text" :stock "gtk-justify-right"
	         :tip-text "Show toolbar text"
	         :tip-private "Toolbar/TextOnly"
		 :value :text))
	      (list
	       #'(lambda (toolbar style) 
		   (setf (toolbar-style toolbar) style))
	       :object :parent))

   :child (make-instance 'separator-tool-item)

   :child (make-instance 'tool-item
	   :child (make-instance 'entry)
	   :tip-text "This is an unusable GtkEntry"
	   :tip-private "Hey don't click me!")

   :child (make-instance 'separator-tool-item)

   :child (make-instance 'tool-button
	   :label "Enable" :stock "gtk-add"
	   :tip-text "Enable tooltips"
	   :tip-private "Toolbar/EnableTooltips"
	   :signal (list 'clicked 
		    #'(lambda (toolbar) 
			(setf (toolbar-show-tooltips-p toolbar) t))
		    :object :parent))

   :child (make-instance 'tool-button
	   :label "Disable" :stock "gtk-remove"
	   :tip-text "Disable tooltips"
	   :tip-private "Toolbar/DisableTooltips"
	   :signal (list 'clicked 
		    #'(lambda (toolbar) 
			(setf (toolbar-show-tooltips-p toolbar) nil))
		    :object :parent))

;;    :child (make-instance 'separator-tool-item)

;;    :child (make-instance 'tool-button
;; 	   :label "GTK" :icon #p"clg:examples;gtk.png"
;; 	   :tip-text "GTK+ Logo"
;; 	   :tip-private "Toolbar/GTK+")
   ))

(define-toplevel create-toolbar-window (window "Toolbar test" :resizable nil)
  (container-add window (create-toolbar window)))



;;; Tooltips test

(define-simple-dialog create-tooltips (dialog "Tooltips" :default-width 200)
  (let ((tooltips (make-instance 'tooltips)))
    (flet ((create-button (label tip-text tip-private)
	     (let ((button (make-instance 'toggle-button :label label)))
	       (tooltips-set-tip tooltips button tip-text tip-private)
	       button)))
      (make-instance 'v-box
       :parent dialog :border-width 10 :spacing 10 :show-all t
       :child (create-button "button1" "This is button 1" "ContextHelp/button/1")
       :child (create-button "button2" "This is button 2. This is also has a really long tooltip which probably won't fit on a single line and will therefore need to be wrapped. Hopefully the wrapping will work correctly." "ContextHelp/button/2")))))


;;; UI Manager

(defvar *ui-description*
  '((:menubar "MenuBar"
     (:menu "FileMenu"
      (:menuitem "New")
      (:menuitem "Open")
      (:menuitem "Save")
      (:menuitem "SaveAs")
      :separator
      (:menuitem "Quit"))
     (:menu "PreferencesMenu"
       (:menu "ColorMenu"
	(:menuitem "Red")
 	(:menuitem "Green")
 	(:menuitem "Blue"))
       (:menu "ShapeMenu"
        (:menuitem "Square")
        (:menuitem "Rectangle")
        (:menuitem "Oval"))
       (:menuitem "Bold"))
     (:menu "HelpMenu"
      (:menuitem "About")))
    (:toolbar "ToolBar"
     (:toolitem "Open")
     (:toolitem "Quit")
     (:separator "Sep1")
     (:toolitem "Logo"))))

(define-toplevel create-ui-manager (window "UI Manager")
  (let ((actions 
	 (make-instance 'action-group 
	  :name "Actions"
	  :action (create-action "FileMenu" nil "_File")
	  :action (create-action "PreferencesMenu" nil "_Preferences")
	  :action (create-action "ColorMenu" nil "_Color")
	  :action (create-action "ShapeMenu" nil "_Shape")
	  :action (create-action "HelpMenu" nil "_Help")
	  :action (create-action "New" "gtk-new" "_New" "<control>N" "Create a new file")
	  :action (create-action "Open" "gtk-open" "_Open" "<control>O" "Open a file" #'create-file-chooser)
	  :action (create-action "Save" "gtk-save" "_Save" "<control>S" "Save current file")
	  :action (create-action "SaveAs" "gtk-save" "Save _As..." "" "Save to a file")
	  :action (create-action "Quit" "gtk-quit" "_Quit" "<control>Q" "Quit" (list #'widget-destroy :object window))
	  :action (create-action "About" nil "_About" "<control>A" "About")
	  :action (create-action "Logo" "demo-gtk-logo" "" nil "GTK+")
	  :action (create-toggle-action "Bold" "gtk-bold" "_Bold" "<control>B" "Bold" t)
	  :actions (create-radio-actions
		    '(("Red" nil "_Red" "<control>R" "Blood")
		      ("Green" nil "_Green" "<control>G" "Grass")
		      ("Blue" nil "_Blue" "<control>B" "Sky"))
		    "Green")
	  :actions (create-radio-actions
		    '(("Square" nil "_Square" "<control>S" "Square")
		      ("Rectangle" nil "_Rectangle" "<control>R" "Rectangle")
		      ("Oval" nil "_Oval" "<control>O" "Egg")))))
	(ui (make-instance 'ui-manager)))
  
    (ui-manager-insert-action-group ui actions)
    (ui-manager-add-ui ui *ui-description*)

    (window-add-accel-group window (ui-manager-accel-group ui))
    
    (make-instance 'v-box 
     :parent window :show-all t
     :child (list 
	     (ui-manager-get-widget ui "/MenuBar")
	     :expand nil :fill nil)
     :child (list 
	     (ui-manager-get-widget ui "/ToolBar")
	     :expand nil :fill nil)
     :child (make-instance 'label
	     :label "Type <alt> to start" 
	     :xalign 0.5 :yalign 0.5
	     :width-request 200 :height-request 200))))
		  


;;; Main window
      
(defun create-main-window ()
;;   (rc-parse "clg:examples;testgtkrc2")
;;   (rc-parse "clg:examples;testgtkrc")

  (let* ((button-specs
	  '(("button box" create-button-box)
 	    ("buttons" create-buttons)
 	    ("calendar" create-calendar)
 	    ("check buttons" create-check-buttons)
 	    ("color selection" create-color-selection)
 	    ("cursors" create-cursors)
 	    ("dialog" create-dialog)
;; ;  	    ("dnd")
 	    ("entry" create-entry)
;; 	    ("event watcher")
 	    ("enxpander" create-expander)
 	    ("file chooser" create-file-chooser)
;; 	    ("font selection")
 	    ("handle box" create-handle-box)
 	    ("image" create-image)
 	    ("labels" create-labels)
 	    ("layout" create-layout)
 	    ("list" create-list)
	    ("menus" create-menus)
;; 	    ("modal window")
 	    ("notebook" create-notebook)
 	    ("panes" create-panes)
 	    ("progress bar" create-progress-bar)
 	    ("radio buttons" create-radio-buttons)
 	    ("range controls" create-range-controls)
;; 	    ("rc file")
 	    ("reparent" create-reparent)
 	    ("rulers" create-rulers)
;; 	    ("saved position")
 	    ("scrolled windows" create-scrolled-windows)
	    ("size group" create-size-group)
;; 	    ("shapes" create-shapes)
 	    ("spinbutton" create-spins)
 	    ("statusbar" create-statusbar)
 	    ("test idle" create-idle-test)
;; 	    ("test mainloop")
;; 	    ("test scrolling")
;; 	    ("test selection")
 	    ("test timeout" create-timeout-test)
 	    ("text" create-text)
 	    ("toggle buttons" create-toggle-buttons)
 	    ("toolbar" create-toolbar-window)
 	    ("tooltips" create-tooltips)
;; 	    ("tree" #|create-tree|#)
	    ("UI manager" create-ui-manager)
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

    (let ((icon (gdk:pixbuf-load #p"clg:examples;gtk.png")))
      (setf 
       (window-icon main-window) 
       (gdk:pixbuf-add-alpha icon t 254 254 252)))

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
