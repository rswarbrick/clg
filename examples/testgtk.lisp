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

;; $Id: testgtk.lisp,v 1.1 2000-08-14 16:44:26 espen Exp $


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
	 (setq window (dialog-new))
	 (signal-connect
	  window 'destroy #'(lambda () (widget-destroyed window)))
	 (setf (window-title window) ,title)
	 (setf (container-border-width window) 0)
	 (let ((main-box (vbox-new nil 0))
	       (action-area (dialog-action-area window)))
	   (box-pack-start (dialog-vbox window) main-box t t 0)
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


(defun build-option-menu (items history)
  (let ((option-menu (option-menu-new))
	(menu (menu-new)))
    (labels ((create-menu (items i group)
	       (when items
		 (let* ((item (first items))
			(menu-item (radio-menu-item-new group (first item))))
		   (signal-connect
		    menu-item 'activate
		    #'(lambda ()
			(when (widget-mapped-p menu-item)
			  (funcall (second item)))))
		   
		   (menu-append menu menu-item)
		   (when (= i history)
		     (setf (check-menu-item-active-p menu-item) t))
		   (widget-show menu-item)
		   (create-menu
		    (rest items) (1+ i) (radio-menu-item-group menu-item))))))
      (create-menu items 0 nil))
    (setf (option-menu-menu option-menu) menu)
    (setf (option-menu-history option-menu) history)
    option-menu))



;;; Pixmaps used in some of the tests

(defvar gtk-mini-xpm
  '("15 20 17 1"
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
  '("16 16 6 1"
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
  '("16 16 4 1"
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
  '("16 16 4 1"
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

(defun create-bbox (class title spacing child-w child-h layout)
  (let* ((frame (make-instance 'frame :title title))
	 (bbox (make-instance 'class
	        :border-width 5
		:layout layout
		:spacing spacing
		:childrent
		(list
		 (make-instance 'button :label "OK")
		 (make-instance 'button :label "Cancel")
		 (make-instance 'button :label "Help"))
		:parent frame)))
    (setf (button-box-child-size bbox) (vector child-w child-h))
    frame))


(define-test-window create-button-box "Button Boxes"
  (setf (container-border-width window) 10)
  (let ((main-box (vbox-new nil 0)))
    (let ((frame (frame-new "Horizontal Button Boxes"))
	  (box (vbox-new nil 0)))
      (container-add window main-box)
      (box-pack-start main-box frame t t 10)
      (setf (container-border-width box) 10)
      (container-add frame box)
      (box-pack-start
       box (create-bbox #'hbutton-box-new "Spread" 40 85 20 :spread) t t 0)
      (box-pack-start
       box (create-bbox #'hbutton-box-new "Edge" 40 85 20 :edge) t t 0)
      (box-pack-start
       box (create-bbox #'hbutton-box-new "Start" 40 85 20 :start) t t 0)
      (box-pack-start
       box (create-bbox #'hbutton-box-new "End" 40 85 20 :end) t t 0))

    (let ((frame (frame-new "Vertical Button Boxes"))
	  (box (hbox-new nil 0)))
      (box-pack-start main-box frame t t 10)
      (setf (container-border-width box) 10)
      (container-add frame box)
      (box-pack-start
       box (create-bbox #'vbutton-box-new "Spread" 30 85 20 :spread) t t 5)
      (box-pack-start
       box (create-bbox #'vbutton-box-new "Edge" 30 85 20 :edge) t t 5)
      (box-pack-start
       box (create-bbox #'vbutton-box-new "Start" 30 85 20 :start) t t 5)
      (box-pack-start
       box (create-bbox #'vbutton-box-new "End" 30 85 20 :end) t t 5))))



(define-standard-dialog create-buttons "Buttons"
  (let ((table (table-new 3 3 nil))
	(buttons `((,(button-new "button1") 0 1 0 1)
		   (,(button-new "button2") 1 2 1 2)
		   (,(button-new "button3") 2 3 2 3)
		   (,(button-new "button4") 0 1 2 3)
		   (,(button-new "button5") 2 3 0 1)
		   (,(button-new "button6") 1 2 2 3)
		   (,(button-new "button7") 1 2 0 1)
		   (,(button-new "button8") 2 3 1 2)
		   (,(button-new "button9") 0 1 1 2))))
    (setf (table-row-spacings table) 5)
    (setf (table-column-spacings table) 5)
    (setf (container-border-width table) 10)
    (box-pack-start main-box table t t 0)
    (do ((tmp buttons (rest tmp)))
	((endp tmp))
      (let ((button (first tmp))
	    (widget (or (first (second tmp))
			(first (first buttons)))))
	(signal-connect (first button) 'clicked
	 #'(lambda ()
	     (if (widget-visible-p widget)
		 (widget-hide widget)
	       (widget-show widget))))
	(apply #'table-attach table button)))))


;; Calenadar

(define-standard-dialog create-calendar "Calendar"
  (setf (container-border-width main-box) 10)
  (box-pack-start main-box (calendar-new) t t 0))



;;; Check buttons

(define-standard-dialog create-check-buttons "GtkCheckButton"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (box-pack-start main-box (check-button-new "button1") t t 0)
  (box-pack-start main-box (check-button-new "button2") t t 0)
  (box-pack-start main-box (check-button-new "button3") t t 0))



;;; CList

(let ((style1 nil)
      (style2 nil)
      (style3 nil))
  (defun insert-row-clist (clist)
    (let* ((text '("This" "is" "an" "inserted" "row"
		   "This" "is" "an" "inserted" "row"
		   "This" "is" "an" "inserted" "row"
		   "This" "is" "an" "inserted" "row"))
	   (row 
	    (if (clist-focus-row clist)
		(clist-insert clist (clist-focus-row clist) text)
	      (clist-prepend clist text))))
      
      (unless style1
	(let ((color1 '#(0 56000 0))
	      (color2 '#(32000 0 56000)))
	  (setq style1 (style-copy (widget-style clist)))
	  (setf
	   (style-base style1 :normal) color1
	   (style-base style1 :selected) color2)

	  (setq style2 (style-copy (widget-style clist)))
 	  (setf
 	   (style-fg style2 :normal) color1
 	   (style-fg style2 :selected) color2)

	  (setq style3 (style-copy (widget-style clist)))
 	  (setf
 	   (style-fg style3 :normal) color1
 	   (style-base style3 :normal) color2
 	   (style-font style3) "-*-courier-medium-*-*-*-*-120-*-*-*-*-*-*")))

      (setf (clist-cell-style clist row 3) style1)
      (setf (clist-cell-style clist row 4) style2)
      (setf (clist-cell-style clist row 0) style3))))


(define-standard-dialog create-clist "clist"
  (let* ((titles '("auto resize" "not resizeable" "max width 100"
		   "min width 50" "hide column" "Title 5" "Title 6"
		   "Title 7" "Title 8"  "Title 9"  "Title 10"
		   "Title 11" "Title 12" "Title 13" "Title 14"
		   "Title 15" "Title 16" "Title 17" "Title 18"
		   "Title 19"))
	 (clist (clist-new titles))
	 (scrolled-window (scrolled-window-new nil nil)))

    (setf (container-border-width scrolled-window) 5)
    (setf (scrolled-window-scrollbar-policy scrolled-window) :automatic)
    (container-add scrolled-window clist)

    (signal-connect
     clist 'click-column
     #'(lambda (column)
	 (cond
	  ((= column 4)
	   (setf (clist-column-visible-p clist column) nil))
	  ((= column (clist-sort-column clist))
	   (if (eq (clist-sort-type clist) :ascending)
	       (setf (clist-sort-type clist) :descending)
	     (setf (clist-sort-type clist) :ascending)))
	  (t
	   (setf (clist-sort-column clist) column)))
	 (clist-sort clist)))

    (let ((box2 (hbox-new nil 5)))
      (setf (container-border-width box2) 5)
      (box-pack-start main-box box2 nil nil 0)
      
      (let ((button (button-new "Insert Row")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked #'insert-row-clist :object clist))

      (let ((button (button-new "Add 1,000 Rows With Pixmaps")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (multiple-value-bind (pixmap mask)
		 (gdk:pixmap-create gtk-mini-xpm)
	       (let ((texts (do ((i 4 (1+ i))
				 (texts '(nil "Center" "Right")))
				((= i (length titles)) (reverse texts))
			      (push (format nil "Column ~D" i) texts))))
		 (clist-freeze clist)
		 (dotimes (i 1000)
		   (let ((row
			  (clist-append
			   clist
			   (cons (format nil "CListRow ~D" (random 1000))
				 texts))))
		     (clist-set-cell-pixtext
		      clist row 3 "gtk+" 5 (list pixmap mask))))
		 (clist-thaw clist))))))

      (let ((button (button-new "Add 10,000 Rows")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (let ((texts (do ((i 3 (1+ i))
			       (texts '("Center" "Right")))
			      ((= i (length titles)) (reverse texts))
			    (push (format nil "Column ~D" i) texts))))
	       (clist-freeze clist)
	       (dotimes (i 10000)
		 (clist-append
		  clist (cons (format nil "CListRow ~D" (random 1000)) texts)))
	       (clist-thaw clist))))))
    

    (let ((box2 (hbox-new nil 5)))
      (setf (container-border-width box2) 5)
      (box-pack-start main-box box2 nil nil 0)
    	    
      (let ((button (button-new "Clear List")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (clist-clear clist))))
    
      (let ((button (button-new "Remove Selection")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (clist-freeze clist)
	     (let ((selection-mode (clist-selection-mode clist)))
	       (labels ((remove-selection ()
		          (let ((selection (clist-selection clist)))
			    (when selection
			      (clist-remove clist (first selection))
			      (unless (eq selection-mode :browse)
				(remove-selection))))))
		 (remove-selection))
	     
	       (when (and
		      (eq selection-mode :extended)
		      (not (clist-selection clist))
		      (clist-focus-row clist))
		 (clist-select-row clist (clist-focus-row clist))))
	     (clist-thaw clist))))

      (let ((button (button-new "Undo Selection")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked #'clist-undo-selection :object clist))

      (let ((button (button-new "Warning Test")))
	(box-pack-start box2 button t t 0)
	(signal-connect button 'clicked #'(lambda ()))))
    

    (let ((box2 (hbox-new nil 5)))
      (setf (container-border-width box2) 5)
      (box-pack-start main-box box2 nil nil 0)
      
      (let ((button (check-button-new "Show Title Buttons")))
	(box-pack-start box2 button t t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (if (toggle-button-active-p button)
		 (clist-column-titles-show clist)
	       (clist-column-titles-hide clist))))
	(setf (toggle-button-active-p button) t))

      (let ((button (check-button-new "Reorderable")))
	(box-pack-start box2 button nil t 0)
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (setf
	      (clist-reorderable-p clist) (toggle-button-active-p button))))
	(setf (toggle-button-active-p button) t))

      (box-pack-start box2 (label-new "Selection Mode : ") nil t 0)      
      (let ((option-menu
	     (build-option-menu
	      `(("Single"
		 ,#'(lambda () (setf (clist-selection-mode clist) :single)))
		("Browse"
		 ,#'(lambda () (setf (clist-selection-mode clist) :browse)))
		("Multiple"
		 ,#'(lambda () (setf (clist-selection-mode clist) :multiple)))
		("Extended"
		 ,#'(lambda () (setf (clist-selection-mode clist) :extended))))
	      3)))
	(box-pack-start box2 option-menu nil t 0)))

    (box-pack-start main-box scrolled-window t t 0)
    (setf (clist-row-height clist) 18)
    (setf (widget-height clist) 300)

    (dotimes (i (length titles))
      (setf (clist-column-width clist i) 80))

    (setf (clist-column-auto-resize-p clist 0) t)
    (setf (clist-column-resizeable-p clist 1) nil)
    (setf (clist-column-max-width clist 2) 100)
    (setf (clist-column-min-width clist 3) 50)
    (setf (clist-selection-mode clist) :extended)
    (setf (clist-column-justification clist 1) :right)
    (setf (clist-column-justification clist 2) :center)

    (let ((style (style-new))
	  (texts (do ((i 3 (1+ i))
		      (texts '("Center" "Right")))
		     ((= i (length titles)) (reverse texts))
		     (push (format nil "Column ~D" i) texts))))
       (setf
        (style-font style) "-adobe-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*"
	(style-fg style :normal) '#(56000 0 0)
	(style-base style :normal) '#(0 56000 32000))
      
      (dotimes (i 10)
        (clist-append clist (cons (format nil "CListRow ~D" i) texts))
	(if (= (mod i 4) 2)
	    (setf (clist-row-style clist i) style)
	  (setf (clist-cell-style clist i (mod i 4)) style))))))



;;; Color selection

(let ((color-dialog nil))
  (defun create-color-selection ()
    (unless color-dialog
      (setq color-dialog
	    (color-selection-dialog-new "color selection dialog"))

      (setf (window-position color-dialog) :mouse)
      (signal-connect
       color-dialog 'destroy #'(lambda () (widget-destroyed color-dialog)))
      
      (let ((colorsel (color-selection-dialog-colorsel color-dialog)))
	(setf (color-selection-use-opacity-p colorsel) t)
	(setf (color-selection-policy colorsel) :continuous)
	
;	(signal-connect colorsel 'color-changed #'(lambda () nil))

	(let ((button (color-selection-dialog-ok-button color-dialog)))
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (let ((color (color-selection-color colorsel)))
		 (format t "Selected color: ~A~%" color)
		 (setf (color-selection-color colorsel) color))))))

      (let ((button (color-selection-dialog-cancel-button color-dialog)))
	(signal-connect
	 button 'clicked #'widget-destroy :object color-dialog)))
       
    (if (not (widget-visible-p color-dialog))
	(widget-show-all color-dialog)
      (widget-destroy color-dialog))))



;;; CTree

(let ((total-pages 0)
      (total-books 0)
      (status-labels)
      (style1)
      (style2)
      (pixmap1)
      (pixmap2)
      (pixmap3))

  (defun after-press (ctree &rest data)
    (declare (ignore data))
    (setf
     (label-text (svref status-labels 0))
     (format nil "~D" total-books))
    (setf
     (label-text (svref status-labels 1))
     (format nil "~D" total-pages))
    (setf
     (label-text (svref status-labels 2))
     (format nil "~D" (length (clist-selection ctree))))
    (setf
     (label-text (svref status-labels 3))
     (format nil "~D" (clist-n-rows ctree)))
    nil)
    
  (defun build-recursive (ctree parent current-depth depth books pages)
    (let ((sibling nil))
      (do ((i (+ pages books) (1- i)))
	  ((= i books))
	(declare (fixnum i))
	(incf total-pages)
	(setq
	 sibling
	 (ctree-insert-node
	  ctree parent sibling
	  (list
	   (format nil "Page ~D" (random 100))
	   (format nil "Item ~D-~D" current-depth i))
	  5 :pixmap pixmap3 :leaf t))
	(when (and parent (eq (ctree-line-style ctree) :tabbed))
	  (setf
	   (ctree-row-style ctree sibling)
	   (ctree-row-style ctree parent))))
      
      (unless (= current-depth depth)
	(do ((i books (1- i)))
	    ((zerop i))
	  (incf total-books)
	  (setq
	   sibling
	   (ctree-insert-node
	    ctree parent sibling
	    (list
	     (format nil "Book ~D" (random 100))
	     (format nil "Item ~D-~D" current-depth i))
	    5 :closed pixmap1 :opened pixmap2))

	  (let ((style (style-new))
		(color (case (mod current-depth 3)
			 (0 (vector
			     (* 10000 (mod current-depth 6))
			     0
			     (- 65535 (mod (* i 10000) 65535))))
			 (1 (vector
			     (* 10000 (mod current-depth 6))
			     (- 65535 (mod (* i 10000) 65535))
			     0))
			 (t (vector
			     (- 65535 (mod (* i 10000) 65535))
			     0
			     (* 10000 (mod current-depth 6)))))))
	    (setf (style-base style :normal) color)
	    (ctree-set-node-data ctree sibling style #'style-unref)
	    
	    (when (eq (ctree-line-style ctree) :tabbed)
	      (setf (ctree-row-style ctree sibling) style)))

	  (build-recursive
	   ctree sibling (1+ current-depth)  depth books pages)))))

  (defun rebuild-tree (ctree depth books pages)
    (let ((n (* (/ (1- (expt books depth)) (1- books)) (1+ pages))))
      (if (> n 10000)
	  (format t "~D total items? Try less~%" n)
	(progn
	  (clist-freeze ctree)
	  (clist-clear ctree)
	  (setq total-books 1)
	  (setq total-pages 0)
	  (let ((parent
		 (ctree-insert-node
		  ctree nil nil '("Root") 5
		  :closed pixmap1 :opened pixmap2 :expanded t))
		(style (style-new)))
	    (setf (style-base style :normal) '#(0 45000 55000))
	    (ctree-set-node-data ctree parent style #'style-unref)
	    
	    (when (eq (ctree-line-style ctree) :tabbed)
	      (setf (ctree-row-style ctree parent) style))

	    (build-recursive ctree parent 1 depth books pages)
	    (clist-thaw ctree)
	    (after-press ctree))))))

  (let ((export-window)
	(export-ctree))
    (defun export-tree (ctree)
      (unless export-window
	(setq export-window (window-new :toplevel))
	(signal-connect
	 export-window 'destroy
	 #'(lambda ()
	     (widget-destroyed export-window)))
	
	(setf (window-title export-window) "Exported ctree")
	(setf (container-border-width export-window) 5)

	(let ((vbox (vbox-new nil 0)))
	  (container-add export-window vbox)

	  (let ((button (button-new "Close")))
	    (box-pack-end vbox button nil t 0)
	    (signal-connect
	     button 'clicked #'widget-destroy :object export-window))

	  (box-pack-end vbox (hseparator-new) nil t 10)

	  (setq export-ctree (ctree-new '("Tree" "Info")))
	  (setf (ctree-line-style export-ctree) :dotted)

	  (let ((scrolled-window (scrolled-window-new)))
	    (container-add scrolled-window export-ctree)
	    (setf
	     (scrolled-window-scrollbar-policy scrolled-window) :automatic)
	    (box-pack vbox scrolled-window)
	    (setf (clist-selection-mode export-ctree) :extended)
	    (setf (clist-column-width export-ctree 0) 200)
	    (setf (clist-column-width export-ctree 1) 200)
	    (setf (widget-width export-ctree) 300)
	    (setf (widget-height export-ctree) 200))))

      (unless (widget-visible-p export-window)
	(widget-show-all export-window))

      (clist-clear export-ctree)
      (let ((node (ctree-nth-node ctree (clist-focus-row ctree))))
	(when node
	  (let ((tree-list
		 (list (ctree-map-to-list ctree node #'(lambda (node) node)))))
	    (ctree-insert-from-list
	     export-ctree nil tree-list
	     #'(lambda (export-ctree-node ctree-node)
		 (multiple-value-bind
		     (text spacing pixmap-closed bitmap-closed pixmap-opened
		      bitmap-opened leaf expanded)
		     (ctree-node-info ctree ctree-node)
		   (ctree-set-node-info
		    export-ctree export-ctree-node text spacing
		    :closed (list pixmap-closed bitmap-closed)
		    :opened (list pixmap-opened bitmap-opened)
		    :leaf leaf :expanded expanded))
		 (unless (eq (ctree-cell-type ctree ctree-node 1) :empty)
		   (setf
		    (ctree-cell-text export-ctree export-ctree-node 1)
		    (ctree-cell-text ctree ctree-node 1))))))))))
  

  (define-test-window create-ctree "CTree"
    (let ((vbox (vbox-new nil 0))
	  (ctree (ctree-new '("Tree" "Info"))))

      (container-add window vbox)

      (let ((hbox (hbox-new nil 5)))
	(setf (container-border-width hbox) 5)
	(box-pack-start vbox hbox nil t 0)

	(let ((spin1 (spin-button-new (adjustment-new 4 1 10 1 5 0) 0 0))
	      (spin2 (spin-button-new (adjustment-new 3 1 20 1 5 0) 0 0))
	      (spin3 (spin-button-new (adjustment-new 5 1 20 1 5 0) 0 0)))

	  (box-pack-start hbox (label-new "Depth :") nil t 0)
	  (box-pack-start hbox spin1 nil t 5)
	  (box-pack-start hbox (label-new "Books :") nil t 0)
	  (box-pack-start hbox spin2 nil t 5)
	  (box-pack-start hbox (label-new "Pages :") nil t 0)
	  (box-pack-start hbox spin3 nil t 5)
	  
	  (let ((button (button-new "Rebuild Tree")))
	    (box-pack-start hbox button t t 0)
	    (signal-connect
	     button 'clicked
	     #'(lambda ()
		 (let ((depth (spin-button-value-as-int spin1))
		       (books (spin-button-value-as-int spin2))
		       (pages (spin-button-value-as-int spin3)))
		   (rebuild-tree ctree depth books pages))))))
	
	(let ((button (button-new "Close")))
	  (box-pack-end hbox button t t 0)
	  (signal-connect button 'clicked #'widget-destroy :object window)))
    
      (let ((scrolled-window (scrolled-window-new)))
	(setf (container-border-width scrolled-window) 5)
	(setf (scrolled-window-hscrollbar-policy scrolled-window) :automatic)
	(setf (scrolled-window-vscrollbar-policy scrolled-window) :always)
	(box-pack-start vbox scrolled-window t t 0)
	
	(container-add scrolled-window ctree)
	(setf (clist-column-auto-resize-p ctree 0) t)
	(setf (clist-column-width ctree 1) 200)
	(setf (clist-selection-mode ctree) :extended)
	(setf (ctree-line-style ctree) :dotted))

      (signal-connect
       ctree 'click-column
       #'(lambda (column)
	   (cond
	    ((/= column (clist-sort-column ctree))
	     (setf (clist-sort-column ctree) column))
	    ((eq (clist-sort-type ctree) :ascending)
	     (setf (clist-sort-type ctree) :descending))
	    (t (setf (clist-sort-type ctree) :ascending)))
	   (ctree-sort-recursive ctree)))

      (signal-connect
       ctree 'button-press-event #'after-press :object t :after t)
      (signal-connect
       ctree 'button-release-event #'after-press :object t :after t)
      (signal-connect
       ctree 'tree-move #'after-press :object t :after t)
      (signal-connect
       ctree 'end-selection #'after-press :object t :after t)
      (signal-connect
       ctree 'toggle-focus-row #'after-press :object t :after t)
      (signal-connect
       ctree 'select-all #'after-press :object t :after t)
      (signal-connect
       ctree 'unselect-all #'after-press :object t :after t)
      (signal-connect
       ctree 'scroll-vertical #'after-press :object t :after t)

      (let ((bbox (hbox-new nil 5)))
	(setf (container-border-width bbox) 5)
	(box-pack-start vbox bbox nil t 0)

	(let ((mbox (vbox-new t 5)))
	  (box-pack bbox mbox :expand nil)
	  (box-pack mbox (label-new "Row Height :") :expand nil :fill nil)
	  (box-pack mbox (label-new "Indent :") :expand nil :fill nil)
	  (box-pack mbox (label-new "Spacing :") :expand nil :fill nil))

	(let ((mbox (vbox-new t 5)))
	  (box-pack bbox mbox :expand nil)
	  
	  (let* ((adjustment (adjustment-new 20 12 100 1 10 0))
		 (spinner (spin-button-new adjustment 0 0)))
	    (box-pack mbox spinner :expand nil :fill nil :padding 5)
	    (flet ((set-row-height ()
		     (setf
		      (clist-row-height ctree)
		      (spin-button-value-as-int spinner))))
	      (signal-connect adjustment 'value-changed #'set-row-height)
	      (set-row-height)))
	  
	  (let* ((adjustment (adjustment-new 20 0 60 1 10 0))
		 (spinner (spin-button-new adjustment 0 0)))
	    (box-pack mbox spinner :expand nil :fill nil :padding 5)
	    (flet ((set-indent ()
		     (setf
		      (ctree-indent ctree)
		      (spin-button-value-as-int spinner))))
	      (signal-connect adjustment 'value-changed #'set-indent)
	      (set-indent)))

	  (let* ((adjustment (adjustment-new 5 0 60 1 10 0))
		 (spinner (spin-button-new adjustment 0 0)))
	    (box-pack mbox spinner :expand nil :fill nil :padding 5)
	    (flet ((set-spacing ()
		     (setf
		      (ctree-spacing ctree)
		      (spin-button-value-as-int spinner))))
	      (signal-connect adjustment 'value-changed #'set-spacing)
	      (set-spacing))))

	
	(let ((mbox (vbox-new t 5)))
	  (box-pack bbox mbox :expand nil)
	  
	  (let ((hbox (hbox-new nil 5)))
	    (box-pack mbox hbox :expand nil :fill nil)

	    (let ((button (button-new "Expand All")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (ctree-expand-recursive ctree nil)
		   (after-press ctree))))

	    (let ((button (button-new "Collapse All")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (ctree-collapse-recursive ctree nil)
		   (after-press ctree))))

	    (let ((button (button-new "Change Style")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (let ((node (ctree-nth-node
				ctree (or (clist-focus-row ctree) 0))))
		     (when node
		       (unless style1
			 (let ((color1 '#(0 56000 0))
			       (color2 '#(32000 0 56000)))
			   (setq style1 (style-new))
			   (setf (style-base style1 :normal) color1)
			   (setf (style-fg style1 :selected) color2)

			   (setq style2 (style-new))
			   (setf (style-base style2 :selected) color2)
			   (setf (style-base style2 :normal) color2)
			   (setf (style-fg style2 :normal) color1)
			   (setf
			    (style-font style2)
			    "-*-courier-medium-*-*-*-*-300-*-*-*-*-*-*")))
		       (setf (ctree-cell-style ctree node 1) style1)
		       (setf (ctree-cell-style ctree node 0) style2)

		       (when (ctree-node-child node)
			 (setf
			  (ctree-row-style ctree (ctree-node-child node))
			  style2)))))))

	    (let ((button (button-new "Export Tree")))
	      (box-pack hbox button)
	      (signal-connect button 'clicked #'export-tree :object ctree)))

	  (let ((hbox (hbox-new nil 5)))
	    (box-pack mbox hbox :expand nil :fill nil)

	    (let ((button (button-new "Select All")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (ctree-select-recursive ctree nil)
		   (after-press ctree))))

	    (let ((button (button-new "Unselect All")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (ctree-unselect-recursive ctree nil)
		   (after-press ctree))))

	    (let ((button (button-new "Remove Selection")))
	      (box-pack hbox button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (clist-freeze ctree)
		   (let ((selection-mode (clist-selection-mode ctree)))
		     (labels
			 ((remove-selection ()
			    (let ((node (first (ctree-selection ctree))))
			      (when node
				
				(ctree-apply-post-recursive
				 ctree node
				 #'(lambda (node)
				     (if (ctree-node-leaf-p node)
					 (decf total-pages)
				       (decf total-books))))
				   
				(ctree-remove-node ctree node)
				(unless (eq selection-mode :browse)
				  (remove-selection))))))
		       (remove-selection))
	     
		     (when (and
			    (eq selection-mode :extended)
			    (not (clist-selection ctree))
			    (clist-focus-row ctree))
		       (ctree-select
			ctree
			(ctree-nth-node ctree (clist-focus-row ctree)))))
		   (clist-thaw ctree)
		   (after-press ctree))))
	    
	    (let ((button (check-button-new "Reorderable")))
	      (box-pack hbox button :expand nil)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (setf
		    (clist-reorderable-p ctree)
		    (toggle-button-active-p button))))
	      (setf (toggle-button-active-p button) t)))

	  (let ((hbox (hbox-new nil 5)))
	    (box-pack mbox hbox :expand nil :fill nil)

	    (flet
		((set-line-style (line-style)
		   (let ((current-line-style (ctree-line-style ctree)))
		     (when (or
			    (and
			     (eq current-line-style :tabbed)
			     (not (eq line-style :tabbed)))
			    (and
			     (not (eq current-line-style :tabbed))
			     (eq line-style :tabbed)))
		       (ctree-apply-pre-recursive
			ctree nil
			#'(lambda (node)
			    (let
				((style
				  (cond
				   ((eq (ctree-line-style ctree) :tabbed) nil)
				   ((not (ctree-node-leaf-p node))
				    (ctree-node-data ctree node))
				   ((ctree-node-parent node)
				    (ctree-node-data
				     ctree (ctree-node-parent node))))))
			      (setf (ctree-row-style ctree node) style))))
		       (setf (ctree-line-style ctree) line-style)))))
	      
	      (let ((option-menu
		     (build-option-menu
		      `(("No lines" ,#'(lambda () (set-line-style :none)))
			("Solid" ,#'(lambda () (set-line-style :solid)))
			("Dotted" ,#'(lambda () (set-line-style :dotted)))
			("Tabbed" ,#'(lambda () (set-line-style :tabbed))))
		      2)))
		(box-pack hbox option-menu :expand nil)))

	    (let ((option-menu
		   (build-option-menu
		    `(("None"
		       ,#'(lambda ()
			    (setf (ctree-expander-style ctree) :none)))
		      ("Square"
		       ,#'(lambda ()
			    (setf (ctree-expander-style ctree) :square)))
		      ("Triangle"
		       ,#'(lambda ()
			    (setf (ctree-expander-style ctree) :triangle)))
		      ("Circular"
		       ,#'(lambda ()
			    (setf (ctree-expander-style ctree) :circular))))
		    1)))
	      (box-pack hbox option-menu :expand nil))

	    (let ((option-menu
		   (build-option-menu
		    `(("Left"
		       ,#'(lambda ()
			    (setf
			     (clist-column-justification ctree 0) :left)))
		      ("Right"
		       ,#'(lambda ()
			    (setf
			     (clist-column-justification ctree 0) :right))))
		    0)))
	      (box-pack hbox option-menu :expand nil))

	    (flet ((set-sel-mode (mode)
		     (setf (clist-selection-mode ctree) mode)
		     (after-press ctree)))
	      (let ((option-menu
		     (build-option-menu
		      `(("Single" ,#'(lambda () (set-sel-mode :single)))
			("Browse" ,#'(lambda () (set-sel-mode :browse)))
			("Multiple" ,#'(lambda () (set-sel-mode :multiple)))
			("Extended" ,#'(lambda () (set-sel-mode :extended))))
		      3)))
		(box-pack hbox option-menu :expand nil))))))

      (let ((frame (frame-new)))
	(setf (container-border-width frame) 0)
	(setf (frame-shadow-type frame) :out)
	(box-pack vbox frame :expand nil)

	(let ((hbox (hbox-new t 2)))
	  (setf (container-border-width hbox) 2)
	  (container-add frame hbox)

	  (setq
	   status-labels
	   (map 'vector
	    #'(lambda (text)
		(let ((frame (frame-new))
		      (hbox2 (hbox-new nil 0)))
		  (setf (frame-shadow-type frame) :in)
		  (box-pack hbox frame :expand nil)
		  (setf (container-border-width hbox2) 2)
		  (container-add frame hbox2)
		  (box-pack hbox2 (label-new text) :expand nil)
		  (let ((label (label-new "")))
		    (box-pack-end hbox2 label nil t 5)
		    label)))
	    '("Books :" "Pages :" "Selected :" "Visible :")))))
      
      (widget-realize window)
      (let ((gdk:window (widget-window window)))
	(setq pixmap1 (multiple-value-list
		       (gdk:pixmap-create book-closed-xpm :window gdk:window)))
	(setq pixmap2 (multiple-value-list
		       (gdk:pixmap-create book-open-xpm :window gdk:window)))
	(setq pixmap3 (multiple-value-list
		       (gdk:pixmap-create mini-page-xpm :window gdk:window))))
      (setf (widget-height ctree) 300)
      
      (rebuild-tree ctree 4 3 5))))



;;; Cursors

(defun clamp (n min-val max-val)
  (declare (number n min-val max-val))
  (max (min n max-val) min-val))

(defun set-cursor (spinner drawing-area label)
  (let ((cursor
	 (gforeign:int-enum
	  (logand (clamp (spin-button-value-as-int spinner) 0 152) #xFE)
	  'gdk:cursor-type)))	
    (setf (label-text label) (string-downcase (symbol-name cursor)))
    (setf (widget-cursor drawing-area) cursor)))
    

(define-standard-dialog create-cursors "Cursors"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 5)
  (let* ((hbox (hbox-new nil 0))
	 (label (label-new "Cursor Value : "))
	 (adj (adjustment-new 0 0 152 2 10 0))
	 (spinner (spin-button-new adj 0 0)))
    (setf (container-border-width hbox) 5)
    (box-pack-start main-box hbox nil t 0)
    (setf (misc-xalign label) 0)
    (setf (misc-yalign label) 0.5)
    (box-pack-start hbox label nil t 0)
    (box-pack-start hbox spinner t t 0)

    (let ((frame (make-frame
		  :shadow-type :etched-in
		  :label-xalign 0.5
		  :label "Cursor Area"
		  :border-width 10
		  :parent main-box
		  :visible t))
	  (drawing-area (drawing-area-new)))
      (setf (widget-width drawing-area) 80)
      (setf (widget-height drawing-area) 80)
      (container-add frame drawing-area)
      (signal-connect
       drawing-area 'expose-event
       #'(lambda (event)
	   (declare (ignore event))
	   (multiple-value-bind (width height)
	       (drawing-area-size drawing-area)
	     (let* ((drawable (widget-window drawing-area))
		    (style (widget-style drawing-area))
		    (white-gc (style-get-gc style :white))
		    (gray-gc (style-get-gc style :background :normal))
		    (black-gc (style-get-gc style :black)))
	       (gdk:draw-rectangle
		drawable white-gc t 0 0 width (floor height 2))
	       (gdk:draw-rectangle
		drawable black-gc t 0 (floor height 2) width (floor height 2))
	       (gdk:draw-rectangle
		drawable gray-gc t (floor width 3) (floor height 3)
		(floor width 3) (floor height 3))))
	     t))
      (setf (widget-events drawing-area) '(:exposure :button-press))
      (signal-connect
       drawing-area 'button-press-event
       #'(lambda (event)
	   (when (and
		  (eq (gdk:event-type event) :button-press)
		  (or
		   (= (gdk:event-button event) 1)
		   (= (gdk:event-button event) 3)))
	     (spin-button-spin
	      spinner
	      (if (= (gdk:event-button event) 1)
		  :step-forward
		:step-backward)
	      0)
	     t)))
      (widget-show drawing-area)

    (let ((label (make-label
		  :visible t
		  :label "XXX"
		  :parent main-box)))
      (setf (box-child-expand-p #|main-box|# label) nil)
      (signal-connect
       spinner 'changed
       #'(lambda ()
	   (set-cursor spinner drawing-area label)))

      (widget-realize drawing-area)
      (set-cursor spinner drawing-area label)))))



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
  (let ((entry (make-instance 'entry
		:test "hello world"
		:visible t
		:parent (list main-box :fill t :expand t))))
    (entry-select-region entry 0 5)

    (let ((combo (make-instance 'combo
		  :visible t
		  :parent (list main-box :expand t :fill t))))
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
      (editable-select-region entry 0 5))
    
    (let ((check-button (check-button-new "Editable")))
      (box-pack-start main-box check-button nil t 0)
      (signal-connect
       check-button 'toggled
       #'(lambda ()
	   (setf
	    (editable-editable-p entry)
	    (toggle-button-active-p check-button))))
      (setf (toggle-button-active-p check-button) t)
      (widget-show check-button))
		    
    (let ((check-button (check-button-new "Visible")))
      (box-pack-start main-box check-button nil t 0)
      (signal-connect
       check-button 'toggled
       #'(lambda ()
	   (setf
	    (entry-visible-p entry)
	    (toggle-button-active-p check-button))))
      (setf (toggle-button-active-p check-button) t)
      (widget-show check-button))
		    
    (let ((check-button (check-button-new "Sensitive")))
      (box-pack-start main-box check-button nil t 0)
      (signal-connect
       check-button 'toggled
       #'(lambda ()
	   (setf
	    (widget-sensitive-p entry)
	    (toggle-button-active-p check-button))))
      (setf (toggle-button-active-p check-button) t)
      (widget-show check-button))))



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
     toolbar "Horizontal" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Horizontal toolbar layout"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

    (toolbar-append-item
     toolbar "Vertical" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Vertical toolbar layout"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

    (toolbar-append-space toolbar)
    
    (toolbar-append-item
     toolbar "Icons" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Only show toolbar icons"
     :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
    (toolbar-append-item
     toolbar "Text" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Only show toolbar text"
     :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
    (toolbar-append-item
     toolbar "Both" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Show toolbar icons and text"
     :callback #'(lambda () (setf (toolbar-style toolbar) :both)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Small" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Use small spaces"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
    (toolbar-append-item
     toolbar "Big" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Use big spaces"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Enable" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Enable tooltips"
     :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

    (toolbar-append-item
     toolbar "Disable" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Disable tooltips"
     :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Borders" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Show borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
    (toolbar-append-item
     toolbar "Borderless" (pixmap-new "cl-gtk:src;test.xpm")
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
  (let ((hbox (hbox-new nil 5)))
    (container-add window hbox)
    (let ((vbox (vbox-new nil 5)))
      (box-pack-start hbox vbox nil nil 0)

      (let ((frame (frame-new  "Normal Label")))
	(container-add frame (label-new "This is a Normal label"))
	(box-pack-start vbox frame nil nil 0))

      (let ((frame (frame-new  "Multi-line Label")))
	(container-add frame (label-new
"This is a Multi-line label.
Second line
Third line"))
	(box-pack-start vbox frame nil nil 0))

      (let ((frame (frame-new  "Left Justified Label"))
	    (label (label-new
"This is a Left-Justified
Multi-line.
Third line")))
	(setf (label-justify label) :left)
	(container-add frame label)
	(box-pack-start vbox frame nil nil 0))

      (let ((frame (frame-new  "Right Justified Label"))
	    (label (label-new
"This is a Right-Justified
Multi-line.
Third line")))
	(setf (label-justify label) :right)
	(container-add frame label)
	(box-pack-start vbox frame nil nil 0)))

    (let ((vbox (vbox-new nil 5)))
      (box-pack-start hbox vbox nil nil 0)
    
      (let ((frame (frame-new  "Line wrapped label"))
	    (label (label-new
"This is an example of a line-wrapped label.  It should not be taking up the entire             width allocated to it, but automatically wraps the words to fit.  The time has come, for all good men, to come to the aid of their party.  The sixth sheik's six sheep's sick.
     It supports multiple paragraphs correctly, and  correctly   adds many          extra  spaces. ")))
	(setf (label-wrap-p label) t)
	(container-add frame label)
	(box-pack-start vbox frame nil nil 0))
      
      (let ((frame (frame-new  "Filled, wrapped label"))
	    (label (label-new
"This is an example of a line-wrapped, filled label.  It should be taking up the entire              width allocated to it.  Here is a seneance to prove my point.  Here is another sentence. Here comes the sun, do de do de do.
    This is a new paragraph.
    This is another newer, longer, better paragraph.  It is coming to an end, unfortunately.")))
	(setf (label-justify label) :fill)
	(setf (label-wrap-p label) t)
	(container-add frame label)
	(box-pack-start vbox frame nil nil 0))
	
      (let ((frame (frame-new  "Underlined label"))
	    (label (label-new
"This label is underlined!
This one is underlined in 日本語の入用quite a funky fashion")))
	(setf (label-justify label) :left)
	(setf (label-pattern label) "_________________________ _ _________ _ _____ _ __ __  ___ ____ _____")
	(container-add frame label)
	(box-pack-start vbox frame nil nil 0)))))



;;; Layout

(defun layout-expose-handler (layout event)
  (multiple-value-bind (x-offset y-offset)
      (layout-offset layout)
    (declare (fixnum x-offset y-offset))
    (multiple-value-bind (area-x area-y area-width area-height)
	(gdk:event-area event)
      (declare (fixnum area-x area-y area-width area-height))
      (let ((imin (truncate (+ x-offset area-x) 10))
	    (imax (truncate (+ x-offset area-x area-width 9) 10))
	    (jmin (truncate (+ y-offset area-y) 10))
	    (jmax (truncate (+ y-offset area-y area-height 9) 10)))
	(declare (fixnum imin imax jmin jmax))
	(gdk:window-clear-area
	 (widget-window layout) area-x area-y area-width area-height)

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
  (let ((scrolled (scrolled-window-new))
	(layout (layout-new)))
    (container-add window scrolled)
    (container-add scrolled layout)
    (setf (adjustment-step-increment (layout-hadjustment layout)) 10.0)
    (setf (adjustment-step-increment (layout-vadjustment layout)) 10.0)
    (setf (widget-events layout) '(:exposure))
    (signal-connect layout 'expose-event #'layout-expose-handler :object t)
    (setf (layout-size layout) '#(1600 128000))

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
    
    (with-open-file (file "cl-gtk:src;gtktypes.lisp")
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
	     (build-option-menu
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
	  (menu-append menu menuitem)
	  (widget-show menuitem)
	  ))
      (let ((group nil))
	(dotimes (i 5)
	  (let ((menuitem
		 (radio-menu-item-new
		  group (format nil "item ~2D - ~D" depth (1+ i)))))
	    (setq group (radio-menu-item-group menuitem)) ; ough!
	    (unless (zerop (mod depth 2))
	    (setf (check-menu-item-toggle-indicator-p menuitem) t))
	    (menu-append menu menuitem)
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
      (menu-bar-append menubar menuitem)
      (widget-show menuitem))

    (let ((menuitem (menu-item-new "foo")))
      (setf (menu-item-submenu menuitem) (create-menu 3 t))
      (menu-bar-append menubar menuitem)
      (widget-show menuitem))

    (let ((menuitem (menu-item-new "bar")))
      (setf (menu-item-submenu menuitem) (create-menu 4 t))
      (menu-item-right-justify menuitem)
      (menu-bar-append menubar menuitem)
      (widget-show menuitem))

    (let ((box2 (vbox-new nil 10))
	  (menu (create-menu 1 nil)))
      (setf (container-border-width box2) 10)
      (box-pack-start main-box box2 t t 0)
      (widget-show box2)
      
      (setf (menu-accel-group menu) accel-group)

      (let ((menuitem (check-menu-item-new "Accelerate Me")))
	(menu-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F1" 0 '(:visible :signal-visible)))
    
      (let ((menuitem (check-menu-item-new "Accelerator Locked")))
	(menu-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F2" 0 '(:visible :locked)))
    
      (let ((menuitem (check-menu-item-new "Accelerator Frozen")))
	(menu-append menu menuitem)
	(widget-show menuitem)
        (widget-add-accelerator
         menuitem 'activate accel-group "F2" 0 '(:visible))
        (widget-add-accelerator
         menuitem 'activate accel-group "F3" 0 '(:visible))
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
		    label-box (pixmap-new (list book-closed book-closed-mask))
		    nil t 0)
		   (box-pack-start label-box (label-new title) nil t 0)
		   (widget-show-all label-box)
		   (box-pack-start
		    menu-box (pixmap-new (list book-closed book-closed-mask))
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
	       (let ((old-page (notebook-current-page-num notebook)))
		 (unless (eq page old-page)
		   (setf
		    (pixmap-pixmap
		     (first
		      (container-children
		       (notebook-tab-label notebook page))))
		    (list book-open book-open-mask))
		   (setf
		    (pixmap-pixmap
		     (first
		      (container-children
		       (notebook-menu-label notebook page))))
		    (list book-open book-open-mask))

		   (when old-page
		     (setf
		      (pixmap-pixmap
		       (first
			(container-children
			 (notebook-tab-label notebook old-page))))
		      (list book-closed book-closed-mask))
		     (setf
		      (pixmap-pixmap
		       (first
			(container-children
			 (notebook-menu-label notebook old-page))))
		      (list book-closed book-closed-mask)))))))
	  
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
		    (build-option-menu
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
		   (container-foreach notebook #'widget-show)))))

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
		  (tab-pos 2))
	      (box-pack-start box2 button t t 0)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (setq tab-pos (mod (1+ tab-pos) 4))
		   (setf (notebook-tab-pos notebook) tab-pos))))))))))



;;; Panes

(defun toggle-resize (child)
  (let* ((paned (widget-parent child))
	 (is-child1-p (eq child (paned-child1 paned))))
    (multiple-value-bind (child resize shrink)
	(if is-child1-p
	    (paned-child1 paned)
	  (paned-child2 paned))
      (widget-ref child)
      (container-remove paned child)
      (if is-child1-p
	  (paned-pack1 paned child (not resize) shrink)
	(paned-pack2 paned child (not resize) shrink))
      (widget-unref child))))

(defun toggle-shrink (child)
  (let* ((paned (widget-parent child))
	 (is-child1-p (eq child (paned-child1 paned))))
    (multiple-value-bind (child resize shrink)
	(if is-child1-p
	    (paned-child1 paned)
	  (paned-child2 paned))
      (widget-ref child)
      (container-remove paned child)
      (if is-child1-p
	  (paned-pack1 paned child resize (not shrink))
	(paned-pack2 paned child resize (not shrink)))
      (widget-unref child))))

(defun create-pane-options (paned frame-label label1 label2)
  (let ((frame (frame-new frame-label))
	(table (table-new 3 2 t)))
    (setf (container-border-width frame) 4)
    (container-add frame table)

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
  (let ((vbox (vbox-new nil 0))
	(vpaned (vpaned-new))
	(hpaned (hpaned-new)))
    (container-add window vbox)
    (box-pack-start vbox vpaned t t 0)
    (setf (container-border-width vpaned) 5)

    (paned-add1 vpaned hpaned)

    (let ((frame (frame-new nil)))
      (setf (frame-shadow-type frame) :in)
      (setf (widget-width frame) 60)
      (setf (widget-height frame) 60)
      (paned-add1 hpaned frame)
      (container-add frame (button-new "Hi there")))

    (let ((frame (frame-new nil)))
      (setf (frame-shadow-type frame) :in)
      (setf (widget-width frame) 80)
      (setf (widget-height frame) 60)
      (paned-add2 hpaned frame))

    (let ((frame (frame-new nil)))
      (setf (frame-shadow-type frame) :in)
      (setf (widget-width frame) 80)
      (setf (widget-height frame) 60)
      (paned-add2 vpaned frame))

    ;; Now create toggle buttons to control sizing

    (box-pack-start
     vbox (create-pane-options hpaned "Horizontal" "Left" "Right") nil nil 0)

    (box-pack-start
     vbox (create-pane-options vpaned "Vertical" "Top" "Bottom") nil nil 0)))
  


;;; Pixmap

(define-standard-dialog create-pixmap "Pixmap"
  (setf (container-border-width main-box) 10)
  (let* ((button (button-new))
	 (hbox (hbox-new nil 0)))
    (box-pack-start main-box button nil nil 0)
    (container-add button hbox)
    (setf (container-border-width hbox) 2)
    (container-add hbox (pixmap-new "cl-gtk:src;test.xpm"))
    (container-add hbox (label-new "Pixmap test"))))



;;; Progress bar

(define-standard-dialog create-progress-bar "Progress bar"
  (setf (window-allow-grow-p window) nil)
  (setf (window-allow-shrink-p window) nil)
  (setf (window-auto-shrink-p window) t)
  
  (setf (container-border-width main-box) 10)

  (let* ((pbar-adj (adjustment-new 0 1 300 0 0 0))
	 (pbar (progress-bar-new pbar-adj))
	 (user-label (label-new "")))
  
    (let ((frame (frame-new "Progress"))
	  (vbox (vbox-new nil 5)))
      (box-pack-start main-box frame nil t 0)
      (container-add frame vbox)
      
      (let ((timer (timeout-add
		    100
		    #'(lambda ()
			(let* ((value (adjustment-value pbar-adj))
			       (new-value
				(if (= value (adjustment-upper pbar-adj))
				    (adjustment-lower pbar-adj)
				  (1+ value))))
			  (setf (progress-value pbar) new-value))
			t))))
	(signal-connect window 'destroy #'(lambda () (timeout-remove timer))))
	
      (signal-connect
       pbar-adj 'value-changed
       #'(lambda ()
	   (setf
	    (label-text user-label)
	    (if (progress-activity-mode-p pbar)
		"???"
	      (format nil "~D" (round (* 100 (progress-percentage pbar))))))))

      (setf (progress-format-string pbar) "%v from [%l,%u] (=%p%%)")
      
      (let ((align (alignment-new 0.5 0.5 0.0 0.0)))
	(box-pack-start vbox align nil nil 0)
	(container-add align pbar))
      
      (let ((hbox (hbox-new nil 5)))
	(box-pack-start hbox (label-new "Label updated by user :") nil t 0)
	(box-pack-start hbox user-label nil t 0)
	
	(let ((align (alignment-new 0.5 0.5 0.0 0.0)))
	  (box-pack-start vbox align nil nil 5)
	  (container-add align hbox))))
    
    (let ((frame (frame-new "Options"))
	  (vbox (vbox-new nil 5)))
      (box-pack-start main-box frame nil t 0)
      (container-add frame vbox)

      (let ((table (table-new 7 2 nil)))
	(box-pack-start vbox table nil t 0)

	(let ((label (label-new "Orientation :")))
	  (setf (misc-xalign label) 0.0)
	  (setf (misc-yalign label) 0.5)
	  (table-attach table label 0 1 0 1 :x-padding 5 :y-padding 5))
	
	(let ((hbox (hbox-new nil 0)))
	  (box-pack-start
	   hbox
	   (build-option-menu
	    `(("Left-Right"
	       ,#'(lambda ()
		    (setf (progress-bar-orientation pbar) :left-to-right)))
	      ("Right-Left"
	       ,#'(lambda ()
		    (setf (progress-bar-orientation pbar) :right-to-left)))
	      ("Bottom-Top"
	       ,#'(lambda ()
		    (setf (progress-bar-orientation pbar) :bottom-to-top)))
	      ("Top-Bottom"
	       ,#'(lambda ()
		    (setf (progress-bar-orientation pbar) :top-to-bottom))))
	    0)
	   t t 0)
	  (table-attach table hbox 1 2 0 1 :x-padding 5 :y-padding 5))
	
	(let* ((button (check-button-new "Show text"))
	       (entry (entry-new))
	       (x-align-adj (adjustment-new 0.5 0.0 1.0 0.1 0.1 0.0))
	       (x-align-spin (spin-button-new x-align-adj 0 1))
	       (y-align-adj (adjustment-new 0.5 0.0 1.0 0.1 0.1 0.0))
	       (y-align-spin (spin-button-new y-align-adj 0 1)))
	       
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (let ((state (toggle-button-active-p button)))
		 (setf (progress-show-text-p pbar) state)
		 (setf (widget-sensitive-p entry) state)
		 (setf (widget-sensitive-p x-align-spin) state)
		 (setf (widget-sensitive-p y-align-spin) state))))
	  (table-attach table button 0 1 1 2 :x-padding 5 :y-padding 5)

	  (signal-connect
	   entry 'changed
	   #'(lambda ()
	       (setf
		(progress-format-string pbar)
		(entry-text entry))))     
	  (setf (entry-text entry) "%v from [%l,%u] (=%p%%)")
	  (setf (widget-width entry) 100)
	  (setf (widget-sensitive-p entry) nil)
	
	  (let ((hbox (hbox-new nil 0)))
	    (box-pack-start hbox (label-new "Format : ") nil t 0)
	    (box-pack-start hbox entry t t 0)
	    (table-attach table hbox 1 2 1 2 :x-padding 5 :y-padding 5))

	  (let ((label (label-new "Text align :")))
	    (setf (misc-xalign label) 0.0)
	    (setf (misc-yalign label) 0.5)
	    (table-attach table label 0 1 2 3 :x-padding 5 :y-padding 5))

	  (flet ((adjust-align ()
  	           (setf
		    (progress-text-xalign pbar)
		    (spin-button-value x-align-spin))
		   (setf
		    (progress-text-yalign pbar)
		    (spin-button-value y-align-spin))))
	    (signal-connect x-align-adj 'value-changed #'adjust-align)
	    (signal-connect y-align-adj 'value-changed #'adjust-align))
	  (setf (widget-sensitive-p x-align-spin) nil)
	  (setf (widget-sensitive-p y-align-spin) nil)
	  
	  (let ((hbox (hbox-new nil 0)))
	    (box-pack-start hbox (label-new "x :") nil t 5)
	    (box-pack-start hbox x-align-spin nil t 0)
	    (box-pack-start hbox (label-new "y :") nil t 5)
	    (box-pack-start hbox y-align-spin nil t 0)
	    (table-attach table hbox 1 2 2 3 :x-padding 5 :y-padding 5)))

	(let ((label (label-new "Bar Style :")))
	  (setf (misc-xalign label) 0.0)
	  (setf (misc-yalign label) 0.5)
	  (table-attach table label 0 1 3 4 :x-padding 5 :y-padding 5))

	(let* ((block-adj (adjustment-new 10 2 20 1 5 0))
	       (block-spin (spin-button-new block-adj 0 0)))
	  (let ((hbox (hbox-new nil 0)))
	    (box-pack-start
	     hbox
	     (build-option-menu
	      `(("Continuous"
		 ,#'(lambda ()
		      (setf (progress-bar-style pbar) :continuous)
		      (setf (widget-sensitive-p block-spin) nil)))
		("Discrete"
		 ,#'(lambda ()
		      (setf (progress-bar-style pbar) :discrete)
		      (setf (widget-sensitive-p block-spin) t))))
	      0)
	     t t 0)
	    (table-attach table hbox 1 2 3 4 :x-padding 5 :y-padding 5))
	
	  (let ((label (label-new "Block count :")))
	    (setf (misc-xalign label) 0.0)
	    (setf (misc-yalign label) 0.5)
	    (table-attach table label 0 1 4 5 :x-padding 5 :y-padding 5))

	  (signal-connect
	   block-adj 'value-changed
	   #'(lambda ()
	       (setf (progress-percentage pbar) 0)
	       (setf
		(progress-bar-discrete-blocks pbar)
		(spin-button-value-as-int block-spin))))
	  (setf (widget-sensitive-p block-spin) nil)
	    
	  (let ((hbox (hbox-new nil 0)))
	    (box-pack-start hbox block-spin nil t 0)
	    (table-attach table hbox 1 2 4 5 :x-padding 5 :y-padding 5)))

	(let* ((step-size-adj (adjustment-new 3 1 20 1 5 0))
	       (step-size-spin (spin-button-new step-size-adj 0 0))
	       (block-adj (adjustment-new 5 2 10 1 5 00))
	       (block-spin (spin-button-new block-adj 0 0)))
	
	(let ((button (check-button-new "Activity mode")))
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (let ((state (toggle-button-active-p button)))
		 (setf (progress-activity-mode-p pbar) state)
		 (setf (widget-sensitive-p step-size-spin) state)
		 (setf (widget-sensitive-p block-spin) state))))
	  (table-attach table button 0 1 5 6 :x-padding 5 :y-padding 5))

	(signal-connect
	 step-size-adj 'value-changed
	 #'(lambda ()
	     (setf
	      (progress-bar-activity-step pbar)
	      (spin-button-value-as-int step-size-spin))))
	(setf (widget-sensitive-p step-size-spin) nil)

	(let ((hbox (hbox-new nil 0)))
	  (box-pack-start hbox (label-new "Step size : ") nil t 0)
	  (box-pack-start hbox step-size-spin nil t 0)
	  (table-attach table hbox 1 2 5 6 :x-padding 5 :y-padding 5))

	(signal-connect
	 block-adj 'value-changed
	 #'(lambda ()
	     (setf
	      (progress-bar-activity-blocks pbar)
	      (spin-button-value-as-int block-spin))))
	(setf (widget-sensitive-p block-spin) nil)

	(let ((hbox (hbox-new nil 0)))
	  (box-pack-start hbox (label-new "Blocks :     ") nil t 0)
	  (box-pack-start hbox block-spin nil t 0)
	  (table-attach table hbox 1 2 6 7 :x-padding 5 :y-padding 5)))))))
      


;;; Radio buttons

(define-standard-dialog create-radio-buttons "Radio buttons"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (let* ((button1 (radio-button-new nil :label "button1"))
	 (button2 (radio-button-new
		   (radio-button-group button1) :label "button2"))
	 (button3 (radio-button-new
		   (radio-button-group button2) :label "button3")))
    (box-pack-start main-box button1 t t 0)
    (box-pack-start main-box button2 t t 0)
    (setf (toggle-button-active-p button2) t)
    (box-pack-start main-box button3 t t 0)))



;;; Rangle controls

(define-standard-dialog create-range-controls "Range controls"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (let ((adjustment (adjustment-new 0.0 0.0 101.0 0.1 1.0 1.0)))

    (let ((scale (hscale-new adjustment)))
      (setf (widget-width scale) 150)
      (setf (widget-height scale) 30)
      (setf (range-update-policy scale) :delayed)
      (setf (scale-digits scale) 1)
      (setf (scale-draw-value-p scale) t)
      (box-pack-start main-box scale t t 0))
    
    (let ((scrollbar (hscrollbar-new adjustment)))
      (setf (range-update-policy scrollbar) :continuous)
      (box-pack-start main-box scrollbar t t 0))))



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

  (let ((table (table-new 2 2 nil)))
    (container-add window table)
    (widget-show table)

    (let ((ruler (hruler-new)))
      (setf (ruler-metric ruler) :centimeters)
      (ruler-set-range ruler 100 0 0 20)
      (signal-connect
       window 'motion-notify-event
       #'(lambda (event) (widget-event ruler event)))
      (table-attach table ruler 1 2 0 1 :y-options '(:fill))
      (widget-show ruler))

    (let ((ruler (vruler-new)))
      (ruler-set-range ruler 5 15 0 20)
      (signal-connect
       window 'motion-notify-event
       #'(lambda (event) (widget-event ruler event)))
      (table-attach table ruler 0 1 1 2 :x-options '(:fill))
      (widget-show ruler))))



;;; Scrolled window

(define-standard-dialog create-scrolled-windows "Scrolled windows"
  (let ((scrolled-window (scrolled-window-new nil nil)))
    (setf (container-border-width scrolled-window) 10)
    (setf (scrolled-window-scrollbar-policy scrolled-window) :automatic)
    (box-pack-start main-box scrolled-window t t 0)

    (let ((table (table-new 20 20 nil)))
      (setf (table-row-spacings table) 10)
      (setf (table-column-spacings table) 10)
      (scrolled-window-add-with-viewport scrolled-window table)
      (setf
       (container-focus-vadjustment table)
       (scrolled-window-vadjustment scrolled-window))
      (setf
       (container-focus-hadjustment table)
       (scrolled-window-hadjustment scrolled-window))
      
      (dotimes (i 20)
	(dotimes (j 20)
	  (let ((button
		 (toggle-button-new (format nil "button (~D,~D)~%" i j))))
	    (table-attach table button i (1+ i) j (1+ j)))))))
  
  (let ((button (button-new "remove")))
    (signal-connect button 'clicked #'(lambda ()))
    (setf (widget-can-default-p button) t)
    (box-pack-start action-area button t t 0)
    (widget-grab-default button))

  (setf (window-default-height window) 300)
  (setf (window-default-width window) 300))



;;; Shapes

(defun shape-create-icon (xpm-file x y px py window-type root-window)
  (let ((window (window-new window-type))
	(fixed (fixed-new)))
    (setf (widget-width fixed) 100)
    (setf (widget-height fixed) 100)
    (container-add window fixed)
    (widget-show fixed)
    
    (setf
     (widget-events window)
     (append
      (widget-events window)
      '(:button-motion :pointer-motion-hint :button-press)))
    (widget-realize window)
    
    (multiple-value-bind (gdk-pixmap gdk-pixmap-mask)
	(gdk:pixmap-create xpm-file)
      (let ((pixmap (pixmap-new (list gdk-pixmap gdk-pixmap-mask)))
	    (x-offset 0)
	    (y-offset 0))
	(declare (fixnum x-offset y-offset))
	(fixed-put fixed pixmap px py)
	(widget-show pixmap)
	(widget-shape-combine-mask window gdk-pixmap-mask px py)
	(signal-connect
	 window 'button-press-event
	 #'(lambda (event)
	     (when (eq (gdk:event-type event) :button-press)
	       (setq x-offset (truncate (gdk:event-x event)))
	       (setq y-offset (truncate (gdk:event-y event)))
	       (grab-add window)
	       (gdk:pointer-grab
		(widget-window window) t
		'(:button-release :button-motion :pointer-motion-hint)
		nil nil 0))
	     t))

	(signal-connect
	 window 'button-release-event
	 #'(lambda (event)
	     (declare (ignore event))
	     (grab-remove window)
	     (gdk:pointer-ungrab 0)
	     t))
	
	(signal-connect
	 window 'motion-notify-event
	 #'(lambda (event)
	     (declare (ignore event))
	     (multiple-value-bind (win xp yp mask)
		 (gdk:window-get-pointer root-window)
	       (declare (ignore mask win) (fixnum xp yp))
	       (widget-set-uposition
		window :x (- xp x-offset) :y (- yp y-offset)))
	     t))))
    
    (widget-set-uposition window :x x :y y)
    (widget-show window)
    window))


(let ((modeller nil)
      (sheets nil)
      (rings nil))
  (defun create-shapes ()
    (let ((root-window (gdk:get-root-window)))
      (if (not modeller)
	  (progn
	    (setq
	     modeller
	     (shape-create-icon
	      "cl-gtk:src;Modeller.xpm"
	      440 140 0 0 :popup root-window))
	    (signal-connect
	     modeller 'destroy
	     #'(lambda () (widget-destroyed modeller))))
	(widget-destroy modeller))

      (if (not sheets)
	  (progn
	    (setq
	     sheets
	     (shape-create-icon
	      "cl-gtk:src;FilesQueue.xpm"
	      580 170 0 0 :popup root-window))
	    (signal-connect
	     sheets 'destroy
	     #'(lambda () (widget-destroyed sheets))))
	(widget-destroy sheets))

      (if (not rings)
	  (progn
	    (setq
	     rings
	     (shape-create-icon
	      "cl-gtk:src;3DRings.xpm"
	      460 270 25 25 :toplevel root-window))
	    (signal-connect
	     rings 'destroy
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
		       (adjustment-new 1 1 31 1 5 0) 0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :out)
	(box-pack-start vbox2 spinner nil t 0))
    
      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Month :"))
	     (spinner (spin-button-new
		       (adjustment-new 1 1 12 1 5 0) 0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :etched-in)
	(box-pack-start vbox2 spinner nil t 0))

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Year :"))
	     (spinner (spin-button-new
		       (adjustment-new 1998 0 2100 1 100 0) 0 0)))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner) t)
	(setf (spin-button-shadow-type spinner) :in)
	(box-pack-start vbox2 spinner nil t 0)))

    (let* ((frame (frame-new "Accelerated"))
	   (vbox (vbox-new nil 0))
	   (hbox (hbox-new nil 0))
	   (spinner1 (spin-button-new
		      (adjustment-new 0 -10000 10000 0.5 100 0) 1.0 2))
	   (adj (adjustment-new 2 1 5 1 1 0))
	   (spinner2 (spin-button-new adj 1.0 0)))
	  
      (box-pack-start main-vbox frame t t 0)
      (setf (container-border-width vbox) 5)
      (container-add frame vbox)
      (box-pack-start vbox hbox nil t 5)

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Value :")))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0)
	(setf (misc-yalign label) 0.5)
	(box-pack-start vbox2 label nil t 0)
	(setf (spin-button-wrap-p spinner1) t)
	(setf (widget-width spinner1) 100)
	(setf (widget-height spinner1) 0)
	(box-pack-start vbox2 spinner1 nil t 0))

      (let* ((vbox2 (vbox-new nil 0))
	     (label (label-new "Digits :")))
	(box-pack-start hbox vbox2 t t 5)
	(setf (misc-xalign label) 0)
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
		(label-text val-label)
		(format nil "~D" (spin-button-value-as-int spinner1)))))
	  (box-pack-start hbox button t t 5))
	
	(let ((button (button-new "Value as Float")))
	  (signal-connect
	   button 'clicked
	   #'(lambda ()
	       (setf
		(label-text val-label)
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
  (let ((box1 (vbox-new nil 0)))
    (container-add window box1)

    (let ((box2 (vbox-new nil 10))
	  (statusbar (statusbar-new))
	  (statusbar-counter 0))
      (setf (container-border-width box2) 10)
      (box-pack-start box1 box2 t t 0)
      (box-pack-end box1 statusbar t t 0)
      (signal-connect
       statusbar 'text-popped
       #'(lambda (context-id text)
	   (declare (ignore context-id))
	   (format nil "Popped: ~A~%" text)))

      (make-button
       :label "push something"
       :visible t
       :parent box2
       :signal (list
		 'clicked
		 #'(lambda ()
		     (statusbar-push
		      statusbar
		      1
		      (format nil "something ~D" (incf statusbar-counter))))))
      
      (make-button
       :label "pop"
       :visible t
       :parent box2
       :signal (list
		'clicked
		#'(lambda ()
		    (statusbar-pop statusbar 1))
		:after t))
      
      (make-button
       :label "steal #4"
       :visible t
       :parent box2
       :signal (list
		'clicked
		#'(lambda ()
		    (statusbar-remove statusbar 1 4))
		:after t))

      (make-button :label "test contexts"
		   :visible t
		   :parent box2
		   :signal (list 'clicked #'(lambda ()))))

    (box-pack-start box1 (hseparator-new) nil t 0)

    (let ((box2 (vbox-new nil 10)))
      (setf (container-border-width box2) 10)
      (box-pack-start box1 box2 nil t 0)

      (let ((button (button-new "close")))
	(signal-connect button 'clicked #'(lambda () (widget-destroy window)))
	(box-pack-start box2 button t t 0)
	(setf (widget-can-default-p button) t)
	(widget-grab-default button)))))



;;; Idle test

(define-standard-dialog create-idle-test "Idle Test"
  (let ((label (label-new "count: 0"))
	(idle nil)
	(count 0))
    (declare (fixnum count))
    (signal-connect
     window 'destroy #'(lambda () (when idle (idle-remove idle))))
 
    (setf (misc-xpad label) 10)
    (setf (misc-ypad label) 10)
    (box-pack-start main-box label t t 0)

    (let* ((container (make-hbox :parent main-box :child label :visible t))
	   (frame (make-frame
		   :border-width 5
		   :label "Label Container"
		   :visible t
		   :parent main-box))
	   (box (make-vbox :visible t :parent frame)))
      (make-check-button
       :label "Resize-Parent"
       :visible t
       :parent box
       :signal
       (list
	'clicked
	#'(lambda ()
	    (setf (container-resize-mode container) :parent))))
      
      (make-check-button
       :label "Resize-Queue"
       :visible t
       :parent box
       :signal
       (list
	'clicked
	#'(lambda ()
	    (setf (container-resize-mode container) :queue))))
      
      (make-check-button
       :label "Resize-Immediate"
       :visible t
       :parent box
       :signal
       (list
	'clicked
	#'(lambda ()
	    (setf (container-resize-mode container) :immediate)))))

    (let ((button (button-new "start")))
      (signal-connect
       button 'clicked
       #'(lambda ()
       (unless idle
	 (setq
	  idle
	  (idle-add
	   #'(lambda ()
	       (incf count)
	       (setf (label-text label) (format nil "count: ~D" count))
	       t))))))
      (setf (widget-can-default-p button) t)
      (box-pack-start action-area button t t 0)
      (widget-show button))
      
    (let ((button (button-new "stop")))
      (signal-connect
       button 'clicked
       #'(lambda ()
       (when idle
	 (idle-remove idle)
	 (setq idle nil))))
      (setf (widget-can-default-p button) t)
      (box-pack-start action-area button t t 0)
      (widget-show button))))
    


;;; Timeout test

(define-standard-dialog create-timeout-test "Timeout Test"
  (let ((label (label-new "count: 0"))
	(timer nil)
	(count 0))
    (declare (fixnum count))
    (signal-connect
     window 'destroy #'(lambda () (when timer (timeout-remove timer))))
      
    (setf (misc-xpad label) 10)
    (setf (misc-ypad label) 10)
    (box-pack-start main-box label t t 0)
    (widget-show label)
      
    (let ((button (button-new "start")))
      (signal-connect
       button 'clicked
       #'(lambda ()
       (unless timer
	 (setq
	  timer
	  (timeout-add
	   100
	   #'(lambda ()
	       (incf count)
	       (setf (label-text label) (format nil "count: ~D" count))
	       t))))))
      (setf (widget-can-default-p button) t)
      (box-pack-start action-area button t t 0)
      (widget-show button))
      
    (let ((button (button-new "stop")))
      (signal-connect
       button 'clicked
       #'(lambda ()
       (when timer
	 (timeout-remove timer)
	 (setq timer nil))))
      (setf (widget-can-default-p button) t)
      (box-pack-start action-area button t t 0)
      (widget-show button))))
  


;;; Text

(define-test-window create-text "Text"
  (setf (widget-name window) "text window")
  (setf (widget-width window) 500)
  (setf (widget-height window) 500)
  (setf (window-allow-grow-p window) t)
  (setf (window-allow-shrink-p window) t)
  (setf (window-auto-shrink-p window) nil)
  (let ((box1 (vbox-new nil 0)))
    (container-add window box1)
    
    (let ((box2 (vbox-new nil 10)))
      (setf (container-border-width box2) 10)
      (box-pack-start box1 box2 t t 0)

      (let ((scrolled-window (scrolled-window-new))
	    (text (text-new)))
	(box-pack-start box2 scrolled-window t t 0)
	(setf (scrolled-window-hscrollbar-policy scrolled-window) :never)
	(setf (scrolled-window-vscrollbar-policy scrolled-window) :always)
	(setf (editable-editable-p text) t)
	(container-add scrolled-window text)
	(widget-grab-focus text)
	
	(text-freeze text)
	(let ((font
	       (gdk:font-load
		"-adobe-courier-medium-r-normal--*-120-*-*-*-*-*-*"))
	      (colors
	       (map 'list
		    #'(lambda (definition)
			(cons
			 (gdk:color-new-from-vector (first definition))
			 (second definition)))
		    '((#(#x0000 #x0000 #x0000) "black")
		      (#(#xFFFF #xFFFF #xFFFF) "white")
		      (#(#xFFFF #x0000 #x0000) "red")
		      (#(#x0000 #xFFFF #x0000) "green")
		      (#(#x0000 #x0000 #xFFFF) "blue")
		      (#(#x0000 #xFFFF #xFFFF) "cyan")
		      (#(#xFFFF #x0000 #xFFFF) "magneta")
		      (#(#xFFFF #xFFFF #x0000) "yellow")))))
	  (dolist (color1 colors)
	    (text-insert text (format nil "~A~,7T" (cdr color1)) :font font)
	    (dolist (color2 colors)
	      (text-insert
	       text "XYZ" :font font
	       :foreground (car color2) :background (car color1)))
	    (text-insert text (format nil "~%")))
	  (dolist (color colors)
	    (gdk:color-destroy (car color)))
	  (gdk:font-unref font))
			 
	(with-open-file (file "cl-gtk:src;testgtk.lisp")
	  (labels ((read-file ()
		     (let ((line (read-line file nil nil)))
		       (when line
			 (text-insert text (format nil "~A~%" line))
			 (read-file)))))
	    (read-file)))

	(text-thaw text)

	(let ((hbox (hbutton-box-new)))
	  (box-pack-start box2 hbox nil nil 0)
	  (let ((check-button (check-button-new "Editable")))
	    (box-pack-start hbox check-button nil nil 0)
	    (signal-connect
	     check-button 'toggled
	     #'(lambda ()
		 (setf
		  (editable-editable-p text)
		  (toggle-button-active-p check-button))))
	    (setf (toggle-button-active-p check-button) t))

	  (let ((check-button (check-button-new "Wrap Words")))
	    (box-pack-start hbox check-button nil t 0)
	    (signal-connect
	     check-button 'toggled
	     #'(lambda ()
		 (setf
		  (text-word-wrap-p text)
		  (toggle-button-active-p check-button))))
	    (setf (toggle-button-active-p check-button) nil)))))

    (box-pack-start box1 (hseparator-new) nil t 0)

    (let ((box2 (vbox-new nil 10)))
      (setf (container-border-width box2) 10)
      (box-pack-start box1 box2 nil t 0)
      
      (let ((button (button-new "insert random")))
	(signal-connect button 'clicked #'(lambda () nil))
	(box-pack-start box2 button t t 0))

      (let ((button (button-new "close")))
	(signal-connect
	 button 'clicked
	 #'(lambda ()
	     (widget-destroy window)
	     (setq window nil)))
	(box-pack-start box2 button t t 0)
	(setf (widget-can-default-p button) t)
	(widget-grab-default button)))))
      


;;; Toggle buttons

(define-standard-dialog create-toggle-buttons "Toggle Button"
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)
  (box-pack main-box (toggle-button-new "button1"))
  (box-pack main-box (toggle-button-new "button2"))
  (box-pack main-box (toggle-button-new "button3")))



;;; Toolbar test

(define-test-window create-toolbar "Toolbar test"
  (setf (window-allow-grow-p window) nil)
  (setf (window-allow-shrink-p window) t)
  (setf (window-auto-shrink-p window) t)
  (widget-realize window)


  (let ((toolbar (toolbar-new :horizontal :both)))
    (setf (toolbar-relief toolbar) :none)

    (toolbar-append-item
     toolbar "Horizontal" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Horizontal toolbar layout"
     :tooltip-private-text "Toolbar/Horizontal"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :horizontal)))

    (toolbar-append-item
     toolbar "Vertical" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Vertical toolbar layout"
     :tooltip-private-text "Toolbar/Vertical"
     :callback #'(lambda () (setf (toolbar-orientation toolbar) :vertical)))

    (toolbar-append-space toolbar)
    
    (toolbar-append-item
     toolbar "Icons" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Only show toolbar icons"
     :tooltip-private-text "Toolbar/IconsOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :icons)))
    
    (toolbar-append-item
     toolbar "Text" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Only show toolbar text"
     :tooltip-private-text "Toolbar/TextOnly"
     :callback #'(lambda () (setf (toolbar-style toolbar) :text)))
  
    (toolbar-append-item
     toolbar "Both" (pixmap-new "cl-gtk:src;test.xpm")
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
     toolbar "Small" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Use small spaces"
     :tooltip-private-text "Toolbar/Small"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 5)))
    
    (toolbar-append-item
     toolbar "Big" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Use big spaces"
     :tooltip-private-text "Toolbar/Big"
     :callback #'(lambda () (setf (toolbar-space-size toolbar) 10)))
    
    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Enable" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Enable tooltips"
     :callback #'(lambda () (toolbar-enable-tooltips toolbar)))

    (toolbar-append-item
     toolbar "Disable" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Disable tooltips"
     :callback #'(lambda () (toolbar-disable-tooltips toolbar)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Borders" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Show borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :normal)))
    
    (toolbar-append-item
     toolbar
     "Borderless" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Hide borders"
     :callback #'(lambda () (setf (toolbar-relief toolbar) :none)))

    (toolbar-append-space toolbar)

    (toolbar-append-item
     toolbar "Empty" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Empty spaces"
     :callback #'(lambda () (setf (toolbar-space-style toolbar) :empty)))

    (toolbar-append-item
     toolbar "Lines" (pixmap-new "cl-gtk:src;test.xpm")
     :tooltip-text "Lines in spaces"
     :callback #'(lambda () (setf (toolbar-space-style toolbar) :line)))

    (container-add window toolbar)))



;;; Tooltips test

(define-standard-dialog create-tooltips "Tooltips"
  (setf (window-allow-grow-p window) t)
  (setf (window-allow-shrink-p window) nil)
  (setf (window-auto-shrink-p window) t)
  (setf (widget-width window) 200)
  (setf (container-border-width main-box) 10)
  (setf (box-spacing main-box) 10)

  (let ((tooltips (tooltips-new)))

    (let ((button (toggle-button-new "button1")))
      (box-pack-start main-box button t t 0)
      (tooltips-set-tip
       tooltips button "This is button 1" "ContextHelp/button/1"))

    (let ((button (toggle-button-new "button2")))
      (box-pack-start main-box button t t 0)
      (tooltips-set-tip
       tooltips button "This is button 2. This is also a really long tooltip which probably won't fit on a single line and will therefore need to be wrapped. Hopefully the wrapping will work correctly."
       "ContextHelp/button/2"))

    (let ((toggle (toggle-button-new "Override TipSQuery Label")))
      (box-pack-start main-box toggle t t 0)
      (tooltips-set-tip
       tooltips toggle "Toggle TipsQuery view" "Hi msw! ;)")

      (let* ((box3 (make-vbox
		    :homogeneous nil
		    :spacing 5
		    :border-width 5
		    :visible t))
	     (tips-query (make-tips-query
			  :visible t
			  :parent box3))
	     (button (make-button
		      :label "[?]"
		      :visible t
		      :parent box3
		      :signal (list
			       'clicked #'tips-query-start-query
			       :object tips-query))))
	     
	(box-set-child-packing box3 button nil nil 0 :start)
	(tooltips-set-tip
	 tooltips button "Start the Tooltip Inspector" "ContextHelp/buttons/?")
	(setf (tips-query-caller tips-query) button)
	
	(signal-connect
	 tips-query 'widget-entered
	 #'(lambda (widget tip-text tip-private)
	     (declare (ignore widget tip-private))
	     (when (toggle-button-active-p toggle)
	       (setf
		(label-text tips-query)
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

	(let ((frame (make-frame
		      :label "ToolTips Inspector"
		      :label-xalign 0.5
		      :border-width 0
		      :visible t
		      :parent main-box
		      :child box3)))
	  (box-set-child-packing main-box frame t t 0 :start))

	(tooltips-set-tip
	 tooltips close-button "Push this button to close window"
	 "ContextHelp/buttons/Close")))))
		  


;;; Tree

(defconstant +default-number-of-items+ 3)
(defconstant +default-recursion-level+ 3)

(defun create-subtree (item level nb-item-max recursion-level-max)
  (unless (and level (= level recursion-level-max))
    (multiple-value-bind (level item-subtree no-root-item)
	(if (not level)
	    (values 0 item t)
	  (values level (tree-new) nil))
      
      (dotimes (nb-item nb-item-max)
	(let ((new-item
	       (tree-item-new (format nil "item ~D-~D" level nb-item))))
	  (tree-append item-subtree new-item)
	  (create-subtree
	   new-item (1+ level) nb-item-max recursion-level-max)
	  (widget-show new-item)))

      (unless no-root-item
	(setf (tree-item-subtree item) item-subtree)))))
  

(defun create-tree-sample (selection-mode draw-line view-line no-root-item
			   nb-item-max recursion-level-max)
  (let ((window (window-new :toplevel)))
    (setf (window-title window) "Tree Sample")
    (signal-connect window 'destroy #'(lambda ()))
		    
    (let ((box1 (vbox-new nil 0))
	  (root-tree (tree-new))
	  (add-button (button-new "Add Item"))
	  (remove-button (button-new "Remove Item(s)"))
	  (subtree-button (button-new "Remove Subtree")))
      (container-add window box1)
      (widget-show box1)

      (let ((box2 (vbox-new nil 0))
	    (scrolled-win (scrolled-window-new nil nil)))
	(box-pack box1 box2)
	(setf (container-border-width box2) 5)
	(widget-show box2)
	(setf (scrolled-window-scrollbar-policy scrolled-win) :automatic)
	(box-pack box2 scrolled-win)
	(setf (widget-width scrolled-win) 200)
	(setf (widget-height scrolled-win) 200)
	(widget-show scrolled-win)
	(signal-connect
	 root-tree 'selection-changed
	 #'(lambda ()
	     (format t "Selection: ~A~%" (tree-selection root-tree))
	     (let ((nb-selected (length (tree-selection root-tree))))
	       (if (zerop nb-selected)
		   (progn
		     (if (container-children root-tree)
			 (setf (widget-sensitive-p add-button) t)
		       (setf (widget-sensitive-p add-button) nil))
		     (setf (widget-sensitive-p remove-button) nil)
		     (setf (widget-sensitive-p subtree-button) nil))
		 (progn
		   (setf (widget-sensitive-p remove-button) t)
		   (setf (widget-sensitive-p add-button) (= 1 nb-selected))
		   (setf
		    (widget-sensitive-p subtree-button) (= 1 nb-selected)))))))
	(scrolled-window-add-with-viewport scrolled-win root-tree)
	(setf (tree-selection-mode root-tree) selection-mode)
	(setf (tree-view-lines-p root-tree) draw-line)
	(setf (tree-view-mode root-tree) (if view-line :line :item))
	(widget-show root-tree)

	(let ((root-item
	       (if no-root-item
		   root-tree
		 (let ((root-item (tree-item-new "root item")))
		   (tree-append root-tree root-item)
		   (widget-show root-item)
		   root-item))))
	  (create-subtree
	   root-item (if no-root-item nil 0) nb-item-max recursion-level-max)))
	  
      (let ((box2 (vbox-new nil 0)))
	(box-pack-start box1 box2 nil nil 0)
	(setf (container-border-width box2) 5)
	(widget-show box2)

	(setf (widget-sensitive-p add-button) nil)
	(let ((nb-item-add 0))
	  (signal-connect
	   add-button 'clicked
	   #'(lambda ()
	       (let* ((selected-list (tree-selection root-tree))
		      (subtree (if (not selected-list)
				   root-tree
				 (let ((selected-item (first selected-list)))
				   (or
				    (tree-item-subtree selected-item)
				    (let ((subtree (tree-new)))
				      (setf
				       (tree-item-subtree selected-item)
				       subtree)
				      subtree)))))
		      (new-item
		       (tree-item-new (format nil "item add ~D" nb-item-add))))
		 (tree-append subtree new-item)
		 (widget-show new-item)
		 (incf nb-item-add)))))
	(box-pack-start box2 add-button t t 0)
	(widget-show add-button)

	(setf (widget-sensitive-p remove-button) nil)
	(signal-connect
	 remove-button 'clicked
	 #'(lambda ()
	     (format t "Remove: ~A~%" (tree-selection root-tree))
	     (tree-remove-items root-tree (tree-selection root-tree))))
	(box-pack-start box2 remove-button t t 0)
	(widget-show remove-button)
	
	(setf (widget-sensitive-p subtree-button) nil)
	(signal-connect
	 subtree-button 'clicked
	 #'(lambda ()
	     (let ((selected-list (tree-selection root-tree)))
	       (when selected-list
		 (let ((item (first selected-list)))
		   (when item
		     (setf (tree-item-subtree item) nil)))))))
	(box-pack-start box2 subtree-button t t 0)
	(widget-show subtree-button))
      
      (let ((separator (hseparator-new)))
	(box-pack-start box1 separator nil nil 0)
	(widget-show separator))

      (let ((box2 (vbox-new nil 0))
	    (button (button-new "Close")))
	(box-pack-start box1 box2 nil nil 0)
	(setf (container-border-width box2) 5)
	(widget-show box2)
	(box-pack-start box2 button t t 0)
	(signal-connect button 'clicked
			#'(lambda ()
			    (widget-destroy window)))
	(widget-show button)))

    (widget-show window)))


(define-test-window create-tree "Set Tree Parameters"
  (let ((box1 (vbox-new nil 0)))
    (container-add window box1)

    (let ((box2 (vbox-new nil 5)))
      (box-pack box1 box2)
      (setf (container-border-width box2) 5)
      
      (let ((box3 (hbox-new nil 5)))
	(box-pack box2 box3)

	(let* ((single-button (radio-button-new nil :label "SIGNLE"))
	       (browse-button
		(radio-button-new
		 (radio-button-group single-button) :label "BROWSE"))
	       (multiple-button
		(radio-button-new
		 (radio-button-group single-button) :label "MULTIPLE"))
	       (draw-line-button (check-button-new "Draw line"))
	       (view-line-button (check-button-new "View Line mode"))
	       (no-root-item-button (check-button-new "Without Root item"))
	       (num-of-items-spinner
		(spin-button-new
		 (adjustment-new
		  +default-number-of-items+ 1 255 1 5 0)
		 0 0))
	       (depth-spinner
		(spin-button-new
		 (adjustment-new
		  +default-recursion-level+ 0 255 1 5 0)
		 5 0)))
	
	  (let ((frame (frame-new "Selection Mode"))
		(box4 (vbox-new nil 0)))
	    (box-pack box3 frame)
	    (container-add frame box4)
	    (setf (container-border-width box4) 5)
	    (box-pack box4 single-button)
	    (box-pack box4 browse-button)
	    (box-pack box4 multiple-button))
	  
	  (let ((frame (frame-new "Options"))
		(box4 (vbox-new nil 0)))
	    (box-pack box3 frame)
	    (container-add frame box4)
	    (setf (container-border-width box4) 5)
	    (box-pack box4 draw-line-button)
	    (box-pack box4 view-line-button)
	    (box-pack box4 no-root-item-button)
	    (setf (toggle-button-active-p draw-line-button) t)
	    (setf (toggle-button-active-p view-line-button) t)
	    (setf (toggle-button-active-p no-root-item-button) nil))

	  (let ((frame (frame-new "Size Parameters"))
		(box4 (vbox-new nil 5)))
	    (box-pack box2 frame)
	    (container-add frame box4)
	    (setf (container-border-width box4) 5)
      
	    (let ((box5 (hbox-new nil 5)))
	      (box-pack box4 box5 :expand nil :fill nil)
	      (let ((label (label-new "Number of items : ")))
		(setf (misc-xalign label) 0)
		(setf (misc-yalign label) 0.5)
		(box-pack box5 label :expand nil)
		(box-pack box5 num-of-items-spinner :expand nil))
	      (let ((label (label-new "Depth : ")))
		(setf (misc-xalign label) 0)
		(setf (misc-yalign label) 0.5)
		(box-pack box5 label :expand nil)
		(box-pack box5 depth-spinner :expand nil))))

	  (box-pack box1 (hseparator-new) :expand nil :fill nil)

	  (let ((box2 (hbox-new t 10)))
	    (box-pack box1 box2)
	    (setf (container-border-width box2) 5)
	    (let ((button (button-new "Create Tree")))
	      (box-pack box2 button)
	      (signal-connect
	       button 'clicked
	       #'(lambda ()
		   (let ((selection-mode
			  (cond
			   ((toggle-button-active-p single-button) :single)
			   ((toggle-button-active-p browse-button) :browse)
			   (t :multiple)))
			 (draw-line
			  (toggle-button-active-p draw-line-button))
			 (view-line
			  (toggle-button-active-p view-line-button))
			 (no-root-item
			  (toggle-button-active-p no-root-item-button))
			 (num-of-items
			  (spin-button-value-as-int num-of-items-spinner))
			 (depth
			  (spin-button-value-as-int depth-spinner)))
		     
		     (if (> (expt num-of-items depth) 10000)
			 (format t "~D total items? That will take a very long time. Try less~%" (expt num-of-items depth))
		       (create-tree-sample
			selection-mode draw-line view-line no-root-item
			num-of-items depth))))))
	    (let ((button (button-new "Close")))
	      (box-pack box2 button)
	      (signal-connect
	       button 'clicked #'widget-destroy :object window))))))))



;;; Main window
      
(defun create-main-window ()
  (let* ((buttons
	  '(("button box" create-button-box)
	    ("buttons" create-buttons)
	    ("calendar" create-calendar)
	    ("check buttons" create-check-buttons)
	    ("clist" create-clist)
	    ("color selection" create-color-selection)
	    ("ctree" create-ctree)
	    ("cursors" create-cursors)
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
	    ("progress bar" create-progress-bar)
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
	    ("text" create-text)
	    ("toggle buttons" create-toggle-buttons)
	    ("toolbar" create-toolbar)
	    ("tooltips" create-tooltips)
	    ("tree" create-tree)
	    ("WM hints")))
	 (main-window (make-instance 'window
		        :type :toplevel :title "testgtk.lisp"
			:name "main window" :x 20 :y 20 :width 200 :height 400
			:allow-grow nil :allow-shrink nil :auto-shrink nil))
	 (scrolled-window (make-instance 'scrolled-window
			   :hscrollbar-policy :automatic
			   :vscrollbar-policy :automatic
			   :border-width 10))
	 (close-button (make-instance 'button
		        :label "close"
			:can-default t ;:has-default t
			:signals
			(list
			 (list
			  'clicked #'widget-destroy :object main-window)))))

    ;; Main box
    (make-instance 'vbox
     :parent main-window
     :children
     (list 
      (list
       (make-instance 'label :label (gtk-version))
       :expand nil :fill nil)
      (list
       (make-instance 'label :label (format nil "clg CVS version"))
       :expand nil :fill nil)
      scrolled-window
      (list (make-instance 'hseparator) :expand nil)
      (list
       (make-instance 'vbox
	:homogeneous nil :spacing 10 :border-width 10
	:children (list (list close-button :expand t :fill t)))
       :expand nil)))

    (let ((button-box
	   (make-instance 'vbox
	    :border-width 10
	    :focus-vadjustment (scrolled-window-vadjustment scrolled-window)
	    :children
	    (map
	     'list
	     #'(lambda (button)
		 (let ((widget (make-instance 'button :label (first button))))
		   (if (second button)
		       (signal-connect widget 'clicked (second button))
		     (setf (widget-sensitive-p widget) nil))
		   widget))
	     buttons))))
    
      (scrolled-window-add-with-viewport scrolled-window button-box))
    
    (widget-grab-default close-button)
    (widget-show-all main-window)
    main-window))
 
;(gdk:rgb-init)
(rc-parse "cl-gtk:src;testgtkrc2")
(rc-parse "cl-gtk:src;testgtkrc")


;(create-main-window)

