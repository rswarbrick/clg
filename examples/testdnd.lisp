;;;; Translation of dragndrop.py from the PyGTK 2.0 Tutorial

#+sbcl(require :gtk)
#+cmu(asdf:oos 'asdf:load-op :gtk)

(defpackage "TESTDND"
  (:use "COMMON-LISP" "GTK")
  (:export "CREATE-TEST"))

(in-package "TESTDND")


(defvar gtk-xpm
  #("32 39 5 1"
    ".      c none"
    "+      c black"
    "@      c #3070E0"
    "#      c #F05050"
    "$      c #35E035"
    "................+..............."
    "..............+++++............."
    "............+++++@@++..........."
    "..........+++++@@@@@@++........."
    "........++++@@@@@@@@@@++........"
    "......++++@@++++++++@@@++......."
    ".....+++@@@+++++++++++@@@++....."
    "...+++@@@@+++@@@@@@++++@@@@+...."
    "..+++@@@@+++@@@@@@@@+++@@@@@++.."
    ".++@@@@@@+++@@@@@@@@@@@@@@@@@@++"
    ".+#+@@@@@@++@@@@+++@@@@@@@@@@@@+"
    ".+##++@@@@+++@@@+++++@@@@@@@@$@."
    ".+###++@@@@+++@@@+++@@@@@++$$$@."
    ".+####+++@@@+++++++@@@@@+@$$$$@."
    ".+#####+++@@@@+++@@@@++@$$$$$$+."
    ".+######++++@@@@@@@++@$$$$$$$$+."
    ".+#######+##+@@@@+++$$$$$$@@$$+."
    ".+###+++##+##+@@++@$$$$$$++$$$+."
    ".+###++++##+##+@@$$$$$$$@+@$$@+."
    ".+###++++++#+++@$$@+@$$@++$$$@+."
    ".+####+++++++#++$$@+@$$++$$$$+.."
    ".++####++++++#++$$@+@$++@$$$$+.."
    ".+#####+++++##++$$++@+++$$$$$+.."
    ".++####+++##+#++$$+++++@$$$$$+.."
    ".++####+++####++$$++++++@$$$@+.."
    ".+#####++#####++$$+++@++++@$@+.."
    ".+#####++#####++$$++@$$@+++$@@.."
    ".++####++#####++$$++$$$$$+@$@++."
    ".++####++#####++$$++$$$$$$$$+++."
    ".+++####+#####++$$++$$$$$$$@+++."
    "..+++#########+@$$+@$$$$$$+++..."
    "...+++########+@$$$$$$$$@+++...."
    ".....+++######+@$$$$$$$+++......"
    "......+++#####+@$$$$$@++........"
    ".......+++####+@$$$$+++........."
    ".........++###+$$$@++..........."
    "..........++##+$@+++............"
    "...........+++++++.............."
    ".............++++..............."))


(defvar *target-type-text* 80)
(defvar *target-type-image* 81)

(defvar from-image 
  (list
   (make-instance 'target-entry :target "text/plain" :id *target-type-text*)
   (make-instance 'target-entry :target "image/png" :id *target-type-image*)))
(defvar to-button 
  (make-instance 'target-entry :target "text/plain" :id *target-type-text*))
(defvar to-canvas 
  (make-instance 'target-entry :target "image/png" :id *target-type-image*))



(defun add-image (layout pixbuf xd yd)
  (let ((button (make-instance 'button
		 :child (make-instance 'image :pixbuf pixbuf))))
    (widget-show-all button)

    (signal-connect button 'drag-data-get
     #'(lambda (context selection target-type time)
	 (declare (ignore context time))
	 (cond
	  ((= target-type *target-type-text*)
	   (selection-data-set-text selection 
	    #+cmu(ext:format-universal-time nil (get-universal-time) :style :rfc1123 :print-timezone nil)
	    #+sbcl(sb-int:format-universal-time nil (get-universal-time) :style :abbreviated :print-timezone nil)))
	  ((= target-type *target-type-image*)
	   (selection-data-set-pixbuf selection pixbuf)))))
     
    (drag-source-set button :button1 from-image :copy)
      
    (with-slots (hadjustment vadjustment) layout
      (layout-put layout button
       (truncate (+ xd (adjustment-value hadjustment)))
       (truncate (+ yd (adjustment-value vadjustment)))))))



(defun create-layout (width height)
  (let* ((table (make-instance 'table
	         :n-rows 2 :n-columns 2 :homogeneous nil))
	 (layout (make-instance 'layout :width width :height height))
	 (vscrollbar (make-instance 'v-scrollbar 
		      :adjustment (layout-vadjustment layout)))
	 (hscrollbar (make-instance 'h-scrollbar
		      :adjustment (layout-hadjustment layout)))
	 (button (make-instance 'button :label "Text target")))
    (table-attach table layout 0 1 0 1 :options '(:fill :expand))
    (table-attach table vscrollbar 1 2 0 1 :options '(:fill :shrink))
    (table-attach table hscrollbar 0 1 1 2 :options '(:fill :shrink))

    (signal-connect layout 'drag-data-received
     #'(lambda (context x y selection target-type time)
	 (declare (ignore context time))
	 (when (= target-type *target-type-image*)
	   (add-image layout (selection-data-get-pixbuf selection) x y))))

    (drag-dest-set layout '(:motion :highlight :drop) to-canvas :copy)

    (add-image layout (gdk:pixbuf-new-from-xpm-data gtk-xpm) 0 0)

    (signal-connect button 'drag-data-received
     #'(lambda (context x y selection target-type time)
	 (declare (ignore context x y time))
	 (when (= target-type *target-type-text*)
	   (setf 
	    (button-label button) 
	    (selection-data-get-text selection)))))

    (drag-dest-set button '(:motion :highlight :drop) to-button :copy)
 
    (make-instance 'v-box 
     :children (list (list table :expand t :fill t)
		     (list button :expand nil :fill nil)))))



(defun create-test ()
  (make-instance 'window
   :title "Drag and Drop Test"
   :visible t :show-children t
   :default-width 300 :default-height 300
   :child (create-layout 600 600)))

(clg-init)
(create-test)
