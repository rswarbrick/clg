(use-package "GTK")

(make-instance 'window
 :type :toplevel
 :title "Test"
 :border-width 5
 :visible t
 :child (make-instance 'button
	 :label "Hello World!"
	 :visible t
	 :signals
	 (list (list 'clicked #'(lambda () (write-line "Button clicked"))))))



