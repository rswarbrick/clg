(in-package "GTK")

(make-instance 'window
 :type :toplevel
 :title "Test"
 :border-width 5
 :show-all t
 :child (make-instance 'button
	 :label "Hello World!"
	 :signal (list 'clicked
		       #'(lambda (button)
			   (print button) (write-line "clicked"))
		       :object t)))


