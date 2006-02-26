(gtk:clg-init)

(make-instance 'gtk:window
; :type :toplevel
 :title "Test"
 :border-width 5
 :visible t :show-children t
 :child (make-instance 'gtk:button
	 :label "Hello World!"
	 :signal (list 'clicked
		       #'(lambda ()
			   (write-line "Hello World!"))))
 :signal (list 'delete-event
	       #'(lambda (event)
		   (declare (ignore event))
		   (write-line "Destroying window")
		   nil ; Returning NIL generates a destroy event
		   )))
