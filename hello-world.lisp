(use-package :gtk)

(make-instance 'window
; :type :toplevel
 :title "Test"
 :border-width 5
 :show-all t
 :child (make-instance 'button
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
