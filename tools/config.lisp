(defun configure-cflags (config-program)
  (let ((process
	 (run-program
	  config-program '("--cflags") :wait t :output :stream)))
    (unless process
      (error "Unable to run %A" config-program))
    (labels ((split (string)
	       (let ((position (position #\sp string)))
		 (if position
		     (cons
		      (subseq string 0 position)
		      (split (subseq string (1+ position))))
		   (list string)))))
    (prog1
	(split (read-line (process-output process)))
      (process-close process)))))
