(defparameter *pkg-config* "/usr/bin/pkg-config")

(defun split-string (string &key (start 0) end)
  (let ((position (position #\sp string :start start :end end)))
    (if position
	(cons
	 (subseq string start position)
	 (split-string string :start (1+ position) :end end))
      (list (subseq string start end)))))

(defun run-pkg-config (package &rest options)
  (let ((process
	 (run-program
	  *pkg-config* (cons package options) :wait t :output :stream)))
    (unless process
      (error "Unable to run ~A" *pkg-config*))
    (unless (zerop (process-exit-code process))
      (error "~A: ~A" *pkg-config* (read-line (process-output process))))
    (prog1
	(delete-if #'(lambda (str) (string= str "")) (split-string (read-line (process-output process))))
      (process-close process))))


(defun pkg-cflags (package)
  (run-pkg-config package "--cflags"))
