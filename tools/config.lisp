(defpackage #:pkg-config
  (:use #:common-lisp)
  (:export #:pkg-cflags #:pkg-libs #:pkg-exists-p #:pkg-version 
	   #:pkg-variable))

(in-package #:pkg-config)

(defparameter *pkg-config* "pkg-config")

(defun split-string (string &key (start 0) (end (length string)))
  (let ((position (position #\sp string :start start :end end)))
    (cond
     ((zerop (- end start)) nil)
     ((not position) (list (subseq string start end)))
     ((= position start) (split-string string :start (1+ start) :end end))
     (t	(cons
	 (subseq string start position)
	 (split-string string :start (1+ position) :end end))))))


(defun read-lines (&optional (stream *standard-input*))
  (let ((line (read-line stream nil)))
    (when line
      (cons line (read-lines stream)))))


(defun read-string (&optional (stream *standard-input*)
		    (delimiter #\newline) (eof-error-p t) eof-value)
  (let ((string (make-array 0 :element-type 'character
			    :fill-pointer t :adjustable t)))
    ;; I really need to learn how to use the loop facility
    (labels ((read-chars ()
               (let ((char (read-char stream (and eof-error-p delimiter))))
		 (when char
		   (vector-push-extend char string)
		   (unless (eq char delimiter)
		     (read-chars))))))
      (read-chars))
    (cond
     ((not (zerop (length string))) string)
     ((not eof-error-p) eof-value)
     ((error 'end-of-file :stream stream)))))


(defun run-pkg-config (package error &rest options)
  (let ((process
	 (ext:run-program
	  *pkg-config* (cons package options) :wait t :output :stream)))
    (unless process
      (error "Unable to run ~A" *pkg-config*))
    (let ((exit-code (ext:process-exit-code process)))
      (unless (or (not error) (zerop exit-code))
	(error
	 (or
	  (read-string (ext:process-error process) nil)
	  (format nil "~A terminated with exit code ~A"
		  *pkg-config* exit-code))))
      (let ((output (read-lines (ext:process-output process))))	  
	(ext:process-close process)
	(values output exit-code)))))


(defun pkg-cflags (package)
  (split-string (first (run-pkg-config package t "--cflags"))))

(defun pkg-libs (package)
  (split-string (first (run-pkg-config package t "--libs"))))


(defun pkg-exists-p (package &key version atleast-version max-version
		     ( error t))
  (let ((version-check
	 (cond
	  (version (format nil "= ~A" version))
	  (atleast-version (format nil ">= ~A" atleast-version))
	  (max-version (format nil "<= ~A" max-version))
	  (t ""))))
    (if error
	(progn
	  (run-pkg-config package t "--print-errors" "--exists" version-check)
	  t)
      (multiple-value-bind (output exit-code)
	  (run-pkg-config package nil "--exists" version-check)
	(declare (ignore output))
	(zerop exit-code)))))


(defun pkg-version (package)
  (first (run-pkg-config package t "--modversion")))


(defun pkg-variable (package variable)
  (first (run-pkg-config package t "--variable" variable)))
