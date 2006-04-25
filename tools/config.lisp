(defpackage #:pkg-config
  (:use #:common-lisp #:clg-utils #+(or cmu clisp) #:ext #+sbcl #:sb-ext)
  #+sbcl
  (:import-from #:sb-int #:featurep)
  (:export #:pkg-cflags #:pkg-libs #:pkg-exists-p #:pkg-version #:pkg-variable)
  (:export #:featurep #:sbcl>=))

(in-package #:pkg-config)

(defparameter *pkg-config* "/usr/bin/pkg-config")


#+(or sbcl cmu)
(defun run-pkg-config (package error-p &rest options)
  (let ((process
	 (run-program
	  *pkg-config* (cons package options) :wait t :output :stream)))
    (unless process
      (error "Unable to run ~A" *pkg-config*))
    (let ((exit-code (process-exit-code process)))
      (unless (or (not error-p) (zerop exit-code))
	(error
	 (or
	  (format nil "~A: ~{~A~%~}" *pkg-config* (read-lines (process-error process)))
	  (format nil "~A terminated with exit code ~A"
		  *pkg-config* exit-code))))
      (let ((output (read-lines (process-output process))))	  
	(process-close process)
	(values output exit-code)))))

#+clisp
(defun run-pkg-config (package error-p &rest options)
  (let ((outfile (format nil "/tmp/clg-pkg-config-~A-output" (os:process-id)))
	(errfile (format nil "/tmp/clg-pkg-config-~A-error" (os:process-id))))
    (unwind-protect
	(let ((exit-code 
	       (run-shell-command 
		(format nil "~A ~A ~{~A ~}2>~A" 
		 *pkg-config* package 
		 (mapcar #'(lambda (option) 
			     (format nil "'~A'" option))
			 options)
		 errfile)
		:output outfile :if-output-exists :overwrite)))
	  (cond
	   ((= exit-code 127) (error "Unable to run ~A" *pkg-config*))
	   ((and error-p (not (zerop exit-code)))
	    (with-open-file (output errfile)
	      (let ((errmsg (read-lines output)))
		(error
		 (if (not errmsg)
		     (format nil "~A terminated with exit code ~A" *pkg-config* exit-code)
		   (format nil "~A: ~{~A~%~}" *pkg-config* errmsg))))))
	   (t
	    (values 
	     (with-open-file (output outfile)
	       (read-lines output))
	     exit-code))))
      (progn
	(delete-file outfile)
	(delete-file errfile)))))


(defun pkg-cflags (package)
  (split-string (first (run-pkg-config package t "--cflags"))))

(defun pkg-libs (package)
  (split-string (first (run-pkg-config package t "--libs"))))


(defun pkg-exists-p (package &key version atleast-version max-version error)
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


(defun |#?-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((not-p (when (char= (peek-char nil stream) #\-)
		 (read-char stream)))
	(conditional (read stream t nil t)))
    (cond
     (*read-suppress* (read stream t nil t))
     ((not *read-eval*)
      (error 'reader-error 
       :format-control "Attempt to read #? while *READ-EVAL* is bound to NIL."
       :format-arguments nil :stream stream))
     ((if not-p
	  (eval conditional)
	(not (eval conditional)))
      (let ((*read-suppress* t))
	(read stream t nil t)))))
  (values))

(set-dispatch-macro-character #\# #\? #'|#?-reader|)


#+sbcl
(progn
  (defun sbcl-version ()
    (let* ((dot1 (position #\. (lisp-implementation-version)))
	   (dot2 (position #\. (lisp-implementation-version) :start (1+ dot1))))
      (values 
       (parse-integer (lisp-implementation-version) :end dot1)
       (parse-integer (lisp-implementation-version) :start (1+ dot1) :end dot2)
       (parse-integer (lisp-implementation-version) :start (1+ dot2) :junk-allowed t))))
  (defun sbcl>= (req-major req-minor req-micro)
    (multiple-value-bind (major minor micro) (sbcl-version)      
      (or 
       (> major req-major)
       (and (= major req-major) (> minor req-minor))
       (and (= major req-major) (= minor req-minor) (>= micro req-micro))))))

#-sbcl
(defun sbcl>= (req-major req-minor req-micro)
  (declare (ignore req-major req-minor req-micro))
  nil)
