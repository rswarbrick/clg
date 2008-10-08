(defpackage #:pkg-config
  (:use #:common-lisp #:clg-utils #+(or cmu clisp) #:ext #+sbcl #:sb-ext)
  #+sbcl
  (:import-from #:sb-int #:featurep)
  (:export #:pkg-cflags #:pkg-libs #:pkg-exists-p #:pkg-version
           #:pkg-variable #:pkg-libdir #:tmpname)
  (:export #:featurep #:sbcl>= #:sbcl< #:clisp>=))

(in-package #:pkg-config)

(defparameter *pkg-config* "pkg-config")


(defun tmpname (suffix)
  (format nil "~Aclg-~A-~A"
   #-win32 "/tmp/" #+win32 "c:/temp/" 
   #+sbcl(sb-posix:getpid)
   #+cmu(unix:unix-getpid)
   #+clisp(os:process-id)   
   suffix))

(defun run-pkg-config (package error-p &rest options)
  (let ((outname (tmpname "pkg-config")))
    (unwind-protect
	(let* ((asdf::*verbose-out* nil)
	       (exit-code 
		(asdf:run-shell-command 
		 "~A ~A ~:[~;--print-errors ~]~{~A ~} >~A 2>&1"
		 *pkg-config* package error-p options outname)))
	  (cond
	   ((= exit-code 127) (error "Unable to run ~A" *pkg-config*))
	   ((and error-p (not (zerop exit-code)))
	    (with-open-file (output outname)
	      (let ((errmsg (read-lines output)))
		(error
		 (if (not errmsg)
		     (format nil "~A terminated with exit code ~A" *pkg-config* exit-code)
		   (format nil "~A: ~{~A~%~}" *pkg-config* errmsg))))))
	   (t
	    (values 
	     (with-open-file (output outname)
	       (read-lines output))
	     exit-code))))
      (delete-file outname))))


(defun pkg-cflags (package)
  (split-string (first (run-pkg-config package t "--cflags"))))

(defun pkg-libs (package)
  (split-string (first (run-pkg-config package t "--libs"))))


;; With
;;   (let ((version-check
;; 	 (cond
;; 	  (version (format nil "= ~A" version))
;; 	  (atleast-version (format nil ">= ~A" atleast-version))
;; 	  (max-version (format nil "<= ~A" max-version))
;; 	  (t ""))))
;;     ...)
;; when running
;;   (PKG-EXISTS-P "glib-2.0" :ATLEAST-VERSION "2.4.0" :ERROR T)
;; the EXIT-CODE in RUN-PKG-CONFIG will be 1 on ms windows and 0 on linux

;; Both on ms windows and linux
;;   "pkg-config glib-2.0 --print-errors -exists >=2.4.0" is O.K. but
;;   "pkg-config glib-2.0 --print-errors -exists >= 2.4.0" prints out
;;     an error message.
;; However,
;;   "pkg-config glib-2.0 --print-errors -exists =2.12.11" prints out
;;     an error message but
;;   "pkg-config glib-2.0 --print-errors -exists = 2.12.11" is O.K.
;; We can get around this problem by using
;;   (let ((version-check
;; 	 (cond
;; 	  (version (format nil "--exact-version=~A" version))
;; 	  (atleast-version (format nil "--atleast-version=~A" atleast-version))
;; 	  (max-version (format nil "--max-version=~A" max-version))
;; 	  (t ""))))
;;     ...)
;;  - cph 17-May-2007

;; Could the problem with --exists on win32 be caused by improper quoting?
;; Since --exact-version, --atleast-version and --max-version doesn't print
;; any error message, we stick to --exists on non Win32 platforms.
;;  - esj 2007-06-14


;; --fix: in win32 sbcl
;;        (pkg-exists-p "glib-2.0" :version "3.12.11" :error t)
;;        will hang indefinitely.   - cph 17-May-2007

(defun pkg-exists-p (package &key version atleast-version max-version error)
  (let ((version-check
	 (cond
	  (version 
	   #-win32(format nil "--exists \"= ~A\"" version)
	   #+win32(format nil "--exact-version=~A" version))
	  (atleast-version 
	   #-win32(format nil "--exists \">= ~A\"" atleast-version)
	   #+win32(format nil "--atleast-version=~A" atleast-version))
	  (max-version 
	   #-win32(format nil "--exists \"<= ~A\"" max-version)
	   #+win32(format nil "--max-version=~A" max-version))
	  (t ""))))
    (zerop (nth-value 1 (run-pkg-config package error version-check)))))

(defun pkg-version (package)
  (first (run-pkg-config package t "--modversion")))

(defun pkg-variable (package variable)
  (first (run-pkg-config package t "--variable" variable)))

(defun pkg-libdir (package)
  #-win32
  (pkg-variable package "libdir")
  #+win32
  (let ((ldir (pkg-variable package "libdir")))
    (format nil "~Abin" (subseq ldir 0 (search "lib" ldir :from-end t)))))
  

(defun |#?-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((not-p (when (char= (peek-char nil stream) #\-)
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
    (values-list
     (loop
      repeat 4
      for part in (split-string (lisp-implementation-version) :delimiter #\.)
      while (every #'digit-char-p part)
      collect (parse-integer part))))
  (defun sbcl>= (major minor micro &optional patch)
    (multiple-value-bind (%major %minor %micro %patch) (sbcl-version)      
      (or 
       (> %major major)
       (and (= %major major) (> %minor minor))
       (and (= %major major) (= %minor minor) (> %micro micro))
       (and 
	(= %major major) (= %minor minor) (= %micro micro)
	(>= (or %patch 0) (or patch 0))))))
  (defun sbcl< (major minor micro &optional patch)
    (not (sbcl>= major minor micro patch))))

#-sbcl
(progn
  (defun sbcl>= (major minor micro &optional patch)
    (declare (ignore major minor micro patch))
    nil)
  (defun sbcl< (major minor micro &optional patch)
    (declare (ignore major minor micro patch))
    nil))

#+clisp
(progn
  (defun clisp-version ()
    (let* ((dot (position #\. (lisp-implementation-version))))
      (values 
       (parse-integer (lisp-implementation-version) :end dot)
       (parse-integer (lisp-implementation-version) :start (1+ dot) :junk-allowed t))))
  (defun clisp>= (req-major req-minor)
    (multiple-value-bind (major minor) (clisp-version)      
      (or 
       (> major req-major)
       (and (= major req-major) (> minor req-minor))))))

#-clisp
(defun clisp>= (req-major req-minor)
  (declare (ignore req-major req-minor))
  nil)
