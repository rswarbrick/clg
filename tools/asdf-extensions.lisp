(in-package :asdf)

(export '*dso-extension*)

(defparameter *dso-extension* 
 #-(and darwin win32)"so" #+darwin"dylib" #+win32"dll")


;;; The following code is more or less copied frm sb-bsd-sockets.asd,
;;; but extended to allow flags to be set in a general way

(defclass unix-dso (module)
  ((ldflags :initform nil :initarg :ldflags)))

(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type *dso-extension*
		    :name (car (last (pathname-directory dir)))
		    :directory (butlast (pathname-directory dir))
		    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((output (first (output-files operation dso)))
	(inputs (mapcar #'unix-name
		 (mapcan #'(lambda (c)
			     (output-files operation c))
		  (module-components dso)))))
    (unless (zerop
	     (run-shell-command "gcc ~A~{ ~A~} -o ~S~{ ~S~}"
	      #-(and darwin win32)"-shared"
	      #+darwin "-bundle"
	      #+win32
	      (format nil "-shared -Wl,--out-implib,~S"
	       (unix-name
		(make-pathname 
		 :type "a" 
		 :name (format nil "lib~Adll" (pathname-name output))
		 :defaults output)))
	      (slot-value dso 'ldflags)
	      (unix-name output)
	      inputs))
      (error 'operation-error :operation operation :component dso))))

#+clisp
(defvar *loaded-libraries* ())

(defun load-dso (filename)
  #+sbcl(sb-alien:load-shared-object filename)
  #+cmu(ext:load-foreign filename)
  #+clisp
  (unless (find filename *loaded-libraries* :test #'equal)
    (ffi::foreign-library (namestring filename))
    (push filename *loaded-libraries*)))


(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      (load-dso filename))))



(defclass c-source-file (source-file) 
  ((cflags :initform nil :initarg :cflags)
   (optimization :initform 2 :initarg :optimization)
   (definitions :initform nil :initarg :definitions)
   (include-paths :initform nil :initarg :include-paths)))


(defmethod output-files ((op compile-op) (c c-source-file))
  (list (make-pathname :type "o" :defaults (component-pathname c))))


(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "gcc ~A~{ ~A~} -o ~S -c ~S"
	    #-win32 "-fPIC"
	    #+win32 "-DBUILD_DLL"
	    (nconc
	     (when (slot-value c 'optimization)
	       (list (format nil "-O~A" (slot-value c 'optimization))))
	     (loop 
	      for symbol in (slot-value c 'definitions)
	      collect (format nil "-D~A" symbol))
	     (loop 
	      for path in (slot-value c 'include-paths)
	      collect (format nil "-I~A" path))
	     (slot-value c 'cflags))
	    (unix-name (first (output-files op c)))
	    (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))


(defmethod perform ((operation load-op) (c c-source-file))
  t)
  

;;; Shared libraries

(defclass library (component) 
  ((libdir :initarg :libdir)
   (libname :initarg :libname :initform nil)))


(defun split-path (path)
  (labels ((split (path)
	     (unless (zerop (length path))
	       (let ((slash (position #\/ path)))
		 (if slash
		     (cons (subseq path 0 slash) (split (subseq path (1+ slash))))
		   (list path))))))
    (if (and (not (zerop (length path))) (char= (char path 0) #\/))
	(cons :absolute (split (subseq path 1)))
      (cons :relative (split path)))))


(defmethod component-pathname ((lib library))
  (make-pathname :type *dso-extension*
		 :name (or (slot-value lib 'libname) (component-name lib))
		 :directory (split-path (slot-value lib 'libdir))))

;; --fix: is UNIX-NAME really necessary for win32?  i know it will bomb
;;        without using it while doing (ASDF:OOS 'ASDF:LOAD-OP :GLIB) but
;;        loading the complete pathname for libglib-2.0-0.dll with
;;        SB-ALIEN:LOAD-SHARED-OBJECT by hand won't explode.  weird.
;;         - cph 18-May-2007
(defmethod perform ((o load-op) (c library))
  (load-dso #-win32 (component-pathname c)
	    #+win32 (unix-name (component-pathname c))))

(defmethod perform ((operation operation) (c library))
  nil)

(defmethod operation-done-p ((o load-op) (c library))
  #+sbcl(find (sb-ext::unix-namestring (component-pathname c)) sb-alien::*shared-objects* :key #'sb-alien::shared-object-file :test #'equal)
  #+cmu(rassoc (unix::unix-namestring (component-pathname c)) 
	system::*global-table* 
	:key #'(lambda (pathname)
		 (when pathname (unix::unix-namestring pathname)))
	:test #'equal)
  #+clisp(find (component-pathname c) *loaded-libraries* :test #'equal))

(defmethod operation-done-p ((o operation) (c library))
  t)
