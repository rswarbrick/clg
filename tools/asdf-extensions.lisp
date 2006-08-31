(in-package :asdf)

(export '*dso-extension*)

(defvar *dso-extension* #-darwin"so" #+darwin"dylib")


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
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      "gcc ~A -o ~S ~{~S ~}"
	      (clg-utils:concatenate-strings
	       (cons
		#+sunos "-shared -lresolv -lsocket -lnsl"
		#+darwin "-bundle"
		#-(or darwin sunos) "-shared"
		(slot-value dso 'ldflags))
	       #\sp)
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
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
      (= 0 (run-shell-command "gcc ~A -o ~S -c ~S"
	    (clg-utils:concatenate-strings
	     (append
	      (list "-fPIC")
	      (when (slot-value c 'optimization)
		(list (format nil "-O~A" (slot-value c 'optimization))))
	      (loop 
	       for symbol in (slot-value c 'definitions)
	       collect (format nil "-D~A" symbol))
	      (loop 
	       for path in (slot-value c 'include-paths)
	       collect (format nil "-I~A" path))
	      (slot-value c 'cflags))
	     #\sp)
	    (unix-name (car (output-files op c)))
	    (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))


(defmethod perform ((operation load-op) (c c-source-file))
  t)
  

;;; Shared libraries

(defclass library (component) 
  ((libdir :initarg :libdir)))


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
		 :name (component-name lib)
		 :directory (split-path (slot-value lib 'libdir))))

(defmethod perform ((o load-op) (c library))
  (load-dso (component-pathname c)))

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
