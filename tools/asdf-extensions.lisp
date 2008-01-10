(in-package :asdf)

(export '(*absolute-paths-as-default* *dso-extension*
	  *operation* *system* *component*))

(defparameter *dso-extension* 
 #-(or darwin win32)"so" #+darwin"dylib" #+win32"dll")

(defparameter *absolute-paths-as-default* nil)

;;; The following code is more or less copied from sb-bsd-sockets.asd,
;;; but extended to allow flags to be set in a general way. The class
;;; has been renamed from unix-dso to shared-object as this code is no
;;; longer specific to unix

(defclass shared-object (module)
  ((ldflags :initform nil :initarg :ldflags)
   (absolute :initform *absolute-paths-as-default* 
	     :initarg :absolute :reader absolute-p)))

(defun ensure-namestring (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod input-files ((operation compile-op) (dso shared-object))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso shared-object))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type *dso-extension*
		    :name (component-name dso)
		    :directory (butlast (pathname-directory dir))
		    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso shared-object))
  (let ((output (first (output-files operation dso)))
	(inputs (mapcar #'ensure-namestring
		 (mapcan #'(lambda (c)
			     (output-files operation c))
		  (module-components dso)))))
    (unless (zerop
	     (run-shell-command "gcc ~A -o ~S ~{~S~^ ~} ~{~A~^ ~}"
	      #-(or darwin win32)"-shared"
	      #+darwin "-bundle"
	      #+win32
	      (format nil "-shared -Wl,--out-implib,~S"
	       (ensure-namestring
		(make-pathname 
		 :type "a" 
		 :name (format nil "lib~Adll" (pathname-name output))
		 :defaults output)))
	      (ensure-namestring output)
	      inputs
	      (slot-value dso 'ldflags)))
      (error 'operation-error :operation operation :component dso))))

#+clisp
(defvar *loaded-libraries* ())

(defun load-shared-object (pathname &optional (absolute-p t))
  (let* ((namestring (ensure-namestring pathname))
	 (directory (namestring (pathname-sans-name+type namestring)))
	 (name+type (subseq namestring (length directory))))
    #+sbcl
    (progn
      (sb-alien:load-shared-object namestring)
      (unless absolute-p
	(let ((shared-object (find namestring sb-alien::*shared-objects* 
			      :key #'sb-alien::shared-object-file 
			      :test #'equal)))
	  (setf (sb-alien::shared-object-file shared-object) name+type))))
    #+cmu
    (progn
      (ext:load-foreign namestring)
      (unless absolute-p
	(let ((shared-object (rassoc namestring system::*global-table* 
			      :test #'equal)))
	  (setf (cdr shared-object) name+type))))
    #+clisp 
    (progn
      (ffi::foreign-library namestring)
      (pushnew 
       (if absolute-p namestring name+type)
       *loaded-libraries* :test #'string=))))


(defmethod perform ((o load-op) (dso shared-object))
  (let ((co (make-instance 'compile-op)))
    (let ((pathname (car (output-files co dso))))
      (load-shared-object pathname (absolute-p dso)))))



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
	    (ensure-namestring (first (output-files op c)))
	    (ensure-namestring (component-pathname c))))
    (error 'operation-error :operation op :component c)))


(defmethod perform ((operation load-op) (c c-source-file))
  t)
  

;;; Shared libraries

(defclass library (component) 
  ((libdir :initarg :libdir :initform nil)
   (libname :initarg :libname :initform nil)
   (absolute :initform *absolute-paths-as-default*
	     :initarg :absolute :reader absolute-p)))


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

(defmethod perform ((o load-op) (lib library))
  (load-shared-object (component-pathname lib) (absolute-p lib)))

(defmethod perform ((operation operation) (lib library))
  nil)

(defmethod operation-done-p ((o load-op) (lib library))
  (let* ((namestring (ensure-namestring (component-pathname lib)))
	 (directory (namestring (pathname-sans-name+type namestring)))
	 (name+type (subseq namestring (length directory)))
	 (stored-name (if (absolute-p lib) namestring name+type)))

    #+sbcl(find stored-name sb-alien::*shared-objects* :key #'sb-alien::shared-object-file :test #'equal)
    #+cmu(rassoc stored-name system::*global-table* :test #'equal)
    #+clisp(find stored-name *loaded-libraries* :test #'equal)))

(defmethod operation-done-p ((o operation) (lib library))
  t)


;;; Binding of dynamic variables during perform

(defvar *operation* nil)
(defvar *system* nil)
(defvar *component* nil)

(defmethod perform :around ((operation operation) (c component))
  (let ((*operation* operation)
	(*component* c)
	(*system* (component-system c)))
    (call-next-method)))
