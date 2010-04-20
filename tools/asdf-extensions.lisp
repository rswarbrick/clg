(in-package :asdf)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :pkg-config))

(export '(*search-library-path-on-reload* *dso-extension*
	  *operation* *system* *component* library shared-object
	  install-init-hook))

(defvar *dso-extension* 
 #-(or darwin win32)"so" #+darwin"dylib" #+win32"dll")

(defvar *search-library-path-on-reload* t)


;;; Since Common Lisp implementations doesn't seem to agree on how to
;;; run init hooks, we have to add our own compatibility layer.

(defvar *init-hooks* ())

(defun install-init-hook (func &optional (only-once t))
  (if only-once
      (pushnew func *init-hooks*)
    (push func *init-hooks*)))

(defun run-init-hooks ()
  (mapcar #'funcall (reverse *init-hooks*)))

(pushnew 'run-init-hooks
  #+cmu ext:*after-save-initializations*
  #+sbcl sb-ext:*init-hooks*
  #+clisp custom:*init-hooks*)

(defvar *reload-shared-objects* ()
  "List of shared objects which should be reloaded from library search
  path in saved images.")

#?-(sbcl>= 1 0 22)
(defvar *dont-save-shared-objects* ()
  "List of shared objects which should not be saved in images.")

(defun namestring-name (namestring)
  (let ((pos (position #\/ namestring :from-end t)))
    (if pos
	(subseq namestring (1+ pos))
      namestring)))

(defun load-shared-object (pathname &optional dont-save-p (reload-p dont-save-p))
  (let* ((namestring (ensure-namestring pathname)))
    #?(sbcl< 1 0 22)(sb-alien:load-shared-object namestring)
    #?(sbcl>= 1 0 22)
    (sb-alien:load-shared-object namestring :dont-save dont-save-p)
    #+cmu(ext:load-foreign namestring)
    #?(clisp< 2 45)(ffi::foreign-library namestring)
    #?(clisp>= 2 45)(ffi:open-foreign-library namestring)
    (when dont-save-p
      #?-(sbcl>= 1 0 22)
      (pushnew namestring *dont-save-shared-objects* :test #'string=)
      (when reload-p
	(pushnew (namestring-name namestring)
	 *reload-shared-objects* :test #'string=)))))

#?(or (sbcl< 1 0 22) (featurep :cmu))
(progn
  (defun remove-shared-objects ()    
    (dolist (namestring *dont-save-shared-objects*)
      #+sbcl
      (setf sb-alien::*shared-objects* 
       (remove namestring sb-alien::*shared-objects* 
        :key #'sb-alien::shared-object-file 
	:test #'string=))
      #+cmu
      (setf system::*global-table* 
       (remove namestring system::*global-table* 
	:key #'cdr :test #'string=))))
  (pushnew 'remove-shared-objects
   #+sbcl sb-ext:*save-hooks*
   #+cmu ext:*before-save-initializations*))

(defun reload-shared-objects ()
  (handler-bind (#+sbcl (style-warning #'muffle-warning))
    (dolist (namestring (reverse *reload-shared-objects*))
      (load-shared-object namestring))))

(install-init-hook 'reload-shared-objects)



;;; The following code is more or less copied from sb-bsd-sockets.asd,
;;; but extended to allow flags to be set in a general way. The class
;;; has been renamed from unix-dso to shared-object as this code is no
;;; longer specific to unix

(defclass shared-object (module)
  ((ldflags :initform nil :initarg :ldflags)
   (search  :initform *search-library-path-on-reload* :initarg :search 
	    :reader search-library-path-on-reload)))

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

(defmethod perform ((o load-op) (dso shared-object))
  (let ((co (make-instance 'compile-op)))
    (let ((pathname (car (output-files co dso))))
      (load-shared-object pathname (search-library-path-on-reload dso)))))



(defclass c-source-file (source-file) 
  ((cflags :initform nil :initarg :cflags)
   (optimization :initform 2 :initarg :optimization)
   (definitions :initform nil :initarg :definitions)
   (include-paths :initform nil :initarg :include-paths)))


(defmethod output-files ((op compile-op) (c c-source-file))
  (list (make-pathname :type "o" :defaults (component-pathname c))))

(defmethod component-pathname ((c c-source-file))
  (make-pathname :type "c" :name (component-name c)
                 :directory (pathname-directory (call-next-method))))

(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "gcc -Wall ~A~{ ~A~} -o ~S -c ~S"
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
   (search  :initform *search-library-path-on-reload* :initarg :search 
	    :reader search-library-path-on-reload)))


(defun split-path (path)
  (when path
    (labels ((split (path)
	       (unless (zerop (length path))
		 (let ((slash (position #\/ path)))
		   (if slash
		       (cons (subseq path 0 slash) (split (subseq path (1+ slash))))
		       (list path))))))
      (if (and (not (zerop (length path))) (char= (char path 0) #\/))
	  (cons :absolute (split (subseq path 1)))
	(cons :relative (split path))))))
  

(defmethod component-pathname ((lib library))
  (or
   (when (slot-value lib 'libname)
     (let ((filename (format nil "~A~A" (namestring (make-pathname :directory (split-path (slot-value lib 'libdir)))) (slot-value lib 'libname))))
       (when (probe-file filename)
	 (pathname filename))))
   
   (make-pathname
    :type *dso-extension*
    :name (or (slot-value lib 'libname) (component-name lib))
    :directory (split-path (slot-value lib 'libdir)))))


(defvar *loaded-libraries* ())

(defmethod perform ((o load-op) (lib library))
  (load-shared-object (component-pathname lib) 
   (search-library-path-on-reload lib))
  (pushnew lib *loaded-libraries*))

(defmethod perform ((operation operation) (lib library))
  nil)

(defmethod operation-done-p ((o load-op) (lib library))
  (find lib *loaded-libraries*))

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
