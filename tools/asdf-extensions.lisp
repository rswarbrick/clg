(in-package :asdf)

(defun concatenate-strings (strings &optional delimiter)
  (if (not (rest strings))
      (first strings)
    (concatenate
     'string
     (first strings)
     (if delimiter (string delimiter) "")
     (concatenate-strings (rest strings) delimiter))))

;;; The following code is more or less copied frm sb-bsd-sockets.asd,
;;; but extended to allow flags set in a general way

(defclass unix-dso (module) ())
(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod asdf::input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type "so"
		    :name (car (last (pathname-directory dir)))
		    :directory (butlast (pathname-directory dir))
		    :defaults dir))))


(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      "gcc ~A -o ~S ~{~S ~}"
	      (concatenate 'string
;; 			   (sb-ext:posix-getenv "EXTRA_LDFLAGS")
;; 			   " "
			   #+sunos "-shared -lresolv -lsocket -lnsl"
			   #+darwin "-bundle"
			   #-(or darwin sunos) "-shared")
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
      (error 'operation-error :operation operation :component dso))))

(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-shared-object filename))))



(defclass c-source-file (source-file) 
  ((cflags :initform nil :initarg :cflags)
   (optimization :initform 2 :initarg :optimization)
   (definitions :initform nil :initarg :definitions)
   (include-paths :initform nil :initarg :include-paths)))


(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))


(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "gcc ~A -o ~S -c ~S"
	    (concatenate-strings
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
  

