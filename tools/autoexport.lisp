(defpackage "AUTOEXPORT"
  (:use "COMMON-LISP")
  (:export "LIST-AUTOEXPORTED-SYMBOLS" "LIST-AUTOEXPORTED-SYMBOLS-IN-FILE"
	   "DEFEXPORT" "EXPORT-FROM-FILE" "EXPORT-FROM-FILES" "INTERNAL"
	   "WITH-EXPORT-HANDLERS" "EXPORT-HANDLER-MAKUNBOUND"
	   "EXPORT-DEFCLASS-FORM"))

(in-package "AUTOEXPORT")

(declaim (special *internal*))

(defvar *export-handlers* (make-hash-table))
(defvar *noexport-prefix* #\%)

(defmacro defexport (operator lambda-list &body body)
  `(setf
    (gethash ',operator *export-handlers*)
    #'(lambda ,lambda-list
	,@body)))

(defmacro internal (&rest symbols)
  (declare (ignore symbols))
  nil)

(defun export-handler-makunbound (handler)
  (remhash handler *export-handlers*))

(defun list-autoexported-symbols (form)
  (let ((handler (gethash (first form) *export-handlers*)))
    (when handler
      (let ((export (apply handler (cdr form))))
	(delete-if
	 #'(lambda (symbol)
	     (char= (char (string symbol) 0) *noexport-prefix*))
	 (if (atom export)
	     (list export)
	   export))))))

(defun export-fname (fname)
  (if (atom fname)
      fname
    (second fname)))

(defun list-autoexported-symbols-in-file (file)
  (let ((*internal* nil))
    (declare (special *internal*))
    (with-open-file (in file)
      (labels ((read-file (in)
	         (let ((form (read in nil nil)))
		   (when form
		     (delete-if
		      #'(lambda (symbol)
			  (member symbol *internal*))
		      (delete-duplicates
		       (nconc
			(list-autoexported-symbols form)
			(read-file in))))))))
	(read-file in)))))
  
(defmacro export-from-file (file &optional package)
  (if package
      `(export ',(list-autoexported-symbols-in-file file) ,package)
    `(export ',(list-autoexported-symbols-in-file file))))

(defmacro export-from-files (files &optional package)
  `(progn 
     ,@(loop for file in files collect `(export-from-file ,file ,package))))

(defun copy-hash-table (hash-table)
  (let ((new-hash-table (make-hash-table 
			 :test (hash-table-test hash-table)
			 :size (hash-table-size hash-table))))
    (maphash 
     #'(lambda (key value)
	 (setf (gethash key new-hash-table) value))
     hash-table)
    new-hash-table))

(defmacro with-export-handlers (&body body)
  `(let ((*export-handlers* (copy-hash-table *export-handlers*)))
     ,@body))
		

;;;; Exporting standard forms

(defexport defun (fname &rest rest)
  (declare (ignore rest))
  (export-fname fname))

(defexport defvar (name &rest rest)
  (declare (ignore rest))
  name)

(defexport defconstant (name &rest rest)
  (declare (ignore rest))
  name)

(defexport defparameter (name &rest rest)
  (declare (ignore rest))
  name)

(defexport defmacro (name &rest rest)
  (declare (ignore rest))
  name)

(defexport deftype (name &rest rest)
  (declare (ignore rest))
  name)

(defun export-defclass-form (class slotdefs &optional (export-slots-p t))
  (cons
   class
   (apply #'nconc
    (map 'list
     #'(lambda (slotdef)
	 (if (symbolp slotdef)
	     (list slotdef)
	   (destructuring-bind
	       (name &key reader writer accessor &allow-other-keys) slotdef
	     (delete nil (list (when export-slots-p name) reader (export-fname writer) accessor)))))
     slotdefs))))

(defexport defclass (class superclasses &optional slotdefs &rest options)
  (declare (ignore superclasses options))
  (export-defclass-form class slotdefs))

(defexport define-condition (class superclasses &optional slotdefs &rest options)
  (declare (ignore superclasses options))
  (export-defclass-form class slotdefs))

(defexport defgeneric (fname &rest args)
  (declare (ignore args))
  (export-fname fname))
  
;; (defexport defmethod (name &rest rest)
;;   (declare (ignore rest))
;;   name)

(defexport progn (&rest body)
  (apply #'nconc (map 'list #'list-autoexported-symbols body)))

(defexport eval-when (case &rest body)
  (declare (ignore case))
  (apply #'nconc (map 'list #'list-autoexported-symbols body)))

(defexport internal (&rest symbols)
  (setq *internal* (nconc *internal* symbols))
  nil)

