(asdf:oos 'asdf:load-op :uffi)

(defpackage #:sharedlib
  (:use #:common-lisp #:uffi)
  (:export #:load-shared-library))

(in-package #:sharedlib)


(defparameter *library-paths* '("/usr/lib/"))



(defun load-shared-library (name &key init prototype initargs)
  (format t ";;; Loading shared library ~A~%" name)
  (let ((pathname (find-foreign-library name *library-paths*)))
    (if pathname
	(unless (load-foreign-library pathname)
	  (error "Couldn't load shared library: ~A" pathname))
	(error "Shared library not found: ~A" name)))	
  (when init
    (apply
     #'alien:alien-funcall
     (alien::%heap-alien
      (alien::make-heap-alien-info
       :type (alien::parse-alien-type (or prototype `(function c-call:void)))
       :sap-form (system:foreign-symbol-address init)))
     initargs)))
