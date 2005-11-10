(in-package "CAIRO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport defoperator (name &optional clip-p)
    (if clip-p
	name
      (let ((tname (intern (format nil "IN~A-P" name)))
	    (ename (intern (format nil "~A-EXTENTS" name))))
	(list name tname ename))))

  (defexport defpath (name &rest args)
    (declare (ignore args))
    (list name (intern (format nil "REL-~A" name)))))


;;; Autogenerating exported symbols
(export-from-file #p"clg:cairo;cairo.lisp")
  