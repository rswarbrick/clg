(in-package "CAIRO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport defoperator (name &optional clip-p)
    (if clip-p
	name
      (let ((tname (intern (format nil "IN-~A-P" name)))
	    (ename (intern (format nil "~A-EXTENTS" name))))
	(list name tname ename))))

  (defexport defpath (name args &optional relative-p)
    (declare (ignore args))
    (if (not relative-p)
	(list name (intern (format nil "FAST-~A" name)))
      (list 
       name 
       (intern (format nil "FAST-~A" name))
       (intern (format nil "REL-~A" name)) 
       (intern (format nil "FAST-REL-~A" name))))))


;;; Autogenerating exported symbols
(export-from-system)
