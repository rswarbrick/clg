(in-package "GTK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport define-style-color-accessor (name type)
    (declare (ignore type))
    name)

  (defexport define-style-gc-reader (name type)
    (declare (ignore type))
    name))

;;; Autogenerating exported symbols
(export-from-system)


;;; Now re-export some of the symbols from the CLG package
(with-export-handlers 
 (export-handler-makunbound 'defvar)
 (export-handler-makunbound 'deftype)
 
 (defexport defclass (class superclasses &optional slotdefs &rest options)
   (declare (ignore superclasses options))
   (export-defclass-form class slotdefs nil))

 (export-from-system "CLG"))
