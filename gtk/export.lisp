(in-package "GTK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gtk-files* 
    (list 
     #p"clg:gtk;gtktypes.lisp"
     #p"clg:gtk;gtkwidget.lisp"
     #p"clg:gtk;gtkcontainer.lisp"
     #p"clg:gtk;gtk.lisp"
     #p"clg:gtk;gtktree.lisp"
     #p"clg:gtk;gtktext.lisp"
     #p"clg:gtk;gtkaction.lisp"
     #p"clg:gtk;gtkselection.lisp"
     #p"clg:gtk;gtkutils.lisp"
     #p"clg:gtk;gtkstyle.lisp"))

  (defexport define-style-color-accessor (name type)
    (declare (ignore type))
    name)

  (defexport define-style-gc-reader (name type)
    (declare (ignore type))
    name))

;;; Autogenerating exported symbols
(export-from-files #.*gtk-files*)


;;; Now re-export some of the symbols from the CLG package
(with-export-handlers 
 (export-handler-makunbound 'defvar)
 (export-handler-makunbound 'deftype)
 
 (defexport defclass (class superclasses &optional slotdefs &rest options)
   (declare (ignore superclasses options))
   (export-defclass-form class slotdefs nil))

 (export-from-files #.*gtk-files* "CLG"))
