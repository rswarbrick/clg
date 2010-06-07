(asdf:oos 'asdf:load-op :clg-tools)

(defpackage :clutter-tests-system (:use :cl :asdf))
(in-package :clutter-tests-system)

(defsystem clutter-tests
  :depends-on (:clutter)
  :components
  ((:file "package")
   (:file "stage-basics" :depends-on ("package"))))
