(defpackage :goocanvas-demo-system
  (:use :cl :asdf))
(in-package :goocanvas-demo-system)

(defsystem goocanvas-demo
  :depends-on (:goocanvas)
  :components ((:file "defpackage")
               (:file "demo-library" :depends-on ("defpackage"))
               (:file "simple" :depends-on ("defpackage" "demo-library"))
               (:file "arrowheads" :depends-on ("defpackage" "demo-library"))))
