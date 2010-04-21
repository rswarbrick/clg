(asdf:oos 'asdf:load-op :clg-tools)

(defpackage :goocanvas-system
  (:use :cl :asdf :pkg-config))
(in-package :goocanvas-system)

(pkg-exists-p "goocanvas" :atleast-version "0.13" :error t)

(defsystem goocanvas
  :depends-on (:gffi :glib :gtk :cairo :atk)
  :components ((:library "libgoocanvas" :libdir #.(pkg-libdir "goocanvas"))
               (:file "defpackage")
               (:file "gootypes" :depends-on ("defpackage" "libgoocanvas"))
               (:file "goocanvas" :depends-on ("gootypes" "libgoocanvas"))
               (:file "export" :depends-on ("goocanvas" "gootypes"))))
