(asdf:oos 'asdf:load-op :clg-tools)

(defpackage :clutter-system
  (:use :cl :asdf :pkg-config))
(in-package :clutter-system)

(pkg-exists-p "clutter-1.0" :atleast-version "1.2" :error t)

(defsystem clutter
  :depends-on (:gffi :glib :gdk :cairo)
  :components
  ((:library "libclutter-glx-1.0" :libdir #.(pkg-libdir "clutter-1.0"))
   (:file "defpackage")
   (:file "types" :depends-on ("defpackage" "libclutter-glx-1.0"))

   (:file "colors" :depends-on ("types"))
   (:file "global" :depends-on ("types"))
   
   (:file "actor" :depends-on ("types"))
   (:file "stage" :depends-on ("types" "actor" "global"))

   (:file "debug" :depends-on ("types"))))
