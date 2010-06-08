(asdf:oos 'asdf:load-op :clg-tools)

(defpackage :clutter-gtk-system
  (:use :cl :asdf :pkg-config))
(in-package :clutter-gtk-system)

(pkg-exists-p "clutter-gtk-0.10" :atleast-version "0.10.2" :error t)

(defsystem clutter-gtk
  :depends-on (:clutter :gtk)
  :components
  ((:library "libclutter-gtk-0.10" :libdir #.(pkg-libdir "clutter-gtk-0.10"))
   (:file "defpackage")
   (:file "types" :depends-on ("defpackage" "libclutter-gtk-0.10"))
   (:file "clutter-gtk" :depends-on ("types"))))
