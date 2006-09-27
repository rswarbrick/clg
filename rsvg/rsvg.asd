;;; -*- Mode: lisp -*-

(defpackage "RSVG-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "RSVG-SYSTEM")

(pkg-exists-p "librsvg-2.0" :atleast-version "2.14.0" :error t)


(defsystem rsvg
    :depends-on (gffi glib gdk cairo)
    :components ((:library "librsvg-2" 
		  :libdir #.(pkg-variable "librsvg-2.0" "libdir"))
		 (:file "defpackage")
		 (:file "rsvg" :depends-on ("defpackage" "librsvg-2"))
		 (:file "export" :depends-on ("rsvg"))))
