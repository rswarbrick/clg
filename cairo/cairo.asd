;;; -*- Mode: lisp -*-

(defpackage "CAIRO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "CAIRO-SYSTEM")

(pkg-exists-p "cairo" :atleast-version "1.0.2")


(defsystem cairo
    :depends-on (glib)
    :components ((:library "libcairo" 
		  :libdir #.(pkg-variable "cairo" "libdir"))
		 (:file "defpackage")
		 (:file "cairo" :depends-on ("defpackage" "libcairo"))
		 (:file "export" :depends-on ("cairo"))))
