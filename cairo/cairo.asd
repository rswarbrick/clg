;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "CAIRO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "CAIRO-SYSTEM")

(pkg-exists-p "cairo" :atleast-version "1.2.0" :error t)


(defsystem cairo
    :depends-on (gffi glib)
    :components ((:library "libcairo" 
		  :libdir #.(pkg-variable "cairo" "libdir"))
		 (:file "defpackage")
		 (:file "cairo" :depends-on ("defpackage" "libcairo"))
		 (:file "export" :depends-on ("cairo"))))
