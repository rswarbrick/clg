;;; -*- Mode: lisp -*-

(defpackage "PANGO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "PANGO-SYSTEM")

(pkg-exists-p "pango" :atleast-version "1.4.0" :error t)


(defsystem pango
    :depends-on (gffi glib cairo)
    :components ((:library "libpango-1.0" :libdir #.(pkg-variable "pango" "libdir"))
		 (:library "libpangoxft-1.0" :libdir #.(pkg-variable "pango" "libdir"))
		 (:library "libpangoft2-1.0" :libdir #.(pkg-variable "pango" "libdir"))
		 (:library "libpangocairo-1.0" :libdir #.(pkg-variable "pango" "libdir"))
		 (:file "defpackage")
		 (:file "pango" :depends-on ("defpackage" "libpango-1.0" "libpangoxft-1.0" "libpangocairo-1.0"))
		 (:file "export" :depends-on ("pango"))))

