;;; -*- Mode: lisp -*-

(defpackage "ATK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "ATK-SYSTEM")

(pkg-exists-p "atk" :atleast-version "1.6.0")


(defsystem atk
    :depends-on (glib gdk)
    :components ((:library "libatk-1.0"
			   :libdir #.(pkg-variable "atk" "libdir"))
		 (:file "defpackage")
		 (:file "atk" :depends-on ("defpackage" "libatk-1.0"))
		 (:file "export" :depends-on ("atk"))))
