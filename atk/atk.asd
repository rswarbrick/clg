;;; -*- Mode: lisp -*-

(defpackage "ATK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG" "SHAREDLIB"))


(in-package "ATK-SYSTEM")

(pkg-exists-p "atk" :atleast-version "1.6.0")


(defsystem atk
    :depends-on (glib)
    :components ((:file "defpackage")
		 (:file "atk" :depends-on ("defpackage"))
		 (:file "export" :depends-on ("atk"))))
