;;; -*- Mode: lisp -*-

(defpackage "ATK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "ATK-SYSTEM")

(pkg-exists-p "atk" :atleast-version "1.6.0" :error t)


(defsystem atk
    :depends-on (gffi glib gdk)
    :components ((:library "libatk-1.0"
			   :libname #-win32 "libatk-1.0"
			            #+win32 "libatk-1.0-0"
			   :libdir #.(pkg-libdir "atk"))
		 (:file "defpackage")
		 (:file "atk" :depends-on ("defpackage" "libatk-1.0"))
		 (:file "export" :depends-on ("atk"))))
