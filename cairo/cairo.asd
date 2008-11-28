;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "CAIRO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "CAIRO-SYSTEM")

(pkg-exists-p "cairo" :atleast-version "1.2.0" :error t)


(defsystem cairo
    :depends-on (gffi glib)
    :components ((:library "libcairo" 
		            :libname #-win32 "libcairo" 
			             #+win32 "libcairo-2" 
			    :libdir #.(pkg-libdir "cairo"))
		 (:library "libjpeg")
		 (:shared-object "cairo-alien" :pathname "alien/"
		  :components ((:c-source-file "cairo-jpeg" 
				:cflags #.(pkg-cflags "cairo")))
		  :depends-on ("libcairo" "libjpeg"))
		 (:file "defpackage")
		 (:file "cairo" 
		  :depends-on ("defpackage" "libcairo" "cairo-alien"))
		 (:file "export" :depends-on ("cairo"))))
