;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GIO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GIO-SYSTEM")

(pkg-exists-p "gio-2.0" :atleast-version "2.16.0" :error t)

(defsystem gio
    :depends-on (clg-tools gffi glib)
    :components ((:file "defpackage")
		 (:library "libgio-2.0"
		  :libdir #.(pkg-libdir "gio-2.0")
		  :libname #-win32 "libgio-2.0"
		           #+win32 "libgio-2.0-0")
		 (:shared-object "gio-alien" :pathname "alien/"
		  :components ((:c-source-file "g_callback_input_stream" 
				:cflags #.(pkg-cflags "gio-2.0"))
			       (:c-source-file "g_callback_output_stream" 
				:cflags #.(pkg-cflags "gio-2.0")))
		  :depends-on ("libgio-2.0"))
		 (:file "gio" 
		  :depends-on ("libgio-2.0" "gio-alien" "defpackage"))
		 (:file "streams" :depends-on ("gio"))
		 (:file "export" :depends-on ("gio" "streams"))))

