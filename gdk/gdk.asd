;;; -*- Mode: lisp -*-

(defpackage "GDK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GDK-SYSTEM")

(pkg-exists-p "gdk-2.0" :atleast-version "2.4.0" :error t)

(defsystem gdk
    :depends-on (gffi glib pango #?(pkg-exists-p "gdk-2.0" :atleast-version "2.8.0") cairo)
    :components ((:file "defpackage")
		 (:library "libgdk_pixbuf-2.0" 
		  :libdir #.(pkg-libdir "gdk-2.0")
		  :libname #-win32 "libgdk_pixbuf-2.0" 
		           #+win32 "libgdk_pixbuf-2.0-0")
		 (:library "libgdk-2.0" 
		  :libdir #.(pkg-libdir "gdk-2.0")
		  :libname #-win32 "libgdk-x11-2.0"
		           #+win32 "libgdk-win32-2.0-0")
		 (:shared-object "gdk-alien" :pathname "alien/"
		  :ldflags #.(pkg-libs "gdk-2.0")
		  :components ((:c-source-file "glue"
				:cflags #.(pkg-cflags "gdk-2.0")))
		  :depends-on (#+cmu "libgdk-2.0"))
		 (:file "gdktypes" :depends-on ("defpackage" "gdk-alien" 
						"libgdk_pixbuf-2.0" 
						"libgdk-2.0"))
		 (:file "gdkevents" :depends-on ("gdktypes"))
		 (:file "pixbuf" :depends-on ("gdktypes"))
		 (:file "gdk" :depends-on ("gdkevents"))
		 (:file "export" :depends-on ("gdkevents" "gdktypes" "gdk"))))
