;;; -*- Mode: lisp -*-

(defpackage "GDK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GDK-SYSTEM")

(pkg-exists-p "gtk+-2.0" :atleast-version "2.4.0")

(when (string>= (pkg-version "gtk+-2.0") "2.6.0")
  (pushnew :gtk2.6 *features*))

(when (string>= (pkg-version "gtk+-2.0") "2.8.0")
  (pushnew :gtk2.8 *features*))

(defsystem gdk
    :depends-on (glib pango #+gtk2.8 cairo)
    :components ((:file "defpackage")
		 (:library "libgdk_pixbuf-2.0" 
			   :libdir #.(pkg-variable "gtk+-2.0" "libdir"))
		 (:library "libgdk-x11-2.0" 
			   :libdir #.(pkg-variable "gtk+-2.0" "libdir"))
		 (:unix-dso "alien"
			    :components ((:c-source-file "glue"
					  :cflags #.(pkg-cflags "gtk+-2.0"))))
		 (:file "gdktypes" :depends-on ("defpackage" "alien" 
						"libgdk_pixbuf-2.0" 
						"libgdk-x11-2.0"))
		 (:file "gdkevents" :depends-on ("gdktypes"))
		 (:file "pixbuf" :depends-on ("gdktypes"))
		 (:file "gdk" :depends-on ("gdkevents"))
		 (:file "export" :depends-on ("gdkevents" "gdktypes" "gdk"))))
