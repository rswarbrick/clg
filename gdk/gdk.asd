;;; -*- Mode: lisp -*-

(defpackage "GDK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG" "SHAREDLIB"))

(in-package "GDK-SYSTEM")

(pkg-exists-p "gtk+-2.0" :atleast-version "2.4.0")


(load-shared-library "libgdk_pixbuf-2.0")
(load-shared-library "libgdk-x11-2.0"
		     :init "gdk_init"
		     :prototype '(alien:function
				  c-call:void
				  alien:system-area-pointer
				  alien:system-area-pointer)
		     :initargs (list (system:int-sap 0) (system:int-sap 0)))



(defsystem gdk
    :depends-on (glib)
    :components ((:file "defpackage")
		 (:unix-dso "alien"
		  :components ((:c-source-file "glue"
				:cflags #.(pkg-cflags "gtk+-2.0"))))
		 (:file "gdktypes" :depends-on ("defpackage" "alien"))
		 (:file "gdkevents" :depends-on ("gdktypes"))
		 (:file "gdk" :depends-on ("gdkevents"))
		 (:file "export" :depends-on ("gdkevents" "gdktypes" "gdk"))))
