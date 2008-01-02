;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GTK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GTK-SYSTEM")

(pkg-exists-p "gtk+-2.0" :atleast-version "2.4.0" :error t)

(defsystem gtk
    :depends-on (gffi glib gdk pango atk)
    :components ((:file "defpackage")
		 (:library "libgtk-2.0" 
		  :libdir #.(pkg-libdir "gtk+-2.0")
		  :libname #-win32 "libgtk-x11-2.0"
		           #+win32 "libgtk-win32-2.0-0")
		 (:shared-object "gtk-alien" :pathname "alien/"
		  :ldflags #.(pkg-libs "gtk+-2.0")
		  :components ((:c-source-file "glue"
				:cflags #.(pkg-cflags "gtk+-2.0")))
		  :depends-on (#+cmu "libgtk-2.0"))
		 (:file "gtkobject" :depends-on ("defpackage" "gtk-alien" "libgtk-2.0"))
 		 (:file "gtktypes" :depends-on ("gtkobject"))
		 (:file "gtkwidget" :depends-on ("gtktypes"))
		 (:file "gtkcontainer" :depends-on ("gtktypes"))
		 (:file "gtktree" :depends-on ("gtktypes"))
		 (:file "gtktext" :depends-on ("gtktypes"))
		 (:file "gtkaction" :depends-on ("gtktypes" "gtk"))
		 (:file "gtkselection" :depends-on ("gtktypes"))
		 (:file "gtkstyle" :depends-on ("gtktypes"))
		 (:file "gtk" :depends-on ("gtktypes" "gtkcontainer"))
		 (:file "gtkutils" :depends-on ("gtk"))
		 (:file "export" :depends-on ("gtktypes" "gtkwidget" "gtkcontainer" "gtk" "gtktree" "gtktext" "gtkaction" "gtkselection" "gtkutils" "gtkstyle"))))
