;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GTK-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GTK-SYSTEM")

(pkg-exists-p "gtk+-2.0" :atleast-version "2.4.0")

(when (string>= (pkg-version "gtk+-2.0") "2.6.0")
  (push :gtk2.6 *features*))

(defsystem gtk
    :depends-on (glib gdk pango atk)
    :components ((:file "defpackage")
		 (:library "libgtk-x11-2.0" 
			   :libdir #.(pkg-variable "gtk+-2.0" "libdir"))
		 (:unix-dso "alien"
			    :components ((:c-source-file "glue"
					  :cflags #.(pkg-cflags "gtk+-2.0")))
			    :depends-on ("libgtk-x11-2.0"))
		 (:file "gtkobject" 
			:depends-on ("defpackage" "alien"))
 		 (:file "gtktypes" :depends-on ("gtkobject"))
		 (:file "gtkwidget" :depends-on ("gtktypes"))
		 (:file "gtkcontainer" :depends-on ("gtktypes"))
		 (:file "gtktree" :depends-on ("gtktypes"))
		 (:file "gtktext" :depends-on ("gtktypes"))
		 (:file "gtkaction" :depends-on ("gtktypes"))
		 (:file "gtkstyle" :depends-on ("gtktypes"))
		 (:file "gtk" :depends-on ("gtktypes"))
		 (:file "gtkutils" :depends-on ("gtk"))
		 (:file "export" :depends-on ("gtktypes" "gtk"))))
