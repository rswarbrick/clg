;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GLIB-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG" "SHAREDLIB")
  (:export "*GTK-LIBRARY-PATH*"))


;;; Better put this in ~/.cmucl-init.lisp or some other file read at startup
;; (setf
;;  (logical-pathname-translations "clg")
;;  '(("**;*.*.*" "/home/espen/src/clg/**/")))

(ext:unlock-all-packages)

(in-package "GLIB-SYSTEM")

(pkg-exists-p "glib-2.0" :atleast-version "2.4.0")

(defvar *cmucl-include-path* "/usr/lib/cmucl/include")
(defvar *gtk-library-path* (pkg-variable "gtk+-2.0" "libdir"))


(load-shared-library "libglib-2.0")
(load-shared-library "libgobject-2.0" :init "g_type_init")


(defsystem glib
    :depends-on (clg-tools)
    :components ((:file "defpackage")
		 (:file "pcl")
		 (:unix-dso "alien"
		  :components ((:c-source-file "callback"
				:definitions ("CMUCL")
				:include-paths (#.*cmucl-include-path*)
				:cflags #.(pkg-cflags "glib-2.0"))
			       (:c-source-file "gobject" 
				:cflags #.(pkg-cflags "glib-2.0"))))
		 (:file "utils" :depends-on ("defpackage"))
		 (:file "ffi" :depends-on ("utils" "alien"))
		 (:file "glib" :depends-on ("ffi"))
		 (:file "proxy" :depends-on ("pcl" "glib"))
		 (:file "gtype" :depends-on ("proxy"))
		 (:file "gboxed" :depends-on ("gtype"))
		 (:file "genums" :depends-on ("gtype"))
		 (:file "gparam" :depends-on ("genums"))
		 (:file "ginterface" :depends-on ("gtype"))
		 (:file "gobject" :depends-on ("gparam"))
		 (:file "gcallback" :depends-on ("gtype" "gparam" "gobject" "alien"))
		 (:file "export" :depends-on ("utils" "glib" "proxy" "gboxed" "gtype" "gparam" "gcallback" "genums" "gobject"))))
