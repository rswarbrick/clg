;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)
(asdf:oos 'asdf:load-op :gffi)

(defpackage "GLIB-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

;;; Better put this in ~/.cmucl-init.lisp or some other file read at startup
;; (setf
;;  (logical-pathname-translations "clg")
;;  '(("**;*.*.*" "/home/espen/src/clg/**/")))

(in-package "GLIB-SYSTEM")

(pkg-exists-p "glib-2.0" :atleast-version "2.4.0" :error t)

(defsystem glib
    :depends-on (clg-tools gffi)
    :components ((:file "defpackage")
		 ;; For preloading to work in glib 2.6, the library needs to 
		 ;; be configured and build with '--disable-visibility'
  		 (:unix-dso "preload"
  		  :components ((:c-source-file "logging"
  				:cflags #.(pkg-cflags "glib-2.0"))))
		 (:library "libglib-2.0" 
			    :libdir #.(pkg-variable "glib-2.0" "libdir")
 			    :depends-on ("preload"))
		 (:library "libgobject-2.0" 
			    :libdir #.(pkg-variable "glib-2.0" "libdir")
			    :depends-on ("libglib-2.0"))
		 (:unix-dso "alien"
		  :components ((:c-source-file "callback"
				:cflags #.(pkg-cflags "glib-2.0"))
			       (:c-source-file "gobject" 
				:cflags #.(pkg-cflags "glib-2.0")))
		  :depends-on ("libgobject-2.0"))
		 (:file "glib" :depends-on ("libglib-2.0" "defpackage"))
		 (:file "gtype" :depends-on ("alien" "libgobject-2.0" "glib"))
		 (:file "gboxed" :depends-on ("gtype"))
		 (:file "genums" :depends-on ("gtype"))
		 (:file "gparam" :depends-on ("genums"))
		 (:file "gobject" :depends-on ("gparam"))
		 (:file "ginterface" :depends-on ("gobject"))
		 (:file "gcallback" :depends-on ("gtype" "gparam" "gobject" "alien"))
		 (:file "gerror" :depends-on ("gcallback"))
		 (:file "export" :depends-on ("glib" "gboxed" "gtype" "gparam" "gcallback" "genums" "gobject" "gerror"))))
