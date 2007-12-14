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
  		 (:shared-object "glib-preload" :pathname "preload/"
  		  :components ((:c-source-file "logging"
  				:cflags #.(pkg-cflags "glib-2.0"))))
		 (:library "libglib-2.0"
		  :libdir #.(pkg-libdir "glib-2.0")
		  :libname #-win32 "libglib-2.0"
		           #+win32 "libglib-2.0-0"
		  :depends-on ("glib-preload"))
		 (:library "libgobject-2.0"
		  :libdir #.(pkg-libdir "glib-2.0")
		  :libname #-win32 "libgobject-2.0"
		           #+win32 "libgobject-2.0-0"
		  :depends-on ("libglib-2.0"))
		 (:shared-object "glib-alien" :pathname "alien/"
		  :components ((:c-source-file "gobject" 
				:cflags #.(pkg-cflags "glib-2.0")))
		  :depends-on ("libgobject-2.0"))
		 (:file "glib" :depends-on ("libglib-2.0" "defpackage"))
		 (:file "gtype" :depends-on ("glib-alien" "libgobject-2.0" "glib"))
		 (:file "gboxed" :depends-on ("gtype"))
		 (:file "genums" :depends-on ("gtype"))
		 (:file "gparam" :depends-on ("genums"))
		 (:file "gobject" :depends-on ("gparam"))
		 (:file "ginterface" :depends-on ("gobject"))
		 (:file "gcallback" :depends-on ("gtype" "gparam" "gobject"))
		 (:file "gerror" :depends-on ("gcallback"))))