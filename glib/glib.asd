;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GLIB-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(ext:unlock-all-packages)

;;; Better put this in ~/.cmucl-init.lisp or some other file read at startup
;; (setf
;;  (logical-pathname-translations "clg")
;;  '(("**;*.*.*" "/home/espen/src/clg/**/")))

(in-package "GLIB-SYSTEM")

(pkg-exists-p "glib-2.0" :atleast-version "2.4.0")

(defvar *cmucl-include-path* "/usr/lib/cmucl/include")


(defsystem glib
    :depends-on (clg-tools)
    :components ((:file "defpackage")
		 (:file "pcl")
		 ;; It is necessary to load this before libglib-2.0.so,
		 ;; otherwise our implementation of g_logv won't be
		 ;; used by the library
		 (:unix-dso "alien"
		  :components ((:c-source-file "callback"
				:definitions ("CMUCL")
				:include-paths (#.*cmucl-include-path*)
				:cflags #.(pkg-cflags "glib-2.0"))
			       (:c-source-file "gobject" 
				:cflags #.(pkg-cflags "glib-2.0"))))
		 (:library "libglib-2.0" 
			    :libdir #.(pkg-variable "glib-2.0" "libdir")
			    :depends-on ("alien"))
		 (:library "libgobject-2.0" 
			    :libdir #.(pkg-variable "glib-2.0" "libdir")
			    :depends-on ("libglib-2.0"))
		 (:file "utils" :depends-on ("defpackage"))
		 (:file "ffi" :depends-on ("utils"))
		 (:file "glib" :depends-on ("ffi" "libglib-2.0"))
		 (:file "proxy" :depends-on ("pcl" "glib"))
		 (:file "gtype" :depends-on ("proxy" "libgobject-2.0"))
		 (:file "gboxed" :depends-on ("gtype"))
		 (:file "genums" :depends-on ("gtype"))
		 (:file "gparam" :depends-on ("genums"))
		 (:file "gobject" :depends-on ("gparam"))
		 (:file "ginterface" :depends-on ("gobject"))
		 (:file "gcallback" :depends-on ("gtype" "gparam" "gobject" "alien"))
		 (:file "export" :depends-on ("utils" "glib" "proxy" "gboxed" "gtype" "gparam" "gcallback" "genums" "gobject"))))
