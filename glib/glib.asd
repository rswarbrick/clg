;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GLIB-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

#+cmu(ext:unlock-all-packages)
#+sbcl
(progn
  (sb-ext:unlock-package "COMMON-LISP")
  (sb-ext:unlock-package "SB-PCL"))

;;; Better put this in ~/.cmucl-init.lisp or some other file read at startup
;; (setf
;;  (logical-pathname-translations "clg")
;;  '(("**;*.*.*" "/home/espen/src/clg/**/")))

(in-package "GLIB-SYSTEM")

(pkg-exists-p "glib-2.0" :atleast-version "2.4.0")

(when (string>= (pkg-version "glib-2.0") "2.6.0")
  (push :glib2.6 *features*))

(when (string>= (pkg-version "glib-2.0") "2.8.0")
  (push :glib2.8 *features*))

(defsystem glib
    :depends-on (clg-tools)
    :components ((:file "defpackage")
		 #+(and cmu (not non-broken-pcl) (not cmu19b))(:file "pcl")
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
		 (:file "utils" :depends-on ("defpackage"))
		 (:file "ffi" :depends-on ("utils"))
		 (:file "glib" :depends-on ("ffi" "libglib-2.0"))
		 (:file "proxy" :depends-on (#+(and cmu (not non-broken-pcl) (not cmu19b))"pcl" "glib"))
		 (:file "gtype" :depends-on ("proxy" "alien" "libgobject-2.0"))
		 (:file "gboxed" :depends-on ("gtype"))
		 (:file "genums" :depends-on ("gtype"))
		 (:file "gparam" :depends-on ("genums"))
		 (:file "gobject" :depends-on ("gparam"))
		 (:file "ginterface" :depends-on ("gobject"))
		 (:file "gcallback" :depends-on ("gtype" "gparam" "gobject" "alien"))
		 (:file "gerror" :depends-on ("gcallback"))
		 (:file "export" :depends-on ("utils" "glib" "proxy" "gboxed" "gtype" "gparam" "gcallback" "genums" "gobject" "gerror"))))
