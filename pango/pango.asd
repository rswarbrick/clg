;;; -*- Mode: lisp -*-

(defpackage "PANGO-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))


(in-package "PANGO-SYSTEM")

(pkg-exists-p "pango" :atleast-version "1.4.0" :error t)


(defsystem pango
    :depends-on (gffi glib cairo)
    :components ((:library "libpango-1.0"
			   :libname #-win32 "libpango-1.0"
			            #+win32 "libpango-1.0-0"
			   :libdir #.(pkg-libdir "pango"))
		 (:library "libpangoxft-1.0"
			   :libname #-win32 "libpangoxft-1.0"
			            #+win32 "libpangowin32-1.0-0"
			   :libdir #.(pkg-libdir "pango"))
		 (:library "libpangoft2-1.0" 
			   :libname #-win32 "libpangoft2-1.0" 
			            #+win32 "libpangoft2-1.0-0" 
			   :libdir #.(pkg-libdir "pango"))
		 (:library "libpangocairo-1.0"
			   :libname #-win32 "libpangocairo-1.0" 
			            #+win32 "libpangocairo-1.0-0" 
			   :libdir #.(pkg-libdir "pango"))
		 (:file "defpackage")
		 (:file "pango" :depends-on ("defpackage"
					     "libpango-1.0" "libpangoxft-1.0" "libpangoft2-1.0" "libpangocairo-1.0"))
		 (:file "export" :depends-on ("pango"))))

