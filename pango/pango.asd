;;; -*- Mode: lisp -*-

(defpackage #:pango-system
  (:use #:common-lisp #:asdf #:pkg-config #:sharedlib))


(in-package #:pango-system)

(pkg-exists-p "pango" :atleast-version "1.4.0")

(sharedlib:load-shared-library "libpango-1.0")

(defsystem pango
    :depends-on (glib)
    :components ((:file "defpackage")
		 (:file "pango" :depends-on ("defpackage"))
		 (:file "export" :depends-on ("pango"))))
