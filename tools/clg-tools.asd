;;; -*- Mode: lisp -*-

(defpackage #:clg-tools
  (:use #:common-lisp #:asdf)
  )


(in-package #:clg-tools)


(defsystem clg-tools
    :depends-on (uffi)
    :components ((:file "autoexport")
		 (:file "config")
		 (:file "sharedlib")
		 (:file "asdf-extensions")))

