;;; -*- Mode: lisp -*-

(defpackage #:clg-tools
  (:use #:common-lisp #:asdf))


(in-package #:clg-tools)


(defsystem clg-tools
    :components ((:file "autoexport")
		 (:file "config")
		 (:file "asdf-extensions")))

