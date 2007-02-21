;;; -*- Mode: lisp -*-

(defpackage #:clg-tools
  (:use #:common-lisp #:asdf))


(in-package #:clg-tools)

#+clisp
(unless custom:*ansi*
  (error "CLISP must be started with the -ansi option"))

(defsystem clg-tools
    :components ((:file "autoexport")
		 (:file "utils")
		 (:file "config" :depends-on ("utils"))
		 (:file "asdf-extensions" :depends-on ("utils"))))

