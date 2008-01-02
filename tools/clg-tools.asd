;;; -*- Mode: lisp -*-

(defpackage #:clg-tools
  (:use #:common-lisp #:asdf))


(in-package #:clg-tools)

#+clisp
(unless custom:*parse-namestring-ansi*
  (error "Standard behaviour of PARSE-NAMESTRING must be enabled by setting CUSTOM:*PARSE-NAMESTRING-ANSI* to non-NIL or running clisp with the -ansi option"))

(defsystem clg-tools
  :depends-on (#+sbcl sb-posix)
  :components ((:file "utils")
	       (:file "config" :depends-on ("utils"))
	       (:file "asdf-extensions" :depends-on ("utils"))
	       (:file "autoexport" :depends-on ("asdf-extensions"))))
