;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GLADE-XML-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

(in-package "GLADE-XML-SYSTEM")

(defsystem glade-xml
  :depends-on (gtk s-xml parse-number)
  :components ((:file "defpackage")
	       (:file "glade-xml" :depends-on ("defpackage"))))
