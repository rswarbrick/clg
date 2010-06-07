(asdf:oos 'asdf:load-op :clg-tools)

(defpackage :glib-json-system
  (:use :cl :asdf :pkg-config))
(in-package :glib-json-system)

(pkg-exists-p "json-glib-1.0" :atleast-version "0.10.2" :error t)

(defsystem glib-json
  :depends-on (:gffi :glib)
  :components
  ((:library "libjson-glib-1.0" :libdir #.(pkg-libdir "json-glib-1.0"))
   (:file "defpackage")
   (:file "types" :depends-on ("defpackage" "libjson-glib-1.0"))

   (:file "parser" :depends-on ("types"))))
