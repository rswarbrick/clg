;;; -*- Mode: lisp -*-

(asdf:oos 'asdf:load-op :clg-tools)

(defpackage "GFFI-SYSTEM"
  (:use "COMMON-LISP" "ASDF" "PKG-CONFIG"))

#+cmu(ext:unlock-all-packages)
#+sbcl
(progn
  (sb-ext:unlock-package "COMMON-LISP")
  (sb-ext:unlock-package "SB-PCL"))

(in-package "GFFI-SYSTEM")


#+(and sbcl (not alien-callbacks))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-symbol "DEFINE-ALIEN-FUNCTION" "SB-ALIEN")
    (error "You need to upgrade SBCL to a version with native C callback support or see the README file about how to add third party callbacks to your current SBCL version.")))

#+(and sbcl alien-callbacks)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "DEFINE-ALIEN-FUNCTION" "SB-ALIEN")
    (error "Third party C callback code detected in a SBCL image with native callback support. As clg now uses native callbacks when available, you need to use a \"clean\" core file.")))


(defsystem gffi
    :depends-on (clg-tools)
    :components ((:file "defpackage")
		 #+(and cmu19a (not non-broken-pcl))(:file "pcl")
		 (:unix-dso "alien" :components ((:c-source-file "memory")))
		 (:file "memory" :depends-on ("defpackage"))
		 (:file "interface" :depends-on ("memory"))
		 (:file "basic-types" :depends-on ("alien" "interface"))
		 (:file "vectors" :depends-on ("basic-types"))
		 (:file "enums" :depends-on ("basic-types"))
		 (:file "virtual-slots" :depends-on (#+(and cmu19a (not non-broken-pcl))"pcl" "interface" "basic-types"))
		 (:file "proxy" :depends-on ("virtual-slots"))))
