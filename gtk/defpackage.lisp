(defpackage "GTK"
  (:use "GFFI" "GLIB" "COMMON-LISP" "AUTOEXPORT" "PKG-CONFIG" "CLG-UTILS")
  #+cmu(:use "PCL" "EXT")
  #+sbcl(:use "SB-PCL" "SB-EXT")
  #+clisp(:use "CLOS")
  #+clisp(:shadowing-import-from "GFFI" "SLOT-DEFINITION-TYPE")
  #+(or cmu sbcl)
  (:import-from #+cmu"PCL" #+sbcl"SB-PCL"
	   "ADD-READER-METHOD" "ADD-WRITER-METHOD")
  #+(or cmu sbcl)
  (:import-from #+cmu"SYSTEM" #+sbcl"SB-SYS" "SAP-INT" "ADD-FD-HANDLER" "REMOVE-FD-HANDLER")
  #?(or (pkg-config:featurep :cmu) (and (pkg-config:featurep :sbcl) (not (pkg-config:sbcl>= 1 0 6))))
  (:import-from #+cmu"LISP" #+sbcl"SB-IMPL"
	   "*PERIODIC-POLLING-FUNCTION*" "*MAX-EVENT-TO-SEC*" 
 	   "*MAX-EVENT-TO-USEC*")		
  (:export "EVENTS-PENDING-P" "GET-CURRENT-EVENT" "MAIN-DO-EVENT" "MAIN"
	   "MAIN-LEVEL" "MAIN-QUIT" "MAIN-ITERATION-DO" "MAIN-ITERATE-ALL")
  (:export "CONTAINER-CHILD-CLASS" "CONTAINER-CHILD" "CONTAINER-CHILD-CLASS")
  ;; Signal names that need to be explicit exported
  (:export "TOGGLED")
  ;; Symbols re-exported from glib -- will not be exported from
  ;; this package in the future
  (:export "SIGNAL-EMIT-STOP" "SIGNAL-CONNECT" "SIGNAL-DISCONNECT"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK"
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE"))


(defpackage "CLG"
  (:use "GLIB" "GTK")
  ;; Symbols re-exported from the GTK package
  (:export "TOGGLE" "CONTAINRE-CHILD")
  ;; Symbols re-exported from GLIB package
  (:export "SIGNAL-EMIT-STOP" "SIGNAL-CONNECT" "SIGNAL-DISCONNECT"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK"
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE"))
