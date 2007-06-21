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
  #?(or 
     (pkg-config:featurep :cmu) 
     (and (pkg-config:sbcl< 1 0 6) (not (featurep :win32))))
  (:import-from #+cmu"LISP" #+sbcl"SB-IMPL"
	   "*PERIODIC-POLLING-FUNCTION*" "*MAX-EVENT-TO-SEC*" 
 	   "*MAX-EVENT-TO-USEC*")		
  #+clisp
  (:import-from "SOCKET" "SOCKET-STATUS")
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
  ;; Symbols re-exported from the GLIB package
  (:export "SIGNAL-EMIT-STOP" "SIGNAL-CONNECT" "SIGNAL-DISCONNECT"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK"
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE")
  ;; Symbols re-exported from the GDK package 
  (:import-from "GDK" "TIMEOUT-ADD-WITH-LOCK" "IDLE-ADD-WITH-LOCK")
  (:export "TIMEOUT-ADD-WITH-LOCK" "IDLE-ADD-WITH-LOCK")
  ;; Symbols not automatically re-exported from the GTK package
  (:export "TOGGLE" "CONTAINER-CHILD"))

