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
  (:import-from #+cmu"SYSTEM" #+sbcl"SB-SYS" "SAP-INT" "ADD-FD-HANDLER")
  #+(or cmu sbcl)
  (:import-from #+cmu"LISP" #+sbcl"SB-IMPL"
	   "*PERIODIC-POLLING-FUNCTION*" "*MAX-EVENT-TO-SEC*" 
	   "*MAX-EVENT-TO-USEC*")		
  (:export "EVENTS-PENDING-P" "GET-CURRENT-EVENT" "MAIN-DO-EVENT" "MAIN"
	   "MAIN-LEVEL" "MAIN-QUIT" "MAIN-ITERATION-DO" "MAIN-ITERATE-ALL")
  (:export "CONTAINER-CHILD-CLASS" "CONTAINER-CHILD")
  ;; Symbols re-exported from glib
  (:export "SIGNAL-EMIT-STOP" "SIGNAL-CONNECT" "SIGNAL-DISCONNECT"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK"
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE"))
