;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <espejohn@online.no>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; $id$


(defpackage "GTK"
  (:use "GLIB" "COMMON-LISP" "PCL" "ALIEN" "AUTOEXPORT")
  (:shadowing-import-from "GLIB" "DEFTYPE")
  (:shadowing-import-from "PCL"
   "CLASS-NAME" "CLASS-OF" "FIND-CLASS" "LOCATION" "ALLOCATION" "DIRECT-SLOTS")
  (:export "*CLG-VERSION*")
  (:export "OBJECT" "OBJECT-ARG" "OBJECT-SINK")
  (:export "REGISTER-USER-DATA" "FIND-USER-DATA" "REGISTER-CALLBACK-FUNCTION"
	   "*CALLBACK-MARSHAL*" "*DESTROY-MARSHAL*")
  (:export "EVENTS-PENDING-P" "GET-CURRENT-EVENT" "MAIN-DO-EVENT" "MAIN"
	   "MAIN-LEVEL" "MAIN-QUIT" "MAIN-ITERATION" "MAIN-ITERATE-ALL"
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE")
  (:export "SIGNAL-EMIT-STOP" "SIGNAL-CONNECT" "SIGNAL-DISCONNECT"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK")
  (:export "OBJECT-CLASS" "WIDGET-CLASS" "CONTAINER-CLASS" "CHILD-CLASS"))
 