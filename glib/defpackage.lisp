;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2006 Espen S. Johnsen <espen@users.sf.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; $Id: defpackage.lisp,v 1.24 2007-09-07 07:31:40 espen Exp $


(defpackage "GLIB"
  (:use "COMMON-LISP" "GFFI" "AUTOEXPORT" "PKG-CONFIG" "CLG-UTILS" #+sbcl"SB-SIMPLE-STREAMS" ;#+sbcl"SB-GRAY"
	)
  #+cmu(:use "PCL" "EXT")
  #+sbcl(:use "SB-MOP" "SB-EXT")
  #+sb-thread(:use "SB-THREAD")
  #+clisp(:use "CLOS")
  #+clisp(:import-from "EXT" "RUN-PROGRAM")
  #+clisp(:shadowing-import-from "GFFI" "SLOT-DEFINITION-TYPE")
  (:export "USER-DATA-DESTROY-CALLBACK")

  ;; Symbols from glib.lisp  
  (:export "REGISTER-USER-DATA" "FIND-USER-DATA" "USER-DATA-EXISTS-P"
	   "UPDATE-USER-DATA" "DESTROY-USER-DATA" "QUARK" "QUARK-INTERN"
	   "QUARK-TO-STRING" "GLIST" "GSLIST" "REALLOCATE-MEMORY"
	    "USER-CALLBACK")
  ;; Symbols from gtype.lisp  
  (:export "TYPE-NUMBER" "GTYPE" "TYPE-QUERY" "TYPE-INSTANCE-SIZE"
	   "TYPE-CLASS-SIZE" "TYPE-CLASS-REF" "TYPE-CLASS-UNREF"
	   "TYPE-CLASS-PEEK" "TYPE-NUMBER-FROM-GLIB-NAME"
	   "REGISTER-TYPE" "REGISTER-TYPE-ALIAS""REGISTER-NEW-TYPE"
	   "FIND-TYPE-NUMBER" "TYPE-FROM-NUMBER" "SUPERTYPE"
	   "GINSTANCE-CLASS" "GINSTANCE" "REGISTER-DERIVABLE-TYPE"
	   "INIT-TYPES-IN-LIBRARY" "DEFINE-TYPES-BY-INTROSPECTION"
	   "TYPE-FROM-GLIB-NAME")
  ;; Symbols from gparam.lisp  
  (:export "GVALUE" "GVALUE-INIT" "GVALUE-NEW" "GVALUE-FREE" "GVALUE-TYPE" 
	   "GVALUE-GET" "GVALUE-SET" "GVALUE-UNSET" "VALUE-P" "WITH-GVALUE" 
	   "+GVALUE-SIZE+" "+GVALUE-VALUE-OFFSET+" "PARAM-FLAG-TYPE" "PARAM" 
	   "PARAM-CHAR" "PARAM-UNSIGNED-CHAR" "PARAM-BOOLEAN" "PARAM-INT" 
	   "PARAM-UNSIGNED-INT" "PARAM-LONG" "PARAM-UNSIGNED-LONG"
	   "PARAM-UNICHAR" "PARAM-ENUM" "PARAM-FLAGS" "PARAM-SINGLE-FLOAT"
	   "PARAM-DOUBLE-FLOAT" "PARAM-STRING" "PARAM-PARAM" "PARAM-BOXED"
	   "PARAM-POINTER" "PARAM-VALUE-ARRAY" "PARAM-OBJECT" 
	   "PARAM-OVERRRIDE" "PARAM-NAME" "PARAM-FLAGS" "PARAM-VALUE-TYPE"
	   "PARAM-OWNER-TYPE" "PARAM-NICKNAME" "PARAM-DOCUMENTATION"
	   "PARAM-MINIMUM" "PARAM-DEFAULT-VALUE" "PARAM-ENUM-CLASS"
	   "PARAM-FLAGS-CLASS" "PARAM-FLOAT-EPSILON"
	   "PARAM-VALUE-ARRAY-ELEMENT-SPEC" "PARAM-VALUE-ARRAY-LENGTH")
  ;; Symbols from gboxed.lisp  
  (:export "BOXED" "BOXED-CLASS" "STRINGS")
  ;; Symbols from gcallback.lisp  
  (:export "GCLOSURE" "REGISTER-CALLBACK-FUNCTION" "INVOKE-CALLBACK" 
	   "TIMEOUT-ADD" "TIMEOUT-REMOVE" "IDLE-ADD" "IDLE-REMOVE"
	   "SOURCE-REMOVE" "+PRIORITY-DEFAULT-IDLE+" "+PRIORITY-DEFAULT+"
	   "+PRIORITY-HIGH+" "+PRIORITY-HIGH-IDLE+" "+PRIORITY-LOW+"
	   "ENSURE-SIGNAL-ID" "SIGNAL-LIST-NAMES" "SIGNAL-LOOKUP" 
	   "SIGNAL-LIST-IDS" "DESCRIBE-SIGNAL" "DEFINE-SIGNAL-HANDLER"
	   "CALL-NEXT-HANDLER" "MAKE-CALLBACK-CLOSURE" "INVOKE-SOURCE-CALLBACK"
	   "SIGNAL-STOP-EMISSION" "SIGNAL-ADD-EMISSION-HOOK"
	   "SIGNAL-REMOVE-EMISSION-HOOK" "SIGNAL-HAS-HANDLER-PENDING-P"
	   "SIGNAL-HANDLER-BLOCK" "SIGNAL-HANDLER-UNBLOCK" 
	   "SIGNAL-HANDLER-DISCONNECT" "SIGNAL-HANDLER-IS-CONNECTED-P" 
	   "COMPUTE-SIGNAL-FUNCTION" "SIGNAL-CONNECT" "COMPUTE-SIGNAL-ID"
	   "SIGNAL-EMIT-WITH-DETAIL" "SIGNAL-EMIT" "DEFINE-CALLBACK-MARSHAL"
	   "WITH-CALLBACK-FUNCTION" "SIGNAL-NEW" "*SIGNAL-STOP-EMISSION*")
  ;; Symbols from gobject.lisp  
  (:export "GOBJECT-CLASS" "INSTANCE-SLOTS-P" "DIRECT-PROPERTY-SLOT-DEFINITION"
	   "SLOT-DEFINITION-PNAME" "CONSTRUCT-ONLY-PROPERTY-P" 
	   "EFFECTIVE-PROPERTY-SLOT-DEFINITION"
	   "DIRECT-USER-DATA-SLOT-DEFINITION" "EFFECTIVE-USER-DATA-SLOT-DEFINITION"
	   "GOBJECT" "OBJECT-FREEZE-NOTIFY" "OBJECT-THAW-NOTIFY" "USER-DATA"
	   "USER-DATA-DESTROY-CALLBACK" "USER-DATA-P" "UNSET-USER-DATA"
	   "QUERY-OBJECT-CLASS-PROPERTIES" "SLOT-DEFINITIONS" 
	   "EXPAND-GOBJECT-TYPE" "GOBJECT-DEPENDENCIES" "SIGNAL-NAME-TO-STRING"
	   "REFERENCED")
  ;; Symbols from ginterface.lisp  
  (:export "INTERFACE" "INTERFACE-CLASS" "QUERY-OBJECT-INTERFACE-PROPERTIES")
  ;; Symbols from gerror.lisp  
  (:export "GERROR" "GERROR-DOMAIN" "GERROR-CODE" "GERROR-MESSAGE" 
	   "GLIB-ERROR" "GLIB-FILE-ERROR" "SIGNAL-GERROR" 
	   "GERROR-SIGNAL" "LOG-LEVELS" "LOG-DOMAIN" "LOG-MESSAGE" 
	   "UNKNOWN-LOG-LEVEL" "ERROR-LOG-LEVEL" "CRITICAL-LOG-LEVEL" 
	   "WARNING-LOG-LEVEL" "INFO-LOG-LEVEL" "DEBUG-LOG-LEVEL")
  ;; Symbols from genum.lisp  
  (:export "QUERY-ENUM-VALUES"))

