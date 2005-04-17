;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: defpackage.lisp,v 1.6 2005-04-17 21:49:19 espen Exp $

;(export 'kernel::type-expand-1 "KERNEL")

(defpackage "GLIB"
  (:use "COMMON-LISP""AUTOEXPORT")
  #+cmu(:use "SYSTEM" "KERNEL" "PCL" "EXT")
  #+sbcl(:use "SB-SYS" "SB-KERNEL" "SB-PCL" "SB-EXT")
  #+cmu(:shadowing-import-from "PCL"
           "CLASS-DIRECT-SUPERCLASSES" "CLASS-DIRECT-SUPERCLASSES")
  (:shadow "POINTER")
  (:import-from #+cmu"PCL" #+sbcl"SB-PCL"
	   "LOCATION" "ALLOCATION" "DIRECT-SLOTS" 
	   "READER-FUNCTION" "WRITER-FUNCTION" "BOUNDP-FUNCTION" 
	   "INITIALIZE-INTERNAL-SLOT-FUNCTIONS" "COMPUTE-SLOT-ACCESSOR-INFO"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS"
	   "INITIALIZE-INTERNAL-SLOT-GFS")
  #+sbcl(:import-from "SB-EXT" "COLLECT")
  #+cmu(:import-from "ALIEN" "CALLBACK")
  (:import-from #+cmu"ALIEN" #+sbcl"SB-ALIEN" 
	   "WITH-ALIEN" "ALIEN-FUNCALL" "%HEAP-ALIEN" "MAKE-HEAP-ALIEN-INFO" 
	   "ADDR" "PARSE-ALIEN-TYPE" "SYSTEM-AREA-POINTER" "EXTERN-ALIEN")
  #+cmu(:import-from "C-CALL" "%NATURALIZE-C-STRING" "VOID")
  #+sbcl(:import-from "SB-ALIEN" 
	   "%NATURALIZE-UTF8-STRING"  "%DEPORT-UTF8-STRING" "VOID")
  (:export "DEFTYPE-METHOD" "TRANSLATE-TYPE-SPEC" "TRANSLATE-TO-ALIEN"
	   "TRANSLATE-FROM-ALIEN" "CLEANUP-ALIEN" "UNREFERENCE-ALIEN"
	   "SIZE-OF" "UNBOUND-VALUE")
  (:export "DEFBINDING" "DEFINE-FOREIGN" "MKBINDING" "USE-PREFIX"
	   "PACKAGE-PREFIX" "DEFCALLBACK" "CALLBACK")
  (:export "LONG" "UNSIGNED-LONG" "INT" "UNSIGNED-INT" "SHORT" "UNSIGNED-SHORT"
	   "SIGNED" "UNSIGNED" "CHAR" "POINTER" "COPY-OF")
  (:export "LOCATION" "ALLOCATION" "DIRECT-SLOTS" "READER-FUNCTION" 
	   "WRITER-FUNCTION" "BOUNDP-FUNCTION" 
	   "INITIALIZE-INTERNAL-SLOT-FUNCTIONS"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS"))

