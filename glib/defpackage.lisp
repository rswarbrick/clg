;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: defpackage.lisp,v 1.9 2006-02-08 21:43:33 espen Exp $

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
	   #-sbcl>=0.9.8"INITIALIZE-INTERNAL-SLOT-GFS")
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
	   "PACKAGE-PREFIX" "DEFCALLBACK" "CALLBACK" "CALL-NEXT-HANDLER")
  (:export "LONG" "UNSIGNED-LONG" "INT" "UNSIGNED-INT" "SHORT" "UNSIGNED-SHORT"
	   "SIGNED" "UNSIGNED" "CHAR" "POINTER" "COPY-OF")
  (:export "LOCATION" "ALLOCATION" "DIRECT-SLOTS" "READER-FUNCTION" 
	   "WRITER-FUNCTION" "BOUNDP-FUNCTION" 
	   "INITIALIZE-INTERNAL-SLOT-FUNCTIONS"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS"))

