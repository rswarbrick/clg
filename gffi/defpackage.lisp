;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: defpackage.lisp,v 1.16 2008-12-10 02:39:17 espen Exp $

(defpackage "GFFI"
  (:use "COMMON-LISP" "AUTOEXPORT" "PKG-CONFIG" "CLG-UTILS")
  #+cmu(:use "SYSTEM" "KERNEL" "PCL" "EXT")
  #+sbcl(:use "SB-SYS" "SB-KERNEL" "SB-MOP" "SB-EXT")
  #+clisp(:use "CLOS" "EXT")
  #+(or cmu sbcl)(:shadow "POINTER")
  #+cmu(:shadowing-import-from "PCL" "CLASS-DIRECT-SUPERCLASSES")
  #+clisp(:shadow "TYPE-EXPAND-1" "SLOT-DEFINITION-TYPE" "BYTE")
  #+(or cmu sbcl)
  (:import-from #+cmu"PCL" #+sbcl"SB-PCL"
	   "READER-FUNCTION" "WRITER-FUNCTION" "BOUNDP-FUNCTION" 
	   "INITIALIZE-INTERNAL-SLOT-FUNCTIONS" "COMPUTE-SLOT-ACCESSOR-INFO"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS"
	   #?-(pkg-config:sbcl>= 0 9 8)"INITIALIZE-INTERNAL-SLOT-GFS")
  #+cmu(:import-from "ALIEN" "CALLBACK")
  #+(or cmu sbcl)
  (:import-from #+cmu"ALIEN" #+sbcl"SB-ALIEN" 
	   "WITH-ALIEN" "ALIEN-FUNCALL" "%HEAP-ALIEN" "MAKE-HEAP-ALIEN-INFO" 
	   "ADDR" "PARSE-ALIEN-TYPE" "SYSTEM-AREA-POINTER" "EXTERN-ALIEN"
	   "ALIEN-SAP")
  #+cmu(:import-from "C-CALL" "VOID" "C-STRING")
  #+sbcl(:import-from "SB-ALIEN" "VOID" "C-STRING")
  ;; Symbols from memory.lisp
  (:export "MAKE-POINTER" "POINTER-ADDRESS" "NULL-POINTER-P" "POINTER=" 
	   "POINTER+" "REF-POINTER" 
	   "REF-INT-8" "REF-INT-16" "REF-INT-32" "REF-INT-64"
	   "REF-UINT-8" "REF-UINT-16" "REF-UINT-32" "REF-UINT-64"
	   "REF-DOUBLE-FLOAT" "REF-SINGLE-FLOAT" "ALLOCATE-MEMORY"
	   "DEALLOCATE-MEMORY" "COPY-MEMORY" "CLEAR-MEMORY" "MEMORY-CLEAR-P"
	   "WITH-MEMORY" "WITH-POINTER" "*MEMORY-ALLOCATOR*" 
	   "*MEMORY-DEALLOCATOR*" #+cmu"WITH-PINNED-OBJECTS")
  ;; Symbols from interface.lisp
  (:export "DEFBINDING" "MKBINDING" "USE-PREFIX" "PACKAGE-PREFIX" 
	   "DEFINE-CALLBACK" "CALLBACK" "CALLBACK-ADDRESS"
	   "DEFINE-TYPE-GENERIC" "DEFINE-TYPE-METHOD" "IN-ARG-P" "OUT-ARG-P"
	   "DEFAULT-ALIEN-TYPE-NAME" "DEFAULT-TYPE-NAME" "TYPE-EXPAND" 
	   "TYPE-EXPAND-1" "TYPE-EXPAND-TO" "TYPE-EQUAL-P")
  ;; Symbols from basic-types.lisp
  (:export "LONG" "UNSIGNED-LONG" "INT" "UNSIGNED-INT" "SHORT" "UNSIGNED-SHORT"
	   "SIGNED" "UNSIGNED" "CHAR" "POINTER" "BOOL" "COPY-OF" "STATIC" 
	   "SIZE-OF" "TYPE-ALIGNMENT" "ALIEN-TYPE" "UNBOUND-VALUE" 
	   "ALIEN-ARG-WRAPPER" "TO-ALIEN-FORM" "FROM-ALIEN-FORM"
	   "CALLBACK-WRAPPER" "TO-ALIEN-FUNCTION" "FROM-ALIEN-FUNCTION" 
	   "READER-FUNCTION" "WRITER-FUNCTION" "GETTER-FUNCTION"
	   "PEEK-FUNCTION" "DESTROY-FUNCTION" "UNBOUND-VALUE"
	   "COPY-FUNCTION" "ASSERT-INLINED" "ASSERT-NOT-INLINED"
	   "UTF8-LENGTH" "OPTIMIZED-DOUBLE-FLOAT" "POINTER-DATA"
	   "ARGUMENT-TYPE" "RETURN-TYPE")
  ;; Symbols from vector.lisp
  (:export "MAKE-C-VECTOR" "MAP-C-VECTOR" "WITH-C-VECTOR" "COUNTED-VECTOR"
	   "NULL-TERMINATED-VECTOR" "VECTOR-READER-FUNCTION"
	   "VECTOR-WRITER-FUNCTION" "VECTOR-REF-INT-8" "VECTOR-REF-INT-16"
	   "VECTOR-REF-UINT-8" "VECTOR-REF-UINT-16" "VECTOR-REF-UINT-32" 
	   "VECTOR-REF-UINT-64" "VECTOR-REF-DOUBLE-FLOAT" 
	   "VECTOR-REF-SINGLE-FLOAT" "UNBOXED-VECTOR")
  ;; Symbols from enums.lisp
  (:export "ENUM" "ENUM-INT" "INT-ENUM" "ENUM-MAPPING" "DEFINE-ENUM-TYPE"
	   "FLAGS" "DEFINE-FLAGS-TYPE")
  ;; Symbols from virtual-slots.lisp  
  (:export "VIRTUAL-SLOTS-CLASS" "DIRECT-VIRTUAL-SLOT-DEFINITION"
	   "EFFECTIVE-VIRTUAL-SLOT-DEFINITION" "DIRECT-SPECIAL-SLOT-DEFINITION"
	   "EFFECTIVE-SPECIAL-SLOT-DEFINITION" "COMPUTE-MOST-SPECIFIC-INITARGS"
	   "MOST-SPECIFIC-SLOT-VALUE" "VIRTUAL-SLOTS-OBJECT"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS" "BOUNDP-FUNCTION"
	   "COMPUTE-SLOT-READER-FUNCTION" "COMPUTE-SLOT-BOUNDP-FUNCTION"
	   "COMPUTE-SLOT-WRITER-FUNCTION" "COMPUTE-SLOT-MAKUNBOUND-FUNCTION"
	   "SLOT-READABLE-P" "SLOT-WRITABLE-P" #+clisp"SLOT-DEFINITION-TYPE")
  ;; Symbols from proxy.lisp  
  (:export "CACHE-INSTANCE" "FIND-CACHED-INSTANCE" "LIST-CACHED-INSTANCES"
           "REMOVE-CACHED-INSTANCE" "PROXY" "INSTANCE-FINALIZER" 
	   "REFERENCE-FUNCTION" "UNREFERENCE-FUNCTION" "INVALIDATE-INSTANCE"
	   "ALLOCATE-FOREIGN" "FOREIGN-LOCATION" "PROXY-VALID-P" 
	   "MOST-SPECIFIC-PROXY-SUPERCLASS" "DIRECT-PROXY-SUPERCLASS"
	   "PROXY-CLASS" "FOREIGN-SIZE-P" "DIRECT-ALIEN-SLOT-DEFINITION"
	   "EFFECTIVE-ALIEN-SLOT-DEFINITION" 
	   "DIRECT-VIRTUAL-ALIEN-SLOT-DEFINITION"
	   "EFFECTIVE-VIRTUAL-ALIEN-SLOT-DEFINITION"
	   "FOREIGN-SIZE" "REF" "UNREF" "REF-COUNTED-OBJECT"
	   "ENSURE-PROXY-INSTANCE" "MAKE-PROXY-INSTANCE"  "STRUCT" 
	   "STRUCT-CLASS" "STATIC-STRUCT-CLASS" "INLINED" "ADJUST-OFFSET"))
