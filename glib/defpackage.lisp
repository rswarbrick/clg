;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: defpackage.lisp,v 1.1 2004-10-27 14:48:00 espen Exp $

;(export 'kernel::type-expand-1 "KERNEL")

(defpackage "GLIB"
  (:use "ALIEN" "C-CALL" "SYSTEM" "COMMON-LISP" "PCL" "AUTOEXPORT"
	"GLIB-SYSTEM")
  (;:shadowing-
   :import-from "PCL"
	   "LOCATION" "ALLOCATION" "DIRECT-SLOTS" 
	   "READER-FUNCTION" "WRITER-FUNCTION" "BOUNDP-FUNCTION" 
	   "INITIALIZE-INTERNAL-SLOT-FUNCTIONS" "COMPUTE-SLOT-ACCESSOR-INFO"
	   "COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS"
	   #:initialize-internal-slot-gfs)
 ; (:import-from "KERNEL" "TYPE-EXPAND-1")
  (:export #:load-shared-library)
  (:export "DEFTYPE-METHOD" "TRANSLATE-TYPE-SPEC" "TRANSLATE-TO-ALIEN"
	   "TRANSLATE-FROM-ALIEN" "CLEANUP-ALIEN" "UNREFERENCE-ALIEN"
	   "SIZE-OF")
  (:export "DEFBINDING" "DEFINE-FOREIGN" "MKBINDING" "USE-PREFIX"
	   "PACKAGE-PREFIX")
  (:export "LONG" "UNSIGNED-LONG" "INT" "UNSIGNED-INT" "SHORT" "UNSIGNED-SHORT"
	   "SIGNED" "UNSIGNED" "CHAR" "POINTER")
  (:export "INTERN-ARGUMENT-TRANSLATOR" "INTERN-RETURN-VALUE-TRANSLATOR"
	   "INTERN-CLEANUP-FUNCTION" "INTERN-WRITER-FUNCTION"
	   "INTERN-READER-FUNCTION" "INTERN-DESTROY-FUNCTION"))
