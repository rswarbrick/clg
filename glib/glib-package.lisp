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

;; $Id: glib-package.lisp,v 1.2 2000-09-30 15:10:54 espen Exp $

(export 'kernel::type-expand-1 "KERNEL")

(defpackage "GLIB"
  (:use "ALIEN" "C-CALL" "SYSTEM" "COMMON-LISP" "PCL" "AUTOEXPORT")
  (:shadow "DEFTYPE")
  (:shadowing-import-from "PCL"
	   "CLASS-NAME" "BUILT-IN-CLASS" "CLASS-OF" "FIND-CLASS" "LOCATION"
	   "ALLOCATION" "DIRECT-SLOTS")
  (:import-from "KERNEL" "TYPE-EXPAND-1")
  (:export "DEFTYPE-METHOD" "TRANSLATE-TYPE-SPEC" "TRANSLATE-TO-ALIEN"
	   "TRANSLATE-FROM-ALIEN" "CLEANUP-ALIEN" "SIZE-OF")
  (:export "DEFINE-FOREIGN" "USE-PREFIX" "PACKAGE-PREFIX")
  (:export "LONG" "UNSIGNED-LONG" "INT" "UNSIGNED-INT" "SHORT" "UNSIGNED-SHORT"
	   "SIGNED" "UNSIGNED" "CHAR" "POINTER" "ENUM" "FLAGS" "TYPE-NUMBER"
	   "STATIC")
  (:export "GET-TO-ALIEN-FUNCTION" "GET-FROM-ALIEN-FUNCTION"
	   "GET-CLEANUP-FUNCTION" "GET-READER-FUNCTION" "GET-WRITER-FUNCTION"
	   "GET-DESTROY-FUNCTION"))

