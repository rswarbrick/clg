;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2001 Espen S. Johnsen <espen@users.sourceforge.org>
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

;; $Id: pango.lisp,v 1.7 2005-02-03 23:09:06 espen Exp $

(in-package "PANGO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string 
			  (pkg-config:pkg-variable "pango" "libdir")
		          "/libpango-1.0.so") :prefix "pango_")
  (init-types-in-library #.(concatenate 'string 
			  (pkg-config:pkg-variable "pango" "libdir")
		          "/libpangoxft-1.0.so") :prefix "pango_xft"))

(define-types-by-introspection "Pango")
