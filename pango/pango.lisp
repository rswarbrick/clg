;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2001-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: pango.lisp,v 1.9 2005-04-23 16:48:52 espen Exp $

(in-package "PANGO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library #.(concatenate 'string 
			  (pkg-config:pkg-variable "pango" "libdir")
		          "/libpango-1.0.so") :prefix "pango_")
  (init-types-in-library #.(concatenate 'string 
			  (pkg-config:pkg-variable "pango" "libdir")
		          "/libpangoxft-1.0.so") :prefix "pango_xft")
  (init-types-in-library #.(concatenate 'string 
			  (pkg-config:pkg-variable "pango" "libdir")
		          "/libpangoft2-1.0.so") :prefix "pango_fc"))

(define-types-by-introspection "Pango")
