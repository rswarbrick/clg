;; Common Lisp bindings for GTK+ 2.x
;; Copyright 1999-2008 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gio.lisp,v 1.1 2008-12-10 02:58:13 espen Exp $


(in-package "GIO")

(use-prefix "g")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library gio "libgio-2.0" :prefix "g_" 
   :ignore ("g_io_extension_get_type"))
  (init-types-in-library gio "gio-alien" :prefix "g_"))


(define-types-in-library gio "libgio-2.0"
  ("GIOErrorEnum" :type io-error)
  ("GIOModule" :ignore t))

(define-types-in-library gio "gio-alien")


