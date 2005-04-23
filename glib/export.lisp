;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2000-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: export.lisp,v 1.5 2005-04-23 16:48:50 espen Exp $


;;; Autogenerating exported symbols

(in-package "GLIB")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport defbinding (name &rest args)
    (declare (ignore args))
    (if (symbolp name)
	name
      (first name)))

  (defexport def-type-method (name &rest args)
    (declare (ignore args))
    name)

  (defexport define-enum-type (name &rest args)
    (declare (ignore args))
    name)

  (defexport define-flags-type (name &rest args)
    (declare (ignore args))
    name)

  (defexport define-types-by-introspection (prefix &rest args)
    (list-autoexported-symbols (expand-type-definitions prefix args))))
  

(export-from-file #p"clg:glib;utils.lisp")
(export-from-file #p"clg:glib;glib.lisp")
(export-from-file #p"clg:glib;proxy.lisp")
(export-from-file #p"clg:glib;gboxed.lisp")
(export-from-file #p"clg:glib;gtype.lisp")
(export-from-file #p"clg:glib;gparam.lisp")
(export-from-file #p"clg:glib;gcallback.lisp")
(export-from-file #p"clg:glib;ginterface.lisp")
(export-from-file #p"clg:glib;gobject.lisp")
(export-from-file #p"clg:glib;genums.lisp")
(export-from-file #p"clg:glib;gerror.lisp")
