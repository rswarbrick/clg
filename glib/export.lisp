;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: export.lisp,v 1.2 2004-10-27 15:24:10 espen Exp $


;;; Autogenerating exported symbols

(in-package "GLIB")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport defbinding (name &rest args)
    (declare (ignore args))
    (if (symbolp name)
	name
      (first name)))

  (defexport define-type-method-fun (name lambda-list)
    (declare (ignore lambda-list))
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
