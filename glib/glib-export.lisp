;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 2000 Espen S. Johnsen <espejohn@online.no>
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

;; $Id: glib-export.lisp,v 1.2 2000-08-17 22:43:02 espen Exp $


;;; Autogenerating exported symbols

(in-package "GLIB")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defexport define-foreign (name &rest args)
    (declare (ignore args))
    (if (symbolp name)
	name
      (second name)))

  (defexport deftype (name &rest args)
    (declare (ignore args))
    (if (symbolp name)
	name
      (first name)))

  (defexport define-type-method-fun (name lambda-list)
    (declare (ignore lambda-list))
    name))

(export-from-file #p"clg:glib;gutils.lisp")
(export-from-file #p"clg:glib;glib.lisp")
(export-from-file #p"clg:glib;gtype.lisp")
(export-from-file #p"clg:glib;gobject.lisp")
