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

;; $Id: gparam.lisp,v 1.1 2001-01-28 14:18:44 espen Exp $

(in-package "GLIB")

(deftype gvalue () 'pointer)

(defconstant +gvalue-size+ (+ (size-of 'type-number) (* 4 (size-of 'double-float))))
(defconstant +gvalue-value-offset+ (size-of 'type-number))

(define-foreign ("g_value_init" gvalue-init) () nil
  (type type-number))

(defun gvalue-new (type)
  (let ((gvalue (allocate-memory +gvalue-size+)))
    (setf (system:sap-ref-32 gvalue 0) type)
;    (gvalue-init (type-number-of type))
    gvalue))

(defun gvalue-free (gvalue free-content)
  (unless (null-pointer-p gvalue)
    (when free-content
      (funcall
       (get-destroy-function (gvalue-type gvalue))
       gvalue +gvalue-value-offset+))
    (deallocate-memory gvalue)))

(defun gvalue-type (gvalue)
  (type-from-number (system:sap-ref-32 gvalue 0)))

(defun gvalue-get (gvalue)
  (funcall
   (get-reader-function (gvalue-type gvalue))
   gvalue +gvalue-value-offset+))

(defun gvalue-set (gvalue value)
  (funcall
   (get-writer-function (gvalue-type gvalue))
   value gvalue +gvalue-value-offset+)
  value)

