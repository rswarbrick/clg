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

;; $Id: gutils.lisp,v 1.6 2001-05-11 15:57:13 espen Exp $


(in-package "KERNEL")

(defun type-expand-1 (form)
  (let ((def (cond ((symbolp form)
		    (info type expander form))
		   ((and (consp form) (symbolp (car form)))
		    (info type expander (car form)))
		   (t nil))))
    (if def
	(values (funcall def (if (consp form) form (list form))) t)
      (values form nil))))

(in-package "PCL")

;;;; Make PCL's class finalization protocol behave as specified in AMOP

(defmethod finalize-inheritance ((class std-class))
  (update-cpl class (compute-class-precedence-list class))
  (update-slots class (compute-slots class))
  (update-gfs-of-class class)
  (update-inits class (compute-default-initargs class))
  (update-make-instance-function-table class))

(defun update-class (class &optional finalizep)  
  (declare (ignore finalizep))
  (unless (class-has-a-forward-referenced-superclass-p class)
    (finalize-inheritance class)
    (dolist (sub (class-direct-subclasses class))
      (update-class sub))))


(in-package "GLIB")

(defun type-expand-to (type form)
  (labels ((expand (form0)
             (if (eq (first (mklist form0)) type)
		 form0
	       (multiple-value-bind (expanded-form expanded-p)
		   (type-expand-1 form0)
		 (if expanded-p
		     (expand expanded-form)
		   (error "~A can not be expanded to ~A" form type))))))
    (expand form)))

(defmacro with-gc-disabled (&body body)
  (let ((gc-inhibit (make-symbol "GC-INHIBIT")))
    `(progn
       (let ((,gc-inhibit lisp::*gc-inhibit*))
	 (ext:gc-off)
	 	 (unwind-protect
	     ,@body
	   (unless ,gc-inhibit
	     (ext:gc-on)))))))

(defun mklist (obj)
  (if (atom obj) (list obj) obj))

(defun namep (obj)
  (and (symbolp obj) (not (member obj '(t nil)))))

(defun all-equal (&rest objects)
  (or
   (null objects)
   (null (rest objects))
   (and
    (equal (first objects) (second objects))
    (apply #'all-equal (rest objects)))))

(defun neq (obj1 obj2)
  (not (eq obj1 obj2)))

(defmacro return-if (form)
  (let ((result (make-symbol "RESULT")))
    `(let ((,result ,form))
       (when ,result
	 (return ,result)))))

(defun make-pointer (address)
  (int-sap address))
  
(defun null-pointer-p (pointer)
  (zerop (sap-int pointer)))
  

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))


(defmacro assoc-ref (key alist &key (test #'eq))
  `(cdr (assoc ,key ,alist :test ,test)))


(defmacro assoc-lref (key alist &key (test #'eq))
  `(cadr (assoc ,key ,alist :test ,test)))


(defun assoc-rem (key alist &key (test #'eq))
  (remove-if #'(lambda (element) (funcall test key (car element))) alist))


(defun assoc-delete (key alist &key (test #'eq))
  (delete-if #'(lambda (element) (funcall test key (car element))) alist))


(defun funcallable (object)
  (if (consp object)
      (fdefinition object)
    object))

(defun intersection-p (list1 list2 &key (test #'eq))
  (dolist (obj list1 nil)
    (when (member obj list2 :test test)
      (return-from intersection-p t))))


(defun split-string (string delimiter)
  (declare (simple-string string) (character delimiter))
  (check-type string string)
  (check-type delimiter character)
  (let ((pos (position delimiter string)))
   (if (not pos)
        (list string)
      (cons
       (subseq string 0 pos)
       (split-string (subseq string (1+ pos)) delimiter)))))

(defun split-string-if (string predicate)
  (declare (simple-string string))
  (check-type string string)
  (check-type predicate (or symbol function))
  (let ((pos (position-if predicate string :start 1)))
    (if (not pos)
        (list string)
      (cons
       (subseq string 0 pos)
       (split-string-if (subseq string pos) predicate)))))

(defun concatenate-strings (strings &optional delimiter)
  (if (not (rest strings))
      (first strings)
    (concatenate
     'string
     (first strings)
     (if delimiter (string delimiter) "")
     (concatenate-strings (rest strings)))))

(defun string-prefix-p (string1 string2)
  (string= string1 string2 :end2 (length string1)))
