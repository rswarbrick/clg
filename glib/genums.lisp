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

;; $Id: genums.lisp,v 1.21 2006-09-05 13:20:08 espen Exp $

(in-package "GLIB")
  
;;;; Definition of enums and flags by introspection

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass enum-value (struct)
    ((value :allocation :alien :type int)
     (name :allocation :alien :type string)
     (nickname :allocation :alien :type string))
    (:metaclass struct-class)))

(defun map-enum-values (values symbolic-p)
  (map 'list 
   #'(lambda (enum-value)
       (with-slots (nickname name value) enum-value
         (list
	  (if symbolic-p	      
	      (intern (substitute #\- #\_ (string-upcase nickname)) "KEYWORD")
	    name)
	  value)))
   values))

(defbinding enum-class-values () (static (vector (inlined enum-value) n-values))
  (class pointer)
  (n-values unsigned-int :out))

(defbinding flags-class-values () (static (vector (inlined enum-value) n-values))
  (class pointer)
  (n-values unsigned-int :out))

(defun query-enum-values (type &optional (symbolic-p t))
  (let ((class (type-class-ref type)))
    (map-enum-values (if (eq (supertype type) 'enum)
			 (enum-class-values class)
		       (flags-class-values class))
		     symbolic-p)))

(defun expand-enum-type (type-number forward-p options)
  (declare (ignore forward-p))
  (let* ((type (type-from-number type-number))
	 (mappings (getf options :mappings))
	 (expanded-mappings
	  (append
	   (delete-if
	    #'(lambda (mapping)
		(or
		 (assoc (first mapping) mappings)
		 (rassoc (cdr mapping) mappings :test #'equal)))
	    (query-enum-values type-number))
	   (remove-if
	    #'(lambda (mapping) (eq (second mapping) nil)) mappings))))
    `(progn
       (register-type ',type ',(find-type-init-function type-number))
       ,(ecase (supertype type-number)
	  (enum `(define-enum-type ,type ,@expanded-mappings))
	  (flags `(define-flags-type ,type ,@expanded-mappings))))))


(register-derivable-type 'enum "GEnum" 'expand-enum-type)
(register-derivable-type 'flags "GFlags" 'expand-enum-type)

