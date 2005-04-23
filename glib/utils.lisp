;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: utils.lisp,v 1.5 2005-04-23 16:48:51 espen Exp $


(in-package "GLIB")

(defun type-expand-1 (form)
  (let ((def (cond ((symbolp form)
		    #+cmu(kernel::info type expander form)
		    #+sbcl(sb-impl::info :type :expander form))
		   ((and (consp form) (symbolp (car form)))
		    #+cmu(kernel::info type expander (car form))
		    #+sbcl(sb-impl::info :type :expander (car form)))
		   (t nil))))
    (if def
	(values (funcall def (if (consp form) form (list form))) t)
      (values form nil))))


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
  #+cmu`(system:without-gcing ,@body)
  #+sbcl`(sb-impl::without-gcing ,@body))

(defun mklist (obj)
  (if (and obj (atom obj)) (list obj) obj))

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
  (let ((pos (position delimiter string)))
   (if (not pos)
        (list string)
      (cons
       (subseq string 0 pos)
       (split-string (subseq string (1+ pos)) delimiter)))))

(defun split-string-if (string predicate)
  (declare (simple-string string))
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
     (concatenate-strings (rest strings) delimiter))))

(defun string-prefix-p (prefix string)
  (and
   (>= (length string) (length prefix))
   (string= prefix string :end2 (length prefix))))

(defun get-all (plist property)
  (multiple-value-bind (property value tail)
      (get-properties plist (list property))
    (when tail
      (cons value (get-all (cddr tail) property)))))

(defun plist-remove (plist property)
  (when plist
    (if (eq (first plist) property)
	(plist-remove (cddr plist) property)
      (list*
       (first plist) (second plist) (plist-remove (cddr plist) property)))))


;;;

(defun utf-8-encode (code)
  (labels ((encode-bytes (bit)
             (unless (zerop bit)
	       (cons
		(deposit-field 
                 #x80 (byte 7 6) (ldb (byte bit (- bit 6)) code))
		(encode-bytes (- bit 6)))))
           (encode-string (length)
             (map 'string #'code-char
              (cons
               (deposit-field
	        (mask-field (byte 7 (- 7 length)) #xFF)
		(byte 7 (- 6 length))
		(ldb (byte (+ (* length 6) 6) (* length 6)) code))
	       (encode-bytes (* length 6))))))
    (cond
     ((< code #x80) (string (code-char code)))
     ((< code #x800) (encode-string 1))
     ((< code #x10000) (encode-string 2))
     ((< code #x200000) (encode-string 3)) 
     ((< code #x4000000) (encode-string 4))
     ((< code #x80000000) (encode-string 5))
     (t (error "Invalid char code ~A" code)))))


(defun latin1-to-unicode (string)
  (reduce
   #'(lambda (str1 str2)
       (concatenate 'string str1 str2))
   (map 'list #'(lambda (char) (utf-8-encode (char-code char))) string)))
