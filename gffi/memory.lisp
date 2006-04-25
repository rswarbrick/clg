;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: memory.lisp,v 1.1 2006-04-25 20:31:35 espen Exp $


(in-package "GFFI")


(defun make-pointer (address)
  #+(or cmu sbcl)(int-sap address)
  #+clisp(ffi:unsigned-foreign-address address)
  #-(or cmu sbcl clisp)address)

(defun pointer-address (pointer)
  #+(or cmu sbcl)(sap-int pointer)
  #+clisp(ffi:foreign-address-unsigned pointer)
  #-(or cmu sbcl clisp)pointer)

(defun null-pointer-p (pointer)
  #+(or cmu sbcl)(zerop (sap-int pointer))
  #+clisp(or (not pointer) (zerop (pointer-address pointer)))
  #-(or cmu sbcl clisp)(zerop pointer))

(defun pointer= (pointer1 pointer2)
  #+(or cmu sbcl)(sap= pointer1 pointer2)
  #+clisp(= (pointer-address pointer1) (pointer-address pointer2))
  #-(or cmu sbcl clisp)(= pointer1 pointer2))

(defun pointer+ (pointer offset)
  #+(or cmu sbcl)(sap+ pointer offset)
  #+clisp(make-pointer (+ (pointer-address pointer) offset))
  #-(or cmu sbcl clisp)(+ pointer offset))

(defun ref-pointer (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-sap location offset)
  #+clisp(ffi:memory-as location 'ffi:c-pointer offset))

(defun (setf ref-pointer) (pointer location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-sap location offset)
   #+clisp(ffi:memory-as location 'ffi:c-pointer offset)
   pointer))

(defun ref-byte (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-8 location offset)
  #+clisp(ffi:memory-as location 'ffi:uchar offset))

(defun (setf ref-byte) (byte location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-8 location offset)
   #+clisp(ffi:memory-as location 'ffi:uchar offset)
   byte))

(defun allocate-memory (size)
  (declare (ignore size))
  (error "Memory allocator not set"))
(declaim (ftype (function (integer) system-area-pointer) allocate-memory))

(defun deallocate-memory (location)
  (declare (ignore location))
  (warn "Memory deallocator not set"))

(defun copy-memory (from length &optional (to (allocate-memory length)))
  #+cmu(system-area-copy from 0 to 0 (* 8 length))
  #+sbcl(system-area-ub8-copy from 0 to 0 length)
  #-(or cmu sbcl)
  (loop
   for offset below length
   do (setf (ref-byte to offset) (ref-byte from offset)))
  to)

(defun clear-memory (from length &optional (offset 0))
  #+sbcl(system-area-ub8-fill 0 from offset length)
  #-sbcl
  (loop
   repeat length
   for byte-offset from offset
   do (setf (ref-byte from byte-offset) 0)))

(defun memory-clear-p (from length &optional (offset 0))
  (loop
   repeat length
   for byte-offset from offset
   unless (zerop (ref-byte from byte-offset))
   do (return-from memory-clear-p nil))
  t)

(defmacro with-memory ((var size) &body body)
  #-clisp
  (if (and #+(or cmu sbcl)t (constantp size))
      (let ((memory (make-symbol "MEMORY"))
	    (size (eval size)))
	`(with-alien ((,memory (array #+sbcl(sb-alien:unsigned 8) #+cmu(alien:unsigned 8) ,size)))
	   (let ((,var (alien-sap ,memory)))
	     (clear-memory ,var ,size)
	     ,@body)))
    `(let ((,var (allocate-memory ,size)))
       (unwind-protect
	   (progn ,@body)
	 (deallocate-memory ,var))))
  #+clisp
  (let ((memory (make-symbol "MEMORY")))	   
    `(ffi:with-foreign-object (,memory `(ffi:c-array ffi:uint8 ,,size))
       (let ((,var (ffi:foreign-address ,memory)))
	 ,@body))))

(defmacro with-pointer ((var &optional (pointer '(make-pointer 0))) &body body)
  "Binds POINTER to VAR in a way which makes it possible to pass the location of VAR to in foreign function call."
  #+(or cmu sbcl)
  `(with-alien ((,var system-area-pointer ,pointer))
     ,@body)
  #+clisp
  `(ffi:with-c-var (,var `ffi:c-pointer ,pointer)
     ,@body))


#+sbcl
(progn
  (defun sb-sizeof-bits (type)
    (sb-alien-internals:alien-type-bits
     (sb-alien-internals:parse-alien-type type nil)))

  (defun sb-sizeof (type)
    (/ (sb-sizeof-bits type) 8)))
