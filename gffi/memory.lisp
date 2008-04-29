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

;; $Id: memory.lisp,v 1.8 2008-04-29 22:11:35 espen Exp $


(in-package "GFFI")

(deftype pointer () 
  #+(or cmu sbcl) 'system-area-pointer
  #+clisp 'ffi:foreign-address)

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


(deftype int-8 () '(signed-byte 8))
(deftype uint-8 () '(unsigned-byte 8))
(deftype int-16 () '(signed-byte 16))
(deftype uint-16 () '(unsigned-byte 16))
(deftype int-32 () '(signed-byte 32))
(deftype uint-32 () '(unsigned-byte 32))
(deftype int-64 () '(signed-byte 64))
(deftype uint-64 () '(unsigned-byte 64))

(declaim 
 (ftype (function (pointer &optional fixnum) int-8) ref-int-8)
 (inline ref-int-8))
(defun ref-int-8 (location &optional (offset 0))
  #+(or cmu sbcl)(signed-sap-ref-8 location offset)
  #+clisp(ffi:memory-as location 'ffi:char offset))

(declaim 
 (ftype (function (int-8 pointer &optional fixnum) int-8) (setf ref-int-8))
 (inline (setf ref-int-8)))
(defun (setf ref-int-8) (byte location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(signed-sap-ref-8 location offset)
   #+clisp(ffi:memory-as location 'ffi:char offset)
   byte))

;; Deprecated functions
(defun ref-byte (location &optional (offset 0))
  (ref-int-8 location offset))
(defun (setf ref-byte) (byte location &optional (offset 0))
  (setf (ref-int-8 location offset) byte))


(declaim 
 (ftype (function (pointer &optional fixnum) uint-8) ref-uint-8)
 (inline ref-uint-8))
(defun ref-uint-8 (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-8 location offset)
  #+clisp(ffi:memory-as location 'ffi:uchar offset))

(declaim 
 (ftype (function (uint-8 pointer &optional fixnum) uint-8) (setf ref-uint-8))
 (inline (setf ref-uint-8)))
(defun (setf ref-uint-8) (byte location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-8 location offset)
   #+clisp(ffi:memory-as location 'ffi:uchar offset)
   byte))


(declaim 
 (ftype (function (pointer &optional fixnum) int-16) ref-native-int-16)
 (inline ref-native-int-16))
(defun ref-native-int-16 (location &optional (offset 0))
  #+(or cmu sbcl)(signed-sap-ref-16 location offset)
  #+clisp(ffi:memory-as location 'ffi:sint16 offset))

(declaim 
 (ftype 
  (function (uint-16 pointer &optional fixnum) int-16) 
  (setf ref-native-int-16))
 (inline (setf ref-native-int-16)))
(defun (setf ref-native-int-16) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(signed-sap-ref-16 location offset)
   #+clisp(ffi:memory-as location 'ffi:sint16 offset)
   value))

(declaim 
 (ftype (function (pointer &optional fixnum) uint-16) ref-native-uint-16)
 (inline ref-native-uint-16))
(defun ref-native-uint-16 (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-16 location offset)
  #+clisp(ffi:memory-as location 'ffi:uint16 offset))

(declaim 
 (ftype 
  (function (uint-16 pointer &optional fixnum) uint-16) 
  (setf ref-native-uint-16))
 (inline (setf ref-native-uint-16)))
(defun (setf ref-native-uint-16) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-16 location offset)
   #+clisp(ffi:memory-as location 'ffi:uint16 offset)
   value))


(declaim 
 (ftype (function (pointer &optional fixnum) int-32) ref-native-int-32)
 (inline ref-native-int-32))
(defun ref-native-int-32 (location &optional (offset 0))
  #+(or cmu sbcl)(signed-sap-ref-32 location offset)
  #+clisp(ffi:memory-as location 'ffi:sint32 offset))

(declaim 
 (ftype (function (int-32 pointer &optional fixnum) int-32) (setf ref-native-int-32))
 (inline (setf ref-native-int-32)))
(defun (setf ref-native-int-32) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(signed-sap-ref-32 location offset)
   #+clisp(ffi:memory-as location 'ffi:sint32 offset)
   value))

(declaim 
 (ftype (function (pointer &optional fixnum) uint-32) ref-native-uint-32)
 (inline ref-native-uint-32))
(defun ref-native-uint-32 (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-32 location offset)
  #+clisp(ffi:memory-as location 'ffi:uint32 offset))

(declaim 
 (ftype 
  (function (uint-32 pointer &optional fixnum) uint-32) 
  (setf ref-native-uint-32))
 (inline (setf ref-native-uint-32)))
(defun (setf ref-native-uint-32) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-32 location offset)
   #+clisp(ffi:memory-as location 'ffi:uint32 offset)
   value))


(declaim 
 (ftype (function (pointer &optional fixnum) int-64) ref-native-int-64)
 (inline ref-native-int-64))
(defun ref-native-int-64 (location &optional (offset 0))
  #+(or cmu sbcl)(signed-sap-ref-64 location offset)
  #+clisp(ffi:memory-as location 'ffi:sint64 offset))

(declaim 
 (ftype (function (int-64 pointer &optional fixnum) int-64) (setf ref-native-int-64))
 (inline (setf ref-native-int-64)))
(defun (setf ref-native-int-64) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(signed-sap-ref-64 location offset)
   #+clisp(ffi:memory-as location 'ffi:sint64 offset)
   value))

(declaim 
 (ftype (function (pointer &optional fixnum) uint-64) ref-native-uint-64)
 (inline ref-native-uint-64))
(defun ref-native-uint-64 (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-64 location offset)
  #+clisp(ffi:memory-as location 'ffi:uint64 offset))

(declaim 
 (ftype 
  (function (uint-64 pointer &optional fixnum) uint-64) 
  (setf ref-native-uint-64))
 (inline (setf ref-native-uint-64)))
(defun (setf ref-native-uint-64) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-64 location offset)
   #+clisp(ffi:memory-as location 'ffi:uint64 offset)
   value))


(declaim 
 (ftype (function (pointer &optional fixnum) single-float) ref-native-single-float)
 (inline ref-native-single-float))
(defun ref-native-single-float (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-single location offset)
  #+clisp(ffi:memory-as location 'single-float offset))

(declaim 
 (ftype 
  (function (single-float pointer &optional fixnum) single-float) 
  (setf ref-native-single-float))
 (inline (setf ref-native-single-float)))
(defun (setf ref-native-single-float) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-single location offset)
   #+clisp(ffi:memory-as location 'single-float offset)
   value))

(declaim 
 (ftype (function (pointer &optional fixnum) double-float) ref-native-double-float)
 (inline ref-native-double-float))
(defun ref-native-double-float (location &optional (offset 0))
  #+(or cmu sbcl)(sap-ref-double location offset)
  #+clisp(ffi:memory-as location 'double-float offset))

(declaim 
 (ftype 
  (function (double-float pointer &optional fixnum) double-float) 
  (setf ref-native-double-float))
 (inline (setf ref-native-double-float)))
(defun (setf ref-native-double-float) (value location &optional (offset 0))
  (setf 
   #+(or cmu sbcl)(sap-ref-double location offset)
   #+clisp(ffi:memory-as location 'double-float offset)
   value))


(defvar *memory-allocator* nil)
(defvar *memory-deallocator* nil)

(defun allocate-memory (size)
  (if *memory-allocator*
      (funcall *memory-allocator* size)
    (error "Memory allocator not set")))

(defun deallocate-memory (location)
  (if *memory-deallocator*
      (funcall *memory-deallocator* location)
    (warn "Memory deallocator not set")))

(defun copy-memory (from length &optional (to (allocate-memory length)))
  #+cmu(system-area-copy from 0 to 0 (* 8 length))
  #+sbcl(system-area-ub8-copy from 0 to 0 length)
  #-(or cmu sbcl)
  (loop
   for offset below length
   do (setf (ref-uint-8 to offset) (ref-uint-8 from offset)))
  to)

(defun clear-memory (from length &optional (offset 0))
  #+sbcl(system-area-ub8-fill 0 from offset length)
  #-sbcl
  (loop
   repeat length
   for byte-offset from offset
   do (setf (ref-uint-8 from byte-offset) 0)))

(defun memory-clear-p (from length &optional (offset 0))
  (loop
   repeat length
   for byte-offset from offset
   unless (zerop (ref-uint-8 from byte-offset))
   do (return-from memory-clear-p nil))
  t)

(defmacro with-memory ((var size) &body body)
  (cond
    #+(or cmu sbcl)
    ((constantp size)
     (let ((memory (make-symbol "MEMORY"))
	   (size (eval size)))
       `(with-alien ((,memory (array #+sbcl(sb-alien:unsigned 8) #+cmu(alien:unsigned 8) ,size)))
	  (let ((,var (alien-sap ,memory)))
	    (clear-memory ,var ,size)
	    ,@body))))
    (t
     #-clisp
     `(let ((,var (allocate-memory ,size)))
	(unwind-protect
	     (progn ,@body)
	  (deallocate-memory ,var)))
     #+clisp
     (let ((memory (make-symbol "MEMORY")))	   
       `(ffi:with-foreign-object (,memory `(ffi:c-array ffi:uint8 ,,size))
	  (let ((,var (ffi:foreign-address ,memory)))
	    ,@body))))))

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
    (/ (sb-sizeof-bits type) 8))

  (defun sb-alignment (type)
    (/ (sb-alien-internals:alien-type-alignment
	(sb-alien-internals:parse-alien-type type nil))
       8)))


(deftype endian () '(member :native :little :big))

(defmacro define-memory-accessor (type)
  (let* ((get-swapped (intern (format nil "GET-~A-SWAPPED" type)))
	 (set-swapped (intern (format nil "SET-~A-SWAPPED" type)))
	 (ref (intern (format nil "REF-~A" type)))
	 (ref-native (intern (format nil "REF-NATIVE-~A" type))))
    `(progn
       (declaim (inline ,get-swapped) (inline ,set-swapped))
       (defbinding ,get-swapped () ,type
	 (location pointer)
	 (offset int))
       (defbinding ,set-swapped () nil
	 (location pointer)
	 (offset int)
	 (value ,type))
       (declaim 
	(ftype (function (pointer &optional fixnum endian) ,type) ,ref)
	(inline ,ref))
       (defun ,ref (location &optional offset (endian :native))
	 (ecase endian
	   ((:native #-big-endian :little #+big-endian :big)
	    (,ref-native location offset))	   
	   ((#-big-endian :big #+big-endian :little)
	    (,get-swapped location offset))))
       (declaim 
	(ftype 
	 (function (,type pointer &optional fixnum endian) ,type) 
	 (setf ,ref))
	(inline (setf ,ref)))
       (defun (setf ,ref) (value location &optional offset (endian :native))
	 (ecase endian
	   ((:native #-big-endian :little #+big-endian :big)
	    (setf (,ref-native location offset) value))
	   ((#-big-endian :big #+big-endian :little)
	    (,set-swapped location offset value)
	    value))))))

#+cmu
(defmacro with-pinned-objects (objects &body body)
  (declare (ignore objects))
  `(without-gcing ,@body))

