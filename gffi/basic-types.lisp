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

;; $Id: basic-types.lisp,v 1.7 2007-06-01 06:22:05 espen Exp $

(in-package "GFFI")


(deftype int ()
  '(signed-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:int)
		#+clisp #.(ffi:bitsizeof 'ffi:int)
		#-(or sbcl clisp) 32))
(deftype unsigned-int () 
  '(unsigned-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:int)
		  #+clisp #.(ffi:bitsizeof 'ffi:int)
		  #-(or sbcl clisp) 32))
(deftype long () 
  '(signed-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:long)
		#+clisp #.(ffi:bitsizeof 'ffi:long)
		#-(or sbcl clisp) 32))
(deftype unsigned-long () 
  '(unsigned-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:long)
		  #+clisp #.(ffi:bitsizeof 'ffi:long)
		  #-(or sbcl clisp) 32))
(deftype short () 
  '(signed-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:short)
		#+clisp #.(ffi:bitsizeof 'ffi:short)
		#-(or sbcl clisp) 16))
(deftype unsigned-short () 
  '(unsigned-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:short)
		  #+clisp #.(ffi:bitsizeof 'ffi:short)
		  #-(or sbcl clisp) 16))
(deftype signed (&optional (size '*)) `(signed-byte ,size))
(deftype unsigned (&optional (size '*)) `(unsigned-byte ,size))
(deftype char () 'base-char)
(deftype pointer () 
  #+(or cmu sbcl) 'system-area-pointer
  #+clisp 'ffi:foreign-address)
(deftype pointer-data () 
  '(unsigned-byte #+sbcl #.(sb-sizeof-bits 'sb-alien:system-area-pointer)
		  #+clisp #.(ffi:bitsizeof 'ffi:c-pointer)
		  #-(or sbcl clisp) 32))
  
(deftype bool (&optional (size '*)) (declare (ignore size)) 'boolean)
(deftype copy-of (type) type)
(deftype static (type) type)
(deftype inlined (type) type)



(define-type-generic alien-type (type)
  "Returns the foreign type corresponding to TYPE")
(define-type-generic size-of (type &key inlined)
  "Returns the foreign size of TYPE. The default value of INLINED is
T for basic C types and NIL for other types.")
(define-type-generic type-alignment (type &key inlined)
  "Returns the alignment of TYPE. The default value of INLINED is
T for basic C types and NIL for other types.")
(define-type-generic alien-arg-wrapper (type var arg style form &optional copy-p)
  "Creates a wrapper around FORM which binds the alien translation of
ARG to VAR in a way which makes it possible to pass the location of
VAR in a foreign function call. It should also do any necessary clean
up before returning the value of FORM.")
(define-type-generic to-alien-form (type form &optional copy-p)
  "Returns a form which translates FORM to alien representation. If
COPY-P is non NIL then any allocated foreign memory must not be
reclaimed later.")
(define-type-generic from-alien-form (type form &key ref)
  "Returns a form which translates FORM from alien to lisp
representation. REF should be :FREE, :COPY, :STATIC or :TEMP")
(define-type-generic to-alien-function (type &optional copy-p)
  "Returns a function of one argument which will translate objects of the given type to alien repesentation. An optional function, taking the origional object and the alien representation as arguments, to clean up after the alien value is not needed any more may be returned as a second argument.")
(define-type-generic from-alien-function (type &key ref)
  "Returns a function of one argument which will translate alien objects of the given type to lisp representation. REF should be :FREE, :COPY, :STATIC or :TEMP")
(define-type-generic callback-wrapper (type var arg form)
  "Creates a wrapper around FORM which binds the lisp translation of
ARG to VAR during a C callback.")

(define-type-generic writer-function (type &key temp inlined)
  "Returns a function taking a value, an address and an optional
offset which when called will write a reference an object at the given
location. If TEMP is non NIL then the object is expected to be valid
as long as the reference exists.")
(define-type-generic reader-function (type &key ref inlined)
  "Returns a function taking an address and optional offset which when
called will return the object at given location. REF should be :READ,
:PEEK or :GET")
(define-type-generic destroy-function (type &key temp inlined)
  "Returns a function taking an address and optional offset which when
called will destroy the reference at the given location. This may
involve freeing the foreign object being referenced or decreasing it's
ref. count. If TEMP is non NIL then the reference is expected to
have been written as temporal.")
(define-type-generic copy-function (type &key inlined))

(define-type-generic unbound-value (type-spec)
  "Returns a value which should be interpreted as unbound for slots with virtual allocation")

(defun assert-inlined (type inlined-p)
  (unless inlined-p
    (error "Type ~A can only be inlined" type)))

(defun assert-not-inlined (type inlined-p)
  (when inlined-p
    (error "Type ~A can not be inlined" type)))


(define-type-method alien-arg-wrapper ((type t) var arg style form &optional 
				       (copy-in-p nil copy-in-given-p))
  (let ((alien-type (alien-type type)))
    (cond
      ((in-arg-p style)
       (let ((to-alien (if copy-in-given-p
			   (to-alien-form type arg copy-in-p)
			 (to-alien-form type arg))))
	 #+(or cmu sbcl)
	 `(with-alien ((,var ,alien-type ,to-alien))
	    ,form)
	 #+clisp
	 `(ffi:with-c-var (,var ',alien-type ,to-alien)
	    ,form)))
      ((out-arg-p style)
       #+(or cmu sbcl)
       `(with-alien ((,var ,alien-type))
	  (clear-memory (alien-sap (addr ,var)) ,(size-of type))
	  ,form)
       #+clisp
       `(ffi:with-c-var (,var ',alien-type)
	  ,form)))))

(define-type-method callback-wrapper ((type t) var arg form)
  `(let ((,var ,(from-alien-form type arg :ref :temp)))
     ,form))

(define-type-method alien-type ((type t))
  (error "No alien type corresponding to the type specifier ~A" type))

(define-type-method to-alien-form ((type t) form &optional copy-p)
  (declare (ignore form copy-p))
  (error "Not a valid type specifier for arguments: ~A" type))

(define-type-method to-alien-function ((type t) &optional copy-p)
  (declare (ignore copy-p))
  (error "Not a valid type specifier for arguments: ~A" type))

(define-type-method from-alien-form ((type t) form &key ref)
  (declare (ignore form ref))
  (error "Not a valid type specifier for return values: ~A" type))

(define-type-method from-alien-function ((type t) &key ref)
  (declare (ignore ref))
  (error "Not a valid type specifier for return values: ~A" type))

(define-type-method destroy-function ((type t) &key temp (inlined t inlined-p))
  (declare (ignore temp))
  (let ((size (if inlined-p 
		  (size-of type :inlined inlined)
		(size-of type))))
    #'(lambda (location &optional (offset 0))
	(clear-memory location size offset))))

(define-type-method copy-function ((type t) &key (inlined t inlined-p))
  (let ((size (if inlined-p 
		  (size-of type :inlined inlined)
		(size-of type))))
    #'(lambda (from to &optional (offset 0))
	(copy-memory (pointer+ from offset) size (pointer+ to offset)))))

(define-type-method to-alien-form ((type real) form &optional copy-p)
  (declare (ignore type copy-p))
  form)

(define-type-method to-alien-function ((type real) &optional copy-p)
  (declare (ignore type copy-p))
  #'identity)

(define-type-method from-alien-form ((type real) form &key ref)
  (declare (ignore type ref))
  form)

(define-type-method from-alien-function ((type real) &key ref)
  (declare (ignore type ref))
  #'identity)


(define-type-method alien-type ((type integer))
  (declare (ignore type))
  (alien-type 'signed-byte))

(define-type-method size-of ((type integer) &key (inlined t))
  (declare (ignore type))
  (size-of 'signed-byte :inlined inlined))

(define-type-method type-alignment ((type integer) &key (inlined t))
  (declare (ignore type))
  (type-alignment 'signed-byte :inlined inlined))

(define-type-method writer-function ((type integer) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (writer-function 'signed-byte))

(define-type-method reader-function ((type integer) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (reader-function 'signed-byte))


;;; Signed Byte
  
(define-type-method alien-type ((type signed-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      #+cmu
      (ecase size
	( 8 '(alien:signed 8))
	(16 '(alien:signed 16))
	(32 '(alien:signed 32))
	(64 '(alien:signed 64)))
      #+sbcl
      (ecase size
	( 8 '(sb-alien:signed  8))
	(16 '(sb-alien:signed 16))
	(32 '(sb-alien:signed 32))
	(64 '(sb-alien:signed 64)))
      #+clisp
      (ecase size
	( 8 'ffi:sint8)
	(16 'ffi:sint16)
	(32 'ffi:sint32)
	(64 'ffi:sint64)))))

(define-type-method size-of ((type signed-byte) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      (ecase size
	( 8 1)
	(16 2)
	(32 4)
	(64 8)))))

(define-type-method type-alignment ((type signed-byte) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      #+sbcl(sb-alignment `(sb-alien:signed ,size))
      #+clisp(ecase size
	       ( 8 (nth-value 1 (ffi:sizeof 'ffi:sint8)))
	       (16 (nth-value 1 (ffi:sizeof 'ffi:sint16)))
	       (32 (nth-value 1 (ffi:sizeof 'ffi:sint32)))
	       (64 (nth-value 1 (ffi:sizeof 'ffi:sint64))))
      #-(or sbcl clisp) 4)))

(define-type-method writer-function ((type signed-byte) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      (ecase size
	( 8 #'(lambda (value location &optional (offset 0))
		(setf 
		 #+(or cmu sbcl)(signed-sap-ref-8 location offset)
		 #+clisp(ffi:memory-as location 'ffi:sint8 offset)
		 value)))
	(16 #'(lambda (value location &optional (offset 0))
		(setf 
		 #+(or cmu sbcl)(signed-sap-ref-16 location offset)
		 #+clisp(ffi:memory-as location 'ffi:sint16 offset)
		 value)))
	(32 #'(lambda (value location &optional (offset 0))	       
		(setf 
		 #+(or cmu sbcl)(signed-sap-ref-32 location offset)
		 #+clisp(ffi:memory-as location 'ffi:sint32 offset)
		 value)))
	(64 #'(lambda (value location &optional (offset 0))
		(setf 
		 #+(or cmu sbcl)(signed-sap-ref-64 location offset)
		 #+clisp(ffi:memory-as location 'ffi:sint64 offset)
		 value)))))))

(define-type-method reader-function ((type signed-byte) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'signed-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      (ecase size
	( 8 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(signed-sap-ref-8 location offset)
		#+clisp(ffi:memory-as location 'ffi:sint8 offset)))
	(16 #'(lambda (location &optional (offset 0))
		#+(or cmu sbcl)(signed-sap-ref-16 location offset)
		#+clisp(ffi:memory-as location 'ffi:sint16 offset)))
	(32 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(signed-sap-ref-32 location offset)
		#+clisp(ffi:memory-as location 'ffi:sint32 offset)))
	(64 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(signed-sap-ref-64 location offset)
		#+clisp(ffi:memory-as location 'ffi:sint64 offset)))))))



;;; Unsigned Byte
  
(define-type-method alien-type ((type unsigned-byte))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      #+cmu
      (ecase size
	( 8 '(alien:unsigned 8))
	(16 '(alien:unsigned 16))
	(32 '(alien:unsigned 32))
	(64 '(alien:unsigned 64)))
      #+sbcl
      (ecase size
	( 8 '(sb-alien:unsigned  8))
	(16 '(sb-alien:unsigned 16))
	(32 '(sb-alien:unsigned 32))
	(64 '(sb-alien:unsigned 64)))
      #+clisp
      (ecase size
	( 8 'ffi:uint8)
	(16 'ffi:uint16)
	(32 'ffi:uint32)
	(64 'ffi:uint64)))))

(define-type-method size-of ((type unsigned-byte) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (size-of `(signed ,size))))

(define-type-method type-alignment ((type unsigned-byte) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (type-alignment `(signed ,size))))

(define-type-method writer-function ((type unsigned-byte) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      (ecase size
	( 8 #'(lambda (value location &optional (offset 0))		
		(setf 
		 #+(or cmu sbcl)(sap-ref-8 location offset)
		 #+clisp(ffi:memory-as location 'ffi:uint8 offset)
		 value)))
	(16 #'(lambda (value location &optional (offset 0))
		(setf 
		 #+(or cmu sbcl)(sap-ref-16 location offset)
		 #+clisp(ffi:memory-as location 'ffi:uint16 offset)
		 value)))
	(32 #'(lambda (value location &optional (offset 0))	       
		(setf 
		 #+(or cmu sbcl)(sap-ref-32 location offset)
		 #+clisp(ffi:memory-as location 'ffi:uint32 offset)
		 value)))
	(64 #'(lambda (value location &optional (offset 0))
		(setf 
		 #+(or cmu sbcl)(sap-ref-64 location offset)
		 #+clisp(ffi:memory-as location 'ffi:uint64 offset)
		 value)))))))
      
(define-type-method reader-function ((type unsigned-byte) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'unsigned-byte type)))
    (let ((size (if (eq size '*) 
		    (second (type-expand-to 'signed-byte 'int))
		  size)))
      (ecase size
	( 8 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(sap-ref-8 location offset)
		#+clisp(ffi:memory-as location 'ffi:uint8 offset)))
	(16 #'(lambda (location &optional (offset 0))
		#+(or cmu sbcl)(sap-ref-16 location offset)
		#+clisp(ffi:memory-as location 'ffi:uint16 offset)))
	(32 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(sap-ref-32 location offset)
		#+clisp(ffi:memory-as location 'ffi:uint32 offset)))
	(64 #'(lambda (location &optional (offset 0)) 
		#+(or cmu sbcl)(sap-ref-64 location offset)
		#+clisp(ffi:memory-as location 'ffi:uint64 offset)))))))



;;; Single Float

(define-type-method alien-type ((type single-float))
  (declare (ignore type))
  #+cmu 'alien:single-float 
  #+sbcl 'sb-alien:single-float
  #+clisp 'single-float)

(define-type-method size-of ((type single-float) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-sizeof 'sb-alien:float)
  #+clisp (ffi:sizeof 'single-float)
  #-(or sbcl clisp) 4)

(define-type-method type-alignment ((type single-float) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-alignment 'single-float)
  #+clisp (nth-value 1 (ffi:sizeof 'single-float))
  #-(or sbcl clisp) 4)

(define-type-method to-alien-form ((type single-float) form &optional copy-p)
  (declare (ignore type copy-p))
  `(coerce ,form 'single-float))

(define-type-method to-alien-function ((type single-float) &optional copy-p)
  (declare (ignore type copy-p))
  #'(lambda (number)
      (coerce number 'single-float)))

(define-type-method writer-function ((type single-float) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  #'(setf ref-single-float))

(define-type-method reader-function ((type single-float) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  #'ref-single-float)



;;; Double Float

(define-type-method alien-type ((type double-float))
  (declare (ignore type))
  #+cmu 'alien:double-float 
  #+sbcl 'sb-alien:double-float
  #+clisp 'double-float)

(define-type-method size-of ((type double-float) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-sizeof 'sb-alien:double)
  #+clisp (ffi:sizeof 'double-float)
  #-(or sbcl clisp) 8)

(define-type-method type-alignment ((type double-float) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-alignment 'double-float)
  #+clisp (nth-value 1 (ffi:sizeof 'double-float))
  #-(or sbcl clisp) 4)

(define-type-method to-alien-form ((type double-float) form &optional copy-p)
  (declare (ignore type copy-p))
  `(coerce ,form 'double-float))

(define-type-method to-alien-function ((type double-float) &optional copy-p)
  (declare (ignore type copy-p))
  #'(lambda (number)
      (coerce number 'double-float)))

(define-type-method writer-function ((type double-float) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  #'(lambda (value location &optional (offset 0))
      (setf (ref-double-float location offset) (coerce value 'double-float))))

(define-type-method reader-function ((type double-float) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  #'ref-double-float)

(deftype optimized-double-float () 'double-float)

(define-type-method to-alien-form ((type optimized-double-float) form &optional copy-p)
  (declare (ignore type copy-p))
  form)



;;; Character

(define-type-method alien-type ((type base-char))
  (declare (ignore type))
  #+cmu 'c-call:char 
  #+sbcl 'sb-alien:char
  #+clisp 'ffi:character)

(define-type-method size-of ((type base-char) &key (inlined t))
  (assert-inlined type inlined)
  1)

(define-type-method type-alignment ((type base-char) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-alignment 'sb-alien:char)
  #+clisp (nth-value 1 (ffi:sizeof 'ffi:character))
  #-(or sbcl clisp) 4)
  
(define-type-method to-alien-form ((type base-char) form &optional copy-p)
  (declare (ignore type copy-p))
  form)

(define-type-method to-alien-function ((type base-char) &optional copy-p)
  (declare (ignore type copy-p))
  #'identity)

(define-type-method from-alien-form ((type base-char) form &key ref)
  (declare (ignore type ref))
  form)

(define-type-method from-alien-function ((type base-char) &key ref)
  (declare (ignore type ref))
  #'identity)

(define-type-method writer-function ((type base-char) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  #'(lambda (char location &optional (offset 0))
      #+(or cmu sbcl)
      (setf (sap-ref-8 location offset) (char-code char))
      #+clisp(setf (ffi:memory-as location 'ffi:character offset) char)))

(define-type-method reader-function ((type base-char) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  #'(lambda (location &optional (offset 0))
      #+(or cmu sbcl)(code-char (sap-ref-8 location offset))
      #+clisp(ffi:memory-as location 'ffi:character offset)))



;;; String

(defun utf8-length (string)
  "Returns the length including the trailing zero, of STRING encoded as UTF8"
  (1+ (loop
       for char across string
       as char-code = (char-code char)
       sum (cond
	    ((< char-code #x7F) 1)
	    ((< char-code #x7FF) 2)
	    ((< char-code #xFFFF) 3)
	    ((< char-code #x1FFFFF) 4)))))

(defun encode-utf8-string (string &optional location)
  (let* ((len (utf8-length string))
	 (location (or location (allocate-memory len))))
    (loop
     for char across string
     for i from 0
     as char-code = (char-code char)
     do (flet ((encode (size)
		 (let ((rem (mod size 6)))
		   (setf (ref-byte location i)
		    (deposit-field 
		     #xFF (byte (- 7 rem) (1+ rem))
		     (ldb (byte rem (- size rem)) char-code)))
		   (loop
		    for pos from (- size rem 6) downto 0 by 6
		    do (setf (ref-byte location (incf i)) 
		        (+ 128 (ldb (byte 6 pos) char-code)))))))
	  (cond
	   ((< char-code #x80) (setf (ref-byte location i) char-code))
	   ((< char-code #x800) (encode 11))
	   ((< char-code #x10000) (encode 16))
	   ((< char-code #x200000) (encode 21)))))
    (setf (ref-byte location (1- len)) 0)
    location))

(defun decode-utf8-string (c-string)
  (with-output-to-string (string)
    (loop
     for i from 0
     as octet = (ref-byte c-string i)
     until (zerop octet)
     do (flet ((decode (size)
		 (loop
		  with rem = (mod size 6)
		  for pos from (- size rem) downto 0 by 6
		  as code = (dpb (ldb (byte rem 0) octet) (byte rem pos) 0)
		  then (dpb 
			(ldb (byte 6 0) (ref-byte c-string (incf i)))
			(byte 6 pos) code)
		  finally (write-char (code-char code) string))))
	  (cond
	   ((< octet 128) (write-char (code-char octet) string))
	   ((< octet 224) (decode 11))
	   ((< octet 240) (decode 16))
	   ((< octet 248) (decode 21)))))))


(define-type-method alien-arg-wrapper ((type string) var string style form &optional copy-in-p)
  (declare (ignore type))
  (cond
   ((and (in-arg-p style) copy-in-p)
    `(with-pointer (,var (encode-utf8-string ,string))
       ,form))
   ((and (in-arg-p style) (not (out-arg-p style)))
    `(with-memory (,var (utf8-length ,string))
       (encode-utf8-string ,string ,var)
       ,form))
   ((and (in-arg-p style) (out-arg-p style))
    (let ((c-string (make-symbol "C-STRING")))
      `(with-memory (,c-string (utf8-length ,string))
         (encode-utf8-string ,string ,c-string)
	 (with-pointer (,var ,c-string)
	   ,form))))
   ((and (out-arg-p style) (not (in-arg-p style)))
    `(with-pointer (,var)
       ,form))))

(define-type-method alien-type ((type string))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type string) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'pointer))

(define-type-method type-alignment ((type string) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'pointer))

(define-type-method to-alien-form ((type string) string &optional copy-p)
  (declare (ignore type copy-p))
  `(encode-utf8-string ,string))

(define-type-method to-alien-function ((type string) &optional copy-p)
  (declare (ignore type))
  (values
   #'encode-utf8-string
   (unless copy-p
     #'(lambda (string c-string)
	 (declare (ignore string))
	 (deallocate-memory c-string)))))

(define-type-method from-alien-form ((type string) form &key (ref :free))
  (declare (ignore type))
  `(let ((c-string ,form))
     (unless (null-pointer-p c-string)
       (prog1
	   (decode-utf8-string c-string)
  	 ,(when (eq ref :free)
  	    `(deallocate-memory c-string))))))

(define-type-method from-alien-function ((type string) &key (ref :free))
  (declare (ignore type))
  (if (eq ref :free)
      #'(lambda (c-string)
	  (unless (null-pointer-p c-string)
	    (prog1
		(decode-utf8-string c-string)
	      (deallocate-memory c-string))))
    #'(lambda (c-string)
	(unless (null-pointer-p c-string)
	  (decode-utf8-string c-string)))))

(define-type-method writer-function ((type string) &key temp inlined)
  (declare (ignore temp))
  (assert-not-inlined type inlined)
  #'(lambda (string location &optional (offset 0))
      (assert (null-pointer-p (ref-pointer location offset)))
      (setf (ref-pointer location offset) (encode-utf8-string string))))

(define-type-method reader-function ((type string) &key (ref :read) inlined)
  (assert-not-inlined type inlined)
  (ecase ref
    ((:read :peek)
     #'(lambda (location &optional (offset 0))
	 (unless (null-pointer-p (ref-pointer location offset))
	   (decode-utf8-string (ref-pointer location offset)))))
    (:get
     #'(lambda (location &optional (offset 0))
	 (unless (null-pointer-p (ref-pointer location offset))
	   (prog1
	       (decode-utf8-string (ref-pointer location offset))
	     (deallocate-memory (ref-pointer location offset))
	     (setf (ref-pointer location offset) (make-pointer 0))))))))

(define-type-method destroy-function ((type string) &key temp inlined)
  (declare (ignore temp))
  (assert-not-inlined type inlined)
  #'(lambda (location &optional (offset 0))
      (unless (null-pointer-p (ref-pointer location offset))
	(deallocate-memory (ref-pointer location offset))
	(setf (ref-pointer location offset) (make-pointer 0)))))

(define-type-method copy-function ((type string) &key inlined)
  (assert-not-inlined type inlined)  
  (lambda (from to &optional (offset 0))
    (let* ((string (ref-pointer from offset))
	   (length (loop
		    for i from 0
		    until (zerop (ref-byte string i))
		    finally (return (1+ i)))))
      (setf (ref-pointer to offset) (copy-memory string length)))))

(define-type-method unbound-value ((type string))
  (declare (ignore type))
  nil)



;;; Pathname

(define-type-method alien-type ((type pathname))
  (declare (ignore type))
  (alien-type 'string))

(define-type-method size-of ((type pathname) &key inlined)
  (assert-not-inlined type inlined)
  (size-of 'string))

(define-type-method type-alignment ((type pathname) &key inlined)
  (assert-not-inlined type inlined)
  (type-alignment 'string))

(define-type-method alien-arg-wrapper ((type pathname) var pathname style form &optional copy-in-p)
  (declare (ignore type))
  (alien-arg-wrapper 'string var `(namestring (translate-logical-pathname ,pathname)) style form copy-in-p))

(define-type-method to-alien-form ((type pathname) path)
  (declare (ignore type))
  (to-alien-form 'string `(namestring (translate-logical-pathname ,path))))

(define-type-method to-alien-function ((type pathname) &optional copy-p)
  (declare (ignore type))
  (let ((string-function (to-alien-function 'string copy-p)))
    #'(lambda (path)
	(funcall string-function (namestring path)))))

(define-type-method from-alien-form ((type pathname) form &key (ref :free))
  (declare (ignore type))
  `(parse-namestring ,(from-alien-form 'string form :ref ref)))

(define-type-method from-alien-function ((type pathname) &key (ref :free))
  (declare (ignore type))
  (let ((string-function (from-alien-function 'string :ref ref)))
    #'(lambda (string)
	(parse-namestring (funcall string-function string)))))

(define-type-method writer-function ((type pathname) &key temp inlined)
  (declare (ignore temp))
  (assert-not-inlined type inlined)
  (let ((string-writer (writer-function 'string)))
    #'(lambda (path location &optional (offset 0))
	(funcall string-writer (namestring path) location offset))))

(define-type-method reader-function ((type pathname) &key ref inlined)
  (declare (ignore ref))
  (assert-not-inlined type inlined)
  (let ((string-reader (reader-function 'string)))
  #'(lambda (location &optional (offset 0))
      (let ((string (funcall string-reader location offset)))
	(when string
	  (parse-namestring string))))))

(define-type-method destroy-function ((type pathname) &key temp inlined)
  (declare (ignore temp))
  (assert-not-inlined type inlined)
  (destroy-function 'string))

(define-type-method copy-function ((type pathname) &key inlined)
  (assert-not-inlined type inlined)
  (copy-function 'string))

(define-type-method unbound-value ((type pathname))
  (declare (ignore type))
  (unbound-value 'string))



;;; Bool

(define-type-method alien-type ((type bool))
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'bool type)))
    (alien-type `(signed-byte ,size))))

(define-type-method size-of ((type bool) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'bool type)))
    (size-of `(signed-byte ,size))))

(define-type-method type-alignment ((type bool) &key (inlined t))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'bool type)))
    (type-alignment `(signed-byte ,size))))

(define-type-method to-alien-form ((type bool) bool &optional copy-p)
  (declare (ignore type copy-p))
  `(if ,bool 1 0))

(define-type-method to-alien-function ((type bool) &optional copy-p)
  (declare (ignore type copy-p))
  #'(lambda (bool)
      (if bool 1 0)))

(define-type-method from-alien-form ((type bool) form &key ref)
  (declare (ignore type ref))
  `(not (zerop ,form)))

(define-type-method from-alien-function ((type bool) &key ref)
  (declare (ignore type ref))
  #'(lambda (bool)
      (not (zerop bool))))

(define-type-method writer-function ((type bool) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'bool type)))
    (let ((writer (writer-function `(signed-byte ,size))))
      #'(lambda (bool location &optional (offset 0))
	  (funcall writer (if bool 1 0) location offset)))))

(define-type-method reader-function ((type bool) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (destructuring-bind (&optional (size '*))
      (rest (mklist (type-expand-to 'bool type)))
    (let ((reader (reader-function `(signed-byte ,size))))
      #'(lambda (location &optional (offset 0))
	  (not (zerop (funcall reader location offset)))))))



;;; Boolean

(define-type-method alien-type ((type boolean))
  (declare (ignore type))
  (alien-type 'bool))

(define-type-method size-of ((type boolean) &key (inlined t))
  (assert-inlined type inlined)
  (size-of 'bool))

(define-type-method type-alignment ((type boolean) &key (inlined t))
  (assert-inlined type inlined)
  (type-alignment 'bool))

(define-type-method to-alien-form ((type boolean) boolean &optional copy-p)
  (declare (ignore type copy-p))
  (to-alien-form 'bool boolean))

(define-type-method to-alien-function ((type boolean) &optional copy-p)
  (declare (ignore type copy-p))
  (to-alien-function 'bool))

(define-type-method from-alien-form ((type boolean) form &key ref)
  (declare (ignore type ref))
  (from-alien-form 'bool form))

(define-type-method from-alien-function ((type boolean) &key ref)
  (declare (ignore type ref))
  (from-alien-function 'bool))

(define-type-method writer-function ((type boolean) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  (writer-function 'bool))

(define-type-method reader-function ((type boolean) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  (reader-function 'bool))


;;; Or

(define-type-method alien-type ((type or))
  (let* ((expanded-type (type-expand-to 'or type))
	 (alien-type (alien-type (second expanded-type))))
    (unless (every #'(lambda (type)
		       (eq alien-type (alien-type type)))
		   (cddr expanded-type))
      (error "No common alien type specifier for union type: ~A" type))
    alien-type))

(define-type-method size-of ((type or) &key (inlined nil inlined-p))
  (loop
   for subtype in (type-expand-to 'or type)
   maximize (if inlined-p
		(size-of subtype inlined)
	      (size-of subtype))))

(define-type-method type-alignment ((type or) &key (inlined nil inlined-p))
  (loop
   for subtype in (type-expand-to 'or type)
   maximize (if inlined-p
		(type-alignment subtype inlined)
	      (type-alignment subtype))))

(define-type-method alien-arg-wrapper ((type or) var value style form &optional copy-in-p)
  (cond 
   ((and (in-arg-p style) (out-arg-p style))
    `(etypecase ,value
       ,@(mapcar	 
	  #'(lambda (type)
	      `(,type ,(alien-arg-wrapper type var value style form copy-in-p)))
	  (rest (type-expand-to 'or type)))))
   ((in-arg-p style)
    (let ((body (make-symbol "BODY")))
      `(flet ((,body (,var)
	         ,form))
	 (etypecase ,value
	   ,@(mapcar	 
	      #'(lambda (type)
		  `(,type ,(alien-arg-wrapper type var value style `(,body ,var) copy-in-p)))
	      (rest (type-expand-to 'or type)))))))
   ((out-arg-p style)
    #+(or cmu sbcl)
    `(with-alien ((,var ,(alien-type type)))
       (clear-memory (alien-sap (addr ,var)) ,(size-of type))
         ,form)
    #+clisp
    `(ffi:with-c-var (,var ',(alien-type type))
       ,form))))

(define-type-method to-alien-form ((type or) form &optional copy-p)
  `(let ((value ,form))
     (etypecase value
       ,@(mapcar	 
	  #'(lambda (type)
	      `(,type ,(to-alien-form type 'value copy-p)))
	  (rest (type-expand-to 'or type))))))

(define-type-method to-alien-function ((type or) &optional copy-p)
  (let* ((expanded-type (type-expand-to 'or type))
	 (functions (loop
		     for type in (rest expanded-type)
		     collect (to-alien-function type copy-p))))
    #'(lambda (value)
	(loop
	 for function in functions
	 for alt-type in (rest expanded-type)
	 when (typep value alt-type)
	 do (return (funcall function value))
	 finally (error "~S is not of type ~A" value type)))))


;;; Pointer

(define-type-method alien-type ((type pointer))
  (declare (ignore type))
  #+(or cmu sbcl) 'system-area-pointer
  #+clisp 'ffi:c-pointer)

(define-type-method size-of ((type pointer) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-sizeof 'sb-alien:system-area-pointer)
  #+clisp (ffi:sizeof 'ffi:c-pointer)
  #-(or sbcl clisp) 4)

(define-type-method type-alignment ((type pointer) &key (inlined t))
  (assert-inlined type inlined)
  #+sbcl (sb-alignment 'system-area-pointer)
  #+clisp (ffi:sizeof 'ffi:c-pointer)
  #-(or sbcl clisp) (size-of 'pointer))

(define-type-method to-alien-form ((type pointer) form &optional copy-p)
  (declare (ignore type copy-p))
  form)

(define-type-method to-alien-function ((type pointer) &optional copy-p)
  (declare (ignore type copy-p))
  #'identity)

(define-type-method from-alien-form ((type pointer) form &key ref)
  (declare (ignore type ref))
  form)

(define-type-method from-alien-function ((type pointer) &key ref)
  (declare (ignore type ref))
  #'identity)

(define-type-method writer-function ((type pointer) &key temp (inlined t))
  (declare (ignore temp))
  (assert-inlined type inlined)
  #'(setf ref-pointer))

(define-type-method reader-function ((type pointer) &key ref (inlined t))
  (declare (ignore ref))
  (assert-inlined type inlined)
  #'ref-pointer)


(define-type-method alien-type ((type null))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method size-of ((type null) &key (inlined t))
  (assert-inlined type inlined)
  (size-of 'pointer))

(define-type-method to-alien-form ((type null) null &optional copy-p)
  (declare (ignore type copy-p))
  `(progn ,null (make-pointer 0)))

(define-type-method to-alien-function ((type null) &optional copy-p)
  (declare (ignore type copy-p))
  #'(lambda (null)
      (declare (ignore null))
      (make-pointer 0)))


(define-type-method alien-type ((type nil))
  (declare (ignore type))
  #+(or cmu sbcl) 'void
  #+clisp nil)

(define-type-method from-alien-form ((type nil) form &key ref)
  (declare (ignore type ref))
  form)

(define-type-method from-alien-function ((type nil) &key ref)
  (declare (ignore type ref))
  #'(lambda (value)
      (declare (ignore value))
      (values)))

(define-type-method to-alien-form ((type nil) form &optional copy-p)
  (declare (ignore type copy-p))
  form)



;;; Callbacks

(define-type-method alien-type ((type callback))
  (declare (ignore type))
  (alien-type 'pointer))

(define-type-method to-alien-form ((type callback) callback &optional copy-p)
  (declare (ignore type copy-p))
  `(callback-address ,callback))



;;; Copy-of

(define-type-method from-alien-form ((type copy-of) form &key (ref :copy))
  (if (eq ref :copy)
      (from-alien-form (second (type-expand-to 'copy-of type)) form :ref ref)
    (error "Keyword arg :REF to FROM-ALIEN-FORM should be :COPY for type ~A. It was give ~A" type ref)))

(define-type-method from-alien-function ((type copy-of) &key (ref :copy))
  (if (eq ref :copy)
      (from-alien-function (second (type-expand-to 'copy-of type)) :ref ref)
    (error "Keyword arg :REF to FROM-ALIEN-FUNCTION should be :COPY for type ~A. It was give ~A" type ref)))

(define-type-method to-alien-form ((type copy-of) form &optional (copy-p t))
  (if copy-p
      (to-alien-form (second (type-expand-to 'copy-of type)) form t)
    (error "COPY-P argument to TO-ALIEN-FORM should always be non NIL for type ~A" type)))

(define-type-method to-alien-function ((type copy-of) &optional (copy-p t))
  (if copy-p
      (to-alien-function (second (type-expand-to 'copy-of type)) t)
    (error "COPY-P argument to TO-ALIEN-FUNCTION should always be non NIL for type ~A" type)))

(define-type-method reader-function ((type copy-of) &key (ref :read) (inlined nil inlined-p))
  (if inlined-p
      (reader-function (second (type-expand-to 'copy-of type)) 
       :ref (if (eq ref :get) :read ref) :inlined inlined)
    (reader-function (second (type-expand-to 'copy-of type))
     :ref (if (eq ref :get) :read ref))))

(define-type-method destroy-function ((type copy-of) &key temp inlined)
  (declare (ignore type temp inlined))
  #'(lambda (location &optional offset)
      (declare (ignore location offset))))

(define-type-method copy-function ((type copy-of) &key (inlined nil inlined-p))
  (let ((size (if inlined-p 
		  (size-of type :inlined inlined)
		(size-of type))))
    #'(lambda (from to &optional (offset 0))
	(copy-memory (pointer+ from offset) size (pointer+ to offset)))))



;;; Static

(define-type-method from-alien-form ((type static) form &key (ref :static))
  (if (eq ref :static)
      (from-alien-form (second (type-expand-to 'static type)) form :ref ref)
    (error "Keyword arg :REF to FROM-ALIEN-FORM should be :STATIC for type ~A. It was give ~A" type ref)))

(define-type-method from-alien-function ((type static) &key (ref :static))
  (if (eq ref :static)
      (from-alien-function (second (type-expand-to 'static type)) :ref ref)
    (error "Keyword arg :REF to FROM-ALIEN-FUNCTION should be :STATIC for type ~A. It was give ~A" type ref)))

(define-type-method to-alien-function ((type static) &optional copy-p)
  (if (not copy-p)
      (to-alien-function (second (type-expand-to 'static type)) t)
  (error "COPY-P argument to TO-ALIEN-FUNCTION should always be NIL for type ~A" type)))

(define-type-method to-alien-form ((type static) &optional copy-p)
  (if (not copy-p)
      (to-alien-function (second (type-expand-to 'static type)) t)
  (error "COPY-P argument to TO-ALIEN-FORM should always be NIL for type ~A" type)))

(define-type-method reader-function ((type static) &key (ref :read) (inlined nil inlined-p))
  (if inlined-p
      (reader-function (second (type-expand-to 'static type)) 
       :ref (if (eq ref :get) :read ref) :inlined inlined)
    (reader-function (second (type-expand-to 'static type))
     :ref (if (eq ref :get) :read ref))))

(define-type-method writer-function ((type static) &key temp inlined)
  (declare (ignore type temp inlined))
  (error "Can't overwrite a static (const) reference"))

(define-type-method destroy-function ((type static) &key temp inlined)
  (declare (ignore type temp inlined))
  #'(lambda (location &optional offset)
      (declare (ignore location offset))))

(define-type-method copy-function ((type static) &key (inlined nil inlined-p))
  (let ((size (if inlined-p 
		  (size-of type :inlined inlined)
		(size-of type))))
    #'(lambda (from to &optional (offset 0))
	(copy-memory (pointer+ from offset) size (pointer+ to offset)))))



;;; Pseudo type for inlining of types which are not inlined by default

(define-type-method size-of ((type inlined) &key (inlined t))
  (assert-inlined type inlined)
  (size-of (second (type-expand-to 'inlined type)) :inlined t))

(define-type-method type-alignment ((type inlined) &key (inlined t))
  (assert-inlined type inlined)
  (type-alignment (second (type-expand-to 'inlined type)) :inlined t))

(define-type-method reader-function ((type inlined) &key (ref :read) (inlined t))
  (assert-inlined type inlined)
  (reader-function (second (type-expand-to 'inlined type)) :ref ref :inlined t))

(define-type-method writer-function ((type inlined) &key temp (inlined t))
  (assert-inlined type inlined)
  (writer-function (second (type-expand-to 'inlined type)) :temp temp :inlined t))

(define-type-method destroy-function ((type inlined) &key temp (inlined t))
  (assert-inlined type inlined)
  (destroy-function (second (type-expand-to 'inlined type)) :temp temp :inlined t))

(define-type-method copy-function ((type inlined) &key (inlined t))
  (assert-inlined type inlined)
  (copy-function (second (type-expand-to 'inlined type)) :inlined t))
