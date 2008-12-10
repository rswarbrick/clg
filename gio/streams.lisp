;; Common Lisp bindings for GTK+ 2.x
;; Copyright 2008 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: streams.lisp,v 1.1 2008-12-10 02:58:13 espen Exp $


(in-package "GIO")

(use-prefix "g")

;;; Input streams 

(defbinding input-stream-read (stream buffer &key length cancellable) gssize
  (stream input-stream)
  (buffer pointer)
  (length gsize)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding input-stream-read-all (stream buffer &key length cancellable) boolean
  (stream input-stream)
  (buffer pointer)
  (length gsize)
  (bytes-read gsize :out)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding input-stream-skip (stream length &key cancellable) gssize
  (stream input-stream)
  (length gsize)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding input-stream-close (stream &key cancellable) boolean
  (stream input-stream)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))


;;; Output streams 

(defbinding output-stream-write (stream buffer &key length cancellable) gssize
  (stream output-stream)
  (buffer (or (unboxed-vector (unsigned-byte 8)) pointer))
  ((or length (length buffer)) gsize)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding output-stream-write-all (stream buffer &key length cancellable) boolean
  (stream output-stream)
  (buffer (or (unboxed-vector (unsigned-byte 8)) pointer))
  ((or length (length buffer)) gsize)
  (bytes-written gsize :out)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding output-stream-flush (stream &key cancellable) boolean
  (stream output-stream)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))

(defbinding output-stream-close (stream &key cancellable) boolean
  (stream output-stream)
  (cancellable (or null cancellable))
  (nil gerror-signal :out))


;;; Unix streams

(defbinding %unix-input-stream-new () pointer
  (fd int)
  (close-fd-p boolean))

(defmethod allocate-foreign ((stream unix-input-stream) &key fd close-fd)
  (%unix-input-stream-new fd close-fd))

(defbinding %unix-output-stream-new () pointer
  (fd int)
  (close-fd-p boolean))

(defmethod allocate-foreign ((stream unix-output-stream) &key fd close-fd)
  (%unix-output-stream-new fd close-fd))


;;; Callback streams (clg extension)

(define-callback callback-stream-read-func gssize 
    ((buffer pointer) (count gsize) (cancellable (or null cancellable)) 
     (gerror pointer) (stream-id pointer-data))
  (declare (ignore cancellable))
  (handler-case
      (let* ((sequence (make-array count :element-type '(unsigned-byte 8)))
	     (stream (find-user-data stream-id))
	     (bytes-read (read-sequence sequence stream)))
	(unless (null-pointer-p buffer)
	  (make-c-vector '(unsigned-byte 8) bytes-read 
	   :content sequence :location buffer))
	bytes-read)
    (serious-condition (condition)
      (gerror-set-in-callback gerror (file-error-domain) 
       (enum-int :failed 'file-error-enum) (princ-to-string condition))
      -1)))

(define-callback callback-stream-write-func gssize 
    ((buffer pointer) (count gsize) (cancellable (or null cancellable)) 
     (gerror pointer) (stream-id pointer-data))
  (declare (ignore cancellable))
  (handler-case
      (let ((stream (find-user-data stream-id)))
	(write-sequence
	 (map-c-vector 'vector 'identity buffer '(unsigned-byte 8) count)
	 stream))
    (serious-condition (condition)
      (gerror-set-in-callback gerror (file-error-domain) 
       (enum-int :failed 'file-error-enum) (princ-to-string condition))
      -1)))

(define-callback callback-stream-flush-func boolean
    ((cancellable (or null cancellable)) (gerror pointer)
     (stream-id pointer-data))
  (declare (ignore cancellable))
  (handler-case (force-output (find-user-data stream-id))
    (serious-condition (condition)
      (gerror-set-in-callback gerror (file-error-domain) 
       (enum-int :failed 'file-error-enum) (princ-to-string condition))
      -1)))

(define-callback callback-stream-close-func boolean
    ((cancellable (or null cancellable)) gerror (stream-id pointer-data))
  (declare (ignore cancellable gerror))
  (destroy-user-data stream-id))

(defbinding %callback-input-stream-new (stream-id) pointer
  (callback-stream-read-func callback)
  (callback-stream-close-func callback)
  (stream-id pointer-data))

(defbinding %callback-output-stream-new (stream-id) pointer
  (callback-stream-read-func callback)
  (callback-stream-flush-func callback)
  (callback-stream-close-func callback)
  (stream-id pointer-data))

(defmethod allocate-foreign ((stream callback-input-stream) &key base-stream)
  (%callback-input-stream-new (register-user-data base-stream)))

(defmethod allocate-foreign ((stream callback-output-stream) &key base-stream)
  (%callback-input-stream-new (register-user-data base-stream)))


;;; Lisp integration

(deftype input-stream-designator () '(or stream input-stream integer))
(deftype output-stream-designator () '(or stream input-stream integer))

(define-type-method alien-type ((type input-stream-designator))
  (declare (ignore type))
  (alien-type 'input-stream))

(define-type-method alien-arg-wrapper ((type input-stream-designator) var stream style form &optional copy-in-p)
  (declare (ignore type))
  (let ((%stream (make-symbol "STREAM")))
    `(let ((,%stream (etypecase ,stream
		       (input-stream ,stream)
		       (integer (make-instance 'unix-input-stream :fd ,stream))
		       (stream (make-instance 'callback-input-stream 
				:base-stream ,stream)))))
       (unwind-protect
	    ,(alien-arg-wrapper 'input-stream var %stream style form copy-in-p)
	 (unless (typep ,stream 'input-stream)
	   (input-stream-close ,%stream))))))

(define-type-method alien-type ((type output-stream-designator))
  (declare (ignore type))
  (alien-type 'output-stream))

(define-type-method alien-arg-wrapper ((type output-stream-designator) var stream style form &optional copy-in-p)
  (declare (ignore type))
  (let ((%stream (make-symbol "STREAM")))
    `(let ((,%stream (etypecase ,stream
		       (output-stream ,stream)
		       (integer (make-instance 'unix-output-stream :fd ,stream))
		       (stream (make-instance 'callback-output-stream 
				:base-stream ,stream)))))
       (unwind-protect
	    ,(alien-arg-wrapper 'input-stream var %stream style form copy-in-p)
	 (unless (typep ,stream 'output-stream)
	   (output-stream-close ,%stream))))))

;; TODO: make GIO streams appear as Lisp streams
