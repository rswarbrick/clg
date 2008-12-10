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

;; $Id: main-loop.lisp,v 1.1 2008-12-10 02:51:59 espen Exp $


(in-package "GLIB")

(use-prefix "g")

;;; Main loop

(defbinding %main-loop-ref () pointer
  (location pointer))

(defbinding %main-loop-unref () nil
  (location pointer))

(defbinding %main-loop-new () pointer
  (context (or null pointer))
  (is-running boolean))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass main-loop (ref-counted-object)
    ((is-running 
      :allocation :virtual :getter "g_main_loop_is_running"
      :reader main-loop-is-running-p :type boolean)
     (context 
      :allocation :virtual :getter "g_main_loop_get_context"
      :reader main-loop-context :type pointer))
    (:metaclass proxy-class)
    (:ref %main-loop-ref)
    (:unref %main-loop-unref)))

(defmethod allocate-foreign ((main-loop main-loop) &key context is-running)
  (%main-loop-new context is-running))

(defbinding main-loop-run () nil
  main-loop)

(defbinding main-loop-quit () nil
  main-loop)

(defbinding %main-context-new () pointer)

(defbinding %main-context-unref () nil
  pointer)

(defmacro with-main-loop ((&optional main-loop) &body body)
  (let ((%main-loop (make-symbol "MAIN-LOOP"))
	(%main-context (make-symbol "MAIN-CONTEXT")))
    `(let* ((,%main-context (%main-context-new))
	    (,%main-loop (or ,main-loop (make-instance 'main-loop :context ,%main-context))))
       (main-loop-run ,%main-loop)
       (unwind-protect
	    (progn ,@body)
	 (main-loop-quit ,%main-loop)
	 (%main-context-unref ,%main-context)))))

