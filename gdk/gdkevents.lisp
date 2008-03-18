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

;; $Id: gdkevents.lisp,v 1.15 2008-03-18 15:08:08 espen Exp $

(in-package "GDK")


;;;; Metaclass for event classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *event-classes* (make-hash-table))

  (defclass event-class (boxed-class)
    ((event-type :reader event-class-type :initform nil)))

  (defmethod validate-superclass ((class event-class) (super standard-class))
    ;(subtypep (class-name super) 'event)
    t)

  (defmethod shared-initialize ((class event-class) names &key name event-type)
    (declare (ignore names))
    (register-type-alias (or name (class-name class)) 'event)
    (call-next-method)
    (when event-type
      (setf (slot-value class 'event-type) (first event-type))
      (setf (gethash (first event-type) *event-classes*) class))))
  
(let ((reader (reader-function 'event-type)))
  (defun %event-class (location)
    (or
     (gethash (funcall reader location 0) *event-classes*)
     (error "No class defined for event type: ~S" (funcall reader location 0)))))

(defmethod make-proxy-instance :around ((class event-class) location 
					&rest initargs)
  (let ((class (%event-class location)))
    (apply #'call-next-method class location initargs)))


;; The class event is the only class that actually exists in the
;; GObject class hierarchy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass event (boxed)
    ((%type
      :allocation :alien
      :type event-type)
     (window
      :allocation :alien
      :accessor event-window
      :initarg :window
      :type window)
     (send-event
      :allocation :alien
      :accessor event-send-event
      :initarg :send-event
      :type (bool 8)))
    (:metaclass boxed-class)))

(defmethod initialize-instance :after ((event event) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value event '%type) (event-class-type (class-of event))))

(defmethod make-proxy-instance ((class (eql (find-class 'event))) location &rest initargs)
  (let ((class (%event-class location)))
    (apply #'make-proxy-instance class location initargs)))


(defclass timed-event (event)
  ((time
    :allocation :alien
    :accessor event-time
    :initarg :time
    :type (unsigned 32)))
  (:metaclass event-class))
  
(defclass delete-event (event)
  ()
  (:metaclass event-class)
  (:event-type :delete))


(defclass destroy-event (event)
  ()
  (:metaclass event-class)
  (:event-type :destroy))

(defclass expose-event (event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type int)
   (width
    :allocation :alien
    :accessor event-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor event-height
    :initarg :height
    :type int)
   (region
    :allocation :alien
    :accessor event-region
    :initarg :region
    :type pointer)
   (count
    :allocation :alien
    :accessor event-count
    :initarg :count
    :type int))
  (:metaclass event-class)
  (:event-type :expose))

(defclass input-event (timed-event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type double-float)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type double-float)
   (axes
    :allocation :alien
    :accessor event-axes
    :initarg :axes
    :type pointer) ;double-float)
   (state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type modifier-type))
  (:metaclass event-class))


(defclass motion-notify-event (input-event)
  ((is-hint
    :allocation :alien
    :accessor event-is-hint
    :initarg :is-hint
    :type (signed 16)			; should it be (boolean 16)?
    )
   (device
    :allocation :alien
    :offset 2
    :accessor event-device
    :initarg :device
    :type device)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float))
  (:metaclass event-class)
  (:event-type :motion-notify))
  
(defclass button-event (input-event)
  ((button
    :allocation :alien
    :accessor event-button
    :initarg :button
    :type unsigned-int)
   (device
    :allocation :alien
    :accessor event-device
    :initarg :device
    :type device)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float))
  (:metaclass event-class))

(defclass button-press-event (button-event)
  ()
  (:metaclass event-class)
  (:event-type :button-press))

(defclass 2-button-press-event (button-press-event)
  ()
  (:metaclass event-class)
  (:event-type :2button-press))

(defclass 3-button-press-event (button-press-event)
  ()
  (:metaclass event-class)
  (:event-type :3button-press))

(defclass button-release-event (button-event)
  ()
  (:metaclass event-class)
  (:event-type :button-release))


(defclass key-event (timed-event)
  ((state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type modifier-type)
   (keyval 
    :allocation :alien
    :accessor event-keyval
    :initarg :keyval
    :type unsigned-int)
   (length
    :allocation :alien
    :accessor event-length
    :initarg :length
    :type unsigned-int)
   (string
    :allocation :alien
    :accessor event-string
    :initarg :string
    :type string)
   (hardware-keycode
    :allocation :alien
    :accessor event-hardware-keycode
    :initarg :hardware-keycode
    :type (unsigned 16))
   (group
    :allocation :alien
    :accessor event-group
    :initarg :group
    :type (unsigned 8)))
  (:metaclass event-class))

(defclass key-press-event (key-event)
  ()
  (:metaclass event-class)
  (:event-type :key-press))

(defclass key-release-event (key-event)
  ()
  (:metaclass event-class)
  (:event-type :key-release))


(defclass crossing-event (event)
  ((subwindow
    :allocation :alien
    :accessor event-subwindow
    :initarg :subwindow
    :type window)
   (time
    :allocation :alien
    :accessor event-time
    :initarg :time
    :type (unsigned 32))
   (x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type double-float)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type double-float)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float)
   (mode
    :allocation :alien
    :accessor event-mode
    :initarg :mode
    :type crossing-mode)
   (detail
    :allocation :alien
    :accessor event-detail
    :initarg :detail
    :type notify-type)
   (focus
    :allocation :alien
    :accessor event-focus
    :initarg :focus
    :type boolean)
   (state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type unsigned-int))
  (:metaclass event-class))


(defclass enter-notify-event (crossing-event)
  ()
  (:metaclass event-class)
  (:event-type :enter-notify))

(defclass leave-notify-event (crossing-event)
  ()
  (:metaclass event-class)
  (:event-type :leave-notify))

(defclass focus-change-event (event)
  ((in
    :allocation :alien
    :accessor event-in
    :initarg :in
    :type (bool 16)))
  (:metaclass event-class)
  (:event-type :focus-change))

(defclass configure-event (event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type int)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type int)
   (width
    :allocation :alien
    :accessor event-width
    :initarg :width
    :type int)
   (height
    :allocation :alien
    :accessor event-height
    :initarg :height
    :type int))
  (:metaclass event-class)
  (:event-type :configure))

(defclass map-event (event)
  ()
  (:metaclass event-class)
  (:event-type :map))

(defclass unmap-event (event)
  ()
  (:metaclass event-class)
  (:event-type :unmap))

(defclass property-notify-event (event)
  ()
  (:metaclass event-class)
  (:event-type :property-notify))

(defclass selection-clear-event (event)
  ()
  (:metaclass event-class)
  (:event-type :selection-clear))

(defclass selection-request-event (event)
  ()
  (:metaclass event-class)
  (:event-type :selection-request))

(defclass selection-notify-event (event)
  ()
  (:metaclass event-class)
  (:event-type :selection-notify))

(defclass dnd-event (event)
  ((context
    :allocation :alien
    :accessor event-contex
    :initarg :context
    :type drag-context)
   (time
    :allocation :alien
    :accessor event-time
    :initarg :time
    :type (unsigned 32))
   (x-root
    :allocation :alien
    :accessor event-x-root
    :initarg :x-root
    :type short)
   (y-root
    :allocation :alien
    :accessor event-y-root
    :initarg :y-root
    :type short))
  (:metaclass event-class))

(defclass drag-enter-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drag-enter))

(defclass drag-leave-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drag-leave))

(defclass drag-motion-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drag-motion))

(defclass drag-status-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drag-status))

(defclass drot-start-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drop-start))

(defclass drop-finished-event (dnd-event)
  ()
  (:metaclass event-class)
  (:event-type :drop-finished))

(defclass client-event (event)
  ()
  (:metaclass event-class)
  (:event-type :client-event))

(defclass visibility-notify-event (event)
  ((state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type visibility-state))
  (:metaclass event-class)
  (:event-type :visibility-notify))

(defclass no-expose-event (event)
  ()
  (:metaclass event-class)
  (:event-type :no-expose))
  
(defclass scroll-event (timed-event)
  ((x
    :allocation :alien
    :accessor event-x
    :initarg :x
    :type double-float)
   (y
    :allocation :alien
    :accessor event-y
    :initarg :y
    :type double-float)
   (state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type modifier-type)
   (direction
    :allocation :alien
    :accessor event-direction
    :initarg :direction
    :type scroll-direction)
   (root-x
    :allocation :alien
    :accessor event-root-x
    :initarg :root-x
    :type double-float)
   (root-y
    :allocation :alien
    :accessor event-root-y
    :initarg :root-y
    :type double-float))
  (:metaclass event-class)
  (:event-type :scroll))

(defclass setting-event (event)
  ((action
    :allocation :alien
    :accessor event-action
    :initarg :action
    :type setting-action)
   (name
    :allocation :alien
    :accessor event-name
    :initarg :name
    :type string))
  (:metaclass event-class)
  (:event-type :setting))

(defclass proximity-event (timed-event)
  ((device
    :allocation :alien
    :accessor event-device
    :initarg :device
    :type device))
  (:metaclass event-class))

(defclass proximity-in-event (proximity-event)
  ()
  (:metaclass event-class)
  (:event-type :proximity-in))

(defclass proximity-out-event (proximity-event)
  ()
  (:metaclass event-class)
  (:event-type :proximity-out))

(defclass window-state-event (event)
  ((change-mask
    :allocation :alien
    :accessor event-change-mask
    :initarg :change-mask
    :type window-state)
   (new-window-state
    :allocation :alien
    :accessor event-new-window-state
    :initarg :new-window-state
    :type window-state))
  (:metaclass event-class)
  (:event-type :window-state))
  
(defclass owner-change-event (event)
  ()
  (:metaclass event-class)
  (:event-type :owner-change))

(defclass grab-broken-event (event)
  ((keyboard
    :allocation :alien
    :accessor event-keyboard
    :initarg :keyboard
    :type boolean)
   (implicit
    :allocation :alien
    :accessor event-implicit
    :initarg :implicit
    :type boolean)
   (grab-window
    :allocation :alien
    :accessor event-grab-window
    :initarg :grab-window
    :type window))
  (:metaclass event-class)
  (:event-type :grab-broken))
