;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
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

;; $Id: gdkevents.lisp,v 1.7 2005-02-26 10:44:09 espen Exp $

(in-package "GDK")


(define-flags-type event-mask
  (:exposure 2)
  :pointer-motion
  :pointer-motion-hint
  :button-motion
  :button1-motion
  :button2-motion
  :button3-motion
  :button-press
  :button-release
  :key-press
  :key-release
  :enter-notify
  :leave-notify
  :focus-change
  :structure
  :property-change
  :visibility-notify
  :proximity-in
  :proximity-out
  :substructure
  :scroll
  (:all-events #x3FFFFE))


;;;; Metaclass for event classes

(defvar *event-classes* (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass event-class (boxed-class)
    ((event-type :reader event-class-type)))

  (defmethod validate-superclass ((class event-class) (super standard-class))
    ;(subtypep (class-name super) 'event)
    t))


(defmethod shared-initialize ((class event-class) names &key name type)
  (call-next-method)
  (setf (slot-value class 'event-type) (first type))
  (setf (gethash (first type) *event-classes*) class)
  (let ((class-name (or name (class-name class))))
    (register-type class-name 'event)))
  
(let ((reader (reader-function 'event-type)))
  (defun %event-class (location)
    (gethash (funcall reader location 0) *event-classes*)))

(defmethod ensure-proxy-instance ((class event-class) location)
  (declare (ignore class))
  (let ((class (%event-class location)))
    (make-instance class :location location)))


;;;;

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
      :type (boolean 8)))
    (:metaclass event-class)))


(defmethod initialize-instance ((event event) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value event '%type) (event-class-type (class-of event))))


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
  (:type :delete))


(defclass destroy-event (event)
  ()
  (:metaclass event-class)
  (:type :destroy))

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
  (:type :expose))

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
  (:type :motion-notify))
  
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
  (:type :button-press))

(defclass 2-button-press-event (button-press-event)
  ()
  (:metaclass event-class)
  (:type :2button-press))

(defclass 3-button-press-event (button-press-event)
  ()
  (:metaclass event-class)
  (:type :3button-press))

(defclass button-release-event (button-event)
  ()
  (:metaclass event-class)
  (:type :button-release))


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
  (:type :key-press))

(defclass key-release-event (key-event)
  ()
  (:metaclass event-class)
  (:type :key-release))


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
  (:type :enter-notify))

(defclass leave-notify-event (crossing-event)
  ()
  (:metaclass event-class)
  (:type :leave-notify))

(defclass focus-change-event (event)
  ((in
    :allocation :alien
    :accessor event-in
    :initarg :in
    :type (boolean 16)))
  (:metaclass event-class)
  (:type :focus-change))

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
  (:type :configure))

(defclass map-event (event)
  ()
  (:metaclass event-class)
  (:type :map))

(defclass unmap-event (event)
  ()
  (:metaclass event-class)
  (:type :unmap))

(defclass property-notify-event (event)
  ()
  (:metaclass event-class)
  (:type :property-notify))

(defclass selection-clear-event (event)
  ()
  (:metaclass event-class)
  (:type :selection-clear))

(defclass selection-request-event (event)
  ()
  (:metaclass event-class)
  (:type :selection-request))

(defclass selection-notify-event (event)
  ()
  (:metaclass event-class)
  (:type :selection-notify))

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
  (:type :drag-enter))

(defclass drag-leave-event (dnd-event)
  ()
  (:metaclass event-class)
  (:type :drag-leave))

(defclass drag-motion-event (dnd-event)
  ()
  (:metaclass event-class)
  (:type :drag-motion))

(defclass drag-status-event (dnd-event)
  ()
  (:metaclass event-class)
  (:type :drag-status))

(defclass drot-start-event (dnd-event)
  ()
  (:metaclass event-class)
  (:type :drop-start))

(defclass drop-finished-event (dnd-event)
  ()
  (:metaclass event-class)
  (:type :drop-finished))

(defclass client-event (event)
  ()
  (:metaclass event-class)
  (:type :client-event))

(defclass visibility-notify-event (event)
  ((state
    :allocation :alien
    :accessor event-state
    :initarg :state
    :type visibility-state))
  (:metaclass event-class)
  (:type :visibility-notify))

(defclass no-expose-event (event)
  ()
  (:metaclass event-class)
  (:type :no-expose))
  
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
  (:type :scroll))

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
  (:type :setting))

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
  (:type :proximity-in))

(defclass proximity-out-event (proximity-event)
  ()
  (:metaclass event-class)
  (:type :proximity-out))

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
  (:type :window-state))
  
(defclass owner-change-event (event)
  ()
  (:metaclass event-class)
  (:type :owner-change))

