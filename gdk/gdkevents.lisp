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

;; $Id: gdkevents.lisp,v 1.4 2004-10-31 11:53:30 espen Exp $

(in-package "GDK")


(defvar *event-classes* (make-hash-table))

(defun %type-of-event (location)
  (class-name
   (gethash
    (funcall (intern-reader-function 'event-type) location 0)
    *event-classes*)))

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
      :type (boolean 8))
     (%align :allocation :alien :offset 2 :type (unsigned 8)))
    (:metaclass boxed-class)))


(defmethod initialize-instance ((event event) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value event '%type) (event-class-type (class-of event))))

(deftype-method translate-from-alien
    event (type-spec location &optional weak-ref)
  (declare (ignore type-spec))    
  `(let ((location ,location))
     (unless (null-pointer-p location)
       (ensure-proxy-instance (%type-of-event location) location ,weak-ref))))


;;;; Metaclass for event classes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass event-class (proxy-class)
    ((event-type :reader event-class-type)))

  
  (defmethod shared-initialize ((class event-class) names &key name type)
    (call-next-method)
    (setf (slot-value class 'event-type) (first type))
    (setf (gethash (first type) *event-classes*) class)
    (let ((class-name (or name (class-name class))))
      (register-type class-name 'event)))
  

  (defmethod validate-superclass
    ((class event-class) (super pcl::standard-class))
    (subtypep (class-name super) 'event)))


;;;;

(defclass timed-event (event)
  ((time
    :allocation :alien
    :accessor event-time
    :initarg :time
    :type (unsigned 32)))
  (:metaclass proxy-class))
  
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
   (count
    :allocation :alien
    :accessor event-count
    :initarg :count
    :type int))
  (:metaclass event-class)
  (:type :expose))

(defclass motion-notify-event (timed-event)
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
    :offset #.(size-of 'pointer)
    :accessor event-state
    :initarg :state
    :type unsigned-int)
   (is-hint
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
  
(defclass button-press-event (timed-event)
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
    :offset #.(size-of 'pointer)
    :accessor event-state
    :initarg :state
    :type modifier-type)
   (button
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

(defclass button-release-event (button-press-event)
  ()
  (:metaclass event-class)
  (:type :button-release))

(defclass key-press-event (event)
  ()
  (:metaclass event-class)
  (:type :key-press))

(defclass key-release-event (event)
  ()
  (:metaclass event-class)
  (:type :key-release))

(defclass enter-notify-event (event)
  ()
  (:metaclass event-class)
  (:type :enter-notify))

(defclass leave-notify-event (event)
  ()
  (:metaclass event-class)
  (:type :leave-notify))

(defclass focus-change-event (event)
  ()
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

(defclass drag-enter-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-enter))

(defclass drag-leave-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-leave))

(defclass drag-motion-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-motion))

(defclass drag-status-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-status))

(defclass drag-start-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-start))

(defclass drag-finished-event (event)
  ()
  (:metaclass event-class)
  (:type :drag-finished))

(defclass client-event (event)
  ()
  (:metaclass event-class)
  ;(:type :client-event)
  )

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
  ()
  (:metaclass event-class)
  (:type :scroll))

(defclass setting-event (timed-event)
  ()
  (:metaclass event-class)
  (:type :setting))
