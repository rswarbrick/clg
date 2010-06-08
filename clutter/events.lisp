(in-package :clutter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: This is pinched almost verbatim from gdkevents.lisp. Combine the two.
  (defvar *event-classes* (make-hash-table))

  (defclass clutter-event-class (boxed-class)
    ((event-type :reader event-class-type :initform nil)))
  
  (defmethod shared-initialize :around ((class clutter-event-class) names
                                        &key name event-type)
    (declare (ignore names))
    (register-type-alias (or name (class-name class)) 'event)
    (call-next-method)
    (when event-type
      (setf (slot-value class 'event-type) (first event-type))
      (setf (gethash (first event-type) *event-classes*) class)))

  (let ((reader (reader-function 'event-type)))
    (defun %event-class (location)
      (or
       (gethash (funcall reader location 0) *event-classes*)
       (error "No class defined for event type: ~S"
              (funcall reader location 0)))))

  (defmethod make-proxy-instance :around ((class clutter-event-class) location 
                                          &rest initargs)
    (let ((class (%event-class location)))
      (apply #'call-next-method class location initargs)))

  (defclass event (boxed)
    ((%type
      :allocation :alien
      :type event-type)
     (time
      :allocation :alien
      :type unsigned-int
      :initarg :time
      :accessor event-time)
     (flags
      :allocation :alien
      :type event-flags
      :initarg :flags
      :accessor event-flags)
     (stage
      :allocation :alien
      :type stage
      :initarg :stage
      :accessor event-stage)
     (source
      :allocation :alien
      :type actor
      :initarg :source
      :accessor event-source))
    (:metaclass boxed-class))

  (defmethod initialize-instance :after ((event event) &rest initargs)
    (declare (ignore initargs))
    (setf (slot-value event '%type) (event-class-type (class-of event))))

  (defmethod make-proxy-instance ((class (eql (find-class 'event))) location
                                  &rest initargs)
    (let ((class (%event-class location)))
      (apply #'make-proxy-instance class location initargs)))

  (defclass key-event (event)
    ((modifier-state
      :allocation :alien
      :type modifier-type
      :initarg :modifier-state
      :accessor event-modifier-state)
     (keyval
      :allocation :alien
      :type unsigned-int
      :initarg :keyval
      :accessor event-keyval)
     (hardware-keycode
      :allocation :alien
      :type unsigned-short
      :initarg :hardware-keycode
      :accessor event-hardware-keycode)
     (unicode-value
      :allocation :alien
      :type unsigned-int
      :initarg :unicode-value
      :accessor event-unicode-value)
     (device
      :allocation :alien
      :type input-device
      :initarg :device
      :accessor event-device))
    (:metaclass clutter-event-class))

  (defclass key-press-event (key-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :key-press))

  (defclass key-release-event (key-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :key-release))

  (defclass positioned-event (event)
    ((x
      :allocation :alien
      :type single-float
      :initarg :x
      :accessor event-x)
     (y
      :allocation :alien
      :type single-float
      :initarg :y
      :accessor event-y))
    (:metaclass clutter-event-class))

  (defclass button-event (positioned-event)
    ((modifier-state
      :allocation :alien
      :type modifier-type
      :initarg :modifier-state
      :accessor event-modifier-state)
     (button
      :allocation :alien
      :type unsigned-int
      :initarg :button
      :accessor event-button)
     (click-count
      :allocation :alien
      :type unsigned-int
      :initarg :click-count
      :accessor event-click-count)
     (axes
      :allocation :alien
      :type pointer
      :initarg :axes
      :accessor event-axes)
     (device
      :allocation :alien
      :type input-device
      :initarg :device
      :accessor event-device))
    (:metaclass clutter-event-class))
  
  (defclass button-press-event (button-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :button-press))

  (defclass button-release-event (button-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :button-release))

  (defclass crossing-event (positioned-event)
    ((device
      :allocation :alien
      :type input-device
      :initarg :device
      :accessor event-device)
     (related
      :allocation :alien
      :type actor
      :initarg :related
      :accessor event-related))
    (:metaclass clutter-event-class))

  (defclass enter-event (crossing-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :enter))

  (defclass leave-event (crossing-event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :leave))

  (defclass motion-event (positioned-event)
    ((modifier-state
      :allocation :alien
      :type modifier-type
      :initarg :modifier-state
      :accessor event-modifier-state)
     (axes
      :allocation :alien
      :type pointer
      :initarg :axes
      :accessor event-axes)
     (device
      :allocation :alien
      :type input-device
      :initarg :device
      :accessor event-device))
    (:metaclass clutter-event-class)
    (:event-type :motion))

  (defclass scroll-event (positioned-event)
    ((direction
      :allocation :alien
      :type scroll-direction
      :initarg :direction
      :accessor event-direction)
     (modifier-state
      :allocation :alien
      :type modifier-type
      :initarg :modifier-state
      :accessor event-modifier-state)
     (axes
      :allocation :alien
      :type pointer
      :initarg :axes
      :accessor event-axes)
     (device
      :allocation :alien
      :type input-device
      :initarg :device
      :accessor event-device))
    (:metaclass clutter-event-class)
    (:event-type :scroll))

  (defclass stage-state-event (event)
    ((changed-mask
      :allocation :alien
      :type stage-state
      :initarg :changed-mask
      :accessor event-changed-mask)
     (new-state
      :allocation :alien
      :type stage-state
      :initarg :new-state
      :accessor event-new-state))
    (:metaclass clutter-event-class)
    (:event-type :stage-state))

  (defclass delete-event (event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :delete))

  (defclass destroy-notify-event (event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :destroy-notify))

  (defclass client-message-event (event)
    ()
    (:metaclass clutter-event-class)
    (:event-type :client-message)))
