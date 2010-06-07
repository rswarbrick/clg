(in-package :clutter)

;; The debug flag API isn't actually exported, so this might stop working, but
;; poking around with gdb to switch on messages is annoying...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-flags-type event-mask
      :misc :actor :texture :event :paint :gl :alpha :behaviour :pango
      :backend :scheduler :script :shader :multistage :animation :layout))

(sb-alien:define-alien-variable "clutter_debug_flags"
    sb-alien:unsigned-int)

(defun debug-flags ()
  (funcall (from-alien-function 'event-mask) clutter-debug-flags))

(defun listify (x) (if (listp x) x (list x)))

(defun (setf debug-flags) (flags)
  (setf clutter-debug-flags
        (funcall (to-alien-function 'event-mask) (listify flags)))
  flags)
