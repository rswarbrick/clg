(in-package :clutter)

(defbinding |stage_get_default| () actor)
(defbinding |stage_new| () actor)

(defun default-stage ()
  "Return Clutter's default stage. Note: you probably don't want to do this,
since the semantics are that when an exit event gets there, the clutter main
loop quits. Unless you then end the lisp image, the window won't be destroyed."
  (|stage_get_default|))

(defun new-stage () (|stage_new|))

(defmacro with-stage (sym &body body)
  `(let ((,sym (new-stage)))
     (glib:signal-connect ,sym "delete-event"
                          (lambda (event)
                            (declare (ignore event))
                            (hide-actor ,sym t)
                            (quit)
                            nil))
     (unwind-protect
          (progn ,@body)
       (destroy-actor ,sym))))
