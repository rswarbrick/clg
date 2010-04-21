(in-package :goocanvas-demo)

(defclass demo ()
  ((name :initarg :name :reader name)
   (setup :initarg :setup :reader setup)))

(defvar *demo-list* (make-hash-table :test #'equal))

(defun register-demo (demo)
  (setf (gethash (name demo) *demo-list*) demo))

(defun demo-library ()
  (gtk:clg-init)
  (gtk:within-main-loop
    (let ((notebook (make-instance 'gtk:notebook)))
      (maphash (lambda (name demo)
                 (gtk:notebook-append notebook (funcall (setup demo))
                                      (make-instance 'gtk:label :label name)))
               *demo-list*)
      (make-instance
       'gtk:window
       :title "Goocanvas Demo Library"
       :default-height 600 :default-width 640
       :visible t :show-children t
       :signal (list 'gdk::delete-event
                     (lambda (e) (declare (ignore e)) nil))
       :child notebook))))
