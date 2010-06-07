(in-package :clutter-tests)

(defun stage-basics ()
  (clutter::init)
  (clutter::with-color (stage-color :string "Blue")
    (clutter::with-stage stage
      (setf (clutter::actor-width stage) 200
            (clutter::actor-height stage) 200
            (clutter::stage-color stage) stage-color)
      (clutter::show-actor stage)
      (clutter::main))))
