(in-package :clutter)

(defbinding |actor_show| (actor) nil (actor actor))
(defbinding |actor_destroy| (actor) nil (actor actor))
(defbinding |actor_hide| (actor) nil (actor actor))
(defbinding |actor_hide_all| (actor) nil (actor actor))

(defun show-actor (actor) (|actor_show| actor))
(defun destroy-actor (actor) (|actor_destroy| actor))
(defun hide-actor (actor &optional recursive?)
  (if recursive? (|actor_hide_all| actor) (|actor_hide| actor)))
