(in-package :clutter-gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library clutter-gtk "libclutter-gtk-0.10")
  (use-prefix "gtkclutter")
  (define-types-by-introspection "GtkClutter")
  
  ;; This is a bit of a hack. The ClutterGtkInitError type is defined in
  ;; gtk-clutter-util.h, but is bolted on to ClutterInitError. I think this is
  ;; right, so let's go with this for now...
  (define-enum-type init-error
      (:success 1) (:error-unknown 0) (:error-threads -1) (:error-backend -2)
      (:error-internal -3) (:error-gtk -5)))
