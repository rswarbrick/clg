(in-package :goocanvas)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-prefix "goo")
  (init-types-in-library goocanvas "libgoocanvas" :prefix "goo_cairo_")
  (init-types-in-library goocanvas "libgoocanvas"
                         :prefix "goo_canvas_"
                         :ignore ("goo_canvas_widget_accessible_get_type"))

  (define-types-by-introspection "GooCairo")
  (define-types-by-introspection "GooCanvas"))
