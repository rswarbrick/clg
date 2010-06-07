(in-package :glib-json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (init-types-in-library glib-json "libjson-glib-1.0")
  (use-prefix "json")
  (define-types-by-introspection "Json"))
