(in-package :clutter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The two ignored functions aren't actually gobject type functions, but take
  ;; a handle to some object and get a subtype.
  (init-types-in-library clutter "libclutter-glx-1.0"
                         :ignore ("cogl_shader_get_type"
                                  "cogl_vertex_buffer_indices_get_type"))

  ;; FIXME: This is an ugly hack. It seems that types only really get registered
  ;; when they're used (via defbinding etc.), but GInitiallyUnowned never is
  ;; registered that way. However, it's the supertype of ClutterPath, so needs
  ;; to have been registered before we define that type.
  (register-type 'glib::initially-unowned "GInitiallyUnowned")

  (define-types-by-introspection
      "Clutter"
      ("ClutterColor"
       (red :allocation :alien
            :accessor color-red
            :initarg :red
            :type (unsigned-byte 8))
       (green :allocation :alien
              :accessor color-green
              :initarg :green
              :type (unsigned-byte 8))
       (blue :allocation :alien
             :accessor color-blue
             :initarg :blue
             :type (unsigned-byte 8))
       (alpha :allocation :alien
              :accessor color-alpha
              :initarg :alpha
              :type (unsigned-byte 8)))))
