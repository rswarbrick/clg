(in-package :clutter-gtk)

(defbinding (get-stage "gtk_clutter_embed_get_stage") (embed) clutter::actor
            (embed embed))

(declaim (inline %init))
(defbinding (%init "gtk_clutter_init") (argc argv) init-error
            (argc pointer) (argv pointer))

(defvar *initialised* nil
  "gtk_clutter_init() is not idempotent (unlike clutter_init()), and so we
  shouldn't call it twice. However, we want to automatically call it if needed
  when trying to start up a main loop or something, so use a flag to check
  whether we've done it already.")

(defun init (&optional (program-name "clutter-user"))
  ;; gtk_clutter_init works the same as clutter_init, so see the binding in
  ;; clutter/global.lisp. TODO: Unify with gtk_init and/or clutter_init.
  (unless *initialised*
    (sb-alien:with-alien ((name sb-alien:c-string program-name))
      (sb-alien:with-alien ((argv (array (* sb-alien:c-string) 1))
                            (argc sb-alien:int 1))
        (sb-alien:with-alien ((pargc (* sb-alien:int) (sb-alien:addr argc)))
          (setf (sb-alien:deref argv 0) (sb-alien:addr name))
          (let ((ret (%init (sb-alien:alien-sap pargc)
                            (sb-alien:alien-sap argv))))
            (unless (eq ret :success)
              (error "Couldn't initialise ClutterGtk. (Reason: ~A)" ret))))))
    (setf *initialised* t))
  :success)

(defbinding (fg-color "gtk_clutter_get_fg_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (bg-color "gtk_clutter_get_bg_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (text-color "gtk_clutter_get_text_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (text-aa-color "gtk_clutter_get_text_aa_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (base-color "gtk_clutter_get_base_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (light-color "gtk_clutter_get_light_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (dark-color "gtk_clutter_get_dark_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))
(defbinding (mid-color "gtk_clutter_get_mid_color") (widget state) nil
  (widget gtk:widget)
  (state gtk:state-type)
  (nil clutter::color :out))

(defbinding (make-texture-from-pixbuf "gtk_clutter_texture_new_from_pixbuf")
    (pixbuf) clutter::actor
    (pixbuf gdk:pixbuf))

(defbinding (make-texture-from-stock "gtk_clutter_texture_new_from_stock")
    (widget id size) clutter::actor
    (widget widget)
    (id string)
    (size icon-size))

(defbinding (make-texture-from-icon-name
             "gtk_clutter_texture_new_from_icon_name")
    (widget name size) clutter::actor
    (widget widget)
    (name string)
    (size icon-size))

(defbinding (set-texture-from-pixbuf "gtk_clutter_texture_set_from_pixbuf")
    (texture pixbuf) boolean
    (texture clutter::texture)
    (pixbuf gdk:pixbuf)
    (nil gerror-signal :out))

(defbinding (set-texture-from-stock "gtk_clutter_texture_set_from_stock")
    (texture widget id size) boolean
    (texture clutter::texture)
    (widget widget)
    (id string)
    (size icon-size)
    (nil gerror-signal :out))

(defbinding (set-texture-from-icon-name
             "gtk_clutter_texture_set_from_icon_name")
    (texture widget name size) boolean
    (texture clutter::texture)
    (widget widget)
    (name string)
    (size icon-size)
    (nil gerror-signal :out))
