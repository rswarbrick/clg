


(in-package "GDK")


;;;; From  gdkcursor.h

(define-enum-by-query "gdk_cursor_type_get_type")


;;;; From  gdkdnd.h

(define-flags-by-query "gdk_drag_action_get_type")
(define-enum-by-query "gdk_drag_protocol_get_type")


;;;; From  gdkevents.h

(define-enum-by-query "gdk_filter_return_get_type")
(define-enum-by-query "gdk_event_type_get_type")
(define-flags-by-query "gdk_event_mask_get_type")
(define-enum-by-query "gdk_visibility_state_get_type")
(define-enum-by-query "gdk_scroll_direction_get_type")
(define-enum-by-query "gdk_notify_type_get_type")
(define-enum-by-query "gdk_crossing_mode_get_type")
(define-enum-by-query "gdk_property_state_get_type")
(define-flags-by-query "gdk_window_state_get_type")
(define-enum-by-query "gdk_setting_action_get_type")


;;;; From  gdkfont.h

(define-enum-by-query "gdk_font_type_get_type")


;;;; From  gdkgc.h

(define-enum-by-query "gdk_cap_style_get_type")
(define-enum-by-query "gdk_fill_get_type")
(define-enum-by-query "gdk_function_get_type" 'gc-function)
(define-enum-by-query "gdk_join_style_get_type")
(define-enum-by-query "gdk_line_style_get_type")
(define-enum-by-query "gdk_subwindow_mode_get_type")
(define-flags-by-query "gdk_gc_values_mask_get_type")


;;;; From  gdkimage.h

(define-enum-by-query "gdk_image_type_get_type")


;;;; From  gdkinput.h

(define-enum-by-query "gdk_extension_mode_get_type")
(define-enum-by-query "gdk_input_source_get_type")
(define-enum-by-query "gdk_input_mode_get_type")
(define-enum-by-query "gdk_axis_use_get_type")


;;;; From  gdkproperty.h

(define-enum-by-query "gdk_prop_mode_get_type")


;;;; From  gdkregion.h

(define-enum-by-query "gdk_fill_rule_get_type")
(define-enum-by-query "gdk_overlap_type_get_type")


;;;; From  gdkrgb.h

(define-enum-by-query "gdk_rgb_dither_get_type")


;;;; From  gdkselection.h

(define-enum-by-query "gdk_selection_get_type")
(define-enum-by-query "gdk_target_get_type")
(define-enum-by-query "gdk_selection_type_get_type")


;;;; From  gdktypes.h

(define-enum-by-query "gdk_byte_order_get_type")
(define-flags-by-query "gdk_modifier_type_get_type")
(define-flags-by-query "gdk_input_condition_get_type")
(define-enum-by-query "gdk_status_get_type")
(define-enum-by-query "gdk_grab_status_get_type")


;;;; From  gdkvisual.h

(define-enum-by-query "gdk_visual_type_get_type")


;;;; From  gdkwindow.h

(define-enum-by-query "gdk_window_class_get_type")
(define-enum-by-query "gdk_window_type_get_type")
(define-flags-by-query "gdk_window_attributes_type_get_type")
(define-flags-by-query "gdk_window_hints_get_type")
(define-enum-by-query "gdk_window_type_hint_get_type")
(define-flags-by-query "gdk_wm_decoration_get_type")
(define-flags-by-query "gdk_wm_function_get_type")
(define-enum-by-query "gdk_gravity_get_type")
(define-enum-by-query "gdk_window_edge_get_type")



