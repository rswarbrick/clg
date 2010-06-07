(in-package :glib-json)

(defbinding |parser_new| () parser)
(defbinding |parser_load_from_file| (parser filename) boolean
  (parser parser)
  (filename string)
  (nil gerror-signal :out))

(defmacro with-parser (sym &body body)
  `(let ((,sym (|parser_new|)))
     (unwind-protect (progn ,@body) (invalidate-instance ,sym t))))

(defun parse-file (parser filename) (|parser_load_from_file| parser filename))

(defbinding (parser-root "json_parser_get_root") (parser) node (parser parser))

(defbinding (parser-current-line "json_parser_get_current_line")
    (parser) unsigned-int (parser parser))

(defbinding (parser-current-pos "json_parser_get_current_pos")
    (parser) unsigned-int (parser parser))

(defbinding (parser-has-assignment "json_parser_has_assignment")
    (parser) bool (parser parser))
