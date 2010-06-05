(in-package :clutter)

(defbinding |color_copy| (color) color (color color))
(defbinding |color_free| (color) nil (color color))

(defbinding |color_equal| (col1 col2) boolean (col1 color) (col2 color))
(defbinding |color_hash| (color) unsigned-int (color color))

(defbinding |color_from_string| (color string) boolean
  (color color) (string string))
(defbinding |color_to_string| (color) string (color color))

(defbinding |color_from_hls| (color h l s) nil
  (color color) (h single-float) (l single-float) (s single-float))
(defbinding |color_to_hls| (color) nil
  (color color)
  (h single-float :out) (l single-float :out) (s single-float :out))

(defbinding |color_from_pixel| (color pixel) nil
  (color color) (pixel unsigned-int))
(defbinding |color_to_pixel| (color) unsigned-int (color color))

(defbinding |color_add| (a b res) nil (a color) (b color) (res color))
(defbinding |color_subtract| (a b res) nil (a color) (b color) (res color))
(defbinding |color_lighten| (a res) nil (a color) (res color))
(defbinding |color_darken| (a res) nil (a color) (res color))
(defbinding |color_shade| (a factor res) nil
  (a color) (factor double-float) (res color))

(defun make-color (&optional (r 0) (g 0) (b 0) (a 255))
  (unless (and (<= 0 r 255) (<= 0 g 255) (<= 0 b 255) (<= 0 a 255)
               (integerp r) (integerp g) (integerp b) (integerp a))
    (error
     "Clutter colors should have components which are integers in [0,255]."))
  (make-instance 'color :red r :green g :blue b :alpha a))

(defun copy-color (color) (|color_copy| color))
(defun free-color (color) (|color_free| color))

(defun color= (color1 color2) (|color_equal| color1 color2))
(defun color-hash (color) (|color_hash| color))

(defun parse-color (string &optional (error-p t))
  "Use clutter_color_from_string() to parse STRING. This can deal with strings
of the form '#RRGGBBAA' or 'LightGrey'. If clutter_color_from_string fails, then
either throw an error or return a default color (black, opaque)."
  (let* ((col (make-color))
         (ret (|color_from_string| col string)))
    (when (and error-p (not ret))
      (free-color col)
      (error "Couldn't parse color string: '~A'." string))
    col))

(defun color-to-string (color) (|color_to_string| color))

(defmacro with-color ((name &key (r 0 is-r) (g 0 is-g) (b 0 is-b) (a 255 is-a)
                            (string nil))
                      &body body)
  "Execute BODY with NAME bound to a color. The color might be initialised: if
STRING is passed, then parse the string to a color. Otherwise, use R, G, B, A,
which have default values for an opaque black."
  (let ((str (gensym)))
    `(let* ((,str ,string)
            (,name (if ,str (parse-color ,str) (make-color ,r ,g ,b ,a))))
       (unwind-protect ,@body
         (free-color ,name)))))

(defun color-from-hls (hue luminance saturation)
  "Given H/L/S (where HUE is in [0,360], and LUMINANCE and SATURATION are in
[0,1]), return a color to represent them."
  (unless (and (<= 0 hue 360) (<= 0 luminance 1) (<= 0 saturation 1))
    (error "Invalid HLS to construct color (~A/~A/~A)."
           hue luminance saturation))
  (let ((c (make-color)))
    (|color_from_hls| c hue luminance saturation)
    c))

(defun color-to-hls (color) (|color_to_hls| color))

(defun color-from-pixel (pixel)
  (let ((c (make-color))) (|color_from_pixel| c pixel) c))

(defun color-to-pixel (color) (|color_to_pixel| color))

(defun color+ (color1 color2)
  (let ((ret (make-color))) (|color_add| color1 color2 ret) ret))

(defun color- (color1 color2)
  (let ((ret (make-color))) (|color_subtract| color1 color2 ret) ret))

(defun color-lighter (color)
  (let ((ret (make-color))) (|color_lighten| color ret) ret))

(defun color-darker (color)
  (let ((ret (make-color))) (|color_darken| color ret) ret))

(defun color-shaded (color factor)
  (let ((ret (make-color))) (|color_shade| color factor ret) ret))
