(in-package "ALIEN")
(export '(load-shared-library))
(in-package "SYSTEM")
(import 'alien:load-shared-library)

(defun load-shared-library (file &key init prototype initargs)
  (format t ";;; Loading shared library ~A~%" file)
  (load-object-file file)
  (when init
    (apply
     #'alien:alien-funcall
     (alien::%heap-alien
      (alien::make-heap-alien-info
       :type (alien::parse-alien-type (or prototype `(function c-call:void)))
       :sap-form (system:foreign-symbol-address init)))
     initargs)))

