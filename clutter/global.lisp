(in-package :clutter)

(declaim (inline |init|))
(defbinding |init| (argc argv) init-error (argc pointer) (argv pointer))

(defbinding |main| () nil)
(defbinding |main_quit| () nil)

(defun init (&optional (program-name "clutter-user"))
  ;; clutter_init expects int *argc and char ***argv (!), so we need to allocate
  ;; the following memory:
  ;;  (1) An integer to be argc & a pointer to it
  ;;  (2) A C string to be argv[0] (PROGRAM-NAME)
  ;;  (3) A single pointer to (2), to which we also need a pointer.

  ;; TODO: Work out how to use the gffi stuff to do this, as this is
  ;; sbcl-specific.
  (sb-alien:with-alien ((name sb-alien:c-string program-name))
    (sb-alien:with-alien ((argv (array (* sb-alien:c-string) 1))
                          (argc sb-alien:int 1))
      (sb-alien:with-alien ((pargc (* sb-alien:int) (sb-alien:addr argc)))
        (setf (sb-alien:deref argv 0) (sb-alien:addr name))
        (|init| (sb-alien:alien-sap pargc)
                (sb-alien:alien-sap argv))))))

(defun main () (|main|))
(defun quit () (|main_quit|))
