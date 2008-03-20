;; Common Lisp bindings for GTK+ v2.x
;; Copyright 1999-2006 Espen S. Johnsen <espen@users.sf.net>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; $Id: gtk.lisp,v 1.91 2008-03-20 15:44:01 espen Exp $


(in-package "GTK")

;;; Gtk version

(defbinding check-version () (copy-of string)
  (required-major unsigned-int)
  (required-minor unsigned-int)
  (required-micro unsigned-int))

(defbinding query-version () nil
  (major unsigned-int :out)
  (minor unsigned-int :out)
  (micro unsigned-int :out))

(defun gtk-version ()
  (multiple-value-bind (major minor micro)
      (query-version)
    (if (zerop micro)
	(format nil "Gtk+ v~A.~A" major minor) 
      (format nil "Gtk+ v~A.~A.~A" major minor micro))))

(defun clg-version ()
  "clg 0.93")


;;;; Initalization and display handling

(defparameter *event-polling-interval* 0.01)

#?(or (featurep :clisp) (featurep :cmu) (and (sbcl>= 1 0 6) (sbcl< 1 0 15 6)))
(defun decompose-time (time)
  (multiple-value-bind (sec subsec) (truncate *event-polling-interval*)
    (values sec (truncate (* subsec 1e6)))))

(defbinding (gtk-init "gtk_parse_args") () boolean
  "Initializes the library without opening the display."
  (nil null)
  (nil null))

(defun clg-init (&optional display multi-threading-p)
  "Initializes the system and starts event handling."
  (unless (gdk:display-get-default)
    #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
    (progn
      #+sbcl(sb-int:set-floating-point-modes :traps nil) 
      #+cmu(ext:set-floating-point-modes :traps nil))

     (gdk:gdk-init)
     (unless (gtk-init)
       (error "Initialization of GTK+ failed."))

    (if (not multi-threading-p) 
	(%init-async-event-handling display)
      #+sb-thread(%init-multi-threaded-event-handling display)
      #-sb-thread(error "Multi threading not supported on this platform")))
  (gdk:ensure-display display t))

(defun clg-init-with-threading (&optional display)
  (clg-init display t))


#?(and (sbcl>= 1 0 6) (sbcl< 1 0 15 6))
;; A very minimal implementation of CLISP's socket-status
(defun socket-status (socket seconds microseconds)
  (sb-alien:with-alien ((read-fds (sb-alien:struct sb-unix:fd-set)))
    (let ((fd (sb-sys:fd-stream-fd (car socket))))
      (sb-unix:fd-zero read-fds)
      (sb-unix:fd-set fd read-fds)

      (let ((num-fds-changed
	     (sb-unix:unix-fast-select
	      (1+ fd) (sb-alien:addr read-fds) nil nil 
	      seconds microseconds)))
	(unless (or (not num-fds-changed) (zerop num-fds-changed))
	  (if (peek-char nil (car socket) nil)
	      :input
	    :eof))))))

(defun %init-async-event-handling (display)
  (let ((style 
	 #?(or (featurep :cmu) (sbcl< 1 0 6) (sbcl>= 1 0 15 6)) :fd-handler
	 #?-(or (featurep :cmu) (sbcl< 1 0 6) (sbcl>= 1 0 15 6)) nil))
    (when (and 
	   (find-package "SWANK")
	   (not (eq (symbol-value (find-symbol "*COMMUNICATION-STYLE*" "SWANK")) style)))
      (error "When running clg in Slime, the communication style ~S must be used in combination with asynchronous event handling on this platform. See the README file and <http://common-lisp.net/project/slime/doc/html/Communication-style.html> for more information." style)))

  #?(or (featurep :cmu) (sbcl< 1 0 6) (sbcl>= 1 0 15 6))
  (progn
    (signal-connect (gdk:display-manager) 'display-opened
     #'(lambda (display)
	 (let ((fd (gdk:display-connection-number display)))
	   (unless (< fd 0)
	     (let ((handler (add-fd-handler 
			     (gdk:display-connection-number display) 
			     :input #'main-iterate-all)))
	       (signal-connect display 'closed
		#'(lambda (is-error-p)
		    (declare (ignore is-error-p))
		    (remove-fd-handler handler))))))))
    (setq *periodic-polling-function* #'main-iterate-all)
    #?(or (featurep :cmu) (sbcl< 1 0 6))
    (multiple-value-setq (*max-event-to-sec* *max-event-to-usec*)
      (decompose-time *event-polling-interval*))
    #?(sbcl>= 1 0 15 6)
    (setq *periodic-polling-period* *event-polling-interval*))
  
  #+(and clisp readline)
  ;; Readline will call the event hook at most ten times per second
  (setf readline:event-hook #'main-iterate-all)
  
  #?(or (featurep :clisp) (and (sbcl>= 1 0 6) (sbcl< 1 0 15 6)))
  ;; When running in Slime we need to hook into the Swank server
  ;; to handle events asynchronously.
  (unless (and
	   (find-package "SWANK")
	   (let ((connection (symbol-value (find-symbol "*EMACS-CONNECTION*" "SWANK"))))
	     (when connection
	       (let ((read-from-emacs (symbol-function (find-symbol "READ-FROM-EMACS" "SWANK")))
		     (stream (funcall (find-symbol "CONNECTION.SOCKET-IO" "SWANK") connection)))
		 (multiple-value-bind (sec usec) 
		     (decompose-time *event-polling-interval*)
		   (setf 
		    (symbol-function (find-symbol "READ-FROM-EMACS" "SWANK"))
		    #'(lambda ()
			(loop
			 (case (socket-status (cons stream :input) sec usec)
			   ((:input :eof) (return (funcall read-from-emacs)))
			   (otherwise (main-iterate-all)))))))))))
    #-(and clisp readline)
    (warn "Asynchronous event handling not supported on this platform. An explicit main loop has to be started."))

  (gdk:display-open display))

#+sb-thread
(progn
  (defvar *main-thread* nil)

  ;; Hopefully, when threading support is added to the Win32 port of
  ;; SBCL in the future, this will work just out of the box.
  #+win32
  (let ((done (sb-thread:make-waitqueue))
	(functions ())
	(results ()))

    ;; In Win32 all GDK calls have to be made from the main loop
    ;; thread, so we add a timeout function which will poll for code and
    ;; execute it.

    (defun funcall-in-main (function)
      (if (or 
	   (not *main-thread*)
	   (eq sb-thread:*current-thread* *main-thread*))
	  (funcall function)
	(gdk:with-global-lock
	  (push function functions)
 	  (sb-thread:condition-wait done gdk:*global-lock*)
 	  (pop results))))

    ;; Will lock REPL on error, need to be fixed!
    (defun %funcall-in-main-poll ()
      (when functions
	(loop
	 for n from 0
	 while functions 
	 do (push (funcall (pop functions)) results)
	 finally (sb-thread:condition-notify done n)))
      t))

  (defmacro within-main-loop (&body body)
    #-win32 `(gdk:with-global-lock ,@body)
    #+win32 `(funcall-in-main #'(lambda () ,@body)))
  
  (defun %init-multi-threaded-event-handling (display)
    (when (and 
	   (find-package "SWANK")
	   (not (eq (symbol-value (find-symbol "*COMMUNICATION-STYLE*" "SWANK")) :spawn)))
      (error "When running clg in Slime, the communication style :spawn must be used in combination with multi threaded event handling. See the README file and <http://common-lisp.net/project/slime/doc/html/slime_45.html> for more information."))
    (gdk:threads-init)	 
    (let ((main-running (sb-thread:make-waitqueue)))
      (gdk:with-global-lock
       (setf *main-thread*
        (sb-thread:make-thread 
	 #'(lambda () 
	     (gdk:with-global-lock 
	       (gdk:display-open display)
	       #+win32(gdk:timeout-add-with-lock (/ *event-poll-interval* 1000)
		       #'%funcall-in-main-poll)
	       (sb-thread:condition-notify main-running)
	       (main)))
	 :name "gtk event loop"))
	(sb-thread:condition-wait main-running gdk:*global-lock*)))
  
    ;; We need to hook into the Swank server to protect calls to GDK properly.
    ;; This will *only* protect code entered directly in the REPL.
    (when (find-package "SWANK")
      (let ((repl-eval-hook (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK")))
	(if repl-eval-hook
	    (push #'(lambda (form) 
		      (within-main-loop (eval form)))
	     (symbol-value (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK")))
	  (warn "Your version of Slime does not have *SLIME-REPL-EVAL-HOOKS* so all calls to Gtk+ functions have to be explicit protected by wrapping them in a WITHIN-MAIN-LOOP form"))))))

#-sb-thread
(defmacro within-main-loop (&body body)
  `(progn ,@body))



;;; Generic functions 

(defgeneric add-to-radio-group (item1 item2))
(defgeneric activate-radio-widget (item))
(defgeneric (setf tool-item-tip-text) (tip-text tool-item))
(defgeneric (setf tool-item-tip-private) (tip-private tool-item))



;;; Misc

(defbinding grab-add () nil
  (widget widget))

(defbinding grab-get-current () widget)

(defbinding grab-remove () nil
  (widget widget))

(defbinding get-default-language () (copy-of pango:language))


;;; About dialog

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(progn
  (define-callback-marshal %about-dialog-activate-link-callback nil
    (about-dialog (link string)))

  (defbinding about-dialog-set-email-hook (function) nil
    (%about-dialog-activate-link-callback callback)
    ((register-callback-function function) unsigned-int)
    (user-data-destroy-callback callback))
  
  (defbinding about-dialog-set-url-hook (function) nil
    (%about-dialog-activate-link-callback callback)
    ((register-callback-function function) unsigned-int)
    (user-data-destroy-callback callback)))


;;; Acccel group

(defbinding %accel-group-connect () nil
  (accel-group accel-group)
  (key unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags)
  (gclosure gclosure))

(defun accel-group-connect (group accelerator function &optional flags)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (let ((gclosure (make-callback-closure function)))
      (%accel-group-connect group key modifiers flags gclosure)
      gclosure)))

(defbinding accel-group-connect-by-path (group path function) nil
  (group accel-group)
  (path string)
  ((make-callback-closure function) gclosure :in/return))

(defbinding %accel-group-disconnect (group gclosure) boolean
  (group accel-group)
  (gclosure gclosure))

(defbinding %accel-group-disconnect-key () boolean
  (group accel-group)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-group-disconnect (group accelerator)
  (etypecase accelerator
    (gclosure (%accel-group-disconnect group accelerator))
    (string 
     (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
       (%accel-group-disconnect-key group key modifiers)))))

(defbinding %accel-group-query () (copy-of (vector (inlined accel-group-entry) n))
  (accel-group accel-group)
  (key unsigned-int)
  (modifiers gdk:modifier-type)
  (n int :out))

(defun accel-group-query (accel-group accelerator)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (%accel-group-query accel-group key modifiers)))

(defbinding %accel-group-activate () boolean
  (accel-group accel-group)
  (acceleratable gobject)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-group-activate (accel-group acceleratable accelerator)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (%accel-group-activate accel-group acceleratable key modifiers)))

(defbinding accel-group-lock () nil
  (accel-group accel-group))

(defbinding accel-group-unlock () nil
  (accel-group accel-group))

(defbinding accel-group-from-accel-closure () accel-group
  (closure gclosure))

(defbinding %accel-groups-activate () boolean
  (object gobject)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-groups-activate (object accelerator)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (%accel-groups-activate object key modifiers)))

(defbinding accel-groups-from-object () (gslist accel-group)
  (object gobject))

(defbinding accelerator-valid-p (key &optional modifiers) boolean
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding %accelerator-parse () nil
  (accelerator string)
  (key unsigned-int :out)
  (modifiers gdk:modifier-type :out))

(defgeneric parse-accelerator (accelerator))

(defmethod parse-accelerator ((accelerator string))
  (multiple-value-bind (key modifiers) (%accelerator-parse accelerator)
    (if (zerop key)
	(error "Invalid accelerator: ~A" accelerator)
      (values key modifiers))))

(defmethod parse-accelerator ((accelerator cons))
  (destructuring-bind (key modifiers) accelerator
    (values
     (etypecase key
       (integer key)
       (string
	(or 
	 (gdk:keyval-from-name key)
	 (error "Invalid key name: ~A" key)))
       (character (parse-accelerator key)))
     modifiers)))

(defmethod parse-accelerator ((key integer))
  key)

(defmethod parse-accelerator ((key character))
  (or
   (gdk:keyval-from-name (string key))
   (error "Invalid key name: ~A" key)))


(defbinding accelerator-name () string
  (key unsigned-int)
  (modifiers gdk:modifier-type))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding accelerator-get-label () string
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding %accelerator-set-default-mod-mask () nil
  (default-modifiers gdk:modifier-type))

(defun (setf accelerator-default-modifier-mask) (default-modifiers)
  (%accelerator-set-default-mod-mask default-modifiers))

(defbinding (accelerator-default-modifier-mask "gtk_accelerator_get_default_mod_mask") () gdk:modifier-type)


;;; Acccel label

(defbinding accel-label-get-accel-width () unsigned-int
  (accel-label accel-label))

(defbinding accel-label-refetch () boolean
  (accel-label accel-label))



;;; Accel map

(defbinding %accel-map-add-entry () nil
  (path string)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-map-add-entry (path accelerator)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (%accel-map-add-entry path key modifiers)))

(defbinding %accel-map-lookup-entry () boolean
  (path string)
  ((make-instance 'accel-key) accel-key :in/return))

(defun accel-map-lookup-entry (path)
  (multiple-value-bind (found-p accel-key) (%accel-map-lookup-entry path)
    (when found-p
      (values 
       (slot-value accel-key 'key)
       (slot-value accel-key 'modifiers)
       (slot-value accel-key 'flags)))))

(defbinding %accel-map-change-entry () boolean
  (path string)
  (key unsigned-int)
  (modifiers gdk:modifier-type)
  (replace boolean))

(defun accel-map-change-entry (path accelerator &optional replace)
  (multiple-value-bind (key modifiers) (parse-accelerator accelerator)
    (%accel-map-change-entry path key modifiers replace)))

(defbinding accel-map-load () nil
  (filename pathname))

(defbinding accel-map-save () nil
  (filename pathname))

(define-callback-marshal %accel-map-foreach-callback nil
  ((accel-path string) (key unsigned-int) 
   (modifiers gdk:modifier-type) (changed boolean)) :callback-id :first)

(defbinding %accel-map-foreach (callback-id) nil
  (callback-id unsigned-int)
  (%accel-map-foreach-callback callback))

(defbinding %accel-map-foreach-unfiltered (callback-id) nil
  (callback-id unsigned-int)
  (%accel-map-foreach-callback callback))

(defun accel-map-foreach (function &optional (filter-p t))
  (with-callback-function (id function)
    (if filter-p			  
	(%accel-map-foreach id)
      (%accel-map-foreach-unfiltered id))))

(defbinding accel-map-add-filter () nil
  (filter string))

(defbinding accel-map-get () accel-map)

(defbinding accel-map-lock-path () nil
  (path string))

(defbinding accel-map-unlock-path () nil
  (path string))



;;; Accessibility

(defbinding accessible-connect-widget-destroyed () nil
  (accessible accessible))


;;; Adjustment

(defmethod initialize-instance ((adjustment adjustment) &key value)
  (prog1
      (call-next-method)
    ;; we need to make sure that the value is set last, otherwise it
    ;; may be outside current limits and ignored
    (when value
      (setf (slot-value adjustment 'value) value))))


(defbinding adjustment-changed () nil
  (adjustment adjustment))

(defbinding adjustment-value-changed () nil
  (adjustment adjustment))

(defbinding adjustment-clamp-page () nil
  (adjustment adjustment)
  (lower single-float)
  (upper single-float))


;;; Alignment

(defbinding alignment-set () nil
  (alognment alignment)
  (x-align single-float)
  (y-align single-float)
  (x-scale single-float)
  (y-scale single-float))

(defbinding alignment-get-padding () nil
  (alognment alignment)
  (top unsigned-int :out)
  (bottom unsigned-int :out)
  (left unsigned-int :out)
  (right unsigned-int :out))

(defbinding alignment-set-padding () nil
  (alognment alignment)
  (top unsigned-int)
  (bottom unsigned-int)
  (left unsigned-int)
  (right unsigned-int))


;;; Assistant

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
(progn
  (defbinding assistant-get-nth-page () widget
    (assistant assistant)
    (page-num int))
  
  (defbinding %assistant-insert-page () int
    (assistant assistant)
    (page widget)
    (pos int))

  (defun assistant-insert-page (assistant page position &rest child-args)    
    (let ((pos (case position
		 (:first 0)
		 (:last -1)
		 (t position))))
      (prog1
	  (%assistant-insert-page assistant page pos)
	(init-child-slots assistant page child-args))))
  
  (defun assistant-append-page (assistant page &rest child-args)
    (apply #'assistant-insert-page assistant page :last child-args))
  
  (defun assistant-prepend-page (assistant page &rest child-args)
    (apply #'assistant-insert-page assistant page :first child-args))

  (define-callback-marshal %assistant-page-func-callback int
    ((current-page int)))

  (defbinding assistant-set-forward-page-func (assistant function) nil
    (assistant assistant)
    (%assistant-page-func-callback callback)
    ((register-callback-function function) pointer-data)
    (user-data-destroy-callback callback))

  (defbinding assistant-add-action-widget () nil
    (assistant assistant)
    (child widget))

  (defbinding assistant-remove-action-widget () nil
    (assistant assistant)
    (child widget))

  (defbinding assistant-update-buttons-state () nil
    (assistant assistant)))



;;; Aspect frame


;;; Bin

(defun (setf bin-child) (child bin)
  (when-bind (current-child (bin-child bin))
    (container-remove bin current-child))
  (container-add bin child)
  child)

(defmethod compute-signal-function ((bin bin) signal function object args)
  (declare (ignore signal))
  (if (eq object :child)
      #'(lambda (&rest emission-args) 
	  (apply function (bin-child bin) (nconc (rest emission-args) args)))
    (call-next-method)))


;;; Box

(defbinding box-pack-start () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(defbinding box-pack-end () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int))

(defun box-pack (box child &key end (expand t) (fill t) (padding 0))
  (if end
      (box-pack-end box child expand fill padding)
    (box-pack-start box child expand fill padding)))

(defbinding box-reorder-child () nil
  (box box)
  (child widget)
  (position int))

(defbinding box-query-child-packing () nil
  (box box)
  (child widget)
  (expand boolean :out)
  (fill boolean :out)
  (padding unsigned-int :out)
  (pack-type pack-type :out))

(defbinding box-set-child-packing () nil
  (box box)
  (child widget)
  (expand boolean)
  (fill boolean)
  (padding unsigned-int)
  (pack-type pack-type))



;;; Button

(defmethod initialize-instance ((button button) &rest initargs &key stock)
  (if stock
      (apply #'call-next-method button 
       :label stock :use-stock t :use-underline t initargs)
    (call-next-method)))


(defbinding button-pressed () nil
  (button button))

(defbinding button-released () nil
  (button button))

(defbinding button-clicked () nil
  (button button))

(defbinding button-enter () nil
  (button button))

(defbinding button-leave () nil
  (button button))



;;; Calendar

(defbinding calendar-select-month () int
  (calendar calendar)
  (month unsigned-int)
  (year unsigned-int))

(defbinding calendar-select-day () nil
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-mark-day () int
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-unmark-day () int
  (calendar calendar)
  (day unsigned-int))

(defbinding calendar-clear-marks () nil
  (calendar calendar))

(defbinding calendar-get-date () nil
  (calendar calendar)
  (year unsigned-int :out)
  (month unsigned-int :out)
  (day unsigned-int :out))

(defbinding calendar-freeze () nil
  (calendar calendar))

(defbinding calendar-thaw () nil
  (calendar calendar))


;;; Check menu item

(defbinding check-menu-item-toggled () nil
  (check-menu-item check-menu-item))


;;; Color selection

(defbinding color-selection-is-adjusting-p () boolean
  (colorsel color-selection))

(defbinding (color-selection-previous-color
	     "gtk_color_selection_get_previous_color") () nil
  (colorsel color-selection)
  ((make-instance 'gdk:color) gdk:color :in/return))


;;; Color selection dialog -- no functions



;;;; Combo Box

(defmethod initialize-instance ((combo-box combo-box) &rest initargs 
				&key model content active)
  (remf initargs :active)
  (if model
      (apply #'call-next-method combo-box initargs)
    (progn
      (apply #'call-next-method combo-box 
       :model (make-instance 'list-store :column-types '(string))
       initargs)
      (unless (typep combo-box 'combo-box-entry)
	(let ((cell (make-instance 'cell-renderer-text)))
	  (cell-layout-pack combo-box cell :expand t)
	  (cell-layout-add-attribute combo-box cell :text 0)))))
  (when content
    (mapc #'(lambda (text)
	      (combo-box-append-text combo-box text))
	  content))
  (when active
    (setf (combo-box-active combo-box) active)))


;; (defmethod shared-initialize :after ((combo-box combo-box) names &key active)
;;   (when active
;;     (signal-emit combo-box 'changed)))

(defbinding combo-box-append-text () nil
  (combo-box combo-box)
  (text string))

(defbinding combo-box-insert-text () nil
  (combo-box combo-box)
  (position int)
  (text string))

(defbinding combo-box-prepend-text () nil
  (combo-box combo-box)
  (text string))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding combo-box-get-active-text () string
  (combo-box combo-box))

(defbinding combo-box-popup () nil
  (combo-box combo-box))

(defbinding combo-box-popdown () nil
  (combo-box combo-box))



;;;; Combo Box Entry

(defmethod initialize-instance ((combo-box-entry combo-box-entry) &key model)
  (call-next-method)
  (unless model
    (setf (combo-box-entry-text-column combo-box-entry) 0)))


;;;; Dialog

(defmethod shared-initialize ((dialog dialog) names &rest initargs 
			      &key button buttons)
  (declare (ignore names button buttons))
  (prog1
      (call-next-method)
    (initial-apply-add dialog #'dialog-add-button initargs :button :buttons)))
  

(defun dialog-response-id (dialog response &optional create-p error-p)
  "Returns a numeric response id"
  (if (typep response 'response-type)
      (response-type-to-int response)
    (let ((responses (user-data dialog 'responses)))
      (cond
       ((and responses (position response responses :test #'equal)))
       (create-p
       (cond
	 (responses
	  (vector-push-extend response responses)
	  (1- (length responses)))
	 (t
	  (setf 
	   (user-data dialog 'responses)
	   (make-array 1 :adjustable t :fill-pointer t 
		       :initial-element response))
	  0)))
      (error-p
       (error "Invalid response: ~A" response))))))

(defun dialog-find-response (dialog id)
  "Finds a symbolic response given a numeric id"
  (cond 
   ((not (numberp id)) id)
   ((< id 0) (int-to-response-type id))
   ((aref (user-data dialog 'responses) id))))


(defmethod compute-signal-id ((dialog dialog) signal)
  (if (dialog-response-id dialog signal)
      (ensure-signal-id 'response dialog)
    (call-next-method)))

(defmethod compute-signal-function ((dialog dialog) signal function object args)
  (declare (ignore function object args))
  (let ((callback (call-next-method))
	(id (dialog-response-id dialog signal)))
    (cond
     (id
      #'(lambda (dialog response)
	  (when (= response id)
	    (funcall callback dialog))))
     ((string-equal signal "response")
      #'(lambda (dialog response)	  
	  (funcall callback dialog (dialog-find-response dialog response))))
     (callback))))

(defbinding dialog-run () nil
  (dialog dialog))

(defbinding dialog-response (dialog response) nil
  (dialog dialog)
  ((dialog-response-id dialog response nil t) int))


(defbinding %dialog-add-button () button
  (dialog dialog)
  (text string)
  (response-id int))

(defun dialog-add-button (dialog label &optional (response label)
			  &key default object after)
  "Adds a button to the dialog."
  (let* ((signal (if (functionp response)
		     label
		   response))
	 (id (dialog-response-id dialog signal t))
	 (button (%dialog-add-button dialog label id)))
    (when (functionp response)
       (signal-connect dialog signal response :object object :after after))
    (when default
      (%dialog-set-default-response dialog id))
    button))


(defbinding %dialog-add-action-widget () nil
  (dialog dialog)
  (action-widget widget)
  (response-id int))

(defun dialog-add-action-widget (dialog widget &optional (response widget)
				 &key default object after)
  (let* ((signal (if (functionp response)
		     widget
		   response))
	 (id (dialog-response-id dialog signal t)))
    (unless (widget-hidden-p widget)
      (widget-show widget))
    (%dialog-add-action-widget dialog widget id)
    (when (functionp response)
       (signal-connect dialog signal response :object object :after after))
    (when default
      (setf (widget-can-default-p widget) t)
      (%dialog-set-default-response dialog id))
    widget))


(defbinding %dialog-set-default-response () nil
  (dialog dialog)
  (response-id int))

(defun dialog-set-default-response (dialog response)
  (%dialog-set-default-response
   dialog (dialog-response-id dialog response nil t)))

(defbinding dialog-set-response-sensitive (dialog response sensitive) nil
  (dialog dialog)
  ((dialog-response-id dialog response nil t) int)
  (sensitive boolean))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding alternative-dialog-button-order-p (&optional screen) boolean
  (screen (or null gdk:screen)))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding (dialog-set-alternative-button-order 
	     "gtk_dialog_set_alternative_button_order_from_array")
    (dialog new-order) nil
  (dialog dialog)
  ((length new-order) int)
  ((map 'vector #'(lambda (response)
		    (dialog-response-id dialog response nil t))
	new-order) (vector int)))


#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(progn
  (defbinding %dialog-get-response-for-widget () int
    (dialog dialog)
    (widget widget))

  (defun dialog-get-response-for-widget (dialog widget)
    (dialog-find-response dialog (dialog-get-response-for-widget dialog widget))))


(defmethod container-add ((dialog dialog) (child widget) &rest args)
  (apply #'container-add (dialog-vbox dialog) child args))


(defmethod container-remove ((dialog dialog) (child widget))
  (container-remove (dialog-vbox dialog) child))

(defmethod container-children ((dialog dialog))
  (container-children (dialog-vbox dialog)))

(defmethod (setf container-children) (children (dialog dialog))
  (setf (container-children (dialog-vbox dialog)) children))


;;; Drawing Area

(defun drawing-area-scroll (drawing-area dx dy)
  (gdk:window-scroll (widget-window drawing-area) dx dy))


;;; Entry

(defbinding entry-get-layout-offsets () nil
  (entry entry)
  (x int :out)
  (y int :out))

(defbinding entry-layout-index-to-text-index () int
  (entry entry)
  (layout-index int))

(defbinding entry-text-index-to-layout-index () int
  (entry entry)
  (text-index int))


;;; Entry Completion

(define-callback-marshal %entry-completion-match-callback boolean 
  (entry-completion string tree-iter))

(defbinding entry-completion-set-match-func (completion function) nil
  (completion entry-completion)
  (%entry-completion-match-callback callback)
  ((register-callback-function function) unsigned-int)
  (user-data-destroy-callback callback))

(defbinding entry-completion-complete () nil
  (completion entry-completion))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.12.0")
(defbinding entry-completion-get-completion-prefix () string
  (completion entry-completion))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding entry-completion-insert-prefix () nil
  (completion entry-completion))

(defbinding entry-completion-insert-action-text () nil
  (completion entry-completion)
  (index int)
  (text string))

(defbinding entry-completion-insert-action-markup () nil
  (completion entry-completion)
  (index int)
  (markup string))

(defbinding entry-completion-delete-action () nil
  (completion entry-completion)
  (index int))


;;; File Chooser

(defmethod initialize-instance ((file-chooser file-chooser) &rest initargs 
				&key filter filters shortcut-folder 
				shortcut-folders shortcut-folder-uti
				shortcut-folder-uris)
  (declare (ignore filter filters shortcut-folder shortcut-folders 
		   shortcut-folder-uti shortcut-folder-uris))
  (prog1
      (call-next-method)
    (initial-add file-chooser #'file-chooser-add-filter
     initargs :filer :filters)
    (initial-add file-chooser #'file-chooser-add-shortcut-folder
     initargs :shortcut-folder :shortcut-folders)
    (initial-add file-chooser #'file-chooser-add-shortcut-folder-uri
     initargs :shortcut-folder-uri :shortcut-folders-uris)))


(defbinding file-chooser-select-filename () boolean
  (file-chooser file-chooser)
  (filename string))

(defbinding file-chooser-unselect-filename () nil
  (file-chooser file-chooser)
  (filename string))

(defbinding file-chooser-select-all () boolean
  (file-chooser file-chooser))

(defbinding file-chooser-unselect-all () boolean
  (file-chooser file-chooser))
  
(defbinding file-chooser-get-filenames () (gslist string)
  (file-chooser file-chooser))

(defbinding file-chooser-select-uri () boolean
  (file-chooser file-chooser)
  (uri string))

(defbinding file-chooser-unselect-uri () nil
  (file-chooser file-chooser)
  (uri string))

(defbinding file-chooser-get-uris () (gslist string)
  (file-chooser file-chooser))

(defbinding file-chooser-add-filter () nil
  (file-chooser file-chooser)
  (filter file-filter))

(defbinding file-chooser-remove-filter () nil
  (file-chooser file-chooser)
  (filter file-filter))

(defbinding file-chooser-list-filters () (gslist file-filter)
  (file-chooser file-chooser))

(defbinding file-chooser-add-shortcut-folder () boolean
  (file-chooser file-chooser)
  (folder string)
  (nil null))

(defbinding file-chooser-remove-shortcut-folder () nil
  (file-chooser file-chooser)
  (folder string)
  (nil null))

(defbinding file-chooser-list-shortcut-folders () (gslist string)
  (file-chooser file-chooser))

(defbinding file-chooser-add-shortcut-folder-uri () boolean
  (file-chooser file-chooser)
  (uri string)
  (nil null))

(defbinding file-chooser-remove-shortcut-folder-uri () nil
  (file-chooser file-chooser)
  (uri string)
  (nil null))

(defbinding file-chooser-list-shortcut-folder-uris () (gslist string)
  (file-chooser file-chooser))


;;; File Filter

(defmethod initialize-instance ((file-filter file-filter) &rest initargs 
				&key mime-type mime-types pattern patterns
				pixbuf-formats)
  (declare (ignore mime-type mime-types pattern patterns))
  (prog1
      (call-next-method)
    (when pixbuf-formats
      #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
      (warn "Initarg :PIXBUF-FORMATS not supportet in this version of Gtk")
      #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
      (file-filter-add-pixbuf-formats file-filter))
    (initial-add file-filter #'file-filter-add-mime-type
     initargs :mime-type :mime-types)
    (initial-add file-filter #'file-filter-add-pattern
     initargs :pattern :patterns)))


(defbinding file-filter-add-mime-type () nil
  (filter file-filter)
  (mime-type string))

(defbinding file-filter-add-pattern () nil
  (filter file-filter)
  (pattern string))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding file-filter-add-pixbuf-formats () nil
  (filter file-filter))

(define-callback-marshal %file-filter-callback boolean (file-filter-info))

(defbinding file-filter-add-custom (filter needed function) nil
  (filter file-filter)
  (needed file-filter-flags)
  (%file-filter-callback callback)
  ((register-callback-function function) unsigned-int)
  (user-data-destroy-callback callback))

(defbinding file-filter-get-needed () file-filter-flags
  (filter file-filter))

(defbinding file-filter-filter () boolean
  (filter file-filter)
  (filter-info file-filter-info))



;;; Image

(defbinding image-set-from-file () nil
  (image image)
  (filename pathname))

(defmethod (setf image-pixmap) ((data vector) (image image))
  (multiple-value-bind (pixmap mask) (gdk:pixmap-create data)
    (setf (image-pixmap image) pixmap)
    (setf (image-mask image) mask)))

(defmethod initialize-instance ((image image) &rest initargs &key pixmap file)
  (cond
   ((typep pixmap 'vector)
    (multiple-value-bind (pixmap mask) (gdk:pixmap-create pixmap)
      (apply #'call-next-method image :pixmap pixmap :mask mask initargs)))
   (file
    (prog1
	(call-next-method)
      (image-set-from-file image file)))
   ((call-next-method))))

(defun create-image-widget (source &optional mask)
  (etypecase source
    (gdk:pixbuf (make-instance 'image :pixbuf source))
    (string (make-instance 'image :stock source))
    (pathname (make-instance 'image :file source))
    ((or list vector) (make-instance 'image :pixmap source))
    (gdk:pixmap (make-instance 'image :pixmap source :mask mask))))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(defbinding image-clear () nil
  (image image))



;;; Image menu item

(defmethod initialize-instance ((item image-menu-item) &rest initargs &key image)
  (if (and image (not (typep image 'widget)))
      (apply #'call-next-method item :image (create-image-widget image) initargs) 
    (call-next-method)))


(defmethod (setf image-menu-item-image) ((widget widget) (item image-menu-item))
  (setf (slot-value item 'image) widget))

(defmethod (setf image-menu-item-image) (image (item image-menu-item))
  (setf (image-menu-item-image item) (create-image-widget image)))


;;; Label

(defmethod shared-initialize ((label label) names &key pattern)
  (declare (ignore names))
  (call-next-method)
  (when pattern
    (setf (label-pattern label) pattern)))

(defbinding label-get-layout-offsets () nil
  (label label)
  (x int :out)
  (y int :out))

(defbinding label-select-region () nil
  (label label)
  (start int)
  (end int))

(defbinding label-get-selection-bounds () boolean
  (label label)
  (start int :out)
  (end int :out))



;;; Radio button

(defbinding %radio-button-get-group () pointer
  (radio-button radio-button))

(defbinding %radio-button-set-group () nil
  (radio-button radio-button)
  (group pointer))

(defmethod add-to-radio-group ((button1 radio-button) (button2 radio-button))
  "Add BUTTON1 to the group which BUTTON2 belongs to."
  (%radio-button-set-group button1 (%radio-button-get-group button2)))

(defun %add-activate-callback (widget signal function object after)
  (if object
      (signal-connect widget signal
       #'(lambda (object)
	   (when (slot-value widget 'active)
	     (funcall function object (slot-value widget 'value))))
       :object object :after after)
    (signal-connect widget signal 
     #'(lambda ()
	 (when (slot-value widget 'active)
	   (funcall function (slot-value widget 'value))))
     :after after)))

(defmethod activate-radio-widget ((button radio-button))
  (signal-emit button 'clicked))

(defgeneric add-activate-callback (action function &key object after))

(defmethod add-activate-callback ((button radio-button) function &key object after)
  (%add-activate-callback button 'clicked function object after))

(defmethod initialize-instance ((button radio-button) &key group)
  (prog1
      (call-next-method)
    (when group
      (add-to-radio-group button group))))


;;; Item

(defbinding item-select () nil
  (item item))

(defbinding item-deselect () nil
  (item item))

(defbinding item-toggle () nil
  (item item))



;;; Menu item

(defmethod initialize-instance ((item menu-item) &key label)
  (prog1
      (call-next-method)
    (when label
      (setf (menu-item-label item) label))))


(defun (setf menu-item-label) (label menu-item)
  (make-instance 'accel-label
   :label label :xalign 0.0 :yalign 0.5 :accel-widget menu-item
   :use-underline (menu-item-use-underline-p menu-item)
   :visible t  :parent menu-item)
  label)

(defun menu-item-label (menu-item)
  (when (and (slot-boundp menu-item 'child) 
	     (typep (bin-child menu-item) 'label))
    (label-label (bin-child menu-item))))

(defbinding menu-item-remove-submenu () nil
  (menu-item menu-item))

(defbinding menu-item-set-accel-path () nil
  (menu-item menu-item)
  (accel-path string))

(defbinding menu-item-select () nil
  (menu-item menu-item))

(defbinding menu-item-deselect () nil
  (menu-item menu-item))

(defbinding menu-item-activate () nil
  (menu-item menu-item))

(defbinding menu-item-toggle-size-request () nil
  (menu-item menu-item)
  (requisition int :out))

(defbinding menu-item-toggle-size-allocate () nil
  (menu-item menu-item)
  (allocation int))


;;; Menu tool button

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding menu-tool-button-set-arrow-tooltip () nil
  (menu-tool-button menu-tool-button)
  (tooltips tooltips)
  (tip-text string)
  (tip-private string))


;;; Message dialog

(defmethod allocate-foreign ((dialog message-dialog) &key (message-type :info)
			     button buttons flags transient-parent)
  (let ((stock-buttons 
	 (cond
	  ((and (not buttons) (not button))
	   (case message-type
	     (:question :yes-no)
	     (t :ok)))
	  ((listp buttons) :none)
	  (t buttons))))
    (%message-dialog-new transient-parent flags message-type stock-buttons)))


(defmethod shared-initialize ((dialog message-dialog) names &rest initargs
			      &key message-type buttons button text 
			      #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
			      secondary-text)
  (declare (ignore names))
  (when text
    (message-dialog-set-markup dialog text))
  #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
  (when secondary-text
    (message-dialog-format-secondary-markup dialog secondary-text))
  (when (and (not buttons) (not button))
    (loop
     for (key value) on initargs by #'cddr
     when (and (eq key :signal) (eq (first value) :close))
     do (warn "Default button configuration changed from ~A to ~A" :close
	 (if (eq message-type :question) :yes-no :ok))
        (loop-finish)))
  (if (typep buttons 'buttons-type)
      (apply #'call-next-method dialog names (plist-remove :buttons initargs))
    (call-next-method)))


(defbinding %message-dialog-new () pointer
  (parent (or null window))
  (flags dialog-flags)
  (type message-type)
  (buttons buttons-type)
  (nil null))

(defbinding message-dialog-set-markup () nil
  (message-dialog message-dialog)
  (markup string))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding message-dialog-format-secondary-text () nil
  (message-dialog message-dialog)
  (text string))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding message-dialog-format-secondary-markup () nil
  (message-dialog message-dialog)
  (markup string))



;;; Radio menu item

(defbinding %radio-menu-item-get-group () pointer
  (radio-menu-item radio-menu-item))

(defbinding %radio-menu-item-set-group () nil
  (radio-menu-item radio-menu-item)
  (group pointer))

(defmethod activate-radio-widget ((item radio-menu-item))
  (menu-item-activate item))

(defmethod add-to-radio-group ((item1 radio-menu-item) (item2 radio-menu-item))
  "Add ITEM1 to the group which ITEM2 belongs to."
  (%radio-menu-item-set-group item1 (%radio-menu-item-get-group item2)))

(defmethod add-activate-callback ((item radio-menu-item) function &key object after)
  (%add-activate-callback item 'activate function object after))

(defmethod initialize-instance ((item radio-menu-item) &key group)
  (prog1
      (call-next-method)
    (when group
      (add-to-radio-group item group))))

  

;;; Radio tool button

(defbinding %radio-tool-button-get-group () pointer
  (radio-tool-button radio-tool-button))

(defbinding %radio-tool-button-set-group () nil
  (radio-tool-button radio-tool-button)
  (group pointer))

(defmethod activate-radio-widget ((button radio-tool-button))
  (signal-emit button 'clicked))

(defmethod add-to-radio-group ((button1 radio-tool-button) (button2 radio-tool-button))
  "Add BUTTON1 to the group which BUTTON2 belongs to."
  (%radio-tool-button-set-group button1 (%radio-tool-button-get-group button2)))
(defmethod add-activate-callback ((button radio-tool-button) function &key object after)
  (%add-activate-callback button 'clicked function object after))

(defmethod initialize-instance ((button radio-tool-button) &key group)
  (prog1
      (call-next-method)
    (when group
      (add-to-radio-group button group))))



;;; Toggle button

(defbinding toggle-button-toggled () nil
  (toggle-button toggle-button))


;;; Window

(defmethod initialize-instance ((window window) &rest initargs 
				&key display accel-group accel-groups)
  (declare (ignore accel-group accel-groups))
  (prog1
      (if display
	  (apply #'call-next-method
	   window :screen (gdk:display-get-default-screen (gdk:ensure-display display)) initargs)
	(call-next-method))
    (initial-add window #'window-add-accel-group 
     initargs :accel-group :accel-groups)))

#-debug-ref-counting
(defmethod print-object ((window window) stream)
  (if (and 
       (proxy-valid-p window) 
       (slot-boundp window 'title) 
       (not (zerop (length (window-title window)))))
      (print-unreadable-object (window stream :type t :identity nil)
        (format stream "~S at 0x~X" 
	 (window-title window) (pointer-address (foreign-location window))))
    (call-next-method)))

(defbinding window-set-wmclass () nil
  (window window)
  (wmclass-name string)
  (wmclass-class string))

(defbinding window-add-accel-group () nil
  (window window)
  (accel-group accel-group))

(defbinding window-remove-accel-group () nil
  (window window)
  (accel-group accel-group))

(defbinding window-activate-focus () int
  (window window))

(defbinding window-activate-default () int
  (window window))

(defbinding window-set-default-size (window width height) int
  (window window)
  ((or width -1) int)
  ((or height -1) int))

(defbinding %window-set-geometry-hints () nil
  (window window)
  (widget (or widget null))
  (geometry gdk:geometry)
  (geometry-mask gdk:window-hints))

(defun window-set-geometry-hints (window &key widget min-width min-height
                                  max-width max-height base-width base-height
				  width-inc height-inc gravity
				  aspect (min-aspect aspect) (max-aspect aspect))
  (let ((geometry (make-instance 'gdk:geometry 
		   :min-width (or min-width -1)
		   :min-height (or min-height -1)
		   :max-width (or max-width -1)
		   :max-height (or max-height -1)
		   :base-width (or base-width 0)
		   :base-height (or base-height 0)
		   :width-inc (or width-inc 0)
		   :height-inc (or height-inc 0)
		   :min-aspect (or min-aspect 0)
		   :max-aspect (or max-aspect 0)))
	(mask ()))
    (when (or min-width min-height)
      (push :min-size mask))
    (when (or max-width max-height)
      (push :max-size mask))
    (when (or base-width base-height)
      (push :base-size mask))
    (when (or width-inc height-inc)
      (push :resize-inc mask))
    (when (or min-aspect max-aspect)
      (push :aspect mask))
    (when gravity
      (push :win-gravity mask)
      (setf (gdk:geometry-gravity geometry) gravity))
    (%window-set-geometry-hints window widget geometry mask)))

(defbinding window-list-toplevels () (glist (copy-of window))
  "Returns a list of all existing toplevel windows.")

(defbinding window-add-mnemonic (window key target) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (target widget))

(defbinding window-remove-mnemonic (window key target) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (target widget))

(defbinding window-mnemonic-activate (window key modifier) nil
  (window window)
  ((gdk:keyval-from-name key) unsigned-int)
  (modifier gdk:modifier-type))

(defbinding window-activate-key () boolean
  (window window)
  (event gdk:key-event))

(defbinding window-propagate-key-event () boolean
  (window window)
  (event gdk:key-event))

#?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(defbinding window-present () nil
  (window window))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(progn
  (defbinding %window-present () nil
    (window window))

  (defbinding %window-present-with-time () nil
    (window window)
    (timespamp unsigned-int))

  (defun window-present (window &optional timestamp)
    (if timestamp
	(%window-present-with-time window timestamp)
      (%window-present window))))

(defbinding window-iconify () nil
  (window window))

(defbinding window-deiconify () nil
  (window window))

(defbinding window-stick () nil
  (window window))

(defbinding window-unstick () nil
  (window window))

(defbinding window-maximize () nil
  (window window))

(defbinding window-unmaximize () nil
  (window window))

(defbinding window-fullscreen () nil
  (window window))

(defbinding window-unfullscreen () nil
  (window window))

(defbinding window-set-keep-above () nil
  (window window)
  (setting boolean))

(defbinding window-set-keep-below () nil
  (window window)
  (setting boolean))

(defbinding window-begin-resize-drag () nil
  (window window)
  (edge gdk:window-edge)
  (button int)
  (root-x int) (root-y int)
  (timestamp unsigned-int))

(defbinding window-begin-move-drag () nil
  (window window)
  (edge gdk:window-edge)
  (button int)
  (root-x int) (root-y int)
  (timestamp unsigned-int))

(defbinding window-set-frame-dimensions () nil
  (window window)
  (left int) (top int) (rigth int) (bottom int))

(defbinding %window-get-default-size () nil
  (window window)
  (width int :out)
  (height int :out))

(defun window-get-default-size (window)
  (multiple-value-bind (width height) (%window-get-default-size window)
    (values (unless (= width -1) width) (unless (= height -1) height))))

(defbinding window-get-frame-dimensions () nil
  (window window)
  (left int :out) (top int :out) (rigth int :out) (bottom int :out))

(defbinding %window-get-icon-list () (glist (copy-of gdk:pixbuf))
  (window window))

(defbinding window-get-position () nil
  (window window)
  (root-x int :out)
  (root-y int :out))

(defbinding window-get-size () nil
  (window window)
  (width int :out)
  (height int :out))

(defbinding window-move () nil
  (window window)
  (x int)
  (y int))

(defbinding window-parse-geometry () boolean
  (window window)
  (geometry string))

(defbinding window-reshow-with-initial-size () nil
  (window window))

(defbinding window-resize () nil
  (window window)
  (width int)
  (heigth int))

(defbinding (window-default-icon-list "gtk_window_get_default_icon_list")
    () (glist gdk:pixbuf))

(defun window-default-icon ()
  (first (window-default-icon-list)))

(defbinding %window-set-default-icon-list () nil
  (icons (glist gdk:pixbuf)))

(defun (setf window-default-icon-list) (icons)
  (%window-set-default-icon-list icons)
  icons)

(defbinding %window-set-default-icon () nil
  (icons (glist gdk:pixbuf)))

(defgeneric (setf window-default-icon) (icon))

(defmethod (setf window-default-icon) ((icon gdk:pixbuf))
  (%window-set-default-icon icon)
  icon)

(defgeneric (setf window-group) (group window))

(defmethod (setf window-group) ((group window-group) (window window))
  (window-group-add-window group window)
  group)

(defbinding %window-set-default-icon-from-file () boolean
  (filename pathname)
  (nil null))

(defmethod (setf window-default-icon) ((icon-file pathname))
  (%window-set-default-icon-from-file icon-file)
  icon-file)

(defbinding %window-set-icon-from-file () boolean
  (window window)
  (filename pathname)
  (nil null))

(defmethod (setf window-icon) ((icon-file pathname) (window window))
  (%window-set-icon-from-file window icon-file)
  icon-file)

(defbinding window-set-auto-startup-notification () nil
  (setting boolean))

(defbinding decorated-window-init () nil
  (window window))

(defbinding decorated-window-calculate-frame-size () nil
  (window window))

(defbinding decorated-window-set-title () nil
  (window window)
  (title string))

(defbinding decorated-window-move-resize-window () nil
  (window window)
  (x int)
  (y int)
  (width int)
  (heigth int))


;;; Window group

(defmethod initialize-instance ((window-group window-group) &rest initargs 
				&key window windows)
  (declare (ignore window windows))
  (prog1
      (call-next-method)
    (initial-add window-group #'window-group-add-window 
     initargs :window :windows)))


(defbinding window-group-add-window () nil
  (window-group window-group)
  (window window))

(defbinding window-group-remove-window () nil
  (window-group window-group)
  (window window))


;;; Scrolled window

(defun (setf scrolled-window-scrollbar-policy) (policy window)
  (setf (scrolled-window-hscrollbar-policy window) policy)
  (setf (scrolled-window-vscrollbar-policy window) policy))

(defbinding scrolled-window-add-with-viewport () nil
   (scrolled-window scrolled-window)
   (child widget))

(defmethod shared-initialize ((window scrolled-window) names &key policy)
  (declare (ignore names))
  (when policy 
    (setf (slot-value window 'hscrollbar-policy) policy)
    (setf (slot-value window 'vscrollbar-policy) policy))
  (call-next-method))


;;; Statusbar

(defbinding statusbar-get-context-id () unsigned-int
  (statusbar statusbar)
  (context-description string))

(defbinding statusbar-push () unsigned-int
  (statusbar statusbar)
  (context-id unsigned-int)  
  (text string))

(defbinding statusbar-pop () nil
  (statusbar statusbar)
  (context-id unsigned-int))

(defbinding statusbar-remove () nil
  (statusbar statusbar)
  (context-id unsigned-int)
  (message-id unsigned-int))



;;; Fixed

(defbinding fixed-put () nil
  (fixed fixed)
  (widget widget)
  (x int) (y int))

(defbinding fixed-move () nil
  (fixed fixed)
  (widget widget)
  (x int) (y int))



;;; Notebook

(defun %ensure-notebook-position (notebook page)
  (etypecase page
    (position page)
    (widget (notebook-page-num notebook page t))))

(defun %ensure-notebook-child (notebook position)
  (typecase position
     (widget position)
     (t (notebook-get-nth-page notebook position))))

(defbinding (notebook-insert "gtk_notebook_insert_page_menu")
    (notebook position child &optional tab-label menu-label) nil
  (notebook notebook)
  (child widget)
  ((if (stringp tab-label)
       (make-instance 'label :label tab-label)
     tab-label) (or null widget))
  ((if (stringp menu-label)
       (make-instance 'label :label menu-label)
     menu-label) (or null widget))
  ((%ensure-notebook-position notebook position) position))

(defun notebook-append (notebook child &optional tab-label menu-label)
  (notebook-insert notebook :last child tab-label menu-label))

(defun notebook-prepend (notebook child &optional tab-label menu-label)
  (notebook-insert notebook :first child tab-label menu-label))
  
(defbinding notebook-remove-page (notebook page) nil
  (notebook notebook)
  ((%ensure-notebook-position notebook page) position))

(defbinding %notebook-page-num () int
  (notebook notebook)
  (child widget))

(defun notebook-page-num (notebook child &optional error-p)
  (let ((page-num (%notebook-page-num notebook child)))
    (if (= page-num -1)
	(when error-p
	  (error "~A is not a page in ~A" child notebook))
      page-num)))

(defbinding notebook-next-page () nil
  (notebook notebook))

(defbinding notebook-prev-page () nil
  (notebook notebook))

(defbinding notebook-reorder-child (notebook child position) nil
  (notebook notebook)
  (child widget)
  ((%ensure-notebook-position notebook position) int))

(defbinding notebook-popup-enable () nil
  (notebook notebook))

(defbinding notebook-popup-disable () nil
  (notebook notebook))

(defbinding notebook-get-nth-page () widget
  (notebook notebook)
  (page position))

(defun %notebook-current-page (notebook)
  (when (slot-boundp notebook 'current-page-num)
    (notebook-get-nth-page notebook (notebook-current-page-num notebook))))

(defun (setf notebook-current-page) (page notebook)
  (setf (notebook-current-page-num notebook) (notebook-page-num notebook page)))

(defbinding (notebook-tab-label "gtk_notebook_get_tab_label")
    (notebook page) widget
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget))

(defbinding (notebook-tab-label-text "gtk_notebook_get_tab_label_text")
    (notebook page) (copy-of string)
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget))

(defbinding %notebook-set-tab-label () nil
  (notebook notebook)
  (page widget)
  (tab-label widget))

(defun (setf notebook-tab-label) (tab-label notebook page)
  (let ((widget (if (stringp tab-label)
		    (make-instance 'label :label tab-label)
		  tab-label)))
    (%notebook-set-tab-label notebook (%ensure-notebook-child notebook page) widget)
    widget))


(defbinding (notebook-menu-label "gtk_notebook_get_menu_label")
    (notebook page) widget
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget))

(defbinding (notebook-menu-label-text "gtk_notebook_get_menu_label_text")
    (notebook page) (copy-of string)
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget))

(defbinding %notebook-set-menu-label () nil
  (notebook notebook)
  (page widget)
  (menu-label widget))

(defun (setf notebook-menu-label) (menu-label notebook page)
  (let ((widget (if (stringp menu-label)
		    (make-instance 'label :label menu-label)
		  menu-label)))
    (%notebook-set-menu-label notebook (%ensure-notebook-child notebook page) widget)
    widget))


(defbinding notebook-query-tab-label-packing (notebook page) nil
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget)
  (expand boolean :out)
  (fill boolean :out)
  (pack-type pack-type :out))

(defbinding notebook-set-tab-label-packing
    (notebook page expand fill pack-type) nil
  (notebook notebook)
  ((%ensure-notebook-child notebook page) widget)
  (expand boolean)
  (fill boolean)
  (pack-type pack-type))



;;; Paned

(defbinding paned-pack1 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))

(defbinding paned-pack2 () nil
  (paned paned)
  (child widget)
  (resize boolean)
  (shrink boolean))


;;; Layout

(defbinding layout-put () nil
  (layout layout)
  (child widget)
  (x int)
  (y int))

(defbinding layout-move () nil
  (layout layout)
  (child widget)
  (x int)
  (y int))

(defbinding layout-set-size () nil
  (layout layout)
  (width unsigned-int)
  (height unsigned-int))

(defbinding layout-get-size () nil
  (layout layout)
  (width unsigned-int :out)
  (height unsigned-int :out))


;;; Menu shell

(defbinding menu-shell-insert (menu-shell menu-item position) nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  ((case position
     (:first 0)
     (:last -1)
     (t position)) int))

(defun menu-shell-append (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item :last))

(defun menu-shell-prepend (menu-shell menu-item)
  (menu-shell-insert menu-shell menu-item :fisrt))

(defbinding menu-shell-deactivate () nil
  (menu-shell menu-shell))

(defbinding menu-shell-select-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item))

(defbinding menu-shell-select-first () nil
  (menu-shell menu-shell)
  (search-sensitive boolean))

(defbinding menu-shell-deselect () nil
  (menu-shell menu-shell))

(defbinding menu-shell-activate-item () nil
  (menu-shell menu-shell)
  (menu-item menu-item)
  (fore-deactivate boolean))

(defbinding menu-shell-cancel () nil
  (menu-shell menu-shell))


;;; Menu

(defun %menu-position (menu child)
  (etypecase child
    (int child)
    (keyword (case child
	       (:first 0)
	       (:last -1)
	       (t (error "Invalid position keyword: ~A" child))))
    (widget (menu-child-position menu child))))


(defbinding menu-reorder-child (menu menu-item position) nil
  (menu menu)
  (menu-item menu-item)
  ((%menu-position menu position) int))

(defbinding menu-attach () nil
  (menu menu)
  (menu-item menu-item)
  (left-attach unsigned-int)
  (right-attach unsigned-int)
  (top-attach unsigned-int)
  (bottom-attach unsigned-int))

(define-callback-marshal %menu-position-callback nil 
  (menu (x int) (y int) (push-in boolean)))

(defbinding %menu-popup () nil
  (menu menu)
  (parent-menu-shell (or null menu-shell))
  (parent-menu-item (or null menu-item))
  (callback (or null callback))
  (callback-id unsigned-int)
  (button unsigned-int)
  (activate-time (unsigned 32)))

(defun menu-popup (menu button activate-time &key callback parent-menu-shell
		   parent-menu-item)
  (if callback
      (with-callback-function (id callback)
	(%menu-popup 
	 menu parent-menu-shell parent-menu-item 
	 %menu-position-callback id button activate-time))
    (%menu-popup
     menu parent-menu-shell parent-menu-item nil 0 button activate-time)))
 
(defbinding menu-set-accel-path () nil
  (menu menu)
  (accel-path string))

(defbinding menu-reposition () nil
  (menu menu))

(defbinding menu-popdown () nil
  (menu menu))

(defun menu-child-position (menu child)
  (position child (container-children menu)))

(defun menu-active-num (menu)
  (menu-child-position menu (menu-active menu)))

(defbinding %menu-set-active () nil
  (menu menu)
  (index unsigned-int))

(defun (setf menu-active) (menu child)
  (%menu-set-active menu (%menu-position menu child))
  child)
  
(define-callback %menu-detach-callback nil ((widget widget) (menu menu))
  (funcall (user-data menu 'detach-func) widget menu))

(defbinding %menu-attach-to-widget (menu widget) nil
  (menu menu)
  (widget widget)
  (%menu-detach-callback callback))

(defun menu-attach-to-widget (menu widget function)
  (setf (user-data menu 'detach-func) function)
  (%menu-attach-to-widget menu widget))

(defbinding menu-detach () nil
  (menu menu))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding menu-get-for-attach-widget () (copy-of (glist widget))
  (widget widget))

(defbinding menu-set-monitor () nil
  (menu menu)
  (monitor-num int))


;;; Table

(defbinding table-resize () nil
  (table table)
  (rows unsigned-int)
  (columns unsigned-int))

(defbinding table-attach (table child left right top bottom
			  &key options x-options y-options
			  (x-padding 0) (y-padding 0)) nil
  (table table)
  (child widget)
  (left unsigned-int)
  (right unsigned-int)
  (top unsigned-int)
  (bottom unsigned-int)
  ((append (mklist options) (mklist x-options)) attach-options)
  ((append (mklist options) (mklist y-options)) attach-options)
  (x-padding unsigned-int)
  (y-padding unsigned-int))


(defbinding %table-set-row-spacing () nil
  (table table)
  (row unsigned-int)
  (spacing unsigned-int))

(defbinding %table-set-row-spacings () nil
  (table table)
  (spacing unsigned-int))

(defun (setf table-row-spacing) (spacing table &optional row)
  (if row
      (%table-set-row-spacing table row spacing)
    (%table-set-row-spacings table spacing))
  spacing)

(defbinding %table-get-row-spacing () unsigned-int
  (table table)
  (row unsigned-int))

(defbinding %table-get-default-row-spacing () unsigned-int
  (table table))

(defun table-row-spacing (table &optional row)
  (if row
      (%table-get-row-spacing table row)
    (%table-get-default-row-spacing table)))


(defbinding %table-set-col-spacing () nil
  (table table)
  (col unsigned-int)
  (spacing unsigned-int))

(defbinding %table-set-col-spacings () nil
  (table table)
  (spacing unsigned-int))

(defun (setf table-column-spacing) (spacing table &optional column)
  (if column
      (%table-set-col-spacing table column spacing)
    (%table-set-col-spacings table spacing))
  spacing)

(defun (setf table-col-spacing) (spacing table &optional col)
  (warn "TABLE-COL-SPACING is deprecatet, use TABLE-COLUMN-SPACING instead")
  (setf (table-column-spacing table col) spacing))

(defbinding %table-get-col-spacing () unsigned-int
  (table table)
  (col unsigned-int))

(defbinding %table-get-default-col-spacing () unsigned-int
  (table table))

(defun table-column-spacing (table &optional column)
  (if column
      (%table-get-col-spacing table column)
    (%table-get-default-col-spacing table)))
  
(defun table-col-spacing (table &optional col)
  (warn "TABLE-COL-SPACING is deprecatet, use TABLE-COLUMN-SPACING instead")
  (table-column-spacing table col))



;;; Toolbar

(defmethod initialize-instance ((toolbar toolbar) &rest initargs &key tooltips)
  (if (eq tooltips t)
      (apply #'call-next-method toolbar
       :tooltips (make-instance 'tooltips) initargs)
    (call-next-method)))

(defbinding %toolbar-insert () nil
  (toolbar toolbar)
  (tool-item tool-item)
  (position position))

(defun toolbar-insert (toolbar tool-item &optional (position :end))
  (%toolbar-insert toolbar tool-item position)
  (%tool-item-update-tooltips tool-item))

(defbinding toolbar-get-item-index () int
  (toolbar toolbar)
  (item tool-item))

(defbinding toolbar-get-nth-item () tool-item
  (toolbar toolbar)
  (n int))

(defbinding toolbar-get-drop-index () int
  (toolbar toolbar)
  (x int) (y int))

(defbinding toolbar-set-drop-highlight-item () nil
  (toolbar toolbar)
  (tool-item tool-item)
  (index int))


;;; Tool button

(defmethod initialize-instance ((button tool-button) &rest initargs &key icon)
  (if (and icon (not (typep icon 'widget)))
      (apply #'call-next-method button :icon (create-image-widget icon) initargs)
    (call-next-method)))


;;; Tool item

(defbinding tool-item-set-tooltip () nil
  (tool-item tool-item)
  (tooltips tooltips)
  (tip-text string)
  (tip-private string))


(defun %tool-item-update-tooltips (tool-item)
  (when (and 
	 (slot-boundp tool-item 'parent)
	 (or 
	  (user-data-p tool-item 'tip-text)
	  (user-data-p tool-item 'tip-private)))
    (tool-item-set-tooltip
     tool-item (toolbar-tooltips (widget-parent tool-item))
     (or (user-data tool-item 'tip-text) "")
     (or (user-data tool-item 'tip-private) ""))))

(defmethod (setf tool-item-tip-text) ((tip-text string) (tool-item tool-item))
  (setf (user-data tool-item 'tip-text) tip-text)
  (%tool-item-update-tooltips tool-item)
  tip-text)

(defmethod (setf tool-item-tip-private) ((tip-private string) (tool-item tool-item))
  (setf (user-data tool-item 'tip-private) tip-private)
  (%tool-item-update-tooltips tool-item)
  tip-private)

(defmethod container-add ((toolbar toolbar) (tool-item tool-item) &rest args)
  (declare (ignore args))
  (prog1
      (call-next-method)
    (%tool-item-update-tooltips tool-item)))


(defbinding tool-item-retrieve-proxy-menu-item () widget
  (tool-item tool-item))

(defbinding (tool-item-proxy-menu-item 
	     "gtk_tool_item_get_proxy_menu_item") () menu-item
  (tool-item tool-item)
  (menu-item-id string))

(defbinding %tool-item-set-proxy-menu-item () nil
  (tool-item tool-item)
  (menu-item-id string)
  (menu-item menu-item))

(defun (setf tool-item-proxy-menu-item) (menu-item menu-item-id tool-item)
  (%tool-item-set-proxy-menu-item menu-item-id tool-item menu-item)
   menu-item)

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding tool-item-rebuild-menu () nil
  (tool-item tool-item))


;;; Editable

(defbinding editable-select-region (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(defbinding editable-get-selection-bounds (editable) nil
  (editable editable)
  (start int :out)
  (end int :out))

(defbinding editable-insert-text (editable text &optional (position 0)) nil
  (editable editable)
  (text string)
  ((length text) int)
  (position position :in/out))

(defun editable-append-text (editable text)
  (editable-insert-text editable text nil))

(defun editable-prepend-text (editable text)
  (editable-insert-text editable text 0))

(defbinding editable-delete-text (editable &optional (start 0) end) nil
  (editable editable)
  (start int)
  ((or end -1) int))

(defbinding (editable-text "gtk_editable_get_chars")
    (editable &optional (start 0) end) string
  (editable editable)
  (start int)
  ((or end -1) int))

(defun (setf editable-text) (text editable)
  (if text
      (editable-delete-text
       editable
       (editable-insert-text editable text))
    (editable-delete-text editable))
  text)

(defbinding editable-cut-clipboard () nil
  (editable editable))

(defbinding editable-copy-clipboard () nil
  (editable editable))

(defbinding editable-paste-clipboard () nil
  (editable editable))

(defbinding editable-delete-selection () nil
  (editable editable))



;;; Spin button

(defbinding spin-button-configure () nil
  (spin-button spin-button)
  (adjustment adjustment)
  (climb-rate double-float)
  (digits unsigned-int))

(defbinding spin-button-set-range () nil
  (spin-button spin-button)
  (min double-float)
  (max double-float))

(defbinding spin-button-get-range () nil
  (spin-button spin-button)
  (min double-float :out)
  (max double-float :out))

(defun spin-button-value-as-int (spin-button)
  (round (spin-button-value spin-button)))

(defbinding %spin-button-spin () nil
  (spin-button spin-button)
  (direction spin-type)
  (increment double-float))

(defun spin-button-spin (spin-button value)
  (etypecase value
    (real (%spin-button-spin spin-button :spin-user-defined value))
    (spin-type (%spin-button-spin spin-button value 0))))


(defbinding spin-button-update () nil
  (spin-button spin-button))



; ;;; Ruler

(defbinding ruler-set-range () nil
  (ruler ruler)
  (lower single-float)
  (upper single-float)
  (position single-float)
  (max-size single-float))

(defbinding ruler-get-range () nil
  (ruler ruler)
  (lower single-float :out)
  (upper single-float :out)
  (position single-float :out)
  (max-size single-float :out))



;;; Range

(defun range-lower (range)
  (adjustment-lower (range-adjustment range)))

(defun range-upper (range)
  (adjustment-upper (range-adjustment range)))

(defun (setf range-lower) (value range)
  (setf (adjustment-lower (range-adjustment range)) value))

(defun (setf range-upper) (value range)
  (setf (adjustment-upper (range-adjustment range)) value))

(defun range-page-increment (range)
  (adjustment-page-increment (range-adjustment range)))

(defun range-step-increment (range)
  (adjustment-step-increment (range-adjustment range)))

(defun (setf range-page-increment) (value range)
  (setf (adjustment-page-increment (range-adjustment range)) value))

(defun (setf range-step-increment) (value range)
  (setf (adjustment-step-increment (range-adjustment range)) value))

(defbinding range-set-range () nil
  (range range)
  (lower double-float)
  (upper double-float))

(defbinding range-set-increments () nil
  (range range)
  (step double-float)
  (page double-float))


;;; Scale

(defbinding scale-get-layout-offsets () nil
  (scale scale)
  (x int :out)
  (y int :out))


;;; Progress bar

(defbinding progress-bar-pulse () nil
  (progress-bar progress-bar))


;;; Size group

(defmethod initialize-instance ((size-group size-group) &rest initargs 
				&key widget widgets)
  (declare (ignore widget widgets))
  (prog1
      (call-next-method)
    (initial-add size-group #'size-group-add-widget 
     initargs :widget :widgets)))


(defbinding size-group-add-widget () nil
  (size-group size-group)
  (widget widget))

(defbinding size-group-remove-widget () nil
  (size-group size-group)
  (widget widget))


;;; Stock items

(defbinding %stock-item-copy () pointer
  (location pointer))

(defbinding %stock-item-free () nil
  (location pointer))

(defbinding stock-add (stock-item) nil
  (stock-item stock-item)
  (1 unsigned-int))

(defbinding stock-list-ids () (gslist string))

(defbinding %stock-lookup () boolean
  (stock-id string)
  (location pointer))

(defun stock-lookup (stock-id)
  (with-memory (stock-item (foreign-size (find-class 'stock-item)))
    (when (%stock-lookup stock-id stock-item)
      (ensure-proxy-instance 'stock-item (%stock-item-copy stock-item)))))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.8.0")
(progn
  (define-callback-marshal %stock-translate-callback string ((path string)))

  (defbinding (stock-set-translate-function "gtk_stock_set_translate_func") 
      (domain function) nil
    (domain string)
    (%stock-translate-callback callback)
    ((register-callback-function function) unsigned-int)
    (user-data-destroy-callback callback)))


;;; Tooltip

;; #?-(pkg-exists-p "gtk+-2.0" :atleast-version "2.12.0")
;; (progn
;;   (defbinding %tooltip-set-markup () nil
;;     tooltip
;;     (markup string))

;;   (defbinding %tooltip-set-text () nil
;;     tooltip
;;     (text string))

;;   (defbinding %tooltip-set-icon () nil
;;     tooltip
;;     (icon gdk:pixbuf))

;;   (defbinding %tooltip-set-from-stock-icon () nil
;;     tooltip
;;     (stock-id string)
;;     icon-size)

;;   (defbinding %tooltip-set-custom () nil
;;     tooltip
;;     widget)

;;   (defun tooltip-set (tooltip value &key (markup t) (icon-size :button))
;;     (etypecase value
;;       (string (if markup
;; 		  (tooltip-set-markup tooltip value)
;; 		(tooltip-set-text tooltip value)))
;;       (pixbuf (tooltip-set-icon tooltip value))
;;       (keyword (tooltip-set-icon-from-stock tooltip value icon-size))
      
  

;;; Tooltips 

;; GtkTooltips has been deprecated in favor of the new tooltip API
;; introduced in in GTK+ 2.12

(defbinding tooltips-enable () nil
  (tooltips tooltips))

(defbinding tooltips-disable () nil
  (tooltips tooltips))

(defun (setf tooltips-enabled-p) (enable tooltips)
  (if enable
      (tooltips-enable tooltips)
    (tooltips-disable tooltips)))

(defbinding tooltips-set-tip () nil
  (tooltips tooltips)
  (widget widget)
  (tip-text string)
  (tip-private string))

(defbinding tooltips-data-get () tooltips-data
  (widget widget))

(defbinding tooltips-force-window () nil
  (tooltips tooltips))

(defbinding tooltips-get-info-from-tip-window () boolean
  (tip-window window)
  (tooltips tooltips :out)
  (current-widget widget :out))


;;; Resource Files

(defbinding rc-get-style () style
  (widget widget))

(defbinding rc-get-style-by-paths (&key path class-path class) style
  (path (or null string))
  (class-path (or null string))
  (class gtype))

(defbinding rc-parse () nil
  (filename pathname))

(defbinding rc-parse-string () nil
  (rc-string string))

(defbinding %rc-reparse-all () boolean)

(defbinding %rc-reparse-all-for-settings () boolean
  (settings settings)
  (force-load-p boolean))

(defun rc-reparse-all (&optional setting force-load-p)
  (if setting
      (%rc-reparse-all-for-settings setting force-load-p)
    (%rc-reparse-all)))

(defbinding rc-reset-styles () nil
  (settings settings))

(defbinding rc-add-default-file () nil
  (filename pathname))

(defbinding rc-get-default-files ()
    (copy-of (null-terminated-vector (copy-of string))))

(defbinding rc-get-module-dir () string)

(defbinding rc-get-im-module-path () string)

(defbinding rc-get-im-module-file () string)

(defbinding rc-get-theme-dir () string)


;;; Settings

(defbinding (settings-get "gtk_settings_get_for_screen")
    (&optional (screen (gdk:display-get-default-screen))) settings
  (screen gdk:screen))


;;; Plug and Socket

(defbinding socket-add-id () nil
  (socket socket)
  (id gdk:native-window))

(defbinding %plug-new () pointer
  (id gdk:native-window))

(defmethod allocate-foreign ((plug plug) &key id)
  (%plug-new (or id 0)))


;;; Link button

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
(progn
  (define-callback-marshal %link-button-uri-callback nil (link-button (link string)))

  (defbinding link-button-set-uri-hook (function) pointer
    (%link-button-uri-callback callback)
    ((register-callback-function function) unsigned-int)
    (user-data-destroy-callback callback)))


;;; Builder

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.12.0")
(progn
  (defmethod initialize-instance ((builder builder) &key interface 
				  (connect-signals t) (package *package*))
    (call-next-method)
    (etypecase interface
      (null)
      (string (builder-add-from-string builder interface))
      (pathname (builder-add-from-file builder interface)))
    (when connect-signals
      (builder-connect-signals builder package)))


  (defbinding builder-add-from-file () boolean
   builder
   pathname
   (nil gerror-signal :out))

  (defbinding builder-add-from-string () boolean
   builder
   (buffer string)
   (-1 int) ; TODO: add gsize type
   (nil gerror-signal :out))

  (defbinding builder-get-object () gobject
   builder
   (name string))

  (defbinding builder-get-objects () (gslist gobject)
   builder)

  (defun intern-with-package-prefix (name default-package)
    (let ((pos (position #\: name)))
      (if pos
	  (intern 
	   (string-upcase (subseq name (1+ pos)))
	   (string-upcase (subseq name 0 pos)))
	(intern (string-upcase name) default-package))))

  (define-callback %builder-connect-function nil
    (builder (object gobject) (signal-name string) (handler-name string)
     (connect-object gobject) connect-flags (package user-data-id))
    (format t "Connect signal ~A for ~A to ~A in default package ~A with flags ~A~%" signal-name object handler-name (find-user-data package) connect-flags)
    (signal-connect 
     object signal-name 
     (intern-with-package-prefix handler-name (find-user-data package))
     :object (or connect-object object) :after (find :after connect-flags)))

  (defbinding %builder-connect-signals-full (builder package) nil
    builder    
    (%builder-connect-function callback)
    (package user-data-id))

  (defun builder-connect-signals (builder &optional (package *package*))
    (with-user-data (id package)
      (%builder-connect-signals-full builder id))))

