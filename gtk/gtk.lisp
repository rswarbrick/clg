;; Common Lisp bindings for GTK+ v2.0
;; Copyright (C) 1999-2001 Espen S. Johnsen <esj@stud.cs.uit.no>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; $Id: gtk.lisp,v 1.39 2005-03-13 18:08:44 espen Exp $


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

(defbinding get-default-language () (copy-of pango:language))


;;;; Initalization

(defbinding (gtk-init "gtk_parse_args") () nil
  "Initializes the library without opening the display."
  (nil null)
  (nil null))

(defun clg-init (&optional display)
  "Initializes the system and starts the event handling"
  (unless (gdk:display-get-default)
    (gdk:gdk-init)
    (gtk-init)
    (prog1
	(gdk:display-open display)
      (add-fd-handler (gdk:display-connection-number) :input #'main-iterate-all)
      (setq *periodic-polling-function* #'main-iterate-all)
      (setq *max-event-to-sec* 0)
      (setq *max-event-to-usec* 1000))))


;;; Misc

(defbinding grab-add () nil
  (widget widget))

(defbinding grab-get-current () widget)

(defbinding grab-remove () nil
  (widget widget))


;;; About dialog

#+gtk2.6
(progn
  (def-callback-marshal %about-dialog-activate-link-func 
    (nil (dialog about-dialog) (link (copy-of string))))

  (defbinding about-dialog-set-email-hook (function) nil
    ((callback %about-dialog-activate-link-func) pointer)
    ((register-callback-function function) unsigned-int)
    ((callback user-data-destroy-func) pointer))
  
  (defbinding about-dialog-set-url-hook (function) nil
    ((callback %about-dialog-activate-link-func) pointer)
    ((register-callback-function function) unsigned-int)
    ((callback user-data-destroy-func) pointer)))


;;; Acccel group

(defbinding %accel-group-connect () nil
  (accel-group accel-group)
  (key unsigned-int)
  (modifiers gdk:modifier-type)
  (flags accel-flags)
  (gclosure gclosure))

(defun accel-group-connect (group accelerator function &optional flags)
  (multiple-value-bind (key modifiers) (accelerator-parse accelerator)
    (let ((gclosure (make-callback-closure function)))
      (%accel-group-connect group key modifiers flags gclosure)
      gclosure)))

(defbinding accel-group-connect-by-path (group path function) nil
  (group accel-group)
  (path string)
  ((make-callback-closure function) gclosure :return))

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
     (multiple-value-bind (key modifiers) (accelerator-parse accelerator)
       (%accel-group-disconnect-key group key modifiers)))))

(defbinding accel-group-lock () nil
  (accel-group accel-group))

(defbinding accel-group-unlock () nil
  (accel-group accel-group))

(defbinding %accel-groups-activate () boolean
  (object gobject)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-groups-activate (object accelerator)
  (multiple-value-bind (key modifiers) (accelerator-parse accelerator)
    (%accel-groups-activate object key modifiers)))

(defbinding accel-groups-from-object () (gslist accel-groups)
  (object gobject))

(defbinding accelerator-valid-p (key &optional modifiers) boolean
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding %accelerator-parse () nil
  (accelerator string)
  (key unsigned-int :out)
  (modifiers gdk:modifier-type :out))

(defun accelerator-parse (accelerator)
  (multiple-value-bind (key modifiers) (%accelerator-parse accelerator)
    (if (zerop key)
	(error "Invalid accelerator: ~A" accelerator)
      (values key modifiers))))

(defbinding accelerator-name () string
  (key unsigned-int)
  (modifiers gdk:modifier-type))

#+gtk2.6
(defbinding accelerator-get-label () string
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defbinding %accelerator-set-default-mod-mask () nil
  (default-modifiers gdk:modifier-type))

(defun (setf accelerator-default-modifier-mask) (default-modifiers)
  (%accelerator-set-default-mod-mask default-modifiers))

(defbinding (accelerator-default-modifier-mask "gtk_accelerator_get_default_mod_mask") () gdk:modifier-type)


;;; Acccel label

(defbinding accel-label-refetch () boolean
  (accel-label accel-label))



;;; Accel map

(defbinding %accel-map-add-entry () nil
  (path string)
  (key unsigned-int)
  (modifiers gdk:modifier-type))

(defun accel-map-add-entry (path accelerator)
  (multiple-value-bind (key modifiers) (accelerator-parse accelerator)
    (%accel-map-add-entry path key modifiers)))

(defbinding accel-map-lookup-entry () boolean
  (path string)
  (key pointer)) ;accel-key))

(defbinding %accel-map-change-entry () boolean
  (path string)
  (key unsigned-int)
  (modifiers gdk:modifier-type)
  (replace boolean))

(defun accel-map-change-entry (path accelerator &optional replace)
  (multiple-value-bind (key modifiers) (accelerator-parse accelerator)
    (%accel-map-change-entry path key modifiers replace)))

(defbinding accel-map-load () nil
  (filename pathname))

(defbinding accel-map-save () nil
  (filename pathname))

(defbinding accel-map-get () accel-map)

(defbinding accel-map-lock-path () nil
  (path string))

(defbinding accel-map-unlock-path () nil
  (path string))



;;; Accessible

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


;;; Aspect frame


;;; Bin

(defun (setf bin-child) (child bin)
  (when-bind (current-child (bin-child bin))
    (container-remove bin current-child))
  (container-add bin child)
  child)

(defmethod compute-signal-function ((bin bin) signal function object)
  (declare (ignore signal))
  (if (eq object :child)
      #'(lambda (&rest args) 
	  (apply function (bin-child bin) (rest args)))
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
      (apply #'call-next-method button :label stock :use-stock t initargs)
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

(defbinding (color-selection-is-adjusting-p
	     "gtk_color_selection_is_adjusting") () boolean
  (colorsel color-selection))



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

#+gtk2.6
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
  (declare (ignore button buttons))
  (prog1
      (call-next-method)
    (initial-apply-add dialog #'dialog-add-button initargs :button :buttons)))
  

(defun dialog-response-id (dialog response &optional create-p error-p)
  "Returns a numeric response id"
  (if (typep response 'response-type)
      (response-type-to-int response)
    (let ((responses (object-data dialog 'responses)))
      (cond
       ((and responses (position response responses :test #'equal)))
       (create-p
       (cond
	 (responses
	  (vector-push-extend response responses)
	  (1- (length responses)))
	 (t
	  (setf 
	   (object-data dialog 'responses)
	   (make-array 1 :adjustable t :fill-pointer t 
		       :initial-element response))
	  0)))
      (error-p
       (error "Invalid response: ~A" response))))))

(defun dialog-find-response (dialog id)
  "Finds a symbolic response given a numeric id"
  (if (< id 0)
      (int-to-response-type id)
    (aref (object-data dialog 'responses) id)))


(defmethod compute-signal-id ((dialog dialog) signal)
  (if (dialog-response-id dialog signal)
      (ensure-signal-id 'response dialog)
    (call-next-method)))

(defmethod compute-signal-function ((dialog dialog) signal function object)
  (declare (ignore function object))
  (let ((callback (call-next-method))
	(id (dialog-response-id dialog signal)))
    (if id
	#'(lambda (dialog response)
	    (when (= response id)
	      (funcall callback dialog)))
      callback)))

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

#+gtk2.6
(defbinding alternative-dialog-button-order-p (&optional screen) boolean
  (screen (or null gdk:screen)))

#+gtk2.6
(defbinding (dialog-set-alternative-button-order 
	     "gtk_dialog_set_alternative_button_order_from_array") 
    (dialog new-order)
  (dialog dialog)
  ((length new-order) int)
  ((map 'vector #'(lambda (response)
		    (dialog-response-id dialog response nil t))
	new-order) (vector int)))


(defmethod container-add ((dialog dialog) (child widget) &rest args)
  (apply #'container-add (dialog-vbox dialog) child args))


(defmethod container-remove ((dialog dialog) (child widget))
  (container-remove (dialog-vbox dialog) child))

(defmethod container-children ((dialog dialog))
  (container-children (dialog-vbox dialog)))

(defmethod (setf container-children) (children (dialog dialog))
  (setf (container-children (dialog-vbox dialog)) children))


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

(def-callback-marshal %entry-completion-match-func
    (boolean entry-completion string (copy-of tree-iter)))

(defbinding entry-completion-set-match-func (completion function) nil
  (completion entry-completion)
  ((callback %entry-completion-match-func) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback user-data-destroy-func) pointer))

(defbinding entry-completion-complete () nil
  (completion entry-completion))

#+gtk2.6
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
      #-gtk2.6(warn "Initarg :PIXBUF-FORMATS not supportet in this version of Gtk")
      #+gtk2.6(file-filter-add-pixbuf-formats file-filter))
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

#+gtk2.6
(defbinding file-filter-add-pixbuf-formats () nil
  (filter file-filter))

(def-callback-marshal %file-filter-func (boolean file-filter-info))

(defbinding file-filter-add-custom (filter needed function) nil
  (filter file-filter)
  (needed file-filter-flags)
  ((callback %file-filter-func) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback user-data-destroy-func) pointer))

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

#+gtk2.6
(defbinding menu-tool-button-set-arrow-tooltip () nil
  (menu-tool-button menu-tool-button)
  (tooltips tooltips)
  (tip-text string)
  (tip-private string))


;;; Message dialog

(defmethod initialize-instance ((dialog message-dialog)
				&key (message-type :info) (buttons :close)
				flags text #+gtk 2.6 secondary-text 
				transient-parent)
  (setf 
   (slot-value dialog 'location)
   (%message-dialog-new transient-parent flags message-type buttons))
  (when text
    (message-dialog-set-markup dialog text))
  #+gtk2.6
  (when secondary-text
    (message-dialog-format-secondary-markup dialog secondary-text))
  (call-next-method))


(defbinding %message-dialog-new () pointer
  (parent (or null window))
  (flags dialog-flags)
  (type message-type)
  (buttons buttons-type)
  (nil null))

(defbinding message-dialog-set-markup () nil
  (message-dialog message-dialog)
  (markup string))

#+gtk2.6
(defbinding message-dialog-format-secondary-text () nil
  (message-dialog message-dialog)
  (text string))

#+gtk2.6
(defbinding message-dialog-format-secondary-markup () nil
  (message-dialog message-dialog)
  (markup string))



;;; Radio menu item

(defbinding %radio-menu-item-get-group () pointer
  (radio-menu-item radio-menu-item))

(defbinding %radio-menu-item-set-group () nil
  (radio-menu-item radio-menu-item)
  (group pointer))

(defmethod add-to-radio-group ((item1 radio-menu-item) (item2 radio-menu-item))
  "Add ITEM1 to the group which ITEM2 belongs to."
  (%radio-menu-item-set-group item1 (%radio-menu-item-get-group item2)))

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

(defmethod add-to-radio-group ((button1 radio-tool-button) (button2 radio-tool-button))
  "Add BUTTON1 to the group which BUTTON2 belongs to."
  (%radio-tool-button-set-group button1 (%radio-tool-button-get-group button2)))

(defmethod add-activate-callback ((widget widget) function &key object after)
  (if object
      (signal-connect widget 'clicked
       #'(lambda (object)
	   (when (slot-value widget 'active)
	     (funcall function object (slot-value widget 'value))))
       :object object :after after)
    (signal-connect widget 'clicked 
     #'(lambda ()
	 (when (slot-value widget 'active)
	   (funcall function (slot-value widget 'value))))
     :after after)))

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
				&key accel-group accel-groups)
  (declare (ignore accel-group accel-groups))
  (prog1
      (call-next-method)
    (initial-add window #'window-add-accel-group 
     initargs :accel-group :accel-groups)))


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
  (geometry gdk:geometry)
  (geometry-mask gdk:window-hints))

(defun window-set-geometry-hints (window &key min-width min-height
                                  max-width max-height base-width base-height
				  width-inc height-inc min-aspect max-aspect
				  (gravity nil gravity-p) min-size max-size)
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
		   :max-aspect (or max-aspect 0)
		   :gravity gravity))
	(mask ()))
    (when (or min-size min-width min-height)
      (push :min-size mask))
    (when (or max-size max-width max-height)
      (push :max-size mask))
    (when (or base-width base-height)
      (push :base-size mask))
    (when (or width-inc height-inc)
      (push :resize-inc mask))
    (when (or min-aspect max-aspect)
      (push :aspect mask))
    (when gravity-p
      (push :win-gravity mask))
    (%window-set-geometry-hints window geometry mask)))

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

(defbinding window-present () nil
  (window window))

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

(defbinding %window-get-icon-list () (glist gdk:pixbuf)
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

(defmethod (setf window-default-icon) ((icon gdk:pixbuf))
  (%window-set-default-icon icon)
  icon)

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
    (notebook position child tab-label &optional menu-label) nil
  (notebook notebook)
  (child widget)
  ((if (stringp tab-label)
       (make-instance 'label :label tab-label)
     tab-label) widget)
  ((if (stringp menu-label)
       (make-instance 'label :label menu-label)
     menu-label) (or null widget))
  ((%ensure-notebook-position notebook position) position))

(defun notebook-append (notebook child tab-label &optional menu-label)
  (notebook-insert notebook :last child tab-label menu-label))

(defun notebook-prepend (notebook child tab-label &optional menu-label)
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
  ((%notebook-position notebook position) int))

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
  (setf (notebook-current-page notebook) (notebook-page-num notebook page)))

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
  ((%notebook-child notebook page) widget)
  (expand boolean :out)
  (fill boolean :out)
  (pack-type pack-type :out))

(defbinding notebook-set-tab-label-packing
    (notebook page expand fill pack-type) nil
  (notebook notebook)
  ((%notebook-child notebook page) widget)
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

(def-callback-marshal %menu-position-func (nil (menu menu) (x int) (y int) (push-in boolean)))

(defbinding %menu-popup () nil
  (menu menu)
  (parent-menu-shell (or null menu-shell))
  (parent-menu-item (or null menu-item))
  (callback-func (or null pointer))
  (callback-id unsigned-int)
  (button unsigned-int)
  (activate-time (unsigned 32)))

(defun menu-popup (menu button activate-time &key callback parent-menu-shell
		   parent-menu-item)
  (if callback
      (with-callback-function (id callback)
	(%menu-popup 
	 menu parent-menu-shell parent-menu-item 
	 (callback %menu-position-func) id button activate-time))
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
  
(defcallback %menu-detach-func (nil (widget widget) (menu menu))
  (funcall (object-data menu 'detach-func) widget menu))

(defbinding %menu-attach-to-widget () nil
  (menu menu)
  (widget widget)
  ((callback %menu-detach-func) pointer))

(defun menu-attach-to-widget (menu widget function)
  (setf (object-data menu 'detach-func) function)
  (%menu-attach-to-widget menu widget))

(defbinding menu-detach () nil
  (menu menu))

#+gtk2.6
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

(defun (setf table-col-spacing) (spacing table &optional col)
  (if col
      (%table-set-col-spacing table col spacing)
    (%table-set-col-spacings table spacing))
  spacing)

(defbinding %table-get-col-spacing () unsigned-int
  (table table)
  (col unsigned-int))

(defbinding %table-get-default-col-spacing () unsigned-int
  (table table))

(defun table-col-spacing (table &optional col)
  (if col
      (%table-get-col-spacing table col)
    (%table-get-default-col-spacing table)))
  


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

#+gtk2.6
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
  (position position :in-out))

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

(defmethod reference-foreign ((class (eql (find-class 'stock-item))) location)
  (%stock-item-copy location))

(defmethod unreference-foreign ((class (eql (find-class 'stock-item))) location)
  (%stock-item-free location))

(defbinding stock-add (stock-item) nil
  (stock-item stock-item)
  (1 unsigned-int))

(defbinding stock-list-ids () (gslist string))

(defbinding %stock-lookup () boolean
  (stock-id string)
  (location pointer))

(defun stock-lookup (stock-id)
  (let ((location 
	 (allocate-memory (proxy-instance-size (find-class 'stock-item)))))
    (unwind-protect
	(when (%stock-lookup stock-id location)
	  (ensure-proxy-instance 'stock-item (%stock-item-copy location)))
	(deallocate-memory location))))


;;; Tooltips

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


;;; Rc

(defbinding rc-add-default-file (filename) nil
  ((namestring (truename filename)) string))

(defbinding rc-parse (filename) nil
  ((namestring (truename filename)) string))

(defbinding rc-parse-string () nil
  (rc-string string))

(defbinding rc-reparse-all () nil)

(defbinding rc-get-style () style
  (widget widget))
