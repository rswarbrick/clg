;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: glade-xml.lisp,v 1.4 2008-10-09 18:45:33 espen Exp $


(in-package "GLADE-XML")


(defmethod build-interface ((interface cons))
  (unless (eq (first interface) :|glade-interface|)
    (error "Not a valid interface specification"))

  (let ((toplevels (loop
		    for spec in (rest interface)
		    collect (ecase (first (mklist (first spec)))
			      (:|widget| (build-widget spec))))))
    (connect-signals toplevels toplevels)
    toplevels))

(defmethod build-interface ((interface string))
  (build-interface (parse-xml-string interface)))

(defmethod build-interface ((interface stream))
  (build-interface (parse-xml interface)))

(defmethod build-interface ((interface pathname))
  (build-interface (parse-xml-file interface)))

(defun load-interface (filename)
  (build-interface (parse-xml-file filename)))



(define-type-generic parse-value (type value))

(define-type-method parse-value ((type string) value)
  (declare (ignore type))
  (or value ""))

(define-type-method parse-value ((type number) value)
  (declare (ignore type))
  (parse-number value))

(define-type-method parse-value ((type boolean) value)
  (declare (ignore type))
  (and (member value '("true" "yes") :test #'string-equal) t))


(defun find-enum-value (value type)
  (second
   (or
    (assoc value (query-enum-values type nil) :test #'string=)
    (assoc value (query-enum-values type :nickname) :test #'string=))))

(define-type-method parse-value ((type enum) value)
  (int-enum (find-enum-value value type) type))

(define-type-method parse-value ((type flags) value)
  (int-enum 
   (reduce #'logior   
    (mapcar 
     #'(lambda (flag)
	 (find-enum-value (string-trim " " flag) type))
     (split-string value :delimiter #\|)))
   type))



(define-type-generic get-property-info (type value))

(defun %get-property-info (class pname)
  (let ((slotd (find-if
		#'(lambda (slotd)
		    (and 
		     (or
		      (typep slotd 'effective-property-slot-definition)
		      (typep slotd 'gtk::effective-child-slot-definition))
		     (string= pname (slot-definition-pname slotd))))
		(class-slots class))))
    (if (not slotd)
	(warn "Ignoring unknown property for ~A: ~A" (class-name class) pname)
      (values 
       (or
	(first (mklist (slot-definition-initargs slotd)))
	(warn "Ignoring property without initarg: ~A" pname))
       (slot-definition-type slotd)))))

(define-type-method get-property-info ((type gobject) pname)
  (%get-property-info (find-class type) pname))

(define-type-method get-property-info ((type gtk::container-child) pname)
  (%get-property-info (find-class type) pname))

(define-type-method get-property-info ((type widget) pname)
  (if (string= pname "visible")
      (values :visible 'boolean)
    (funcall (gffi::find-next-type-method 'get-property-info 'widget) type pname)))

(define-type-method get-property-info ((type menu-item) pname)
  (cond
   ((string= pname "label") (values :label 'string))
   ((string= pname "use-underline") (values :use-underline 'boolean))
   ((string= pname "use-stock") (values :use-stock 'boolean))
   (t (funcall (gffi::find-next-type-method 'get-property-info 'menu-item) type pname))))



(defun parse-property (class attributes body)
  (let ((pname (substitute #\- #\_ (getf attributes :|name|))))
    (multiple-value-bind (initarg type) (get-property-info (class-name class) pname)
      (when initarg
	(let ((parsed-value (handler-case (parse-value type (first body))
			      (serious-condition (condition)
			        (declare (ignore condition))
				(warn "Ignoring property for ~A with unhandled type or invalid value: ~A" (class-name class)  pname)
				(return-from parse-property)))))
	  (list initarg parsed-value))))))

(defun parse-properties (class properites)
  (unless (class-finalized-p class)
    (finalize-inheritance class))

  (loop
   for (tag . body) in properites
   as id = (first (mklist tag))
   as attributes = (rest (mklist tag))
   as arg = (when (eq id :|property|)
	      (parse-property class attributes body))
   when arg
   nconc arg))


(defmethod add-child ((parent container) (child widget) args)
  (apply #'container-add parent child args))

(defmethod add-child ((menu-item menu-item) (menu menu) args)
  (declare (ignore args))
  (setf (menu-item-submenu menu-item) menu))



(defun build-widget (spec)
  (let* ((attributes (rest (first spec)))
	 (class (find-class (type-from-glib-name (getf attributes :|class|))))
	 (id (getf attributes :|id|)))

    ;; Get properties and create widget
    (let* ((initargs (parse-properties class (rest spec)))
	   (widget (apply #'make-instance class :name id initargs)))

      (loop 
       for (tag . body) in (rest spec)
       as element = (first (mklist tag))
       as attributes = (rest (mklist tag))
       do (cond
	   ((and (eq element :|child|) (not (eq (first body) :|placeholder|)))
	    (let ((initargs (parse-properties (find-child-class class) (rest (second body)))))
	      (add-child widget (build-widget (first body)) initargs)))

	   ((eq element :|signal|)
	    (let ((name (getf attributes :|name|))
		  (callback (intern-with-package-prefix (string-upcase (getf attributes :|handler|))))
		  (after (parse-value 'boolean (getf attributes :|after|)))
		  (object (or (getf attributes :|object|) t)))
	      ;; We can't connect the signal at this point because the
	      ;; name object may not yet have been created, so we
	      ;; store it as user data until all widgets are created
	      (push 
	       (list name callback :after after :object object)
	       (user-data widget 'signals))))))
      widget)))


(defun intern-with-package-prefix (name)
  (let ((pos (position #\: name)))
    (if pos
	(intern (subseq name (1+ pos))(subseq name 0 pos))
      (intern name))))


(defun connect-signals (widgets toplevels)
  (loop
   for widget in widgets
   do
   (loop
    for signal in (user-data widget 'signals)
    do (destructuring-bind (name callback &key after object) signal
	 (signal-connect widget name callback :after after 
	  :object (if (eq object t)
		      widget
		    (widget-find object toplevels)))))
    (unset-user-data widget 'signals)
   (when (typep widget 'container)
     (connect-signals (container-children widget) toplevels))))
