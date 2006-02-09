;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gtkselection.lisp,v 1.5 2006-02-09 22:32:47 espen Exp $


(in-package "GTK")


;;;; Selection

(defbinding %target-list-ref () pointer
  (location pointer))
  
(defbinding %target-list-unref () nil
  (location pointer))

(defmethod reference-foreign ((class (eql (find-class 'target-list))) location)
  (declare (ignore class))
  (%target-list-ref location))

(defmethod unreference-foreign ((class (eql (find-class 'target-list))) location)
  (declare (ignore class))
  (%target-list-unref location))

(defbinding %target-list-new () pointer
  (targets (vector (inlined target-entry)))
  ((length targets) int))
  
(defmethod allocate-foreign ((target-list target-list) &key targets)
  (%target-list-new targets))

(defbinding target-list-add (target-list target &optional flags info) nil
  (target-list target-list)
  (target gdk:atom)
  (flags unsigned-int)
  (info unsigned-int))

(defbinding target-list-add-table (target-list targets) nil
  (target-list target-list)
  ((etypecase targets 
     ((or vector list) targets)
     (target-entry (vector targets)))
   (vector (inlined target-entry)))
  ((etypecase targets 
     ((or vector list) (length targets))
     (target-entry 1))
   int))

#+gtk2.6
(progn
  (defbinding target-list-add-text-targets (target-list info &optional writable-p) nil
    (target-list target-list)
    (info unsigned-int)
    (writable-p boolean))
  
  (defbinding target-list-add-image-targets (target-list info &optional writable-p) nil
    (target-list target-list)
    (info unsigned-int)
    (writable-p boolean))

  (defbinding target-list-add-uri-targets (target-list info &optional writable-p) nil
    (target-list target-list)
    (info unsigned-int)
    (writable-p boolean)))

(defbinding target-list-remove () nil
  (target-list target-list)
  (target gdk:atom))

;; (defbinding target-list-find () nil
;;   (target-list target-list)
;;   (target gdk:atom)
;;   ...)

(defbinding (selection-set-owner "gtk_selection_owner_set_for_display")
    (widget selection time &optional (display (gdk:display-get-default))) 
    boolean
  (display gdk:display)
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom))

(defbinding selection-add-target () nil
  (widget widget)
  (selection gdk:atom)
  (target gdk:atom)
  (info unsigned-int))

(defbinding selection-add-targets (widget selection targets) nil
  (widget widget)
  (selection gdk:atom)
  ((etypecase targets 
     ((or vector list) targets)
     (target-entry (vector targets)))
   (vector (inlined target-entry)))
  ((etypecase targets 
     ((or vector list) (length targets))
     (target-entry 1))
   int))

(defbinding selection-clear-targets () nil
  (widget widget)
  (selection gdk:atom))

(defbinding selection-convert () boolean
  (widget widget)
  (selection gdk:atom)
  (target gdk:atom)
  (time unsigned-int))

(defbinding selection-data-set () boolean
  (selection-data selection-data)
  (type gdk:atom)
  (format int)
  (data pointer)
  (length int))

(defbinding selection-data-set-text () boolean
  (selection-data selection-data)
  (text string)
  (-1 integer))

(defbinding selection-data-get-text () string
  (selection-data selection-data))

#+gtk2.6
(progn
  (defbinding selection-data-set-pixbuf () boolean
    (selection-data selection-data)
    (puxbuf gdk:pixbuf))

  (defbinding selection-data-get-pixbuf () gdk:pixbuf
    (selection-data selection-data))

  (defbinding selection-data-set-uris () boolean
    (selection-data selection-data)
    (uris (null-terminated-vector string)))

  (defbinding selection-data-get-uris () (null-terminated-vector string)
    (selection-data selection-data)))

(defbinding selection-data-get-targets () boolean
  (selection-data selection-data)
  (targets (vector gdk:atom n-atoms))
  (n-atoms int))

#+gtk2.6
(defbinding selection-data-targets-include-image-p (selection-data &optional writable-p) boolean
  (selection-data selection-data)
  (writable-p boolean))

(defbinding selection-data-targets-include-text-p (selection-data) boolean
  (selection-data selection-data))

(defbinding selection-remove-all () boolean
  (widget widget))


;;; Clipboard -- untestet

(defbinding (clipboard-get "gtk_clipboard_get_for_display") 
    (selection &optional (display (gdk:display-get-default))) clipboard
  (display gdk:display)
  ((gdk:atom-intern selection) gdk:atom))


(defcallback %clipboard-get-func (nil (clipboard pointer)
				      (selection-data selection-data)
				      (info int)
				      (user-data unsigned-int))
  (funcall (car (find-user-data user-data)) selection-data info))

(defcallback %clipboard-clear-func (nil (clipboard pointer)
					(user-data unsigned-int))
  (funcall (cdr (find-user-data user-data))))

(defbinding clipboard-set-with-data (clipboard targets get-func clear-func) gobject
  (clipboard clipboard)
  (targets (vector (inlined target-entry)))
  ((length targets) unsigned-int)
  (%clipboard-get-func callback)
  (%clipboard-clear-func callback)
  ((register-user-data (cons get-func clear-func)) unsigned-int))

(defbinding clipboard-clear () nil
  (clipboard clipboard))

(defbinding clipboard-set-text (clipboard text) nil
  (clipboard clipboard)
  (text string)
  ((length text) int))

#+gtk2.6
(defbinding clipboard-set-image () nil
  (clipboard clipboard)
  (pixbuf gdk:pixbuf))

(defun clipboard-set (clipboard object)
  (etypecase object
    (string (clipboard-set-text clipboard object))
    #+gtk2.6
    (gdk:pixbuf (clipboard-set-image clipboard object))))

(defcallback %clipboard-receive-func (nil (clipboard pointer)
					  (selection-data selection-data)
					  (user-data unsigned-int))
  (funcall (find-user-data user-data) selection-data))

(defbinding clipboard-request-contents (clipboard target callback) nil
  (clipboard clipboard)
  ((gdk:atom-intern target) gdk:atom)
  (%clipboard-receive-func callback)
  ((register-callback-function callback) unsigned-int))

(defcallback %clipboard-text-receive-func (nil (clipboard pointer)
					       (text (copy-of string))
					       (user-data unsigned-int))
  (funcall (find-user-data user-data) text))

(defbinding clipboard-request-text (clipboard callback) nil
  (clipboard clipboard)
  (%clipboard-text-receive-func callback)
  ((register-callback-function callback) unsigned-int))

#+gtk2.6
(progn
  (defcallback %clipboard-image-receive-func (nil (clipboard pointer)
						  (image gdk:pixbuf)
						  (user-data unsigned-int))
    (funcall (find-user-data user-data) image))

  (defbinding clipboard-request-image (clipboard callback) nil
    (clipboard clipboard)
    (%clipboard-image-receive-func callback)
    ((register-callback-function callback) unsigned-int)))


(defcallback %clipboard-targets-receive-func 
    (nil (clipboard pointer)
	 (atoms (vector gdk:atom n-atoms))
	 (n-atoms unsigned-int)
	 (user-data unsigned-int))
  (funcall (find-user-data user-data) atoms))

(defbinding clipboard-request-targets (clipboard callback) nil
  (clipboard clipboard)
  (%clipboard-targets-receive-func callback)
  ((register-callback-function callback) unsigned-int))

(defbinding clipboard-wait-for-contents () selection-data
  (clipboard clipboard))

(defbinding clipboard-wait-for-text () string
  (clipboard clipboard))

#+gtk2.6
(defbinding clipboard-wait-for-image () (referenced gdk:pixbuf)
  (clipboard clipboard))

(defbinding clipboard-wait-is-text-available-p () boolean
  (clipboard clipboard))

#+gtk2.6
(defbinding clipboard-wait-is-image-available-p () boolean
  (clipboard clipboard))

(defbinding clipboard-wait-for-targets () boolean
  (clipboard clipboard)
  (targets (vector gdk:atom n-targets) :out)
  (n-targets unsigned-int :out))

#+gtk2.6
(defbinding clipboard-wait-is-target-available-p () boolean
  (clipboard clipboard)
  (target gdk:atom))

#+gtk2.6
(defbinding clipboard-set-can-store () nil
  (clipboard clipboard)
  (targets (vector gdk:atom))
  ((length targets) int))

#+gtk2.6
(defbinding clipboard-store () nil
  (clipboard clipboard))


;;;; Drag and Drop

(defbinding drag-dest-set (widget flags targets actions) nil
  (widget widget)
  (flags dest-defaults)
  ((etypecase targets 
     ((or vector list) targets)
     (target-entry (vector targets)))
   (vector (inlined target-entry)))
  ((etypecase targets 
     ((or vector list) (length targets))
     (target-entry 1))
   int)
  (actions gdk:drag-action))

(defbinding drag-dest-set-proxy () nil
  (widget widget)
  (window gdk:window)
  (protocol gdk:drag-protocol)
  (use-coordinates-p boolean))

(defbinding drag-dest-unset () nil
  (widget widget))

(defbinding drag-dest-find-target () gdk:atom
  (widget widget)
  (context gdk:drag-context)
  (targets target-list))

(defbinding drag-dest-get-target-list () target-list
  (widget widget))

(defbinding drag-dest-set-target-list () nil
  (widget widget)
  (targets target-list))

#+gtk2.6
(progn
  (defbinding drag-dest-add-text-targets () nil
    (widget widget))

  (defbinding drag-dest-add-image-targets () nil
    (widget widget))

  (defbinding drag-dest-add-uri-targets () nil
    (widget widget)))

(defbinding drag-finish () nil
  (context gdk:drag-context)
  (success boolean)
  (delete boolean)
  (time unsigned-int))

(defbinding drag-get-data () nil
  (widget widget)
  (context gdk:drag-context)
  (target gdk:atom)
  (time unsigned-int))

(defbinding drag-get-source-widget () widget
  (context gdk:drag-context))

(defbinding drag-highlight () nil
  (widget widget))

(defbinding drag-unhighlight () nil
  (widget widget))

(defbinding drag-begin () gdk:drag-context
  (widget widget)
  (targets target-list)
  (actions gdk:drag-action)
  (button int)
  (event gdk:event))

(defbinding %drag-set-icon-widget () nil
  (context gdk:drag-context)
  (widget widget)
  (hot-x int)
  (hot-y int))

(defbinding %drag-set-icon-pixmap () nil
  (context gdk:drag-context)
  (pixmap gdk:pixmap)
  (mask gdk:bitmap)
  (hot-x int)
  (hot-y int))

(defbinding %drag-set-icon-pixbuf () nil
  (context gdk:drag-context)
  (pixbuf gdk:pixbuf)
  (hot-x int)
  (hot-y int))

(defbinding %drag-set-icon-stock () nil
  (context gdk:drag-context)
  (stock-id string)
  (hot-x int)
  (hot-y int))

(defbinding %drag-set-icon-default () nil
  (context gdk:drag-context))

(defun drag-set-icon (context icon &optional (hot-x 0) (hot-y 0))
  (etypecase icon
    (widget (%drag-set-icon-widget context icon hot-x hot-y))
    (gdk:pixbuf (%drag-set-icon-pixbuf context icon hot-x hot-y))
    (string (%drag-set-icon-stock context icon hot-x hot-y))
    (vector (multiple-value-bind (pixmap mask) (gdk:pixmap-create icon)
	      (%drag-set-icon-pixmap context pixmap mask hot-x hot-y)))
    (pathname (let ((pixbuf (gdk:pixbuf-load icon)))
		(%drag-set-icon-pixbuf context pixbuf hot-x hot-y)))
    (null (%drag-set-icon-default context))))

(defbinding drag-check-threshold-p () boolean
  (widget widget)
  (start-x int)
  (start-y int)
  (current-x int)
  (current-y int))

(defbinding drag-source-set (widget start-button-mask targets actions) nil
  (widget widget)
  (start-button-mask gdk:modifier-type)
  ((etypecase targets 
     ((or vector list) targets)
     (target-entry (vector targets)))
   (vector (inlined target-entry)))
  ((etypecase targets 
     ((or vector list) (length targets))
     (target-entry 1)) 
   int)
  (actions gdk:drag-action))

(defbinding %drag-source-set-icon-pixbuf () nil
  (widget widget)
  (pixbuf gdk:pixbuf))
  
(defbinding %drag-source-set-icon-stock () nil
  (widget widget)
  (pixbuf gdk:pixbuf))

(defun drag-source-set-icon (widget icon)
  (etypecase icon
    (gdk:pixbuf (%drag-source-set-icon-pixbuf widget icon))
    (string (%drag-source-set-icon-stock widget icon))
;    (vector )
    (pathname (let ((pixbuf (gdk:pixbuf-load icon)))
		(%drag-source-set-icon-pixbuf widget pixbuf)))))

(defbinding drag-source-unset () nil
  (widget widget))

(defbinding drag-source-set-target-list () nil
  (widget widget)
  (targets (or null target-list)))

(defbinding drag-source-get-target-list () target-list
  (widget widget))

#+gtk2.6
(progn
  (defbinding drag-source-add-text-targets () nil
    (widget widget))

  (defbinding drag-source-add-image-targets () nil
    (widget widget))

  (defbinding drag-source-add-uri-targets () nil
    (widget widget)))
