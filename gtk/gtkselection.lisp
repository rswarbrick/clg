;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2005-2006 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gtkselection.lisp,v 1.11 2007-12-12 15:47:29 espen Exp $


(in-package "GTK")


;;;; Selection

(defbinding %target-list-ref () pointer
  (location pointer))
  
(defbinding %target-list-unref () nil
  (location pointer))

(defbinding %target-list-new () pointer
  (targets (vector (inlined target-entry)))
  ((length targets) int))
  
(defmethod allocate-foreign ((target-list target-list) &key targets)
  (%target-list-new targets))

(defbinding target-list-add (target-list target &optional flags info) nil
  (target-list target-list)
  ((gdk:atom-intern target) gdk:atom)
  (flags target-flags)
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

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
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

(defbinding target-list-remove (target-list target) nil
  (target-list target-list)
  ((gdk:atom-intern target) gdk:atom))

(defbinding target-list-find (target-list target) boolean
  (target-list target-list)
  ((gdk:atom-intern target) gdk:atom)
  (info unsigned-int :out))

(defbinding target-table-new-from-list () (vector (inlined target-entry) n-targets)
  (target-list target-list)
  (n-targets int :out))

(defun ensure-target-table (targets)
  (etypecase targets
    (target-list (target-table-new-from-list targets))
    ((or vector list) targets)))

(defbinding (selection-set-owner "gtk_selection_owner_set_for_display")
    (widget selection time &optional (display (gdk:display-get-default))) 
    boolean
  (display gdk:display)
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom)
  (time (unsigned 32)))

(defbinding selection-add-target (widget selection target info) nil
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom)
  ((gdk:atom-intern target) gdk:atom)
  (info unsigned-int))

(defbinding selection-add-targets (widget selection targets) nil
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom)
  ((etypecase targets 
     ((or vector list) targets)
     (target-entry (vector targets)))
   (vector (inlined target-entry)))
  ((etypecase targets 
     ((or vector list) (length targets))
     (target-entry 1))
   int))

(defbinding selection-clear-targets (widget selection) nil
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom))

(defbinding selection-convert (widget selection target time) boolean
  (widget widget)
  ((gdk:atom-intern selection) gdk:atom)
  ((gdk:atom-intern target) gdk:atom)
  (time unsigned-int))

(defbinding selection-data-set (selection-data type format data length) boolean
  (selection-data selection-data)
  ((gdk:atom-intern type) gdk:atom)
  (format int)
  (data pointer)
  (length int))

(defbinding selection-data-set-text () boolean
  (selection-data selection-data)
  (text string)
  (-1 integer))

(defbinding selection-data-get-text () string
  (selection-data selection-data))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
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

(defbinding %selection-data-get-targets () boolean
  (selection-data selection-data)
  (targets (vector gdk:atom n-targets) :out)
  (n-targets int :out))

(defun selection-data-get-targets (selection-data)
  (multiple-value-bind (valid-p targets) 
      (%selection-data-get-targets selection-data) 
    (when valid-p
      (map-into targets #'gdk:atom-name targets))))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding selection-data-targets-include-image-p (selection-data &optional writable-p) boolean
  (selection-data selection-data)
  (writable-p boolean))

(defbinding selection-data-targets-include-text-p () boolean
  (selection-data selection-data))

(defbinding selection-remove-all () boolean
  (widget widget))


;;; Clipboard -- untestet

(defbinding (clipboard-get "gtk_clipboard_get_for_display") 
    (selection &optional (display (gdk:display-get-default))) clipboard
  (display gdk:display)
  ((gdk:atom-intern selection) gdk:atom))

(define-callback %clipboard-get-callback nil
    ((clipboard pointer) (selection-data selection-data)
     (info unsigned-int) (callback-ids unsigned-int))
  (declare (ignore clipboard))
  (funcall (car (find-user-data callback-ids)) selection-data info))

(define-callback %clipboard-clear-callback nil
    ((clipboard pointer) (callback-ids unsigned-int))
  (declare (ignore clipboard))
  (funcall (cdr (find-user-data callback-ids))))

;; Deprecated, use clipboard-set
(defbinding clipboard-set-with-data (clipboard targets get-func clear-func) boolean
  (clipboard clipboard)
  (targets (vector (inlined target-entry)))
  ((length targets) unsigned-int)
  (%clipboard-get-callback callback)
  (%clipboard-clear-callback callback)
  ((register-user-data (cons get-func clear-func)) unsigned-int))

(defun clipboard-set (clipboard targets get-func &optional clear-func)
  (%clipboard-set-with-data clipboard (ensure-target-table targets) 
   get-func (or clear-func #'(lambda ()))))

(defbinding clipboard-clear () nil
  (clipboard clipboard))

(defbinding clipboard-set-text () nil
  (clipboard clipboard)
  (text string)
  ((length text) int))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding clipboard-set-image () nil
  (clipboard clipboard)
  (pixbuf gdk:pixbuf))

(defun clipboard-set (clipboard object)
  (etypecase object
    (string (clipboard-set-text clipboard object))
    #?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
    (gdk:pixbuf (clipboard-set-image clipboard object))))

(define-callback-marshal %clipboard-receive-callback nil 
 ((:ignore clipboard) selection-data))

(defbinding clipboard-request-contents (clipboard target callback) nil
  (clipboard clipboard)
  ((gdk:atom-intern target) gdk:atom)
  (%clipboard-receive-callback callback)
  ((register-callback-function callback) unsigned-int))

(define-callback-marshal %clipboard-text-receive-callback nil
  ((:ignore clipboard) (text string)))


(defbinding clipboard-request-text (clipboard callback) nil
  (clipboard clipboard)
  (%clipboard-text-receive-callback callback)
  ((register-callback-function callback) unsigned-int))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(progn
  (define-callback-marshal %clipboard-image-receive-callback nil 
    ((:ignore clipboard) (image gdk:pixbuf)))

  (defbinding clipboard-request-image (clipboard callback) nil
    (clipboard clipboard)
    (%clipboard-image-receive-callback callback)
    ((register-callback-function callback) unsigned-int)))


(define-callback %clipboard-targets-receive-callback nil
    ((clipboard pointer) (atoms (vector gdk:atom n-atoms))
     (n-atoms unsigned-int) (callback-id unsigned-int))
  (declare (ignore clipboard))
  (funcall (find-user-data callback-id) (map-into atoms #'gdk:atom-name atoms)))

(defbinding clipboard-request-targets (clipboard callback) nil
  (clipboard clipboard)
  (%clipboard-targets-receive-callback callback)
  ((register-callback-function callback) unsigned-int))

(defbinding clipboard-wait-for-contents (clipboard target) selection-data
  (clipboard clipboard)
  ((gdk:atom-intern target) gdk:atom))

(defbinding clipboard-wait-for-text () string
  (clipboard clipboard))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding clipboard-wait-for-image () (referenced gdk:pixbuf)
  (clipboard clipboard))

(defbinding clipboard-wait-is-text-available-p () boolean
  (clipboard clipboard))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding clipboard-wait-is-image-available-p () boolean
  (clipboard clipboard))

(defbinding %clipboard-wait-for-targets () boolean
  (clipboard clipboard)
  (targets (vector gdk:atom n-targets) :out)
  (n-targets unsigned-int :out))

(defun clipboard-wait-for-targets (clipboard)
  (multiple-value-bind (valid-p targets) 
      (%clipboard-wait-for-targets clipboard) 
    (when valid-p
      (map-into targets #'gdk:atom-name targets))))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding clipboard-wait-is-target-available-p (clipboard target) boolean
  (clipboard clipboard)
  ((gdk:atom-intern target) gdk:atom))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding clipboard-set-can-store (clipboard targets) nil
  (clipboard clipboard)
  ((map 'vector #'gdk:atom-intern targets) (vector gdk:atom))
  ((length targets) int))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
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

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
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

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(progn
  (defbinding drag-source-add-text-targets () nil
    (widget widget))

  (defbinding drag-source-add-image-targets () nil
    (widget widget))

  (defbinding drag-source-add-uri-targets () nil
    (widget widget)))
