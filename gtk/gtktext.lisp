;; Common Lisp bindings for GTK+ v2.x
;; Copyright 2004-2005 Espen S. Johnsen <espen@users.sf.net>
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

;; $Id: gtktext.lisp,v 1.10 2007-10-22 09:21:50 espen Exp $


(in-package "GTK")

;;; Text Buffer

(defbinding %text-buffer-insert () nil
  (buffer text-buffer)
  (iter text-iter)
  (text string)
  (-1 int))

(defun text-buffer-insert (buffer iter text &rest tags)
  (if tags
      (let ((start-offset (text-iter-offset iter)))
	(%text-buffer-insert buffer iter text)
	(let ((start (text-buffer-get-iter-at-offset buffer start-offset)))
	  (loop
	   for tag in tags
	   do (text-buffer-apply-tag buffer tag start iter))))
    (%text-buffer-insert buffer iter text)))

(defbinding %text-buffer-insert-at-cursor () nil
  (buffer text-buffer)
  (text string)
  (-1 int))

(defun text-buffer-insert-at-cursor (buffer text &rest tags)
  (if tags
      (apply #'text-buffer-insert buffer 
       (text-buffer-get-iter-at-insert buffer) text tags)
    (%text-buffer-insert-at-cursor buffer text)))
  
(defbinding text-buffer-insert-interactive () boolean
  (buffer text-buffer)
  (iter text-iter)
  (text string)
  (-1 int)
  (default-editable boolean))

(defbinding text-buffer-insert-interactive-at-cursor () boolean
  (buffer text-buffer)
  (text string)
  (-1 int)
  (default-editable boolean))

(defbinding text-buffer-insert-range () nil
  (buffer text-buffer)
  (iter text-iter)
  (start text-iter)
  (end text-iter))

(defbinding text-buffer-insert-range-interactive () nil
  (buffer text-buffer)
  (iter text-iter)
  (start text-iter)
  (end text-iter)
  (default-editable boolean))

(defbinding text-buffer-delete () nil
  (buffer text-buffer)
  (start text-iter)
  (end text-iter))

(defbinding text-buffer-delete-interactive () boolean
  (buffer text-buffer)
  (start text-iter)
  (end text-iter)
  (default-editable boolean))

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.6.0")
(defbinding text-buffer-backspace () boolean
  (buffer text-buffer)
  (iter text-iter)
  (interactive boolean)
  (default-editable boolean))

(defbinding text-buffer-set-text () nil
  (buffer text-buffer)
  (text string)
  (-1 int))

(defbinding text-buffer-get-text () string
  (buffer text-buffer)
  (start text-iter)
  (end text-iter)
  (include-hidden-char boolean))

(defbinding text-buffer-get-slice () string
  (buffer text-buffer)
  (start text-iter)
  (end text-iter)
  (include-hidden-char boolean))

(defbinding text-buffer-insert-pixbuf () nil
  (buffer text-buffer)
  (iter text-iter)
  (pixbuf gdk:pixbuf))

(defbinding text-buffer-insert-child-anchor () nil
  (buffer text-buffer)
  (iter text-iter)
  (anchor text-child-anchor))

(defbinding text-buffer-create-child-anchor () text-child-anchor
  (buffer text-buffer)
  (iter text-iter))

(defbinding text-buffer-create-mark () text-mark
  (buffer text-buffer)
  (mark-name (or null string))
  (where text-iter)
  (left-gravity boolean))

(defbinding %text-buffer-move-mark () nil
  (buffer text-buffer)
  (mark text-mark)
  (where text-iter))

(defbinding %text-buffer-move-mark-by-name () nil
  (buffer text-buffer)
  (mark-name string)
  (where text-iter))

(defgeneric text-buffer-move-mark (buffer mark where))

(defmethod text-buffer-move-mark ((buffer text-buffer) (mark text-mark) where)
  (%text-buffer-move-mark buffer mark where))

(defmethod text-buffer-move-mark ((buffer text-buffer) (name string) where)
  (%text-buffer-move-mark-by-name buffer name where))

(defbinding %text-buffer-delete-mark () nil
  (buffer text-buffer)
  (mark text-mark))

(defbinding %text-buffer-delete-mark-by-name () nil
  (buffer text-buffer)
  (mark-name string))

(defgeneric text-buffer-delete-mark (buffer mark))

(defmethod text-buffer-delete-mark ((buffer text-buffer) (mark text-mark))
  (%text-buffer-delete-mark buffer mark))

(defmethod text-buffer-delete-mark ((buffer text-buffer) (mark-name string))
  (%text-buffer-delete-mark-by-name buffer mark-name))

(defbinding text-buffer-get-mark () text-mark
  (buffer text-buffer)
  (mark-name string))

(defbinding text-buffer-get-insert () text-mark
  (buffer text-buffer))

(defbinding text-buffer-get-selection-bound () text-mark
  (buffer text-buffer))

(defbinding text-buffer-place-cursor () nil
  (buffer text-buffer)
  (where text-iter))

(defbinding text-buffer-select-range () nil
  (buffer text-buffer)
  (insert text-iter)
  (bound text-iter))

(defbinding %text-buffer-apply-tag () nil
  (buffer text-buffer)
  (tag text-tag)
  (start text-iter)
  (end text-iter))

(defbinding %text-buffer-remove-tag () nil
  (buffer text-buffer)
  (tag text-tag)
  (start text-iter)
  (end text-iter))

(defbinding %text-buffer-apply-tag-by-name () nil
  (buffer text-buffer)
  (tag-name string)
  (start text-iter)
  (end text-iter))

(defbinding %text-buffer-remove-tag-by-name () nil
  (buffer text-buffer)
  (tag-name string)
  (start text-iter)
  (end text-iter))

(defgeneric text-buffer-apply-tag (buffer tag start end))

(defmethod text-buffer-apply-tag ((buffer text-buffer) (tag text-tag) start end)
  (%text-buffer-apply-tag buffer tag start end))

(defmethod text-buffer-apply-tag ((buffer text-buffer) (name string) start end)
  (%text-buffer-apply-tag-by-name buffer name start end))

(defgeneric text-buffer-remove-tag (buffer tag start end))

(defmethod text-buffer-remove-tag ((buffer text-buffer) (tag text-tag) start end)
  (%text-buffer-remove-tag buffer tag start end))

(defmethod text-buffer-remove-tag ((buffer text-buffer) (name string) start end)
  (%text-buffer-remove-tag-by-name buffer name start end))

(defbinding text-buffer-remove-all-tags () nil
  (buffer text-buffer)
  (start text-iter)
  (end text-iter))

(defun text-buffer-create-tag (buffer name &rest initargs)
  (text-tag-table-add 
   (text-buffer-tag-table buffer)
   (if name
       (apply #'make-instance 'text-tag :name name initargs)
     (apply #'make-instance 'text-tag initargs))))

(defbinding text-buffer-get-iter-at-line-offset 
    (buffer line offset &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  (line int)
  (offset int))

(defbinding text-buffer-get-iter-at-offset 
    (buffer offset &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  ((case offset
     (:start 0)
     (:end -1)
     (t offset)) int))

(defbinding text-buffer-get-iter-at-line 
    (buffer line &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  (line int))

(defbinding text-buffer-get-iter-at-line-index 
    (buffer line index &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  (line int)
  (index int))

(defbinding text-buffer-get-iter-at-mark 
    (buffer mark &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  (mark text-mark))

(defun text-buffer-get-iter-at-insert (buffer &optional (iter (make-instance 'text-iter)))
  (text-buffer-get-iter-at-mark buffer (text-buffer-get-insert buffer) iter))

(defbinding text-buffer-get-iter-at-child-anchor 
    (buffer anchor &optional (iter (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (iter text-iter :in/return)
  (anchor text-child-anchor))

(defbinding text-buffer-get-bounds 
    (buffer &optional (start (make-instance 'text-iter))
     (end (make-instance 'text-iter))) nil
  (buffer text-buffer)
  (start text-iter :in/return)
  (end text-iter :in/return))

(defbinding text-buffer-delete-selection () boolean
  (buffer text-buffer)
  (interactive boolean)
  (default-editable boolean))

(defbinding text-buffer-paste-clipboard () nil
  (buffer text-buffer)
  (clipboard clipboard)
  (override-location (or null text-iter))
  (default-editable boolean))

(defbinding text-buffer-copy-clipboard () nil
  (buffer text-buffer)
  (clipboard clipboard))

(defbinding text-buffer-cut-clipboard () nil
  (buffer text-buffer)
  (clipboard clipboard)
  (default-editable boolean))

(defbinding text-buffer-get-selection-bounds 
    (buffer &optional (start (make-instance 'text-iter))
     (end (make-instance 'text-iter))) boolean
  (buffer text-buffer)
  (start text-iter :in/return)
  (end text-iter :in/return))

(defbinding text-buffer-begin-user-action () nil
  (buffer text-buffer))

(defbinding text-buffer-end-user-action () nil
  (buffer text-buffer))

(defmacro text-buffer-with-user-action ((buffer) &body body)
  (let ((bufvar (make-symbol "BUFFER")))
    `(let ((,bufvar ,buffer))
       (text-buffer-begin-user-action ,bufvar)
       (unwind-protect
	   (progn ,@body)
	 (text-buffer-end-user-action ,bufvar)))))

(defbinding text-buffer-add-selection-clipboard () nil
  (buffer text-buffer)
  (clipboard clipboard))

(defbinding text-buffer-remove-selection-clipboard () nil
  (buffer text-buffer)
  (clipboard clipboard))


;;; Text Iter

#?(pkg-exists-p "gtk+-2.0" :atleast-version "2.10.0")
(defmethod allocate-foreign ((text-iter text-iter) &rest initargs)
  (declare (ignore initargs))
  (let ((size (foreign-size (class-of text-iter))))
    (slice-alloc size)))

(defbinding text-iter-get-char () int
  (iter text-iter))

(defbinding text-iter-get-slice () string
  (start text-iter)
  (end text-iter))

(defbinding text-iter-get-text () string
  (start text-iter)
  (end text-iter))

(defbinding text-iter-get-visible-slice () string
  (start text-iter)
  (end text-iter))

(defbinding text-iter-get-visible-text () string
  (start text-iter)
  (end text-iter))

(defbinding text-iter-get-pixbuf () string
  (iter text-iter))

(defbinding text-iter-get-marks () (gslist text-mark)
  (iter text-iter))

(defbinding text-iter-get-toggled-tags () (gslist text-tag)
  (iter text-iter)
  (toggoled-on boolean))

(defbinding text-iter-get-child-anchor () text-child-anchor
  (iter text-iter))

(defun %ensure-tag (tag iter)
  (etypecase tag
    (text-tag tag)
    (string 
     (text-tag-table-lookup 
      (text-buffer-tag-table (text-iter-buffer iter)) tag))))

(defbinding text-iter-begins-tag-p (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(defbinding text-iter-ends-tag-p (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(defbinding text-iter-toggles-tag-p (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(defbinding text-iter-has-tag-p (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(defbinding text-iter-get-tags () (gslist text-tag)
  (iter text-iter))

(defbinding text-iter-editable-p () boolean
  (iter text-iter)
  (default-setting boolean))

(defbinding text-iter-can-insert-p () boolean
  (iter text-iter)
  (default-editability boolean))

(defbinding text-iter-starts-word-p () boolean
  (iter text-iter))

(defbinding text-iter-ends-word-p () boolean
  (iter text-iter))

(defbinding text-iter-inside-word-p () boolean
  (iter text-iter))

(defbinding text-iter-starts-line-p () boolean
  (iter text-iter))

(defbinding text-iter-ends-line-p () boolean
  (iter text-iter))

(defbinding text-iter-starts-sentence-p () boolean
  (iter text-iter))

(defbinding text-iter-ends-sentence-p () boolean
  (iter text-iter))

(defbinding text-iter-inside-sentence-p () boolean
  (iter text-iter))

(defbinding text-iter-is-cursor-position-p () boolean
  (iter text-iter))

(defbinding text-iter-get-chars-in-line () int
  (iter text-iter))

(defbinding text-iter-get-bytes-in-line () int
  (iter text-iter))

(defbinding text-iter-get-attributes 
    (iter &optional (values (make-instance 'text-attributes))) boolean
  (iter text-iter)
  (values text-attributes :in/return))

(defbinding text-iter-get-language () pango:language
  (iter text-iter))

(defbinding text-iter-is-end-p () boolean
  (iter text-iter))

(defbinding text-iter-is-start-p () boolean
  (iter text-iter))

(defbinding text-iter-forward-char () boolean
  (iter text-iter))

(defbinding text-iter-backward-char () boolean
  (iter text-iter))

(defbinding text-iter-forward-chars () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-chars () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-line () boolean
  (iter text-iter))

(defbinding text-iter-backward-line () boolean
  (iter text-iter))

(defbinding text-iter-forward-lines () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-lines () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-word-ends () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-word-starts () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-word-end () boolean
  (iter text-iter))

(defbinding text-iter-backward-word-start () boolean
  (iter text-iter))

(defbinding text-iter-forward-cursor-position () boolean
  (iter text-iter))

(defbinding text-iter-backward-cursor-position () boolean
  (iter text-iter))

(defbinding text-iter-forward-cursor-positions () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-cursor-positions () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-sentence-start () boolean
  (iter text-iter))

(defbinding text-iter-backward-sentence-starts () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-sentence-end () boolean
  (iter text-iter))

(defbinding text-iter-forward-sentence-ends () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-visible-word-ends () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-visible-word-starts () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-visible-word-end () boolean
  (iter text-iter))

(defbinding text-iter-backward-visible-word-start () boolean
  (iter text-iter))

(defbinding text-iter-forward-visible-cursor-position () boolean
  (iter text-iter))

(defbinding text-iter-backward-visible-cursor-position () boolean
  (iter text-iter))

(defbinding text-iter-forward-visible-cursor-positions () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-backward-visible-cursor-positions () boolean
  (iter text-iter)
  (count int))

(defbinding text-iter-forward-to-end () nil
  (iter text-iter))

(defbinding text-iter-forward-to-line-end () boolean
  (iter text-iter))

(defbinding text-iter-forward-to-tag-toggle (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(defbinding text-iter-backward-to-tag-toggle (iter tag) boolean
  (iter text-iter)
  ((%ensure-tag tag iter) text-tag))

(define-callback-marshal %text-char-predicate-callback boolean (int))

(defbinding text-iter-forward-find-char (iter predicate &optional limit) boolean
  (iter text-iter)
  (%text-char-predicate-callback callback)
  ((register-callback-function predicate) unsigned-int)
  (limit (or null text-iter)))

(defbinding text-iter-backward-find-char (iter predicate &optional limit) boolean
  (iter text-iter)
  (%text-char-predicate-callback callback)
  ((register-callback-function predicate) unsigned-int)
  (limit (or null text-iter)))

(defbinding text-iter-forward-search 
    (iter text &optional flags limit (match-start (make-instance 'text-iter))
     (match-end (make-instance 'text-iter))) boolean
  (iter text-iter)
  (text string)
  (flags text-search-flags)
  (match-start text-iter :in/return)
  (match-end text-iter :in/return)
  (limit (or null text-iter)))

(defbinding text-iter-backward-search 
    (iter text &optional flags limit (match-start (make-instance 'text-iter))
     (match-end (make-instance 'text-iter))) boolean
  (iter text-iter)
  (text string)
  (flags text-search-flags)
  (match-start text-iter :in/return)
  (match-end text-iter :in/return)
  (limit (or null text-iter)))

(defbinding text-iter-equal-p () boolean
  (lhs text-iter)
  (rhs text-iter))

(defbinding text-iter-compare () int
  (lhs text-iter)
  (rhs text-iter))

(defbinding text-iter-in-range-p () boolean
  (iter text-iter)
  (start text-iter)
  (end text-iter))

(defbinding text-iter-order () nil
  (first text-iter)
  (second text-iter))



;;; Text Tag

(defbinding text-tag-event () boolean
  (tag text-tag)
  (event-object gobject)
  (event gdk:event)
  (iter text-iter))



;;; Text Tag Table

(defbinding text-tag-table-add () nil
  (table text-tag-table)
  (tag text-tag))

(defbinding text-tag-table-remove () nil
  (table text-tag-table)
  (tag text-tag))

(defbinding text-tag-table-lookup () text-tag
  (table text-tag-table)
  (name string))

(define-callback-marshal %text-tag-table-foreach-callback nil (text-tag))

(defbinding text-tag-table-foreach (table function) nil
  (table text-tag-table)
  (%text-tag-table-foreach-callback callback)
  ((register-callback-function function) unsigned-int))


;;; Text View

(defbinding text-view-scroll-to-mark () nil
  (text-view text-view)
  (mark text-mark)
  (within-margin boolean)
  (use-align boolean)
  (xalign double-float)
  (yalign double-float))

(defbinding text-view-scroll-to-iter () nil
  (text-view text-view)
  (iter text-iter)
  (within-margin boolean)
  (use-align boolean)
  (xalign double-float)
  (yalign double-float))

(defbinding text-view-scroll-mark-onscreen () nil
  (text-view text-view)
  (mark text-mark))

(defbinding text-view-move-mark-onscreen () nil
  (text-view text-view)
  (mark text-mark))

(defbinding text-view-place-cursor-onscreen () nil
  (text-view text-view))

(defbinding text-view-get-visible-rect 
    (text-view &optional (rect (make-instance 'gdk:rectangle))) nil
  (text-view text-view)
  (rect gdk:rectangle :in/return))

(defbinding text-view-get-iter-location 
    (text-view iter &optional (location (make-instance 'gdk:rectangle))) nil
  (text-view text-view)
  (iter text-iter)
  (location gdk:rectangle :in/return))

(defbinding text-view-get-line-at-y 
    (text-view y &optional (iter (make-instance 'text-iter))) nil
  (text-view text-view)
  (iter text-iter :in/return)
  (y int)
  (line-top int :out))

(defbinding text-view-get-line-yrange () nil
  (text-view text-view)
  (iter text-iter)
  (y int :out)
  (height int :out))

(defbinding text-view-get-iter-at-location 
    (text-view x y &optional (iter (make-instance 'text-iter))) nil
  (text-view text-view)
  (iter text-iter :in/return)
  (x int)
  (y int))

(defbinding text-view-buffer-to-window-coords () nil
  (text-view text-view)
  (win text-window-type)
  (buffer-x int) 
  (buffer-y int)
  (window-x int :out)
  (window-y int :out))

(defbinding text-view-window-to-buffer-coords () nil
  (text-view text-view)
  (win text-window-type)
  (window-x int)
  (window-y int)
  (buffer-x int :out) 
  (buffer-y int :out))

(defbinding text-view-get-window () gdk:window
  (text-view text-view)
  (win text-window-type))

(defbinding text-view-get-window-type () text-window-type
  (text-view text-view)
  (window gdk:window))

(defbinding %text-view-set-border-window-size () nil
  (text-view text-view)
  (win text-window-type)
  (size int))

(defun (setf text-view-border-window-size) (size text-view win)
  (%text-view-set-border-window-size text-view win size)
  size)

(defbinding (text-view-border-window-size 
	     "gtk_text_view_get_border_window_size") () int
  (text-view text-view)
  (win text-window-type))

(defbinding text-view-forward-display-line () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-backward-display-line () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-forward-display-line-end () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-backward-display-line-start () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding (text-view-starts-display-line-p 
	     "gtk_text_view_starts_display_line") () boolean
  (text-view text-view)
  (iter text-iter))

(defbinding text-view-move-visually () boolean
  (text-view text-view)
  (iter text-iter)
  (count int))

(defbinding text-view-add-child-at-anchor () nil
  (text-view text-view)
  (child widget)
  (anchor text-child-anchor))

(defbinding text-view-add-child-in-window () nil
  (text-view text-view)
  (child widget)
  (win text-window-type)
  (x int)
  (y int))

(defbinding text-view-move-child () nil
  (text-view text-view)
  (child widget)
  (x int)
  (y int))


;; Text Child Anchor

(defbinding (text-view-child-anchor-deleted-p
	     "gtk_text_child_anchor_get_deleted") () boolean
  (anchor text-child-anchor))
