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

;; $Id: gtktree.lisp,v 1.1 2004-11-07 17:22:06 espen Exp $


(in-package "GTK")


;;;; Cell Layout

(defbinding cell-layout-pack-start () nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  (expand boolean))

(defbinding cell-layout-pack-end () nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  (expand boolean))

(defun cell-layout-pack (layout cell &key end expand)
  (if end
      (cell-layout-pack-end layout cell expand)
    (cell-layout-pack-start layout cell expand)))

(defbinding cell-layout-reorder () nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  (position int))

(defbinding cell-layout-clear () nil
  (cell-layout cell-layout))

(defbinding cell-layout-add-attribute (cell-layout cell attribute column) nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  ((string-downcase attribute) string)
  (column int))

(def-callback-marshal %cell-layout-data-func 
    (nil cell-layout cell-renderer tree-model tree-iter))

(defbinding cell-layout-set-cell-data-func (cell-layout cell function) nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  ((callback %cell-layout-data-func) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback %destroy-user-data) pointer))

(defbinding cell-layout-clear-attributes () nil
  (cell-layout cell-layout)
  (cell cell-renderer))



;;;; List Store

(defmethod initialize-instance ((list-store list-store) &key columns)
  (call-next-method)
  (%list-store-set-column-types list-store (length columns)
   (map 'vector #'find-type-number columns)))


(defbinding %list-store-set-column-types () nil
  (list-store list-store)
  (n-columns unsigned-int)
  (columns (vector type-number)))

(defbinding %list-store-set-value () nil
  (list-store list-store)
  (tree-iter tree-iter)
  (column int)
  (value gvalue))

(defun list-store-set-value (list-store tree-iter column type value)
  (let ((gvalue (gvalue-new type value)))
    (unwind-protect
	 (%list-store-set-value list-store tree-iter column gvalue)
      (gvalue-free gvalue))))

(defbinding list-store-remove () boolean
  (list-store list-store)
  (tree-iter tree-iter))

(defbinding list-store-insert () nil
  (list-store list-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (position int))

(defbinding list-store-insert-before (list-store &optional sibling) nil
  (list-store list-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (sibling (or null tree-iter)))

(defbinding list-store-insert-after (list-store &optional sibling) nil
  (list-store list-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (sibling (or null tree-iter)))

(defbinding list-store-prepend () nil
  (list-store list-store)
  ((make-instance 'tree-iter) tree-iter :in-out))

(defbinding list-store-append () nil
  (list-store list-store)
  ((make-instance 'tree-iter) tree-iter :in-out))

(defbinding list-store-clear () nil
  (list-store list-store))

(defbinding list-store-reorder () nil
  (list-store list-store)
  (new-order (vector int)))

(defbinding list-store-swap () nil
  (list-store list-store)
  (a tree-iter)
  (b tree-iter))

(defbinding list-store-move-before () nil
  (list-store list-store)
  (iter tree-iter)
  (psoition (or null tree-iter)))


(defbinding list-store-move-after () nil
  (list-store list-store)
  (iter tree-iter)
  (psoition tree-iter))


;;; Tree Model



;;; Tree Store

(defbinding %tree-store-set-column-types () nil
  (tree-store tree-store)
  (n-columns unsigned-int)
  (columns (vector type-number)))

(defmethod initialize-instance ((tree-store tree-store) &key columns)
  (call-next-method)
  (%tree-store-set-column-types tree-store (length columns)
   (map 'vector #'find-type-number columns)))


(defbinding %tree-store-set-value () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (column int)
  (value gvalue))

(defbinding tree-store-remove () boolean
  (tree-store tree-store)
  (tree-iter tree-iter))

(defbinding tree-store-insert (tree-store position &optional parent) nil
  (tree-store tree-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (parent (or null tree-iter))
  (position int))

(defbinding tree-store-insert-before (tree-store &optional parent sibling) nil
  (tree-store tree-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (parent (or null tree-iter))
  (sibling (or null tree-iter)))

(defbinding tree-store-insert-after (tree-store &optional parent sibling) nil
  (tree-store tree-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (parent (or null tree-iter))
  (sibling (or null tree-iter)))

(defbinding tree-store-prepend (tree-store &optional parent) nil
  (tree-store tree-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (parent (or null tree-iter)))

(defbinding tree-store-append (tree-store &optional parent) nil
  (tree-store tree-store)
  ((make-instance 'tree-iter) tree-iter :in-out)
  (parent (or null tree-iter)))

(defbinding (tree-store-is-ancestor-p "gtk_tree_store_is_ancestor") () boolean
  (tree-store tree-store)
  (tree-iter tree-iter)
  (descendant tree-iter))

(defbinding tree-store-iter-depth () int
  (tree-store tree-store)
  (tree-iter tree-iter))

(defbinding tree-store-clear () nil
  (tree-store tree-store))

(defbinding tree-store-reorder () nil
  (tree-store tree-store)
  (parent tree-iter)
  (new-order (vector int)))

(defbinding tree-store-swap () nil
  (tree-store tree-store)
  (a tree-iter)
  (b tree-iter))

(defbinding tree-store-move-before () nil
  (tree-store tree-store)
  (iter tree-iter)
  (psoition (or null tree-iter)))


(defbinding tree-store-move-after () nil
  (tree-store tree-store)
  (iter tree-iter)
  (psoition tree-iter))



;;; Tree View

(defbinding tree-view-get-selection () tree-selection
  (tree-view tree-view))

(defbinding tree-view-columns-autosize () nil
  (tree-view tree-view))

(defbinding tree-view-append-column () int
  (tree-view tree-view)
  (tree-view-column tree-view-column))

(defbinding tree-view-remove-column () int
  (tree-view tree-view)
  (tree-view-column tree-view-column))

(defbinding tree-view-insert-column (view columnd position) int
  (view tree-view)
  (column tree-view-column)
  ((if (eq position :end) -1 position) int))

(defbinding tree-view-get-column () tree-view-column
  (tree-view tree-view)
  (position int))

(defbinding tree-view-move-column-after () nil
  (tree-view tree-view)
  (column tree-view-column)
  (base-column (or null tree-view-column)))

;;(defbinding tree-view-set-column drag-function ...)

(defbinding tree-view-scroll-to-point () nil
  (tree-view tree-view)
  (tree-x int)
  (tree-y int))

(defbinding tree-view-scroll-to-cell () nil
  (tree-view tree-view)
  (path (or null tree-path))
  (column (or null tree-view-column))
  (use-align boolean)
  (row-align single-float)
  (col-align single-float))

(defbinding tree-view-set-cursor () nil
  (tree-view tree-view)
  (path tree-path)
  (focus-column tree-view-column)
  (start-editing boolean))

(defbinding tree-view-set-cursor-on-cell () nil
  (tree-view tree-view)
  (path tree-path)
  (focus-column (or null tree-view-column))
  (focus-cell (or null cell-renderer))
  (start-editing boolean))

(defbinding tree-view-get-cursor () nil
  (tree-view tree-view)
  (path tree-path :out )
  (focus-column tree-view-column :out))

(defbinding tree-view-row-activated () nil
  (tree-view tree-view)
  (path tree-path )
  (column tree-view-column))

(defbinding tree-view-expand-all () nil
  (tree-view tree-view))

(defbinding tree-view-collapse-all () nil
  (tree-view tree-view))

(defbinding tree-view-expand-to-path () nil
  (tree-view tree-view)
  (path tree-path))

(defbinding tree-view-expand-row () nil
  (tree-view tree-view)
  (path tree-path)
  (open-all boolean))

(defbinding tree-view-collapse-row () nil
  (tree-view tree-view)
  (path tree-path))

(def-callback-marshal %tree-view-mapping-func (nil tree-view tree-path))

(defbinding %tree-view-map-expanded-rows () nil
  (tree-view tree-view)
  ((callback %tree-view-mapping-func) pointer)
  (callback-id unsigned-int))

(defun map-expanded-rows (function tree-view)
  (with-callback-function (id function)
    (%tree-view-map-expanded-rows tree-view id)))

(defbinding (tree-view-row-expanded-p "gtk_tree_view_row_expanded") () boolean
  (tree-view tree-view)
  (path tree-path))

(defbinding tree-view-get-path-at-pos 
    (tree-view x y &optional (cell-x 0) (cell-y 0)) boolean
  (tree-view tree-view)
  (x int)
  (y int)
  (path tree-path :out)
  (column tree-view-column :out)
  (cell-x int)
  (cell-y int))

(defbinding tree-view-get-cell-area () nil
  (tree-view tree-view)
  (path (or null tree-path))
  (column (or null tree-view-column))
  ((make-instance 'gdk:rectangle) gdk:rectangle :in-out))

(defbinding tree-view-get-background-area () nil
  (tree-view tree-view)
  (path (or null tree-path))
  (column (or null tree-view-column))
  ((make-instance 'gdk:rectangle) gdk:rectangle :in-out))

(defbinding tree-view-get-visible-rect () nil
  (tree-view tree-view)
  ((make-instance 'gdk:rectangle) gdk:rectangle :in-out))

;; and many more functions which we'll add later

