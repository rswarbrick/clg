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

;; $Id: gtktree.lisp,v 1.6 2005-02-03 23:09:09 espen Exp $


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
    (nil cell-layout cell-renderer tree-model (copy-of tree-iter)))

(defbinding cell-layout-set-cell-data-func (cell-layout cell function) nil
  (cell-layout cell-layout)
  (cell cell-renderer)
  ((callback %cell-layout-data-func) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback user-data-destroy-func) pointer))

(defbinding cell-layout-clear-attributes () nil
  (cell-layout cell-layout)
  (cell cell-renderer))



;;;; List Store

(defmethod initialize-instance ((list-store list-store) &key column-types
				column-names initial-content)
  (call-next-method)
  (%list-store-set-column-types list-store column-types)
  (when column-names
    (setf (object-data list-store 'column-names) column-names))
  (when initial-content
    (loop
     with iter = (make-instance 'tree-iter)
     for row in initial-content
     do (list-store-append list-store row iter))))


(defmethod column-setter-name ((list-store list-store))
  (declare (ignore list-store))
  "gtk_list_store_set")

(defbinding %list-store-set-column-types () nil
  (list-store list-store)
  ((length columns) unsigned-int)
  (columns (vector gtype)))

(defbinding %list-store-remove () boolean
  (list-store list-store)
  (tree-iter tree-iter))

(defun list-store-remove (store row)
  (etypecase row
    (tree-iter 
     (%list-store-remove store row))
    (tree-path 
     (multiple-value-bind (valid iter) (tree-model-get-iter store row)
       (if valid
	   (%list-store-remove store iter)
	   (error "~A not poiniting to av valid iterator in ~A" row store))))
    (tree-row-reference 
     (let ((path (tree-row-reference-get-path row)))
       (if path
	   (list-store-remove store path)
	 (error "~A not valid" row))))))


(defbinding %list-store-insert () nil
  (list-store list-store)
  (tree-iter tree-iter)
  (position int))

(defun list-store-insert
    (store position &optional data (iter (make-instance 'tree-iter)))
  (%list-store-insert store iter position)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %list-store-insert-before () nil
  (list-store list-store)
  (tree-iter tree-iter)
  (sibling (or null tree-iter)))

(defun list-store-insert-before
    (store sibling &optional data (iter (make-instance 'tree-iter)))
  (%list-store-insert-before store iter sibling)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %list-store-insert-after 
    (list-store &optional sibling (tree-iter (make-instance 'tree-iter))) nil
  (list-store list-store)
  (tree-iter tree-iter)
  (sibling (or null tree-iter)))

(defun list-store-insert-after
    (store sibling &optional data (iter (make-instance 'tree-iter)))
  (%list-store-insert-after store iter sibling)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %list-store-prepend () nil
  (list-store list-store)
  (tree-iter tree-iter))

(defun list-store-prepend 
    (store &optional data (iter (make-instance 'tree-iter)))
  (%list-store-prepend store iter)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %list-store-append () nil
  (list-store list-store)
  (tree-iter tree-iter))

(defun list-store-append 
    (store &optional data (iter (make-instance 'tree-iter)))
  (%list-store-append store iter)
  (when data (%tree-model-set store iter data))
  iter)

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

(defbinding %tree-path-free () nil
  (location pointer))

(defbinding %tree-path-get-indices () pointer
  (location pointer))

(defbinding %tree-path-get-depth () int
  (location pointer))

(defun %make-tree-path (path)
  (let ((c-vector (make-c-vector 'int (length path) path))
	(location (allocate-memory (+ (size-of 'int) (size-of 'pointer)))))
    (funcall (writer-function 'int) (length path) location)
    (funcall (writer-function 'pointer) c-vector location (size-of 'int))
    location))

(defun %tree-path-to-vector (location)
  (let ((indices (%tree-path-get-indices location))
	(depth (%tree-path-get-depth location)))
    (if (null-pointer-p indices)
	#()
      (map-c-vector 'vector #'identity indices 'int depth))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod alien-type ((type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    (alien-type 'pointer))
  
  (defmethod size-of ((type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    (size-of 'pointer))
  
  (defmethod to-alien-form (path (type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    `(%make-tree-path ,path))
  
  (defmethod from-alien-form (location (type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    `(let ((location ,location))
       (prog1
           (%tree-path-to-vector location)
	 (%tree-path-free location))))
  
  (defmethod copy-from-alien-form (location (type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    `(%tree-path-to-vector ,location))
  
  (defmethod cleanup-form (location (type (eql 'tree-path)) &rest args)
    (declare (ignore type args))
    `(%tree-path-free ,location)))

(defmethod to-alien-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  #'%make-tree-path)
  
(defmethod from-alien-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  #'(lambda (location)
      (prog1
	  (%tree-path-to-vector location)
	(%tree-path-free location))))

(defmethod copy-from-alien-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  #'%tree-path-to-vector)
  
(defmethod cleanup-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  #'%tree-path-free)

(defmethod writer-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  (let ((writer (writer-function 'pointer)))
    #'(lambda (path location &optional (offset 0))
	(funcall writer (%make-tree-path path) location offset))))

(defmethod reader-function ((type (eql 'tree-path)) &rest args)
  (declare (ignore type args))
  (let ((reader (reader-function 'pointer)))
    #'(lambda (location &optional (offset 0))
	(%tree-path-to-vector (funcall reader location offset)))))


(defbinding %tree-row-reference-new () pointer
  (model tree-model)
  (path tree-path))

(defmethod initialize-instance ((reference tree-row-reference) &key model path)
  (setf
   (slot-value reference 'location)
   (%tree-row-reference-new model path))
  (call-next-method))

(defbinding tree-row-reference-get-path () tree-path
  (reference tree-row-reference))

(defbinding (tree-row-reference-valid-p "gtk_tree_row_reference_valid") () boolean
  (reference tree-row-reference))


(defbinding tree-model-get-column-type () gtype ;type-number
  (tree-model tree-model)
  (index int))

(defbinding tree-model-get-iter 
    (model path &optional (iter (make-instance 'tree-iter))) boolean
  (model tree-model)
  (iter tree-iter :return)
  (path tree-path))
 
(defbinding tree-model-get-path () tree-path
  (tree-model tree-model)
  (iter tree-iter))

(defbinding %tree-model-get-value () nil
  (tree-model tree-model)
  (iter tree-iter)
  (column int)
  (gvalue gvalue))

(defun tree-model-column-value (model iter column)
  (let ((index (column-index model column)))
    (with-gvalue (gvalue)
      (%tree-model-get-value model iter index gvalue))))

(defbinding tree-model-iter-next () boolean
  (tree-model tree-model)
  (iter tree-iter :return))

(defbinding tree-model-iter-children 
    (tree-model parent &optional (iter (make-instance 'tree-iter))) boolean
  (tree-model tree-model)
  (iter tree-iter :return)
  (parent (or null tree-iter)))

(defbinding (tree-model-iter-has-child-p "gtk_tree_model_iter_has_child") 
    () boolean
  (tree-model tree-model)
  (iter tree-iter))

(defbinding tree-model-iter-n-children () int
  (tree-model tree-model)
  (iter tree-iter))

(defbinding tree-model-iter-nth-child
    (tree-model parent n &optional (iter (make-instance 'tree-iter))) boolean
  (tree-model tree-model)
  (iter tree-iter :return)
  (parent (or null tree-iter))
  (n int))

(defbinding tree-model-iter-parent
    (tree-model child &optional (iter (make-instance 'tree-iter))) boolean
  (tree-model tree-model)
  (iter tree-iter :return)
  (child tree-iter))

(def-callback-marshal %tree-model-foreach-func 
  (boolean tree-model (path (copy-of tree-path)) (iter (copy-of tree-iter))))

(defbinding %tree-model-foreach () nil
  (tree-model tree-model)
  ((callback %tree-model-foreach-func) pointer)
  (callback-id unsigned-int))

(defun tree-model-foreach (model function)
  (with-callback-function (id function)
    (%tree-model-foreach model id)))

(defbinding tree-model-row-changed () nil
  (tree-model tree-model)
  (path tree-path)
  (iter tree-iter))

(defbinding tree-model-row-inserted () nil
  (tree-model tree-model)
  (path tree-path)
  (iter tree-iter))

(defbinding tree-model-row-has-child-toggled () nil
  (tree-model tree-model)
  (path tree-path)
  (iter tree-iter))

(defbinding tree-model-row-deleted () nil
  (tree-model tree-model)
  (path tree-path)
  (iter tree-iter))

(defbinding tree-model-rows-reordered () nil
  (tree-model tree-model)
  (path tree-path)
  (iter tree-iter)
  (new-order int))


(defun column-types (model columns)
  (map 'vector 
       #'(lambda (column)
	   (find-type-number (first (mklist column))))
       columns))
	       
(defun column-index (model column)
  (or
   (etypecase column
     (number column)
     (symbol (position column (object-data model 'column-names)))
     (string (position column (object-data model 'column-names)
		   :test #'string=)))
   (error "~A has no column ~S" model column)))

(defun tree-model-column-value-setter (model column)
  (let ((setters (or
		  (object-data model 'column-setters)
		  (setf 
		   (object-data model 'column-setters)
		   (make-array (tree-model-n-columns model) 
		    :initial-element nil)))))
    (let ((index (column-index model column)))
    (or
     (svref setters index)
     (setf 
      (svref setters index)
      (let ((setter 
	     (mkbinding (column-setter-name model)
	      nil (type-of model) 'tree-iter 'int
;	      (type-from-number (tree-model-get-column-type model index))
	      (tree-model-get-column-type model index)
	      'int)))
	#'(lambda (value iter)
	    (funcall setter model iter index value -1))))))))

(defun tree-model-row-setter (model)
  (or 
   (object-data model 'row-setter)
   (progn
     ;; This will create any missing column setter
     (loop 
      for i from 0 below (tree-model-n-columns model)
      do (tree-model-column-value-setter model i))
     (let ((setters (object-data model 'column-setters)))
       (setf    
	(object-data model 'row-setter)
	#'(lambda (row iter)
	    (map nil #'(lambda (value setter)
			 (funcall setter value iter))
		 row setters)))))))

(defun (setf tree-model-column-value) (value model iter column)
  (funcall (tree-model-column-value-setter model column) value iter)
  value)

(defun (setf tree-model-row-data) (data model iter)
  (funcall (tree-model-row-setter model) data iter)
  data)

(defun %tree-model-set (model iter data)
  (etypecase data
    (vector (setf (tree-model-row-data model iter) data))
    (cons 
     (loop
      as (column value . rest) = data then rest
      do (setf (tree-model-column-value model iter column) value)
      while rest))))


;;; Tree Selection

(def-callback-marshal %tree-selection-func (boolean tree-selection tree-model (path (copy-of tree-path)) (path-currently-selected boolean)))

(defbinding tree-selection-set-select-function (selection function) nil
  (selection tree-selection)
  ((callback %tree-selection-func) pointer)
  ((register-callback-function function) unsigned-int)
  ((callback user-data-destroy-func) pointer))

(defbinding tree-selection-get-selected 
    (selection &optional (iter (make-instance 'tree-iter))) boolean
  (selection tree-selection)
  (nil null) 
  (iter tree-iter :return))

(def-callback-marshal %tree-selection-foreach-func (nil tree-model (path (copy-of tree-path)) (iter (copy-of tree-iter))))

(defbinding %tree-selection-selected-foreach () nil
  (tree-selection tree-selection)
  ((callback %tree-selection-foreach-func) pointer)
  (callback-id unsigned-int))

(defun tree-selection-selected-foreach (selection function)
  (with-callback-function (id function)
    (%tree-selection-selected-foreach selection id)))

(defbinding tree-selection-get-selected-rows () (glist tree-path)
  (tree-selection tree-selection)
  (nil null))

(defbinding tree-selection-count-selected-rows () int
  (tree-selection tree-selection))

(defbinding %tree-selection-select-path () nil
  (tree-selection tree-selection)
  (tree-path tree-path))

(defbinding %tree-selection-unselect-path () nil
  (tree-selection tree-selection)
  (tree-path tree-path))

(defbinding %tree-selection-path-is-selected () boolean
  (tree-selection tree-selection)
  (tree-path tree-path))

(defbinding %tree-selection-select-iter () nil
  (tree-selection tree-selection)
  (tree-path tree-path))

(defbinding %tree-selection-unselect-iter () nil
  (tree-selection tree-selection)
  (tree-path tree-path))

(defbinding %tree-selection-iter-is-selected () boolean
  (tree-selection tree-selection)
  (tree-path tree-path))

(defun tree-selection-select (selection row)
  (etypecase row
    (tree-path (%tree-selection-select-path selection row))
    (tree-iter (%tree-selection-select-iter selection row))))

(defun tree-selection-unselect (selection row)
  (etypecase row
    (tree-path (%tree-selection-unselect-path selection row))
    (tree-iter (%tree-selection-unselect-iter selection row))))

(defun tree-selection-is-selected-p (selection row)
  (etypecase row
    (tree-path (%tree-selection-path-is-selected selection row))
    (tree-iter (%tree-selection-iter-is-selected selection row))))

(defbinding tree-selection-select-all () nil
  (tree-selection tree-selection))

(defbinding tree-selection-unselect-all () nil
  (tree-selection tree-selection))

(defbinding tree-selection-select-range () nil
  (tree-selection tree-selection)
  (start tree-path)
  (end tree-path))

(defbinding tree-selection-unselect-range () nil
  (tree-selection tree-selection)
  (start tree-path)
  (end tree-path))



;;; Tree Store

(defbinding %tree-store-set-column-types () nil
  (tree-store tree-store)
  ((length columns) unsigned-int)
  (columns (vector gtype)))

(defmethod initialize-instance ((tree-store tree-store) &key column-types
				column-names)
  (call-next-method)
  (%tree-store-set-column-types tree-store column-types)
  (when column-names
    (setf (object-data tree-store 'column-names) column-names)))

(defmethod column-setter-name ((tree-store tree-store))
  (declare (ignore tree-store))
  "gtk_tree_store_set")

(defbinding tree-store-remove () boolean
  (tree-store tree-store)
  (tree-iter tree-iter))

(defbinding %tree-store-insert () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (parent (or null tree-iter))
  (position int))

(defun tree-store-insert 
    (store parent position &optional data (iter (make-instance 'tree-iter)))
  (%tree-store-insert store iter parent position)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %tree-store-insert-before () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (parent (or null tree-iter))
  (sibling (or null tree-iter)))

(defun tree-store-insert-before 
    (store parent sibling &optional data (iter (make-instance 'tree-iter)))
  (%tree-store-insert-before store iter parent sibling)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %tree-store-insert-after () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (parent (or null tree-iter))
  (sibling (or null tree-iter)))

(defun tree-store-insert-after 
    (store parent sibling &optional data (iter (make-instance 'tree-iter)))
  (%tree-store-insert-after store iter parent sibling)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %tree-store-prepend () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (parent (or null tree-iter)))

(defun tree-store-prepend 
    (store parent &optional data (iter (make-instance 'tree-iter)))
  (%tree-store-prepend store iter parent)
  (when data (%tree-model-set store iter data))
  iter)

(defbinding %tree-store-append () nil
  (tree-store tree-store)
  (tree-iter tree-iter)
  (parent (or null tree-iter)))

(defun tree-store-append 
    (store parent &optional data (iter (make-instance 'tree-iter)))
  (%tree-store-append store iter parent)
  (when data (%tree-model-set store iter data))
  iter)

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

(defmethod initialize-instance ((tree-view tree-view) &rest initargs 
				&key column)
  (call-next-method)
  (mapc #'(lambda (column)
	    (tree-view-append-column tree-view column))
	(get-all initargs :column)))


(defbinding tree-view-columns-autosize () nil
  (tree-view tree-view))

(defbinding tree-view-append-column () int
  (tree-view tree-view)
  (tree-view-column tree-view-column))

(defbinding tree-view-remove-column () int
  (tree-view tree-view)
  (tree-view-column tree-view-column))

(defbinding tree-view-insert-column (view column position) int
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

(def-callback-marshal %tree-view-mapping-func (nil tree-view (path (copy-of tree-path))))

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
  ((make-instance 'gdk:rectangle) gdk:rectangle :return))

(defbinding tree-view-get-background-area () nil
  (tree-view tree-view)
  (path (or null tree-path))
  (column (or null tree-view-column))
  ((make-instance 'gdk:rectangle) gdk:rectangle :return))

(defbinding tree-view-get-visible-rect () nil
  (tree-view tree-view)
  ((make-instance 'gdk:rectangle) gdk:rectangle :return))

;; and many more functions which we'll add later


;;; Tree View Column

