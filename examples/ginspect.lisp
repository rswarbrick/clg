(in-package :gtk)

(defvar *ginspect-unbound-object-marker* 
  #+cmu (gensym "UNBOUND-OBJECT-")
  #+sbcl sb-impl::*inspect-unbound-object-marker*)


(defgeneric insert-object (object store parent &optional prefix))
(defgeneric insert-parts (object store parent))
(defgeneric object-has-parts-p (object))
(defgeneric decompose-describe-object (object))


(defun ginspect (object)
  (let* ((store (make-instance 'tree-store 
		 :column-types '(string string gobject boolean)
		 :column-names '(name pprinted object expanded)))
	 (view (make-instance 'tree-view :model store :headers-visible nil)))

    (let ((column (make-instance 'tree-view-column))
 	  (name (make-instance 'cell-renderer-text))
 	  (object (make-instance 'cell-renderer-text)))	  
      (tree-view-append-column view column)
      (cell-layout-pack column name :expand nil)
      (cell-layout-add-attribute column name 'text 'name store)
      (cell-layout-pack column object :expand t)
      (cell-layout-add-attribute column object 'text 'pprinted store))

    (insert-object object store nil)

    (signal-connect view 'row-expanded 
     #'(lambda (iter path)
	 (when (setf 
		(tree-model-column-value store iter 'expanded)
		(not (tree-model-column-value store iter 'expanded)))
	   (multiple-value-bind (valid child-iter) 
	       (tree-model-iter-children store iter)
	     ;; Remove old children
	     (when valid
	       (loop while (tree-store-remove store child-iter))))
	   (let ((gobject (tree-model-column-value store iter 'object)))
	     (insert-parts (object-data gobject 'object) store iter))
	   (tree-view-expand-row view path nil))))

    (make-instance 'dialog
     :title "Object Inspector" :show-children t :visible t
     :default-width 600 :default-height 600
     :button (list "gtk-close" #'widget-destroy :object t)
     :child (make-instance 'scrolled-window 
	     :hscrollbar-policy :automatic :child view))))


(defmethod decompose-describe-object ((object t))
  #+cmu
  (destructuring-bind (description named-p &rest parts) 
      (inspect::describe-parts object)
    (if (equal parts (list object))
	(values description nil nil)
      (values description named-p parts)))
  (sb-impl::inspected-parts object))

(defmethod decompose-describe-object ((object (eql t)))
  (values (call-next-method) nil nil))

(defmethod decompose-describe-object ((object (eql nil)))
  (values (call-next-method) nil nil))

(defun propper-list-p (object)
  (and (listp object) (null (cdr (last object)))))

(defmethod decompose-describe-object ((object cons))
  (if (propper-list-p object)
      (values (call-next-method) nil object)
    (values "The object is a CONS." nil (list (car object) (cdr object)))))

(defmethod decompose-describe-object ((object #+cmu alien:system-area-pointer
					      #+sbcl sb-alien:system-area-pointer))
  (values "The object is a SYSTEM-AREA-POINTER" nil nil))

(defmethod decompose-describe-object ((object (eql *ginspect-unbound-object-marker*)))
  (values "The slot is unbound" nil nil))

#+cmu
(defmethod decompose-describe-object ((object symbol))
  (values 
   (call-next-method) t
   (cons "Name" (symbol-name object)) 
   (cons "Package" (symbol-package objecy))
   (cons "Value" (if (boundp object)
		     (symbol-value object)
		   *ginspect-unbound-object-marker*))
   (cons "Function" (if (fboundp object)
			(symbol-function  object)
		      *ginspect-unbound-object-marker*))
   (cons "Plist" (symbol-plist object))))

#+cmu
(defmethod decompose-describe-object ((object standard-object))
  (values 
   (call-next-method) t
   (loop
    for slotd in (class-slots (class-of object))
    collect (let* ((slot-name (pcl:slot-definition-name slotd))
		   (slot-value (if (slot-boundp object slot-name)
				   (slot-value object slot-name)
				 *ginspect-unbound-object-marker*)))
	      (cons (string slot-name) slot-value)))))


(defmethod object-has-parts-p ((object t))
  (nth-value 2 (decompose-describe-object object)))

(defmethod object-has-parts-p ((object cons))
  t)

(defmethod object-has-parts-p ((object standard-object))
  (class-slots (class-of object)))

(defmethod object-has-parts-p ((object vector))
  (not (zerop (length object))))


(defmethod object-to-string ((object t))
  (with-output-to-string (stream)
    (write object :stream stream :lines 1 :right-margin 80)))

(defmethod object-to-string ((object (eql *ginspect-unbound-object-marker*)))
  "<unbound>")

(defmethod insert-object ((object t) store parent &optional (name ""))
  (let ((gobject (make-instance 'gobject)) ; to "hang" the lisp object on
	(has-parts (object-has-parts-p object)))
    (setf (object-data gobject 'object) object)
    (let ((iter (tree-store-append store parent 
		 (vector name (object-to-string object) 
			 gobject (not has-parts)))))
      (when has-parts
	;; Insert dummy child
	(tree-store-append store iter (vector "" "" gobject t))))))

(defmethod insert-parts :around ((object t) store parent)
  (when (object-has-parts-p object)
    (call-next-method)))

(defmethod insert-parts ((object t) store parent)
  (multiple-value-bind (description named-p parts) 
      (decompose-describe-object object)
    (declare (ignore description))
    (loop
     for part in parts
     do (if named-p
	    (insert-object (cdr part) store parent (string (car part)))
	  (insert-object part store parent)))))


(defun ginspect-toplevels ()
  (ginspect (window-list-toplevels)))
