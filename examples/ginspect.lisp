(in-package :gtk)

(defgeneric insert-object (object store parent &optional prefix))
(defgeneric insert-parts (object store parent))
(defgeneric object-hash-parts-p (object))


(defun ginspect (object)
  (let* ((store (make-instance 'tree-store 
		 :column-types '(string string gobject boolean)
		 :column-names '(prefix pprint object expanded)))
	 (view (make-instance 'tree-view :model store :headers-visible nil)))

    (let ((column (make-instance 'tree-view-column))
 	  (prefix (make-instance 'cell-renderer-text))
 	  (object (make-instance 'cell-renderer-text)))	  
      (cell-layout-pack column prefix :expand nil)
      (cell-layout-add-attribute column prefix 'text 0)
      (cell-layout-pack column object :expand t)
      (cell-layout-add-attribute column object 'text 1)
      (tree-view-append-column view column))

    (insert-object object store nil)

    (signal-connect view 'row-expanded 
     #'(lambda (iter path)
	 (unless (tree-model-column-value store iter 'expanded)
	   (multiple-value-bind (valid dummy)
	       (tree-model-iter-children store iter)
	     ;; Remove dummy child
	     (when valid
	       (tree-store-remove store dummy)))
	   (let ((gobject (tree-model-column-value store iter 'object)))
	     (insert-parts (object-data gobject 'object) store iter))
	   (setf (tree-model-column-value store iter 'expanded) t)
	   (tree-view-expand-row view path nil))))

    (make-instance 'dialog
     :title "Object Inspector" :show-all t
     :default-width 600 :default-height 600
     :button (list "gtk-close" #'widget-destroy :object t)
     :child (make-instance 'scrolled-window 
	     :hscrollbar-policy :automatic :child view))))


(defun object-to-string (object)
  (with-output-to-string (stream)
    (write object :stream stream :lines 1 :right-margin 80)))

(defmethod insert-object ((object t) store parent &optional (prefix ""))
  (let ((gobject (make-instance 'gobject)) ; to "hang" the lisp object on
	(has-parts (object-has-parts-p object)))
    (setf (object-data gobject 'object) object)
    (let ((iter (tree-store-append store parent 
		 (vector prefix (object-to-string object) 
			 gobject (not has-parts)))))
      (when has-parts
	;; Insert dummy child
	(tree-store-append store iter (vector "" "" gobject t))))))

(defmethod object-has-parts-p ((object t))
  (declare (ignore object))
  t)

(defmethod insert-parts ((object t) store parent)
  (let ((parts (nth-value 1 (swank-backend:inspected-parts object))))
    (unless (and (endp (rest parts)) (eq object (cdar parts)))
      (loop
       for (prefix . part) in parts
       do (insert-object part store parent prefix)))))

(defun propper-list-p (object)
  (and (listp object) (null (cdr (last object)))))

(defmethod insert-parts ((object cons) store parent)
  (if (propper-list-p object)
      (loop
       for element in object
       do (insert-object element store parent))
    (progn
      (insert-object (car object) store parent)
      (insert-object (cdr object) store parent))))

(defmethod insert-parts ((object vector) store parent)
  (loop
   for element across object
   do (insert-object element store parent)))

(defmethod insert-parts ((object (eql t)) store parent)
  (declare (ignore object store parent)))

(defmethod object-has-parts-p ((object (eql t)))
  (declare (ignore object))
  nil)

(defmethod insert-parts ((object (eql nil)) store parent)
  (declare (ignore object store parent)))

(defmethod object-has-parts-p ((object (eql nil)))
  (declare (ignore object))
  nil)

(defvar *unbound-object-marker* (gensym "UNBOUND-OBJECT-"))

(defmethod insert-parts ((object symbol) store parent)
  (insert-object 
   (if (boundp object)
       (symbol-value object)
     *unbound-object-marker*)
   store parent "Value")
  (insert-object 
   (if (fboundp object)
       (symbol-function object)
     *unbound-object-marker*)
   store parent "Function")
  (insert-object (symbol-plist object) store parent "Plist")
  (insert-object (symbol-package object) store parent "Package"))


(defmethod insert-parts ((object standard-object) store parent)
  (loop
   for slotd in (class-slots (class-of object))
   do (let* ((slot-name (slot-value slotd 'pcl::name))
	     (slot-value (if (slot-boundp object slot-name)
			     (slot-value object slot-name)
			   *unbound-object-marker*)))
	    (insert-object slot-value store parent (string slot-name)))))
	  
(defmethod insert-object ((object (eql *unbound-object-marker*))
			   store parent &optional prefix)
  (tree-store-append store parent (vector prefix "<unbound>" (make-instance 'gobject) t nil)))

(defmethod insert-parts ((object (eql *unbound-object-marker*)) store parent)
  (declare (ignore object store parent)))

(defmethod object-has-parts-p ((object character))
  (declare (ignore object))
  nil)

(defmethod object-has-parts-p ((object number))
  (declare (ignore object))
  nil)

(defmethod object-has-parts-p ((object alien:system-area-pointer))
  (declare (ignore object))
  nil)
