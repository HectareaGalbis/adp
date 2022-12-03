
(in-package :adppvt)


(defstruct table-element
  (elements nil :type (vector element))
  (used nil :type boolean))


(defclass tag-table ()
  ((table :initform (make-hash-table)
	  :type hash-table))
  (:documentation
   "Relates a tag with an element."))


(defun tag-table-tag-p (tag-table tag)
  "Check if a tag is present in the tag-table."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (nth-value 1 (gethash tag table))))


(defun tag-table-set-element (tag-table tag element)
  "Associate an element with a tag. Multiple elements can be associated with the same tag."
  (declare (type tag-table tag-table) (type symbol tag) (type element element))
  (with-slots (table) tag-table
    (setf (gethash tag table) (make-table-element :elements (vector element)))))


(defun tag-table-push-element (tag-table tag element)
  "Associate an element with a tag. Multiple elements can be associated with the same tag."
  (declare (type tag-table tag-table) (type symbol tag) (type element element))
  (with-slots (table) tag-table
    (if (tag-table-tag-p tag-table tag)
	(vector-push-extend element (table-element-elements (gethash tag table)))
	(setf (gethash tag table) (make-table-element :elements (vector element))))))


(defun tag-table-find-elements (tag-table tag)
  "Retrieve the elements associated with a tag and if the elements were certainly found. If the tag is not
present, return nil."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (multiple-value-bind (possible-element foundp) (gethash tag table)
      (and foundp
	   (values (table-element-elements posible-element) foundp)))))


(defun tag-table-element-using-tag (tag-table tag)
  "Retrieve the elements associated with a tag and mark the tag as used. If the tag is not present, return nil."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (multiple-value-bind (possible-element foundp) (gethash tag table)
      (and foundp
	   (progn
	     (setf (table-element-used possible-element) t)
	     (table-element-eleemnts posible-element))))))


(defmacro with-tag-elements ((tag element) tag-table &body body)
  (with-gensyms (table)
    `(with-slots ((,table table)) ,tag-table
       (loop for ,tag being the hash-key in ,table using hash-value ,element
	     do ,@body))))
