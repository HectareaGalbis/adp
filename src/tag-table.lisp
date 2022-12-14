
(in-package :adppvt)


(defstruct table-element
  (elements nil :type (vector element))
  (used nil :type boolean))

(defclass tag-table ()
  ((table :initform (make-hash-table)
	  :type hash-table))
  (:documentation
   "Relates a tag with an element."))


(defmacro define-with-tag-tables (name &body tag-tables)
  (with-gensyms (body let-bindings table)
    `(defmacro ,name (&body ,body)
       (let ((,let-bindings (mapcar (lambda (,table)
				      `(,,table (make-instance 'tag-table)))
				    ',tag-tables)))
	 `(let ,,let-bindings
	    ,@,body)))))

(defmacro define-tag-tables (with-tag-tables-name &body names)
  (let ((definitions (mapcar (lambda (name)
			       `(defvar ,name nil))
			     names)))
    `(progn
       ,@definitions
       (define-with-tag-tables ,with-tag-tables-name
	 ,@names))))

(define-tag-tables with-tag-tables
  *header-tags*
  *symbol-tags*
  *function-tags*
  *type-tags*
  *code-tags*)


(defun tag-table-tags (tag-table)
  "Return the list of tags from a tag-table."
  (declare (type tag-table tag-table))
  (with-slots (table) tag-table
    (loop for tag being the hash-key of table
	  collect tag)))


(defun tag-table-unused-elements (tag-table)
  "Return the list of unused elements."
  (declare (type tag-table tag-table))
  (with-slots (table) tag-table
    (loop for element being the hash-value of table
	  if (not (table-element-used element))
	    collect (table-element-elements element))))


(defun tag-table-tag-p (tag-table tag)
  "Check if a tag is present in the tag-table."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (nth-value 1 (gethash tag table))))


(defun tag-table-set-element (tag-table tag element)
  "Associate an element with a tag. Multiple elements can be associated with the same tag."
  (declare (type tag-table tag-table) (type symbol tag) (type element element))
  (with-slots (table) tag-table
    (let ((new-array (make-array 1 :adjustable t :fill-pointer 0)))
      (vector-push-extend element new-array)
      (setf (gethash tag table) (make-table-element :elements new-array)))))


(defun tag-table-push-element (tag-table tag element)
  "Associate an element with a tag. Multiple elements can be associated with the same tag."
  (declare (type tag-table tag-table) (type symbol tag) (type element element))
  (with-slots (table) tag-table
    (if (tag-table-tag-p tag-table tag)
	(vector-push-extend element (table-element-elements (gethash tag table)))
	(let ((new-array (make-array 1 :adjustable t :fill-pointer 0)))
	  (vector-push-extend element new-array)
	  (setf (gethash tag table) (make-table-element :elements new-array))))))


(defun tag-table-find-elements (tag-table tag)
  "Retrieve the elements associated with a tag and if the elements were certainly found. If the tag is not
present, return nil."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (multiple-value-bind (possible-element foundp) (gethash tag table)
      (and foundp
	   (values (table-element-elements possible-element) foundp)))))


(defun tag-table-find-elements-using-tag (tag-table tag)
  "Retrieve the elements associated with a tag and mark the tag as used. If the tag is not present, return nil."
  (declare (type tag-table tag-table) (type symbol tag))
  (with-slots (table) tag-table
    (multiple-value-bind (possible-element foundp) (gethash tag table)
      (and foundp
	   (progn
	     (setf (table-element-used possible-element) t)
	     (values (table-element-elements possible-element) foundp))))))

