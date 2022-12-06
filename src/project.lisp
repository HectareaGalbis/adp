
(in-package :adppvt)


(defclass project ()
  ((root-directory :initarg :root-directory
		   :type pathname)
   (files :initform (make-array 10 :adjustable t :fill-pointer 0 :element-type 'file)
	  :type (vector file))
   (current-file :type pathname)
   (header-tags :initform (make-instance 'tag-table)
		:type tag-table)
   (symbol-tags :initform (make-instance 'tag-table)
		:type tag-table)
   (function-tags :initform (make-instance 'tag-table)
		  :type tag-table)
   (type-tags :initform (make-instance 'tag-table)
	      :type tag-table)
   (code-tags :initform (make-instance 'tag-table)
	      :type tag-table)))
