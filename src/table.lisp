
(in-package :adppvt)


(defclass table (element)
  ((rows :initarg :rows
	 :type list))
  (:documentation
   "Represent a table element. Each row is a list of text elements."))

(define-customizable-writer *table-writer* define-table-writer 2)

(defmethod element-print ((element table) stream)
  (let ((processed-table (loop for row in rows
			       collect (mapcar (lambda (text-element)
						 (with-output-to-string (str-stream)
						   (element-print text-element str-stream)))
					       row))))
    (funcall *table-writer* stream processed-table)))
