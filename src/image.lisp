
(in-package :adppvt)


(defclass image (element)
  ((path :initarg path
	 :type pathname))
  (:documentation
   "Represents an image element."))

(define-customizable-writer *image-writer* define-image-writer 2)

(defmethod element-print ((element image) stream)
  (funcall *image-writer* stream (slot-value element 'path)))
