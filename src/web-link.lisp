
(in-package :adppvt)


(defclass web-link (element)
  ((text :initarg :text
	 :type string)
   (address :initarg :address
	    :type string))
  (:documentation
   "Represents a web-link element."))

(define-customizable-writer *web-link-writer* define-web-link-writer 3)

(defmethod element-print ((element web-link) stream)
  (with-slots (text address) element
    (funcall *web-link-writer* stream text address)))
