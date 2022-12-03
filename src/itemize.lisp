
(in-package :adppvt)


(defclass item (element)
  ((text :initarg :text
	 :type text))
  (:documentation
   "Represents an item element for an itemize."))


(defclass itemize (element)
  ((elements :initarg :elements
	     :type list)
   (type :initarg type
	 :type (member :itemize :enumerate)))
  (:documentation
   "Represents an itemize element."))


(defgeneric process-itemize (element)
  (:documentation
   "Turn an itemize element into a style-maker suitable representation."))

(defmethod process-itemize ((element item))
  (list :item (text-to-string (slot-value item 'text))))

(defmethod process-itemize ((element itemize))
  (with-slots (elements type) element
    (let ((processed-elements (mapcar #'process-itemize elements)))
      (list* type processed-elements))))

(define-customizable-writer *itemize-writer* define-itemize-writer 2)

(defmethod element-print ((element itemize) stream)
  (funcall *itemize-writer* stream (process-itemize element)))


