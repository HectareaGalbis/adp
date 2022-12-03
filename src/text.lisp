
(in-package :adppvt)


(defclass text (element)
  ((text-elements :initarg text-elements
		  :type list))
  (:documentation
   "Represents a text element."))

(define-customizable-writer *text-writer* define-text-writer 2)
(define-customizable-writer *escape-text* define-escape-text-writer 1)

(defun text-to-string (text)
  "Turn a text element into a string."
  (declare (type text text))
  (let ((processed-elements (mapcar (lambda (text-element)
				      (if (typep text-element 'element)
					  (with-output-to-string (str-stream)
					    (element-print text-element str-stream))
					  (funcall *escape-text* text-element)))
				    text-elements)))
    (apply #'concatenate 'string processed-elements)))

(defmethod element-print ((element text) stream)
  (let ((text-elements (slot-value element 'text-elements)))
    (funcall *text-writer* stream (text-to-string element))))
