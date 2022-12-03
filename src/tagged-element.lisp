
(in-package :adppvt)


(defclass tagged-element (element)
  ((tag :initarg :tag
	:type symbol))
  (:documentation
   "Represent an element that can be associated with a tag."))
