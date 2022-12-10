
(in-package :adppvt)


(defclass file ()
  ((path :initarg :path
	 :type pathname)
   (elements :initform (make-array 100 :adjustable t :fill-pointer 0)
	     :type (vector element)))
  (:documentation
   "Represents a unit of documentation that groups several elements."))

