
(in-package :adppvt)


(defclass element ()
  ((source-location :initform *load-truename*
		    :type pathname)
   (file-location :initarg :file-location
		  :type file)
   (top-level :initform t
	      :type boolean))
  (:documentation
   "Represent the most basic unit of documentation."))

(defgeneric element-print (element stream)
  (:documentation
   "Print an element into the stream."))



