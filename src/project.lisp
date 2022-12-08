
(in-package :adppvt)


(defclass project ()
  ((root-directory :initarg :root-directory
		   :type pathname)
   (files :initform (make-array 10 :adjustable t :fill-pointer 0 :element-type 'file)
	  :type (vector file))
   (current-file :type pathname)))
