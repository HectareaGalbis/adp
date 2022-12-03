
(in-package :adppvt)


(defclass file ()
  ((path :initarg :path
	 :type pathname)
   (elements :initform (make-array 100 :adjustable t :fill-pointer 0 :element-type 'element)
	     :type (vector element)))
  (:documentation
   "Represents a unit of documentation that groups several elements."))


(defun make-file (rel-path)
  "file constructor. Receive a path which will ignore every part except the directory and name parts.
The path is turned into a elative path as well."
  (declare (type pathname rel-path))
  (assert (pathname-name rel-path) (rel-path) "The pathname ~s must have a name part." rel-path)
  (let ((relative-path (make-pathname :directory (if (pathname-directory rel-path)
						     (cons :relative (cdr (pathname-directory rel-path)))
						     nil)
				      :name (pathname-name ,file-path))))
    (make-instance 'file :path rel-path)))


(defun file-push-element (file element)
  "Add an element in a file."
  (declare (type element element))
  (with-slots ((file-elements elements)) file
    (vector-push-extend element file-elements)))
