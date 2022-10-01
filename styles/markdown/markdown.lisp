
(in-package :adp)


(defparameter *file-documentation* (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'string))


(def-defconstant-writer (defconstant-body)
    (with-defconstant-components ((name initial-value documentation) defconstant-body)
      (vector-push-extend
       (with-output-to-string (s)
			    (format s "```lisp~%(defconstant ~s ~s)~%```~%~%~@[~a~%~%~]"
				    name initial-value documentation))
       *file-documentation*)))


(def-defparameter-writer (defparameter-body)
    (with-defparameter-components ((name initial-value documentation) defparameter-body)
      (vector-push-extend
       (with-output-to-string (s str)
	 (format s "```lisp~%(defparameter ~s ~s)~%```~%~%~@[~a~%~%~]"
		 name initial-value documentation))
       *file-documentation*)))


(def-defvar-writer (defvar-body)
    (with-defvar-components ((name initial-value documentation) defvar-body)
      (vector-push-extend
       (with-output-to-string (s)
	 (format s "```lisp~%(defvar ~s ~s)~%```~%~%~@[~a~%~%~]"
		 name initial-value documentation))
       *file-documentation*)))


(def-defun-writer (defun-body)
    (with-defun-components ((function-name lambda-list documentation) defun-body)
      (vector-push-extend
       (with-output-to-string (s)
	 (format s "```lisp~%(defun ~s ~s)~%```~%~%~@[~a~%~%~]"
		 function-name lambda-list documentation))
       *file-documentation*)))


(def-defmacro-writer (defun-body)
    (with-defun-components ((name lambda-list documentation) defmacro-body)
      (vector-push-extend
       (with-output-to-string (s)
	 (format s "```lisp~%(defmacro ~s ~s)~%```~%~%~@[~a~%~%~]"
		 name lambda-list documentation))
       *file-documentation*)))


(defparameter *system-documentation* (make-array 10 :fill-pointer 0 :adjustable t :element-type 'string))


(def-documentation-in-file-writer (file)
    (vector-push-extend (cons file (with-output-to-string (s)
				     (loop for doc across *file-documentation*
					   do (princ doc s))))
			*system-documentation*)
      (setf (fill-pointer *file-documentation*) 0))


(def-load-with-documentation-writer (root-directory)
    (loop for (file . doc) in *system-documentation*
	  for complete-file = (merge-pathnames file root-directory)
	  for actual-file = (setf (pathname-type complete-file) "md")
	  do (ensure-directories-exist actual-file)
	     (with-open-file (stream actual-file :direction :output :if-does-not-exist :create :if-exists :supersede)
	       (princ doc actual-file))))
