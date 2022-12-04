
(in-package :adppvt)


(defclass project ()
  ((root-directory :initarg :root-directory
		   :type pathname)
   (files :initform (make-array 10 :adjustable t :fill-pointer 0 :element-type 'file)
	  :type (vector file))
   (current-file :type pathname)
   (ordered-headers :initform (make-array 100 :adjustable t :fill-pointer 0 :element-type header)
		    :type (vector header))
   (header-tags :initform (make-instance 'tag-table)
		:type tag-table)
   (symbol-tags :initform (make-instance 'tag-table)
		:type tag-table)
   (function-tags :initform (make-instance 'tag-table)
		  :type tag-table)
   (type-tags :initform (make-instance 'tag-table)
	      :type tag-table)
   (code-tags :initform (make-instance 'tag-table)
	      :type tag-table)))


(defun project-relative-pathname (project path)
  "Make path to be relative to the project root directory."
  (declare (type project project) (type pathname path))
  (with-slots (root-directory) project
    (let* ((root-level (length (cdr (pathname-directory root-directory))))
	   (relative-path (make-pathname :directory (cons :relative (nthcdr (1+ root-level) (pathname-directory path)))
					 :name (pathname-name path)
					 :type (pathname-type path))))
      (values relative-path))))

  
(defun project-add-file (project file)
  "Add a file into a project."
  (declare (type project project) (type file file))
  (with-slots (files) project
    (vector-push-extend file files)))


(defun project-find-file (project path)
  "Return a file whose path is PATH. If there is not a file with path PATH, return NIL."
  (declare (type project project) (type pathname path))
  (with-slots (files) project
    (loop for file across files
	  if (equal path (slot-value file 'path))
	    return file
	  finally (return nil))))


(defun project-select-file (project path)
  (let ((file (or (project-find-file project path)
		  (let ((new-file (make-instance 'file :path path)))
		    (project-add-file project new-file)
		    new-file))))
    (with-slots (current-file) project
      (setf current-file file))))


(defgeneric project-add-element (project element)
  (:documentation
   "Add an element into a project.")
  
  (:method (project (element element))
    (with-slots (current-file) project
      (when (not current-file)
	(error "No file asigned. Use adp:in-file first."))
      (with-slots (source-location file-location) element
	(setf file-location current-file)
      (setf source-location (project-relative-pathname project source-location))
      (file-push-element current-file element))))

  (:method :after (project (element header-type))
    (with-slots (header-tags) project
      (with-slots (tag (header-location source-location)) element 
	(multiple-value-bind (previous-header foundp) (tag-table-find-elements header-tags tag)
	  (if foundp
	      (let ((previous-location (slot-value previous-header 'source-location)))
		(error "The header tag ~s was already defined.~%  Current definition at: '~a'~%  Previous definition at: '~a'~%"
		       tag header-location previous-location))
	      (tag-table-set-element header-tags tag element))))))


  (:method :after (project (element symbol-definition))
    (with-slots (symbol-tags) project
      (with-slots (tag) element
	(tag-table-set-element symbol-tags tag element))))

  (:method :after (project (element function-definition))
    (with-slots (function-tags) project
      (with-slots (tag) element
	(tag-table-set-element function-tags tag element))))

  (:method :after (project (element type-definition))
    (with-slots (type-tags) project
      (with-slots (tag) element
	(tag-table-set-element type-tags tag element))))


  (:method :after (project (element code-block))
    (with-slots (code-tags) project
      (setf (slot-value element 'code-tags) code-tags))))


(defun project-add-code-tag (project tag code)
  "Associate a tag with a piece of code."
  (delcare (type project project) (type symbol tag) (type code code))
  (with-slots (code-tags) project
    (tag-table-push-element code-tags tag code)))


(defmacro define-tag-finder (function-name project-slot tag-type)
  (let ((function-docstring (format nil "Retrieve an element associated with a ~a tag. If tag is not present, return nil."
				    (string-downcase (symbol-name tag-type)))))
    (with-gensyms (project tag)
      `(defun ,function-name (,project ,tag)
	 ,function-docstring
	 (declare (type project ,project) (type symbol ,tag))
	 (with-slots (,project-slot) ,project
	   (tag-table-find-elements ,project-slot ,tag))))))

(define-tag-p project-find-header header-tags header)
(define-tag-p project-find-symbol symbol-tags symbol)
(define-tag-p project-find-function function-tags function)
(define-tag-p project-find-type type-tags type)
(define-tag-p project-find-code code-tags code)


(defmacro define-tag-adder (function-name project-slot tag-type element-type)
  (let ((function-docstring (format nil "Associate a ~a tag with a ~a element."
				    (string-downcase (symbol-name tag-type))
				    (string-downcase (symbol-name element-type)))))
    (with-gensyms (project tag element)
      `(defun ,function-name (,project ,tag ,element)
	 ,function-docstring
	 (declare (type project ,project) (type symbol ,tag) (type ,element-type ,element))
	 (with-slots (,project-slot) ,project
	   (tag-table-push-element ,project-slot ,tag ,element))))))

(define-tag-adder project-add-header-tag header-tags header header)
(define-tag-adder project-add-symbol-tag symbol-tags symbol definition)
(define-tag-adder project-add-function-tag function-tags function definition)
(define-tag-adder project-add-type-tag type-tags type definition)
(define-tag-adder project-add-code-tag code-tags code code)



