
(in-package :adppvt)


;; ----- adp parameters -----

(defvar *add-documentation* nil)
(defparameter *current-style* nil)


;; ----- adp elements -----

(deftype adp-element ()
  '(cons keyword list))

(declaim (ftype (function (keyword &rest t) adp-element) create-adp-element))
(defun create-adp-element (key-type &rest contents)
  (cons key-type contents))

(declaim (ftype (function (adp-element) keyword) adp-element-key-type))
(defun adp-element-key-type (elem)
  (car elem))

(declaim (ftype (function (adp-element) list) adp-element-contents))
(defun adp-element-contents (elem)
  (cdr elem))


(declaim (type (vector adp-element) *file-adp-elements*))
(defvar *file-adp-elements* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'adp-element))

(declaim (ftype (function (adp-element) t) push-adp-element))
(defun push-adp-element (elem)
  (vector-push-extend elem *file-adp-elements*))

(declaim (ftype (function (keyword &rest t) t) emplace-adp-element))
(defun emplace-adp-element (key-type &rest contents)
  (push-adp-element (apply #'create-adp-element key-type contents)))

(declaim (ftype (function () t) empty-adp-elements))
(defun empty-adp-elements ()
  (setf (fill-pointer *file-adp-elements*) 0))


;; ----- adp files -----

(deftype adp-file ()
  '(cons pathname (vector adp-element)))

(declaim (ftype (function (pathname (vector adp-element)) adp-file) create-adp-file))
(defun create-adp-file (path elements)
  (cons path elements))

(declaim (ftype (function (adp-file) pathname) adp-file-path))
(defun adp-file-path (adp-file)
  (car adp-file))

(declaim (ftype (function (adp-file) (vector adp-element)) adp-file-elements))
(defun adp-file-elements (adp-file)
  (cdr adp-file))


(declaim (type (vector adp-file) *project-adp-files*))
(defvar *project-adp-files* (make-array 10 :adjustable t :fill-pointer 0))

(declaim (ftype (function (adp-file) t) push-adp-file))
(defun push-adp-file (adp-file)
  (vector-push-extend adp-file *project-adp-files*))

(declaim (ftype (function (pathname (vector adp-element)) t) emplace-adp-file))
(defun emplace-adp-file (path elements)
  (vector-push-extend (create-adp-file path elements) *project-adp-files*))

(declaim (ftype (function () t) empty-adp-files))
(defun empty-adp-files ()
  (setf (fill-pointer *project-adp-files*) 0))


;; ----- adp ref tags -----

(declaim (type (vector (cons symbol string)) *header-tags*))
(defvar *header-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (type (vector symbol) *symbol-tags* *function-tags* *type-tags*))
(defvar *symbol-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))
(defvar *function-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))
(defvar *type-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (ftype (function (symbol string) t) push-header-tag))
(defun push-header-tag (tag str)
  (vector-push-extend (cons tag str) *header-tags*))

(declaim (ftype (function (symbol) t) push-symbol-tag))
(defun push-symbol-tag (tag)
  (vector-push-extend tag *symbol-tags*))

(declaim (ftype (function (symbol) t) push-function-tag))
(defun push-function-tag (tag)
  (vector-push-extend tag *function-tags*))

(declaim (ftype (function (symbol) t) push-type-tag))
(defun push-type-tag (tag)
  (vector-push-extend tag *type-tags*))

(declaim (ftype (function (symbol) boolean) header-tagp))
(defun header-tagp (tag)
  (loop for header-tag across *header-tags*
	  thereis (eq tag header-tag)))

(declaim (ftype (function (symbol) boolean) symbol-tagp))
(defun symbol-tagp (tag)
  (loop for symbol-tag across *symbol-tags*
	  thereis (eq tag symbol-tag)))

(declaim (ftype (function (symbol) boolean) function-tagp))
(defun function-tagp (tag)
  (loop for function-tag across *function-tags*
	  thereis (eq tag function-tag)))

(declaim (ftype (function (symbol) boolean) type-tagp))
(defun type-tagp (tag)
  (loop for type-tag across *type-tags*
	  thereis (eq tag type-tag)))

(defun empty-header-tags ()
  (setf (fill-pointer *header-tags*) 0))

(defun empty-symbol-tags ()
  (setf (fill-pointer *symbol-tags*) 0))

(defun empty-function-tags ()
  (setf (fill-pointer *function-tags*) 0))

(defun empty-type-tags ()
  (setf (fill-pointer *type-tags*) 0))


(declaim (type hash-table *header-tags-table* *symbol-tags-table* *function-tags-table* *type-tags-table*))
(defvar *header-tags-table* (make-hash-table))
(defvar *symbol-tags-table* (make-hash-table))
(defvar *function-tags-table* (make-hash-table))
(defvar *type-tags-table* (make-hash-table))

(declaim (ftype (function (symbol string pathname) t) add-header-tag-path))
(defun add-header-tag-path (tag str path)
  (setf (gethash tag *header-tags-table*) (cons str path)))

(declaim (ftype (function (symbol pathname) t) add-symbol-tag-path))
(defun add-symbol-tag-path (tag path)
  (setf (gethash tag *symbol-tags-table*) path))

(declaim (ftype (function (symbol pathname) t) add-function-tag-path))
(defun add-function-tag-path (tag path)
  (setf (gethash tag *function-tags-table*) path))

(declaim (ftype (function (symbol pathname) t) add-type-tag-path))
(defun add-type-tag-path (tag path)
  (setf (gethash tag *type-tags-table*) path))

(declaim (ftype (function (symbol) (or (cons string pathname) null)) get-header-tag-path))
(defun get-header-tag-path (tag)
  (values (gethash tag *header-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-symbol-tag-path))
(defun get-symbol-tag-path (tag)
  (values (gethash tag *symbol-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-function-tag-path))
(defun get-function-tag-path (tag)
  (values (gethash tag *function-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-type-tag-path))
(defun get-type-tag-path (tag)
  (values (gethash tag *type-tags-table*)))


;; ----- adp code tags -----

(declaim (type hash-table *code-tags*))
(defvar *code-tags* (make-hash-table))

(declaim (ftype (function (symbol &rest t) t) add-code-tag))
(defun add-code-tag (tag &rest list-code)
  (when (not (gethash tag *code-tags*))
      (setf (gethash tag *code-tags*) (make-array 10 :adjustable t :fill-pointer 0)))
  (loop for code in list-code
	do (vector-push-extend code (gethash tag *code-tags*))))

(declaim (ftype (function (symbol) boolean) code-tagp))
(defun code-tagp (tag)
  (values (gethash tag *code-tags*)))

(declaim (ftype (function (symbol) vector) get-code-tag))
(defun get-code-tag (tag)
  (values (gethash tag *code-tags*)))


(declaim (type symbol *hide-symbol*))
(defparameter *hide-symbol* '#:hide)

(declaim (ftype (function (t) boolean) code-hidep))
(defun code-hidep (code)
  (eq code *hide-symbol*))

(declaim (ftype (function (t) boolean) plistp))
(defun plistp (code)
  (or (null code)
      (and (consp code)
	   (plistp (cdr code)))))

(declaim (ftype (function (t) t) remove-code-tag-exprs))
(defun remove-code-tag-exprs (code)
  (labels ((remove-code-tag-exprs-aux (code)
	     (if (plistp code)
		 (cond
		   ((member (car code) '(code-tag code-focus))
		    (mapcan #'remove-code-tag-exprs-aux (cddr code)))
		   (t
		    (list (mapcan #'remove-code-tag-exprs-aux code))))
		 (list code))))
    (remove-code-tag-exprs-aux code)))

(declaim (ftype (function (t) t) remove-own-code-focus-exprs))
(defun remove-own-code-focus-exprs (code)
  (labels ((remove-own-code-focus-exprs-aux (code)
	     (if (plistp code)
		 (cond
		   ((eq (car code) 'code-focus)
		    (mapcan #'remove-own-code-focus-exprs-aux (cddr code)))
		   ((eq (car code) 'code-tag)
		    (list code))
		   (t
		    (list (mapcan #'remove-own-code-focus-exprs-aux code))))
		 (list code))))
    (remove-own-code-focus-exprs-aux code)))

(declaim (ftype (function (symbol t) t) process-code-tag))
(defun process-code-tag (tag code)
  (labels ((process-aux (tag code)
	     (if (plistp code)
		 (if (member (car code) '(code-tag code-focus))
		     (if (and (eq (car code) 'code-focus)
			      (or (null (cadr code))
				  (member tag (cadr code))))
			 (list (list (remove-code-tag-exprs code)) t)
			 (loop for expr in (cddr code)
			       for (processed-expr expr-focus-found) = (process-aux tag expr)
			       for focus-found = (or expr-focus-found focus-found)
			       count (not expr-focus-found) into hide-count
			       if expr-focus-found
				 do (setf hide-count 0)
			       if (<= hide-count 1)
				 append processed-expr into processed-code
			       finally (return (list processed-code focus-found))))
		     (let* ((car-processed-values (process-aux tag (car code)))
			    (car-processed (car car-processed-values))
			    (car-focus-found (cadr car-processed-values)))
		       (loop for expr in (cdr code)
			     for (processed-expr expr-focus-found) = (process-aux tag expr)
			     for focus-found = (or expr-focus-found car-focus-found) then (or expr-focus-found focus-found)
			     count (not expr-focus-found) into hide-count
			     if expr-focus-found
			       do (setf hide-count 0)
			     if (<= hide-count 1)
			       append processed-expr into processed-code
			     finally (return (if focus-found
						 (if car-focus-found
						     (list (list (append car-processed processed-code)) t)
						     (list (list (append (remove-code-tag-exprs (car code)) processed-code)) t))
						 (list (list *hide-symbol*) nil))))))
		 (list (list *hide-symbol*) nil))))
    (destructuring-bind (processed-code focus-found) (process-aux tag code)
      (if focus-found
	  (car processed-code)
	  (remove-code-tag-exprs code)))))


;; ----- text variations -----

(declaim (type symbol *bold-symbol* *italic-symbol* *code-inline-symbol* *header-ref-symbol*
	       *symbol-ref-symbol* *function-ref-symbol* *type-ref-symbol*))
(defparameter *bold-symbol* '#:bold)
(defparameter *italic-symbol* '#:italic)
(defparameter *code-inline-symbol* '#:code-inline)
(defparameter *header-ref-symbol* '#:bold)
(defparameter *symbol-ref-symbol* '#:bold)
(defparameter *function-ref-symbol* '#:bold)
(defparameter *type-ref-symbol* '#:bold)

(declaim (ftype (function (&rest t) (cons keyword list)) create-bold-text create-italic-text
		create-code-inline-text))
(defun create-bold-text (&rest args)
  (cons *bold-symbol* args))

(defun create-italic-text (&rest args)
  (cons *italic-symbol* args))

(defun create-code-inline-text (&rest args)
  (cons *code-inline-symbol* args))

(declaim (ftype (function (symbol) (cons keyword list)) create-header-ref-text create-symbol-ref-text
		create-function-ref-text create-type-ref-text))
(defun create-header-ref-text (label)
  (list *header-ref-symbol* label))

(defun create-symbol-ref-text (label)
  (list *symbol-ref-symbol* label))

(defun create-function-ref-text (label)
  (list *function-ref-symbol* label))

(defun create-type-ref-text (label)
  (list *type-ref-symbol* label))

(declaim (ftype (function (t) boolean) bold-textp italic-textp code-inline-textp header-ref-textp
		symbol-ref-textp function-ref-textp type-ref-textp))
(defun bold-textp (arg)
  (and (listp arg)
       (eq (car arg) *bold-symbol*)))

(defun italic-textp (arg)
  (and (listp arg)
       (eq (car arg) *italic-symbol*)))

(defun code-inline-textp (arg)
  (and (listp arg)
       (eq (car arg) *code-inline-symbol*)))

(defun header-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *header-ref-symbol*)))

(defun symbol-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *symbol-ref-symbol*)))

(defun function-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *function-ref-symbol*)))

(defun type-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *type-ref-symbol*)))


;; ----- style parameters -----

(declaim (type list *style-parameters*))
(defvar *style-parameters* nil)

(defun add-style-parameter (name key-name required)
  (push (list name key-name required) *style-parameters*))

(defun style-parameterp (key-name)
  (member key-name *style-parameters* :key #'cadr))

(defun style-parameter-requiredp (key-name)
  (cadar (member key-name *style-parameters* :key #'cadr)))

(defun style-required-parameters ()
  (loop for (name key-name requiredp) in *style-parameters*
	if requiredp
	  collect key-name))

(defun set-parameter-value (key-name value)
  (loop for (name key requiredp) in *style-parameters*
	until (eq key key-name)
	finally (setf (symbol-value name) value)))

(defun check-style-parameters (style-params)
  (let ((required-parameters (style-required-parameters))
	(style-parameter-key-names (loop for key-name in style-params by #'cddr
					 collect key-name)))
    (loop for style-parameter-key-name in style-parameter-key-names
	  if (not (style-parameterp style-parameter-key-name))
	    do (error "The parameter ~s is not allowed." style-parameter-key-name))
    (loop for required-param in required-parameters
	  if (not (member required-param style-parameter-key-names))
	    do (error "The required param ~s is not used." required-param))))


;; ----- writing functions -----

(declaim (type (or null (function (stream string symbol) t)) *header-proc* *subheader-proc* *subsubheader-proc*))
(defvar *header-proc* nil)
(defvar *subheader-proc* nil)
(defvar *subsubheader-proc* nil)

(declaim (type (or null (function (stream string) t)) *text-proc*))
(defvar *text-proc* nil)

(declaim (type (or null (function (stream list) t)) *table-proc*))
(defvar *table-proc* nil)

(declaim (type (or null (function (stream list) t)) *itemize-proc*))
(defvar *itemize-proc* nil)

(declaim (type (or null (function (stream string pathname pathname) t)) *image-proc*))
(defvar *image-proc* nil)

(declaim (type (or null (function (stream string) t)) *bold-proc*))
(defvar *bold-proc* nil)

(declaim (type (or null (function (stream string) t)) *italic-proc*))
(defvar *italic-proc* nil)

(declaim (type (or null (function (stream t) t)) *code-inline-proc*))
(defvar *code-inline-proc* nil)

(declaim (type (or null (function (stream string string) t)) *web-link-proc*))
(defvar *web-link-proc* nil)

(declaim (type (or null (function (stream symbol string pathname pathname) t)) *header-ref-proc*))
(defvar *header-ref-proc* nil)

(declaim (type (or null (function (stream symbol pathname pathname) t)) *symbol-ref-proc*))
(defvar *symbol-ref-proc* nil)

(declaim (type (or null (function (stream symbol pathname pathname) t)) *function-ref-proc*))
(defvar *function-ref-proc* nil)

(declaim (type (or null (function (stream symbol pathname pathname) t)) *type-ref-proc*))
(defvar *type-ref-proc* nil)

(declaim (type (or null (function (stream list) t)) *code-block-proc*))
(defvar *code-block-proc* nil)

(declaim (type (or null (function (stream list) t)) *code-example-proc*))
(defvar *code-example-proc* nil)

(declaim (type (or null (function (stream t) t)) *defclass-proc* *defconstant-proc* *defgeneric-proc*
	       *define-compiler-macro-proc* *define-condition-proc* *define-method-combination-proc*
	       *define-modify-macro-proc* *define-setf-expander-proc* *define-symbol-macro-proc*
	       *defmacro-proc* *defmethod-proc* *defpackage-proc* *defparameter-proc* *defsetf-proc*
	       *defstruct-proc* *deftype-proc* *defun-proc* *defvar-proc*))
(defvar *defclass-proc* nil)
(defvar *defconstant-proc* nil)
(defvar *defgeneric-proc* nil)
(defvar *define-compiler-macro-proc* nil)
(defvar *define-condition-proc* nil)
(defvar *define-method-combination-proc* nil)
(defvar *define-modify-macro-proc* nil)
(defvar *define-setf-expander-proc* nil)
(defvar *define-symbol-macro-proc* nil)
(defvar *defmacro-proc* nil)
(defvar *defmethod-proc* nil)
(defvar *defpackage-proc* nil)
(defvar *defparameter-proc* nil)
(defvar *defsetf-proc* nil)
(defvar *defstruct-proc* nil)
(defvar *deftype-proc* nil)
(defvar *defun-proc* nil)
(defvar *defvar-proc* nil)

(declaim (type (or null (function () string)) *get-file-extension-proc*))
(defvar *get-file-extension-proc* nil)

(declaim (type (or null (function (stream) t)) *file-header-proc* *file-foot-proc*))
(defvar *file-header-proc* nil)
(defvar *file-foot-proc* nil)

(declaim (type (or null (function (pathname) t)) *system-files-proc*))
(defvar *system-files-proc* nil)


(declaim (ftype (function (pathname &rest t) string) slice-format))
(defun slice-format (root-path &rest args)
  (with-output-to-string (stream)
    (loop for arg in args
	  do (cond
	       ((bold-textp arg)
		(let ((bold-args (cdr arg)))
		  (funcall *bold-proc* stream (apply #'slice-format root-path bold-args))))
	       ((italic-textp arg)
		(let ((italic-args (cdr arg)))
		  (funcall *italic-proc* stream (apply #'slice-format root-path italic-args))))
	       ((code-inline-textp arg)
		(let ((code-inline-args (cdr arg)))
		  (funcall *code-inline-proc* stream (apply #'slice-format root-path code-inline-args))))
	       ((header-ref-textp arg)
		(assert (get-header-tag-path (cadr arg)) ((cadr arg)) "~s is not a header tag." (cadr arg)) 
		(let* ((header-tag (cadr arg))
		       (header-str-path (get-header-tag-path header-tag))
		       (header-str (car header-str-path))
		       (header-rel-path (cdr header-str-path)))
		  (funcall *header-ref-proc* stream header-tag header-str root-path header-rel-path)))
	       ((symbol-ref-textp arg)
		(assert (get-header-tag-path (cadr arg)) ((cadr arg)) "~s is not a symbol tag." (cadr arg))
		(let* ((symbol-tag (cadr arg))
		       (symbol-path (get-symbol-tag-path symbol-tag)))
		  (funcall *symbol-ref-proc* stream symbol-tag root-path symbol-path)))
	       ((function-ref-textp arg)
		(assert (get-function-tag-path (cadr arg)) ((cadr arg)) "~s is not a function tag." (cadr arg))
		(let* ((function-tag (cadr arg))
		       (function-path (get-function-tag-path function-tag)))
		  (funcall *function-ref-proc* stream function-tag root-path function-path)))
	       ((type-ref-textp arg)
		(assert (get-type-tag-path (cadr arg)) ((cadr arg)) "~s is not a type tag." (cadr arg))
		(let* ((type-tag (cadr arg))
		       (type-path (get-type-tag-path type-tag)))
		  (funcall *type-ref-proc* stream type-tag root-path type-path)))
	       (t (princ arg stream))))))


(declaim (ftype (function (stream pathname (vector adp-element)) t) write-file-contents))
(defun write-file-contents (stream root-path elements)
  (loop for element across elements
	do (case (adp-element-key-type element)
	     (:header (apply *header-proc* stream (adp-element-contents element)))
	     (:subheader (apply *subheader-proc* stream (adp-element-contents element)))
	     (:subsubheader (apply *subsubheader-proc* stream (adp-element-contents element)))
	     (:text (funcall *text-proc* stream (apply #'slice-format root-path (adp-element-contents element))))
	     (:table (funcall *table-proc* stream (loop for row in (adp-element-contents element)
							collect (loop for elem in row
								      collect (apply #'slice-format root-path (cdr elem))))))
	     (:itemize (labels ((write-itemize (item-list)
				  (loop for item in item-list
					if (eq (car item) :item)
					  collect (apply #'slice-format root-path (cdr item))
					else
					  collect (write-itemize (cdr item)))))
			 (let ((item-list (adp-element-contents element)))
			   (funcall *itemize-proc* stream (write-itemize item-list)))))
	     (:image (destructuring-bind (alt-text image-path) (adp-element-contents element)
		       (funcall *image-proc* stream alt-text root-path image-path)))
	     (:code-block (apply *code-block-proc* stream (adp-element-contents element)))
	     (:code-example (apply *code-example-proc* stream (adp-element-contents element)))
	     (:defclass (apply *defclass-proc* stream (adp-element-contents element)))
	     (:defconstant (apply *defconstant-proc* stream (adp-element-contents element)))
	     (:defgeneric (apply *defgeneric-proc* stream (adp-element-contents element)))
	     (:define-compiler-macro (apply *define-compiler-macro-proc* stream (adp-element-contents element)))
	     (:define-condition (apply *define-condition-proc* stream (adp-element-contents element)))
	     (:define-method-combination (apply *define-method-combination-proc* stream (adp-element-contents element)))
	     (:define-modify-macro (apply *define-modify-macro-proc* stream (adp-element-contents element)))
	     (:define-setf-expander (apply *define-setf-expander-proc* stream (adp-element-contents element)))
	     (:define-symbol-macro (apply *define-symbol-macro-proc* stream (adp-element-contents element)))
	     (:defmacro (apply *defmacro-proc* stream (adp-element-contents element)))
	     (:defmethod (apply *defmethod-proc* stream (adp-element-contents element)))
	     (:defpackage (apply *defpackage-proc* stream (adp-element-contents element)))
	     (:defparameter (apply *defparameter-proc* stream (adp-element-contents element)))
	     (:defsetf (apply *defsetf-proc* stream (adp-element-contents element)))
	     (:defstruct (apply *defstruct-proc* stream (adp-element-contents element)))
	     (:deftype (apply *deftype-proc* stream (adp-element-contents element)))
	     (:defun (apply *defun-proc* stream (adp-element-contents element)))
	     (:defvar (apply *defvar-proc* stream (adp-element-contents element)))
	     (t (error "Element not recognized: ~s" (adp-element-key-type element))))))


(declaim (ftype (function (pathname pathname (vector adp-element)) t) write-file))
(defun write-file (root-path rel-path elements)
  (let* ((complete-path (make-pathname :host (pathname-host root-path)
				       :device (pathname-device root-path)
				       :directory (append (pathname-directory root-path) (cdr (pathname-directory rel-path)))
				       :name (pathname-name rel-path)
				       :type (funcall *get-file-extension-proc*))))
    (ensure-directories-exist complete-path :verbose nil)
    (with-open-file (stream complete-path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (funcall *file-header-proc* stream)
      (write-file-contents stream root-path elements)
      (funcall *file-foot-proc* stream))))


(defun write-system-files (root-path)
  (funcall *system-files-proc* root-path)
  (loop for file-content across *project-adp-files*
	do (let ((rel-path (adp-file-path file-content))
		 (file-elements (adp-file-elements file-content)))
	     (write-file root-path rel-path file-elements))))
