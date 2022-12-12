
(in-package :adppvt)


;; -----------------------
;; ----- select-file -----
;; -----------------------

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

(defun select-file (project path)
  (with-slots (root-directory) project
    (let* ((complete-path (merge-pathnames path root-directory))
	   (file (or (project-find-file project path)
		     (let ((new-file (make-instance 'file :path complete-path)))
		       (project-add-file project new-file)
		       new-file))))
      (with-slots (current-file) project
	(setf current-file file)))))


;; ------------------------
;; ----- add-code-tag -----
;; ------------------------

(defun add-code-tag (tag code)
  "Associate a tag with a piece of code."
  (declare (type symbol tag) (type tagged-code code))
  (tag-table-push-element *code-tags* tag code))


;; -------------------------------------
;; ----- project-relative-pathname -----
;; -------------------------------------

(defun relative-truename (project &optional (path *load-truename*))
  "Return the *load-truename* path relative to the project root directory."
  (declare (type project project))
  (with-slots (root-directory) project
    (let* ((root-level (length (cdr (pathname-directory root-directory))))
	   (relative-path (make-pathname :directory (cons :relative (nthcdr (1+ root-level) (pathname-directory path)))
					 :name (pathname-name path)
					 :type (pathname-type path))))
      (values relative-path))))


;; -----------------------
;; ----- add-element -----
;; -----------------------

(defgeneric check-subelements (element)
  (:documentation
   "Check the use of subelements.")

  (:method ((element element))
    (values)))



(defmethod check-subelements ((element text-subelement))
  (with-slots (text-elements) element
    (loop for text-subelement in text-elements
	  when (typep text-subelement 'element)
	    do (error 'text-subelement-error :text element :subelement text-subelement))))

(defmethod check-subelements ((element text-element))
  (with-slots (text-elements) element
    (loop for text-subelement in text-elements
	  do (typecase text-subelement
	       (text-subelement (check-subelements text-subelement))
	       (text-subelement-type (values))
	       (element (error 'text-subelement-error :text element :subelement text-subelement))))))

(defmethod check-subelements ((element table))
  (with-slots (rows) element
    (loop for row in rows
	  do (loop for cell-element in row
		   do (if (typep cell-element 'cell)
			  (check-subelements cell-element)
			  (error 'table-subelement-error :table element :subelement cell-element))))))

(defmethod check-subelements ((element itemize-type))
  (with-slots ((items elements)) element
    (when (null items)
      (error 'null-itemize-error :itemize element))
    (when (not (typep (car items) 'item))
      (error 'itemize-first-element-not-item-error :itemize element :subelement (car items)))
    (loop for item in (cdr items)
	  do (if (typep item '(or itemize-type item))
		 (check-subelements item)
		 (error 'itemize-subelement-error :itemize element :subelement item)))))


(defun file-push-element (file element)
  "Add an element in a file."
  (declare (type file file) (type element element))
  (with-slots (elements) file
    (vector-push-extend element elements)))


(defgeneric add-element (project element)
  (:documentation
   "Add an element into a project."))

(defmethod add-element (project (element element))
  (with-slots (current-file) project
    (when (not current-file)
      (error 'file-not-selected-error :first-element element))
    (check-subelements element)
    (setf (slot-value element 'file-location) (slot-value current-file 'path))
    (file-push-element current-file element)))

(defmethod add-element :after (project (element header-type))
  (with-slots (tag (header-location source-location)) element
    (multiple-value-bind (previous-header foundp) (tag-table-find-elements *header-tags* tag)
      (if foundp
	  (error 'already-defined-tag-error :source-element element :previous-source-element previous-header :tag tag)
	  (tag-table-set-element *header-tags* tag element)))))


(defmethod add-element :after (project (element table-of-contents))
  (setf (slot-value element 'project) project))

(defmethod add-element :after (project (element file-table-of-contents))
  (with-slots (current-file) project
    (setf (slot-value element 'file) current-file)))


(defmethod add-element :after (project (element symbol-definition))
  (with-slots (tag) element
    (tag-table-set-element *symbol-tags* tag element)))

(defmethod add-element :after (project (element function-definition))
  (with-slots (tag) element
    (tag-table-set-element *function-tags* tag element)))

(defmethod add-element :after (project (element type-definition))
  (with-slots (tag) element
    (tag-table-set-element *type-tags* tag element)))


;; ----------------------
;; ----- file-print -----
;; ----------------------

(defun file-print (file)
  "Create a documentation file and prints the documentation in it."
  (declare (type file file))
  (with-slots (elements path) file
    (let ((complete-path (merge-pathnames path (make-pathname :type (funcall *file-extension*)))))
      (ensure-directories-exist complete-path :verbose nil)
      (with-open-file (stream complete-path :direction :output :if-does-not-exist :create :if-exists :supersede)
	(when *begin-file-writer*
	  (funcall *begin-file-writer* stream))
	(loop for element across elements
	      do (element-print element stream))
	(when *end-file-writer*
	  (funcall *end-file-writer* stream))))))


;; -------------------------
;; ----- project-print -----
;; -------------------------

(defun warn-unused-header-tags ()
  (let ((unused-headers (tag-table-unused-elements *header-tags*)))
    (loop for unused-header in unused-headers
	  do (with-slots (tag user-tag-p source-location) (aref unused-header 0)
	       (when user-tag-p
		 (warn "ADP warning: The header tag ~s defined at '~a' is never used."
		       tag source-location))))))

(defun warn-unused-code-tags ()
  (let ((unused-code-elements (tag-table-unused-elements *code-tags*)))
    (loop for unused-code-elements in unused-code-elements
	  do (let* ((unused-elements-list (coerce unused-code-elements 'list))
		    (unused-elements-paths (remove-duplicates (mapcar (lambda (code-element)
									(slot-value code-element 'source-location))
								      unused-elements-list))))
	       (with-slots (tag) (aref unused-code-elements 0)
		 (warn "ADP warning: The code tag ~s defined at ~{~#[~;'~a'~;'~a' and '~a'~:;'~a', ~]~} is never used."
		       tag unused-elements-paths))))))

(defun project-print (project)
  "Generate a project documentation."
  (declare (type project project))
  (with-slots (files root-directory) project
    (when *begin-project-writer*
      (funcall *begin-project-writer* root-directory))
    (loop for file across files
	  do (format t "Printing file '~a'.~%" (relative-truename project (slot-value file 'path)))
	     (file-print file))
    (when *end-project-writer*
      (funcall *end-project-writer* root-directory)))
  (warn-unused-header-tags)
  (warn-unused-code-tags))


;; -------------------------
;; ----- element-print -----
;; -------------------------

(defgeneric element-print (element stream)
  (:documentation
   "Print an element into the stream."))

;; header
(defmethod element-print ((element header) stream)
  (declare (special *header-writer*))
  (with-slots (title tag) element
    (funcall *header-writer* stream title tag)))

(defmethod element-print ((element subheader) stream)
  (declare (special *subheader-writer*))
  (with-slots (title tag) element
    (funcall *subheader-writer* stream title tag)))

(defmethod element-print ((element subsubheader) stream)
  (declare (special *subsubheader-writer*))
  (with-slots (title tag) element
    (funcall *subsubheader-writer* stream title tag)))

;; text
(defun text-type-to-string (text)
  "Turn a text element into a string."
  (declare (type text-type text))
  (with-slots (text-elements) text
    (let ((processed-elements (mapcar (lambda (text-element)
					(if (typep text-element 'element)
					    (with-output-to-string (str-stream)
					      (element-print text-element str-stream))
					    (let ((princed-element (princ-to-string text-element)))
					      (if *escape-text*
						  (funcall *escape-text* princed-element)
						  princed-element))))
				      text-elements)))
      (apply #'concatenate 'string processed-elements))))

(defmethod element-print ((element text) stream)
  (funcall *text-writer* stream (text-type-to-string element)))

;; text enrichment
(defmethod element-print ((element bold) stream)
  (funcall *bold-writer* stream (text-type-to-string element)))

(defmethod element-print ((element italic) stream)
  (funcall *italic-writer* stream (text-type-to-string element)))

(defmethod element-print ((element emphasis) stream)
  (funcall *emphasis-writer* stream (text-type-to-string element)))

(defmethod element-print ((element inline-code) stream)
  (funcall *inline-code-writer* stream (text-type-to-string element)))

;; text reference
(defmethod element-print ((element header-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements-using-tag *header-tags* tag)
      (if element-found-p
	  (with-slots (file-location title) (aref associated-elements 0)
	    (funcall *header-ref-writer* stream tag title file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element symbol-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *symbol-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *symbol-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element function-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *function-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *function-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element type-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *type-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *type-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

;; web link
(defmethod element-print ((element web-link) stream)
  (with-slots (text address) element
    (funcall *web-link-writer* stream text address)))

;; image
(defmethod element-print ((element image) stream)
  (with-slots (alt-text path) element
    (funcall *image-writer* stream alt-text (slot-value element 'path))))

;; table
(defmethod element-print ((element table) stream)
  (with-slots (rows) element
    (let ((processed-table (loop for row in rows
				 collect (mapcar #'text-type-to-string row))))
      (funcall *table-writer* stream processed-table))))

;; itemize
(defgeneric process-itemize (element)
  (:documentation
   "Turn an itemize element into a style-maker suitable representation.")

  (:method ((element item))
    (list :item (text-type-to-string element)))

  (:method ((element itemize))
    (with-slots (elements type) element
      (let ((processed-elements (mapcar #'process-itemize elements)))
	(list* :itemize processed-elements))))

  (:method ((element enumerate))
    (with-slots (elements type) element
      (let ((processed-elements (mapcar #'process-itemize elements)))
	(list* :enumerate processed-elements)))))

(defmethod element-print ((element itemize) stream)
  (funcall *itemize-writer* stream (process-itemize element)))


;; table-of-contents
(defun file-headers (file)
  "Return the header-type elements of a file."
  (declare (type file file))
  (let ((headers (make-array 10 :adjustable t :fill-pointer 0)))
    (with-slots (elements) file
      (loop for element across elements
	    when (typep element 'header-type)
	      do (vector-push-extend element headers)))
    (values headers)))

(defun project-headers (project)
  "Return the header-type elements of a project."
  (declare (type project project))
  (let ((headers (make-array 10 :adjustable t :fill-pointer 0)))
    (with-slots (files) project
      (loop for file across files
	    do (let ((file-headers (file-headers file)))
		 (loop for file-header across file-headers
		       if (typep file-header '(or header subheader))
			 do (vector-push-extend file-header headers)))))
    (values headers)))

(defun header-deep-level (header)
  "Return the level of deepness of a header."
  (declare (type header-type header))
  (typecase header
    (header 0)
    (subheader 1)
    (subsubheader 2)
    (t (error "The object ~s is not a header-type element." header))))

(defun make-toc-deep-levels (headers)
  "Return a vector of deepness levels the headers must have in a table of contents."
  (declare (type (vector element) headers))
  (let ((deep-levels (make-array 100 :adjustable t :fill-pointer 0 :element-type 'unsigned-byte)))
    (loop for header across headers
	  for prev-min-deep-level = 2 then next-min-deep-level
	  for prev-deep-level =     2 then next-deep-level
	  for (next-min-deep-level next-deep-level) = (let ((header-deep-level (header-deep-level header)))
							(cond
							  ((> header-deep-level prev-deep-level)
							   (let ((next-deep-level (1+ prev-deep-level)))
							     (list prev-min-deep-level next-deep-level)))
							  ((< header-deep-level prev-deep-level)
							   (if (>= header-deep-level prev-min-deep-level)
							       (list prev-min-deep-level (- header-deep-level prev-min-deep-level))
							       (list header-deep-level 0)))
							  (t
							   (list prev-min-deep-level header-deep-level))))
	  do (vector-push-extend next-deep-level deep-levels))
    (values deep-levels)))

(defun make-itemize-toc (source-element headers)
  (with-slots (source-location) source-element
    (let* ((deep-levels (make-toc-deep-levels headers))
	   (total-deep-levels (length deep-levels))
	   (index 0))
      (labels ((make-itemize-toc-aux (current-level)
		 (loop while (< index total-deep-levels)
		       for header = (aref headers index)
		       for deep-level = (aref deep-levels index)
		       until (< deep-level current-level)
		       if (> deep-level current-level)
			 collect (make-instance 'itemize
						:name "itemize"
						:elements (make-itemize-toc-aux (1+ current-level))
						:source-location source-location)
			   into toc-list
		       else
			 collect (make-instance 'item
						:name "item"
						:text-elements (list (make-instance 'header-ref
										    :name "header-ref"
										    :tag (slot-value header 'tag)
										    :source-location source-location))
						:source-location source-location)
			   into toc-list
			   and do (incf index)
		       finally (return toc-list))))
	(make-instance 'itemize
		       :name "itemize"
		       :elements (make-itemize-toc-aux 0)
		       :source-location source-location)))))

(defmethod element-print ((element table-of-contents) stream)
  (with-slots (project) element
    (let ((headers (project-headers project)))
      (funcall *itemize-writer* (process-itemize (make-itemize-toc element headers))))))

(defmethod element-print ((element mini-table-of-contents) stream)
  (with-slots (file) element
    (let ((headers (file-headers file)))
      (funcall *itemize-writer* stream (process-itemize (make-itemize-toc element headers))))))


;; table-of-function/symbols/types
(defun split-symbols (symbols)
  (if (null symbols)
      nil
      (labels ((take-first-symbols (syms char)
		 (let ((sym (car syms)))
		   (cond
		     ((and (not (null syms))
			   (char= (aref (symbol-name sym) 0) char))
		      (multiple-value-bind (rest-first-syms rest) (take-first-symbols (cdr syms) char)
			(values (cons sym rest-first-syms)
				rest)))
		     (t (values nil syms)))))
	       (split-ordered-symbols-aux (syms)
		 (if (null syms)
		     nil
		     (let ((first-sym (car syms)))
		       (multiple-value-bind (first-symbols rest-symbols) (take-first-symbols syms (aref (symbol-name first-sym) 0))
			 (cons first-symbols (split-ordered-symbols-aux rest-symbols)))))))
	(split-ordered-symbols-aux symbols))))

(defun make-itemize-table (source-element tag-table reftype refname)
  (with-slots (source-location) source-element
    (let* ((syms-list (sort (tag-table-tags tag-table) #'string>=))
	   (split-syms (split-symbols syms-list)))
      (loop for syms-group in split-syms
	    collect (let ((char (aref (symbol-name (car syms-group)) 0)))
		      (make-instance 'item
				     :name "item"
				     :text-elements (list char)
				     :source-location source-location))
	      into items-list
	    collect (flet ((make-ref (tag)
			     (make-instance reftype
					    :name refname
					    :tag tag
					    :source-location source-location)))
		      (make-instance 'itemize
				     :name "itemize"
				     :elements (loop for sym in syms-group
						     collect (make-instance 'item
									    :name "item"
									    :text-elements (list (make-ref sym))
									    :source-location source-location))
				     :source-location source-location))
	      into items-list
	    finally (return (make-instance 'itemize
					   :name "itemize"
					   :elements items-list
					   :source-location source-location))))))

(defun make-itemize-tof (source-element)
  (make-itemize-table source-element *function-tags* 'function-ref "function-ref"))

(defmethod element-print ((element table-of-functions) stream)
  (funcall *itemize-writer* stream (make-itemize-tof element)))

(defun make-itemize-tos (source-element)
  (make-itemize-table source-element *symbol-tags* 'symbol-ref "symbol-ref"))

(defmethod element-print ((element table-of-symbols) stream)
  (funcall *itemize-writer* stream (make-itemize-tos element)))

(defun make-itemize-tot (source-element)
  (make-itemize-table source-element *type-tags* 'type-ref "type-ref"))

(defmethod element-print ((element table-of-types) stream)
  (funcall *itemize-writer* stream (make-itemize-tot element)))


;; code
(defun code-to-string (code)
  "Turn a code element into a string."
  (declare (type code code))
  (with-slots (expr) code
    (let ((*print-pprint-dispatch* *adp-pprint-dispatch*))
      (with-output-to-string (stream)
	(prin1 expr stream)))))

(defmethod element-print ((element code-block) stream)
  (with-slots (code-type code-elements) element
    (let* ((complete-code-elements (mapcan (lambda (code-or-ref)
					     (if (typep code-or-ref 'code-ref)
						 (with-slots (tag) code-or-ref
						   (multiple-value-bind (tag-code-elements tag-foundp) (tag-table-find-elements-using-tag *code-tags* tag)
						     (if tag-foundp
							 (coerce tag-code-elements 'list)
							 (error 'tag-not-defined-error :source-element element :tag tag))))
						 (list code-or-ref)))
					   code-elements))
	   (code-string (format nil "~{~a~^~%~%~}" (mapcar #'code-to-string complete-code-elements))))
      (funcall *code-block-writer* stream code-type code-string))))

(defmethod element-print ((element verbatim-code-block) stream)
  (with-slots (code-type code-text) element
    (funcall *code-block-writer* stream code-type code-text)))

(defmethod element-print ((element code-example) stream)
  (with-slots (code-elements output result) element
    (let ((code-string (format nil "~{~a~^~%~%~}" (mapcar #'code-to-string code-elements))))
      (funcall *code-example-writer* stream code-string output result))))

;; definition
(defmacro define-definition-element-print (type writer)
  (with-gensyms (element stream expr)
    `(defmethod element-print ((,element ,type) ,stream)
       (with-slots ((,expr expr)) ,element
	 (funcall ,writer ,stream ,expr)))))

(define-definition-element-print defclass-definition *defclass-writer*)
(define-definition-element-print defconstant-definition *defconstant-writer*)
(define-definition-element-print defgeneric-definition *defgeneric-writer*)
(define-definition-element-print define-compiler-macro-definition *define-compiler-macro-writer*)
(define-definition-element-print define-condition-definition *define-condition-writer*)
(define-definition-element-print define-method-combination-definition *define-method-combination-writer*)
(define-definition-element-print define-modify-macro-definition *define-modify-macro-writer*)
(define-definition-element-print define-setf-expander-definition *define-setf-expander-writer*)
(define-definition-element-print define-symbol-macro-definition *define-symbol-macro-writer*)
(define-definition-element-print defmacro-definition *defmacro-writer*)
(define-definition-element-print defmethod-definition *defmethod-writer*)
(define-definition-element-print defpackage-definition *defpackage-writer*)
(define-definition-element-print defparameter-definition *defparameter-writer*)
(define-definition-element-print defsetf-definition *defsetf-writer*)
(define-definition-element-print defstruct-definition *defstruct-writer*)
(define-definition-element-print deftype-definition *deftype-writer*)
(define-definition-element-print defun-definition *defun-writer*)
(define-definition-element-print defvar-definition *defvar-writer*)

