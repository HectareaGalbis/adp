
(in-package :addpvt)


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
  (let ((file (or (project-find-file project path)
		  (let ((new-file (make-instance 'file :path path)))
		    (project-add-file project new-file)
		    new-file))))
    (with-slots (current-file) project
      (setf current-file file))))


;; ------------------------
;; ----- add-code-tag -----
;; ------------------------

(defun add-code-tag (project tag code)
  "Associate a tag with a piece of code."
  (delcare (type project project) (type symbol tag) (type code code))
  (with-slots (code-tags) project
    (tag-table-push-element code-tags tag code)))


;; -------------------------------------
;; ----- project-relative-pathname -----
;; -------------------------------------

(defun relative-truename (project)
  "Return the *load-truename* path relative to the project root directory."
  (declare (type project project))
  (with-slots (root-directory) project
    (let* ((root-level (length (cdr (pathname-directory root-directory))))
	   (relative-path (make-pathname :directory (cons :relative (nthcdr (1+ root-level) (pathname-directory *load-truename*)))
					 :name (pathname-name *load-truename*)
					 :type (pathname-type *load-truename*))))
      (values relative-path))))


;; -----------------------
;; ----- add-element -----
;; -----------------------

(defgeneric check-subelements (element)
  (:documentation
   "Check the use of subelements."))

(defmethod check-subelements ((element element))
  (values))

(defmethod check-subelements ((element (or text cell)))
  (with-slots (text-elements) element
    (loop for text-element in text-elements
	  when (and (typep text-element 'element)
		    (not (typep text-element 'web-link)))
	    do (if (typep text-element 'text-enrichment)
		   (check-subelements text-element)
		   (error 'text-subelement-error :text element :subelement text-element)))))

(defmethod check-subelements ((element text-type))
  (with-slots (text-elements) element
    (loop for text-element in text-elements
	  when (typep text-element 'element)
	    do (error 'text-subelement-error :text element :subelement text-element))))

(defmethod check-subelements ((element table))
  (with-slots (rows) element
    (loop for row in rows
	  do (loop for cell-element in row
		   if (typep cell-element 'cell)
		     do (check-subelements cell-element)
		   else
		     do (error 'table-subelement-error :table element :subelement cell-element)))))

(defmethod check-subelements ((element itemize-type))
  (with-slots ((items elements)) element
    (when (null items)
      (error 'null-itemize-error :itemize element))
    (when (not (typep (car items) 'item))
      (error 'itemize-first-element-not-item-error :itemize element :subelement (car items)))
    (loop for item in (cdr items)
	  if (typep item '(or itemize-type item))
	    do (check-subelements item)
	  else
	    do (error 'itemize-subelement-error :itemize element :subelement item))))


(defgeneric add-element (project element)
  (:documentation
   "Add an element into a project."))

(defmethod add-element (project (element element))
  (with-slots (current-file) project
    (when (not current-file)
      (error 'file-not-selected-error :first-element element))
    (check-subelements element)
    (setf (slot-value element 'file-location) current-file)
    (file-push-element current-file element)))

(defmethod add-element :after (project (element header-type))
  (with-slots (header-tags) project
    (with-slots (tag (header-location source-location)) element 
      (multiple-value-bind (previous-header foundp) (tag-table-find-elements header-tags tag)
	(if foundp
	    (error 'already-defined-tag-error :source-element element :previous-source-element previous-header
		   :tag tag)
	    (tag-table-set-element header-tags tag element))))))


(defmethod add-element :after (project (element text))
  (with-slots (header-tags symbol-tags function-tags type-tags) project
    (with-slots (text-elements) text
      (loop for text-element in text-elements
	    do (typecase text-element
		 (header-ref (setf (slot-value text-element 'header-tags) header-tags))
		 (symbol-ref (setf (slot-value text-element 'symbol-tags) symbol-tags))
		 (function-ref (setf (slot-value text-element 'function-tags) function-tags))
		 (type-ref (setf (slot-value text-element 'type-tags) type-tags)))))))


(defmethod add-element :after (project (element code-block))
  (with-slots (code-tags) project
    (with-slots (code-elements) element
      (loop for code-element in code-elements
	    if (typep code-element code-reference)
	      do (setf (slot-value code-element 'code-tags) code-tags)))))


(defmethod add-element :after (project (element symbol-definition))
  (with-slots (symbol-tags) project
    (with-slots (tag) element
      (tag-table-set-element symbol-tags tag element))))

(defmethod add-element :after (project (element function-definition))
  (with-slots (function-tags) project
    (with-slots (tag) element
      (tag-table-set-element function-tags tag element))))

(defmethod add-element :after (project (element type-definition))
  (with-slots (type-tags) project
    (with-slots (tag) element
      (tag-table-set-element type-tags tag element))))


;; -------------------------
;; ----- element-print ----- ; Convertir en doc-print y hacer metodos para project y file
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
					    (funcall *escape-text* text-element)))
				      text-elements)))
      (apply #'concatenate 'string processed-elements))))

(defmethod element-print ((element text) stream)
  (let ((text-elements (slot-value element 'text-elements)))
    (funcall *text-writer* stream (text-type-to-string element))))

;; text enrichment
(defmethod element-print ((element bold) stream)
  (funcall *bold-writer* stream (text-type-to-string element)))

(defmethod element-print ((element italic) stream)
  (funcall *italic-writer* stream (text-type-to-string element)))

(defmethod element-print ((element bold-italic) stream)
  (funcall *bold-italic-writer* stream (text-type-to-string element)))

(defmethod element-print ((element code-inline) stream)
  (funcall *code-inline-writer* stream (text-type-to-string element)))

;; text reference
(defmethod element-print ((element header-ref) stream)
  (with-slots (tag header-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements header-tags tag)
      (if element-found-p
	  (with-slots (file-location title) (aref associated-elements 0)
	    (funcall *header-ref-writer* stream tag title file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element symbol-ref) stream)
  (with-slots (tag symbol-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements symbol-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *symbol-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element function-ref) stream)
  (with-slots (tag function-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements function-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *function-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element type-ref) stream)
  (with-slots (tag type-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements type-tags tag)
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
  (funcall *image-writer* stream (slot-value element 'path)))

;; table
(defmethod element-print ((element table) stream)
  (let ((processed-table (loop for row in rows
			       collect (mapcar #'text-type-to-string row))))
    (funcall *table-writer* stream processed-table)))

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

;; code
(defun shortest-string (strings)
  (loop for str in strings
	for shortest = str then (if (< (length str) (length shortest))
				    str
				    shortest)
	finally (return shortest)))

(defun make-custom-symbol-pprint-function (hide-symbol hide-str)
  (lambda (stream sym)
    (if (eq sym hide-symbol)
	(format stream hide-str)
	(let* ((sym-package (symbol-package sym))
	       (nickname (and sym-package
			      (shortest-string (package-nicknames sym-package))))
	       (print-package-mode (and sym-package
					(not (equal sym-package (find-package "CL")))
					(case (nth-value 1 (find-symbol (symbol-name sym) sym-package))
					  (:external :external)
					  (:internal (if (or (boundp sym) (fboundp sym))
							 :internal
							 nil))
					  (t nil))))
	       (package-to-print (and print-package-mode
				      (or nickname
					  (and (keywordp sym) "")
					  (package-name sym-package))))
	       (*print-escape* nil)
	       (*print-pprint-dispatch* normal-pprint-dispatch))
	  (case print-package-mode
	    (:external (format stream "~a:~a" package-to-print (symbol-name sym)))
	    (:internal (format stream "~a::~a" package-to-print (symbol-name sym)))
	    (t (format stream "~a" (symbol-name sym))))))))

(defun comment-pprint (stream code-comment)
  (format stream ";; ~a" (cadr code-comment)))

(defun make-custom-pprint-dispatch (hide-symbol hide-str comment-symbol)
  (let ((normal-pprint-dispatch *print-pprint-dispatch*)
	(custom-pprint-dispatch (copy-pprint-dispatch)))    
    (set-pprint-dispatch '(cons (member adp:defclass)) (pprint-dispatch '(defclass)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defconstant)) (pprint-dispatch '(defconstant)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defgeneric)) (pprint-dispatch '(defgeneric)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-compiler-macro)) (pprint-dispatch '(define-compiler-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-condition)) (pprint-dispatch '(define-condition)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-method-combination)) (pprint-dispatch '(define-method-combination)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-modify-macro)) (pprint-dispatch '(define-modify-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-setf-expander)) (pprint-dispatch '(define-setf-expander)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-symbol-macro)) (pprint-dispatch '(define-symbol-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defmacro)) (pprint-dispatch '(defmacro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defmethod)) (pprint-dispatch '(defmethod)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defpackage)) (pprint-dispatch '(defpackage)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defparameter)) (pprint-dispatch '(defparameter)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defsetf)) (pprint-dispatch '(defsetf)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defstruct)) (pprint-dispatch '(defstruct)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:deftype)) (pprint-dispatch '(deftype)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defun)) (pprint-dispatch '(defun)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defvar)) (pprint-dispatch '(defvar)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch 'symbol (make-custom-symbol-pprint-function hide-symbol hide-str) 0 custom-pprint-dispatch)
    (set-pprint-dispatch `(cons (member ,comment-symbol)) #'comment-pprint 0 custom-pprint-dispatch)))

(defun code-to-string (code)
  "Turn a code element into a string."
  (declare (type code code))
  (with-slots (expr hide-symbol comment-symbol) code
    (let ((custom-pprint-dispatch (make-custom-pprint-dispatch hide-symbol hide-str comment-symbol)))
      (let ((*print-pprint-dispatch* custom-pprint-dispatch))
	(with-output-to-string (stream)
	  (prin1 expr stream))))))

(defmethod element-print ((element code-block) stream)
  (with-slots (code-type code-elements) element
    (let ((complete-code-elements (mapcan (lambda (code-or-ref)
					    (if (typep code-or-ref 'code-reference)
						(with-slots (tag code-tags) code-or-ref
						  (multiple-value-bind (tag-code-elements tag-foundp) (tag-table-find-elements code-tags tag)
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
  (with-slots (code output result) element
    (let ((code-string (format nil "~{~a~^~%~%~}" (mapcar #'code-to-string code-elements))))
      (funcall *code-example-writer* stream code output result))))

;; definition
(defmacro define-definition-element-print (type writer)
  (with-gensyms (element stream expr)
    `(defmethod element-print ((,element ,type) ,stream)
       (with-slots (,expr) ,element
	 (funcall ,writer ,stream ,expr)))))

(define-definition-element-print defclass *defclass-writer*)
(define-definition-element-print defconstant *defconstant-writer*)
(define-definition-element-print defgeneric *defgeneric-writer*)
(define-definition-element-print define-compiler-macro *define-compiler-macro-writer*)
(define-definition-element-print define-condition *define-condition-writer*)
(define-definition-element-print define-method-combination *define-method-combination-writer*)
(define-definition-element-print define-modify-macro *define-modify-macro-writer*)
(define-definition-element-print define-setf-expander *define-setf-expander-writer*)
(define-definition-element-print define-symbol-macro *define-symbol-macro-writer*)
(define-definition-element-print defmacro *defmacro-writer*)
(define-definition-element-print defmethod *defmethod-writer*)
(define-definition-element-print defpackage *defpackage-writer*)
(define-definition-element-print defparameter *defparameter-writer*)
(define-definition-element-print defsetf *defsetf-writer*)
(define-definition-element-print defstruct *defstruct-writer*)
(define-definition-element-print deftype *deftype-writer*)
(define-definition-element-print defun *defun-writer*)
(define-definition-element-print defvar *defvar-writer*)

