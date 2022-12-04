
(in-package :addpvt)


;; -----------------------
;; ----- add-element -----
;; -----------------------

(defun project-relative-pathname (project path)
  "Make path to be relative to the project root directory."
  (declare (type project project) (type pathname path))
  (with-slots (root-directory) project
    (let* ((root-level (length (cdr (pathname-directory root-directory))))
	   (relative-path (make-pathname :directory (cons :relative (nthcdr (1+ root-level) (pathname-directory path)))
					 :name (pathname-name path)
					 :type (pathname-type path))))
      (values relative-path))))

(defgeneric add-element (project element)
  (:documentation
   "Add an element into a project."))

(defmethod add-element (project (element element))
  (with-slots (current-file) project
    (when (not current-file)
      ;; Usar condition e interceptar en adp.lisp
      )
    (with-slots (source-location file-location) element
      (setf file-location current-file)
      (setf source-location (project-relative-pathname project source-location))
      (file-push-element current-file element))))

(defmethod add-element :after (project (element header-type))
  (with-slots (header-tags) project
    (with-slots (tag (header-location source-location)) element 
      (multiple-value-bind (previous-header foundp) (tag-table-find-elements header-tags tag)
	(if foundp
	    ;; Usar condition
	    (tag-table-set-element header-tags tag element))))))


(defmethod add-element :after (project (element code-block))
  (with-slots (code-tags) project
    (setf (slot-value element 'code-tags) code-tags)))


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
(defun text-to-string (text)
  "Turn a text element into a string."
  (declare (type text text))
  (let ((processed-elements (mapcar (lambda (text-element)
				      (if (typep text-element 'element)
					  (with-output-to-string (str-stream)
					    (element-print text-element str-stream))
					  (funcall *escape-text* text-element)))
				    text-elements)))
    (apply #'concatenate 'string processed-elements)))

(defmethod element-print ((element text) stream)
  (let ((text-elements (slot-value element 'text-elements)))
    (funcall *text-writer* stream (text-to-string element))))

;; text enrichment
(defmethod element-print ((element bold) stream)
  (funcall *bold-writer* stream (text-to-string (slot-value element 'text))))

(defmethod element-print ((element italic) stream)
  (funcall *italic-writer* stream (text-to-string (slot-value element 'text))))

(defmethod element-print ((element bold-italic) stream)
  (funcall *bold-italic-writer* stream (text-to-string (slot-value element 'text))))

(defmethod element-print ((element code-inline) stream)
  (funcall *code-inline-writer* stream (text-to-string (slot-value element 'text))))

;; text reference
(defmethod element-print ((element header-ref) stream)
  (with-slots (tag header-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements header-tags tag)
      (if element-found-p
	  (with-slots (file-location title) (aref associated-elements 0)
	    (funcall *header-ref-writer* stream tag title file-location))
	  ;; usar condition e interceptarlos en adp.lisp
	  ))))

(defmethod element-print ((element symbol-ref) stream)
  (with-slots (tag symbol-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements symbol-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *symbol-ref-writer* stream tag file-location))
	  ;; usar condition e interceptarlos en adp.lisp
	  ))))

(defmethod element-print ((element function-ref) stream)
  (with-slots (tag function-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements function-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *function-ref-writer* stream tag file-location))
	  ;; usar condition e interceptarlos en adp.lisp
	  ))))

(defmethod element-print ((element type-ref) stream)
  (with-slots (tag type-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements type-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *type-ref-writer* stream tag file-location))
	  ;; usar condition e interceptarlos en adp.lisp
	  ))))

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
			       collect (mapcar #'text-to-string row))))
    (funcall *table-writer* stream processed-table)))

;; itemize
(defgeneric process-itemize (element)
  (:documentation
   "Turn an itemize element into a style-maker suitable representation.")

  (:method ((element item))
    (list :item (text-to-string (slot-value item 'text))))

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
							;; Usar un condition e interceptarlo en adp.lisp
							)))
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

