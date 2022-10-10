
(in-package :adp)


(cl:defparameter *add-documentation* nil)
(cl:defparameter *current-style* nil)


;; ----- adp concepts -----

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
(defparameter *file-adp-elements* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'adp-element))

(declaim (ftype (function (adp-element) t)) push-adp-element)
(defun push-adp-element (elem)
  (vector-push-extend elem *file-adp-elements*))

(declaim (ftype (function (keyword &rest t) t) emplace-adp-element))
(defun emplace-adp-element (key-type &rest contents)
  (push-file-adp-element (apply #'create-adp-element key-type contents)))

(declaim (ftype (function () t) empty-adp-elements))
(defun empty-adp-elements ()
  (setf (fill-pointer *file-adp-elements*) 0))


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
(defparameter *project-adp-files* (make-vector 10 :adjustable t :fill-pointer 0))

(declaim (ftype (function (adp-file) t) push-adp-file))
(defun push-adp-file (adp-file)
  (vector-push-extend adp-file *project-adp-files*))

(declaim (ftype (function (pathname (vector adp-element)) t) emplace-adp-file))
(defun emplace-adp-file (path elements)
  (vector-push-extent (create-adp-file path elements) *project-adp-files*))

(declaim (ftype (function () t) empty-adp-files))
(defun empty-adp-files ()
  (setf (fill-pointer *project-adp-files*) 0))



;; ----- toplevel guide functions -----

(cl:defmacro def-customizable-writer (ftype-decl global-proc-name writer-name)
  (with-gensyms (global-proc-name writer-arg writer-args body-arg)
    `(progn
       (declaim (type ,ftype-decl ,global-proc-name))
       (defparameter ,global-proc-name nil)
       (defmacro ,writer-name (,writer-args &body ,body-arg)
	 (check-type ,writer-args list)
	 (loop for ,writer-arg in ,writer-args
	       do (check-type ,writer-arg symbol))
	 `(setq ,',global-proc-name (lambda ,,writer-args
				      ,@,body-arg))))))

(cl:defun slice-format (&rest args)
  (with-output-to-string (str)
    (loop for arg in args
	  do (if (stringp arg)
		 (princ arg str)
		 (prin1 arg str)))))


(def-customizable-writer
    (function (stream string) t)
  *header-proc*
  def-header-writer)

(defmacro header (str &optional label)
  (when *add-documentation*
    `(emplace-adp-element :header ,str ,label)))


(def-customizable-writer
    (function (stream string) t)
  *subheader-proc*
  def-subheader-writer)

(defmacro subheader (str &optional label)
  (when *add-documentation*
    `(emplace-adp-element :subheader ,str ,label)))


(def-customizable-writer
    (function (stream string) t)
  *subsubheader-proc*
  def-subsubheader-writer)

(defmacro subsubheader (str &optional label)
  (when *add-documentation*
    `(emplace-adp-element :subsubheader ,str ,label)))


(def-customizable-writer
    (function (stream string) t)
  *text-proc*
  def-text-writer)

(defmacro text (&rest objects)
  (when *add-documentation*
      `(emplace-adp-element :text (slice-format ,@objects))))


(def-customizable-writer
    (function (stream list) t)
  *table-proc*
  def-table-writer)

(defmacro table (&rest rows)
  (when *add-documentation*
    (loop for row in rows
	  do (check-type row list "a list"))
    `(emplace-adp-element :table (list ,(loop for row in rows
					      collect `(list ,(loop for elem in row
								    if (listp elem)
								      collect `(slice-format ,@elem)
								    else
								      collect `(slice-format ,elem))))))))


(def-customizable-writer
    (function (stream list) t)
  *itemize-proc*
  def-itemize-writer)

(defmacro itemize (&rest items)
  (when *add-documentation*
    (labels ((process-items (item-list)
	       (cond
		 ((eq (car item-list) :itemize)
		  (loop for item in item-list
			collect (process-items item)))
		 ((eq (car item-list) :item)
		  `(:item (slice-format ,@(cdr item-list))))
		 (t (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" (car item-list))))))
      `(emplace-adp-element :itemize ,(process-items (cons :itemize items))))))


(def-customizable-writer
    (function (stream string pathname pathname) t)
  *image-proc*
  def-image-writer)

(defmacro image (alt-text path)
  (when *add-documentation*
    (with-gensyms (let-alt-text let-path)
      `(let ((,let-alt-text ,alt-text)
	     (,let-path ,path))
	 (declare (type string ,let-alt-text)
		  (type pathname ,let-path))
	 (emplace-adp-element :image ,let-alt-text ,let-path)))))


(def-customizable-writer
    (function (stream string) t)
  *bold-proc*
  def-bold-writer)

(defmacro bold (&rest args)
  (when *add-documentation*
    (with-gensyms (stream)
      `(with-output-to-string (,stream)
	 (funcall *bold-proc* ,stream (slice-format ,@args))))))


(def-customizable-writer
    (function (stream string) t)
  *italic-proc*
  def-italic-writer)

(defmacro italic (&rest args)
    (when *add-documentation*
      (with-gensyms (stream)
	`(with-output-to-string (,stream)
	   (funcall *italic-proc* ,stream (slice-format ,@args))))))


(def-customizable-writer
    (function (stream t) t)
  *code-inline-proc*
  def-code-inline-writer)

(defmacro code-inline (code)
  (when *add-documentation*
    (with-gensyms (stream)
      `(with-output-to-string (,stream)
	 (funcall *code-inline-proc* ,stream ',code)))))


(def-customizable-writer
    (function (stream string string) t)
  *web-link-proc*
  def-web-link-writer)

(defmacro web-link (name link)
  (when *add-documentation*
    (with-gensyms (let-name let-link stream)
      `(let ((,let-name ,name)
	     (,let-link ,link))
	 (declare (type string ,let-name ,let-link))
	 (with-output-to-string (,stream)
	   (funcall *web-link-proc* ,stream ,let-name ,let-link))))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *symbol-ref-proc*
  def-symbol-ref-writer)

(defmacro symbol-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:symbol-ref ,let-label)))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *function-ref-proc*
  def-function-ref-writer)

(defmacro function-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:function-ref ,let-label)))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *header-ref-proc*
  def-header-ref-writer)

(defmacro header-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:header-ref ,let-label)))))


(declaim (type hash-table *code-tags*))
(defparameter *code-tags* (make-hash-table))

(declaim (ftype (function (symbol &rest t) t) add-code-tag))
(defun add-code-tag (tag &rest list-code)
  (when (not (get-hash tag *code-tags*))
      (setf (get-hash tag *code-tags*) (make-array 10 :adjustable t :fill-pointer 0)))
  (loop for code in list-code
	do (vector-push-extend code (get-hash tag *code-tags*))))

(declaim (ftype (function (symbol) boolean) code-tagp))
(defun code-tagp (tag)
  (get-hash tag *code-tags*))

(declaim (ftype (function (symbol) vector) get-code-tag))
(defun get-code-tag (tag)
  (get-hash tag *code-tags*))


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

(cl:defamcro code-focus (tags &rest code)
  `(progn ,@code))

(cl:defmacro code-tag (tags &rest code)
  (with-gensyms (tag)
    `(progn
     ,@(loop for expr in code
	     collect (remove-own-code-tag-exprs expr))
     ,@(when *add-documentation*
	 (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol nor a list" tags)
	 (when (listp tags)
	   (assert (every #'symbolp tags) (tags) "Thare is a non-symbol in ~s" tags))
	 `((loop for ,tag in ',tags
		 do (apply #'add-code-tag ,tag ',code)))))))


(def-customizable-writer
    (function (list) t)
  *code-block-proc*
  def-code-block-writer)

(defmacro code-block (tags &rest code)
  (when *add-documentation*
    (with-gensyms (expr)
      `(emplace-adp-element :code-block (loop for ,expr in ',code
					      if (and (symbolp ,expr)
						      (member ,expr ',tags))
						append (process-code-tag ,expr (get-code-tag ,expr))
					      else
						collect (process-code-tag '#:dummy-tag ,expr))))))


(def-customizable-writer
    (function (list) t)
  *code-example-proc*
  def-code-example-writer)
 
(defmacro code-example (tags &rest code)
  (when *add-documentation*
    (let ((evaluated-code (loop for expr in code
				collect (with-gensyms (output result)
					  `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
										  ,(remove-code-tag-exprs (if (and (symbolp expr)
														   (member expr tags))
													      (get-code-tag expr)
													      expr))))))
					     (list ',(if (and (symbolp expr)
							      (member expr tags))
							 (process-code-tag expr (get-code-tag expr))
							 (process-code-tag '#:dummy-tag expr))
						   ,output
						   ,result))))))
      `(emplace-adp-element :code-example (list ,@evaluated-code)))))




;; ----- api functions -----

(cl:defmacro def-cl-customizable-writer (name name-proc name-writer)
  (with-gensyms (definer-body)
    `(progn
       (def-customizable-writer
	   (function (t) t)
	 ,name-proc
	 ,name-writer)
       (cl:defmacro ,name (&rest ,definer-body)
	 `(progn
	    ,(cons ',(find-symbol (symbol-name name) '#:cl) ,definer-body)
	    ,@(when *add-documentation*
		`((emplace-adp-element ,',(intern (symbol-name name) '#:keyword) ',,definer-body))))))))

;; TODO: Coger tags
(def-cl-customizable-writer defclass *defclass-proc* def-defclass-writer)
(def-cl-customizable-writer defconstant *defconstant-proc* def-defconstant-writer)
(def-cl-customizable-writer defgeneric *defgeneric-proc* def-defgeneric-writer)
(def-cl-customizable-writer define-compiler-macro *define-compiler-macro-proc* def-define-compiler-macro-writer)
(def-cl-customizable-writer define-condition *define-condition-proc* def-define-condition-writer)
(def-cl-customizable-writer define-method-combination *define-method-combination-proc* def-define-method-combination-writer)
(def-cl-customizable-writer define-modify-macro *define-modify-macro-proc* def-define-modify-macro-writer)
(def-cl-customizable-writer define-setf-expander *define-setf-expander-proc* def-define-setf-expander-writer)
(def-cl-customizable-writer define-symbol-macro *define-symbol-macro-proc* def-define-symbol-macro-writer)
(def-cl-customizable-writer defmacro *defmacro-proc* def-defmacro-writer)
(def-cl-customizable-writer defmethod *defmethod-proc* def-defmethod-writer)
(def-cl-customizable-writer defpackage *defpackage-proc* def-defpackage-writer)
(def-cl-customizable-writer defparameter *defparameter-proc* def-defparameter-writer)
(def-cl-customizable-writer defsetf *defsetf-proc* def-defsetf-writer)
(def-cl-customizable-writer defstruct *defstruct-proc* def-defstruct-writer)
(def-cl-customizable-writer deftype *deftype-proc* def-deftype-writer)
(def-cl-customizable-writer defun *defun-proc* def-defun-writer)
(def-cl-customizable-writer defvar *defvar-proc* def-defvar-writer)


;; ----- writer functions -----

(macrolet ((def-add-documentation-in-file-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-add-documentation-in-file-writer ((file-path) &body body)
		    (unless (symbolp file-path)
		      (error "The argument must be a symbol. Found: ~s" file-path))
		    `(setq ,',doc-global-proc (lambda (,file-path)
						,@body)))
		  (defun add-documentation-in-file-impl (file-path)
		    (unless (eq :relative (car (pathname-directory (pathname file-path))))
		      (error "The pathname ~s is not a relative path." file-path))
		    (unless (pathname-name (pathname file-path))
		      (error "The pathname ~s does not designate any file." file-path))
		    (when ,doc-global-proc
		      (apply ,doc-global-proc file-path)))
		  (defmacro add-documentation-in-file (file-path)
		    (when *add-documentation*
		      `(add-documentation-in-file-impl ,file-path)))))))
  (def-add-documentation-in-file-definer))


(cl:defun style-file (style)
  (let* ((style-name (string style))
	 (style-file (asdf:system-relative-pathname #:apd (concatenate 'string "/styles/" style-name ".lisp"))))
    (truename style-file)))


(cl:defun load-style (style)
  (load (style-file style))
  (setf *current-style* style))


(macrolet ((def-add-documentation-please-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-add-documentation-please-writer ((root-dir) &body body)
		    (unless (symbolp root-dir)
		      (error "Expected a symbol. Found: ~s" root-dir))
		    `(setq ,',doc-global-proc (lambda (,root-dir)
						,@body)))
		  (defun add-documentation-please (system root-dir)
		    (unless (eq :absolute (car (pathname-directory root-dir)))
		      (error "The pathname ~s is not absolute." root-dir))
		    (unless (uiop:directory-exists-p root-dir)
		      (error "The directory ~s does no exist." root-dir))
		    (when (not *current-style*)
		      (load-style :markdown))
		    (let ((*add-documentation* t))
		      (asdf:load-system system :force t))
		    (funcall ,doc-global-proc root-dir))))))
  (def-add-documentation-please-definer))



