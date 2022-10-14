
(in-package :adp)


(cl:defparameter *add-documentation* nil)
(cl:defparameter *current-style* nil)


;; ----- adp concepts -----

(cl:deftype adp-element ()
  '(cons keyword list))

(declaim (ftype (function (keyword &rest t) adp-element) create-adp-element))
(cl:defun create-adp-element (key-type &rest contents)
  (cons key-type contents))

(declaim (ftype (function (adp-element) keyword) adp-element-key-type))
(cl:defun adp-element-key-type (elem)
  (car elem))

(declaim (ftype (function (adp-element) list) adp-element-contents))
(cl:defun adp-element-contents (elem)
  (cdr elem))


(declaim (type (vector adp-element) *file-adp-elements*))
(cl:defparameter *file-adp-elements* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'adp-element))

(declaim (ftype (function (adp-element) t) push-adp-element))
(cl:defun push-adp-element (elem)
  (vector-push-extend elem *file-adp-elements*))

(declaim (ftype (function (keyword &rest t) t) emplace-adp-element))
(cl:defun emplace-adp-element (key-type &rest contents)
  (push-adp-element (apply #'create-adp-element key-type contents)))

(declaim (ftype (function () t) empty-adp-elements))
(cl:defun empty-adp-elements ()
  (setf (fill-pointer *file-adp-elements*) 0))


(cl:deftype adp-file ()
  '(cons pathname (vector adp-element)))

(declaim (ftype (function (pathname (vector adp-element)) adp-file) create-adp-file))
(cl:defun create-adp-file (path elements)
  (cons path elements))

(declaim (ftype (function (adp-file) pathname) adp-file-path))
(cl:defun adp-file-path (adp-file)
  (car adp-file))

(declaim (ftype (function (adp-file) (vector adp-element)) adp-file-elements))
(cl:defun adp-file-elements (adp-file)
  (cdr adp-file))


(declaim (type (vector adp-file) *project-adp-files*))
(cl:defparameter *project-adp-files* (make-array 10 :adjustable t :fill-pointer 0))

(declaim (ftype (function (adp-file) t) push-adp-file))
(cl:defun push-adp-file (adp-file)
  (vector-push-extend adp-file *project-adp-files*))

(declaim (ftype (function (pathname (vector adp-element)) t) emplace-adp-file))
(cl:defun emplace-adp-file (path elements)
  (vector-push-extend (create-adp-file path elements) *project-adp-files*))

(declaim (ftype (function () t) empty-adp-files))
(cl:defun empty-adp-files ()
  (setf (fill-pointer *project-adp-files*) 0))


(declaim (type (vector (cons symbol string)) *header-tags*))
(cl:defparameter *header-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (type (vector symbol) *symbol-tags* *function-tags* *type-tags*))
(cl:defparameter *symbol-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))
(cl:defparameter *function-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))
(cl:defparameter *type-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (ftype (function (symbol string) t) push-header-tag))
(cl:defun push-header-tag (tag str)
  (vector-push-extend (cons tag str) *header-tags*))

(declaim (ftype (function (symbol) t) push-symbol-tag))
(cl:defun push-symbol-tag (tag)
  (vector-push-extend tag *symbol-tags*))

(declaim (ftype (function (symbol) t) push-function-tag))
(cl:defun push-function-tag (tag)
  (vector-push-extend tag *function-tags*))

(declaim (ftype (function (symbol) t) push-type-tag))
(cl:defun push-type-tag (tag)
  (vector-push-extend tag *type-tags*))

(declaim (ftype (function (symbol) boolean) header-tagp))
(cl:defun header-tagp (tag)
  (loop for header-tag across *header-tags*
	  thereis (eq tag header-tag)))

(declaim (ftype (function (symbol) boolean) symbol-tagp))
(cl:defun symbol-tagp (tag)
  (loop for symbol-tag across *symbol-tags*
	  thereis (eq tag symbol-tag)))

(declaim (ftype (function (symbol) boolean) function-tagp))
(cl:defun function-tagp (tag)
  (loop for function-tag across *function-tags*
	  thereis (eq tag function-tag)))

(declaim (ftype (function (symbol) boolean) type-tagp))
(cl:defun type-tagp (tag)
  (loop for type-tag across *type-tags*
	  thereis (eq tag type-tag)))

(cl:defun empty-header-tags ()
  (setf (fill-pointer *header-tags*) 0))

(cl:defun empty-symbol-tags ()
  (setf (fill-pointer *symbol-tags*) 0))

(cl:defun empty-function-tags ()
  (setf (fill-pointer *function-tags*) 0))

(cl:defun empty-type-tags ()
  (setf (fill-pointer *type-tags*) 0))


(declaim (type hash-table *header-tags-table* *symbol-tags-table* *function-tags-table* *type-tags-table*))
(cl:defparameter *header-tags-table* (make-hash-table))
(cl:defparameter *symbol-tags-table* (make-hash-table))
(cl:defparameter *function-tags-table* (make-hash-table))
(cl:defparameter *type-tags-table* (make-hash-table))

(declaim (ftype (function (symbol string pathname) t) add-header-tag-path))
(cl:defun add-header-tag-path (tag str path)
  (setf (gethash tag *header-tags-table*) (cons str path)))

(declaim (ftype (function (symbol pathname) t) add-symbol-tag-path))
(cl:defun add-symbol-tag-path (tag path)
  (setf (gethash tag *symbol-tags-table*) path))

(declaim (ftype (function (symbol pathname) t) add-function-tag-path))
(cl:defun add-function-tag-path (tag path)
  (setf (gethash tag *function-tags-table*) path))

(declaim (ftype (function (symbol pathname) t) add-type-tag-path))
(cl:defun add-type-tag-path (tag path)
  (setf (gethash tag *type-tags-table*) path))

(declaim (ftype (function (symbol) (or (cons string pathname) null)) get-header-tag-path))
(cl:defun get-header-tag-path (tag)
  (values (gethash tag *header-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-symbol-tag-path))
(cl:defun get-symbol-tag-path (tag)
  (values (gethash tag *symbol-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-function-tag-path))
(cl:defun get-function-tag-path (tag)
  (values (gethash tag *function-tags-table*)))

(declaim (ftype (function (symbol) (or pathname null)) get-type-tag-path))
(cl:defun get-type-tag-path (tag)
  (values (gethash tag *type-tags-table*)))


;; ----- toplevel guide functions -----

(cl:defmacro def-customizable-writer (ftype-decl global-proc-name writer-name)
  (with-gensyms (writer-arg writer-args body-arg)
    `(progn
       (declaim (type (or ,ftype-decl null) ,global-proc-name))
       (cl:defparameter ,global-proc-name nil)
       (cl:defmacro ,writer-name (,writer-args &body ,body-arg)
	 (check-type ,writer-args list)
	 (loop for ,writer-arg in ,writer-args
	       do (check-type ,writer-arg symbol))
	 `(setq ,',global-proc-name (lambda ,,writer-args
				      ,@,body-arg))))))


(def-customizable-writer
    (function (stream string symbol) t)
  *header-proc*
  def-header-writer)

(cl:defmacro header (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :header ,str ,label))))


(def-customizable-writer
    (function (stream string symbol) t)
  *subheader-proc*
  def-subheader-writer)

(cl:defmacro subheader (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :subheader ,str ,label))))


(def-customizable-writer
    (function (stream string symbol) t)
  *subsubheader-proc*
  def-subsubheader-writer)

(cl:defmacro subsubheader (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :subsubheader ,str ,label))))


(def-customizable-writer
    (function (stream string) t)
  *text-proc*
  def-text-writer)

(cl:defmacro text (&rest objects)
  (when *add-documentation*
      `(emplace-adp-element :text ,@objects)))


(def-customizable-writer
    (function (stream list) t)
  *table-proc*
  def-table-writer)

(cl:defmacro table (&rest rows)
  (when *add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(emplace-adp-element :table (list ,(loop for row in rows
					      collect `(list ,(loop for elem in row
								    collect (cons 'list elem))))))))


(def-customizable-writer
    (function (stream list) t)
  *itemize-proc*
  def-itemize-writer)

(cl:defmacro itemize (&rest items)
  (when *add-documentation*
    (labels ((check-items (item-list)
	       (loop for item in item-list
		     if (not (eq (car item) :item))
		       do (if (eq (car item) :itemize) 
			      (check-items (cdr item))
			      (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" item)))))
      (check-items items))
    `(emplace-adp-element :itemize ,@items)))


(def-customizable-writer
    (function (stream string pathname pathname) t)
  *image-proc*
  def-image-writer)

(cl:defmacro image (alt-text path)
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

(cl:defmacro bold (&rest args)
  (when *add-documentation*
    (with-gensyms (stream)
      `(with-output-to-string (,stream)
	 (funcall *bold-proc* ,stream (slice-format ,@args))))))


(def-customizable-writer
    (function (stream string) t)
  *italic-proc*
  def-italic-writer)

(cl:defmacro italic (&rest args)
    (when *add-documentation*
      (with-gensyms (stream)
	`(with-output-to-string (,stream)
	   (funcall *italic-proc* ,stream (slice-format ,@args))))))


(def-customizable-writer
    (function (stream t) t)
  *code-inline-proc*
  def-code-inline-writer)

(cl:defmacro code-inline (code)
  (when *add-documentation*
    (with-gensyms (stream)
      `(with-output-to-string (,stream)
	 (funcall *code-inline-proc* ,stream ',code)))))


(def-customizable-writer
    (function (stream string string) t)
  *web-link-proc*
  def-web-link-writer)

(cl:defmacro web-link (name link)
  (when *add-documentation*
    (with-gensyms (let-name let-link stream)
      `(let ((,let-name ,name)
	     (,let-link ,link))
	 (declare (type string ,let-name ,let-link))
	 (with-output-to-string (,stream)
	   (funcall *web-link-proc* ,stream ,let-name ,let-link))))))


(def-customizable-writer
    (function (stream symbol string pathname pathname) t)
  *header-ref-proc*
  def-header-ref-writer)

(cl:defmacro header-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:header-ref ,let-label)))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *symbol-ref-proc*
  def-symbol-ref-writer)

(cl:defmacro symbol-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:symbol-ref ,let-label)))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *function-ref-proc*
  def-function-ref-writer)

(cl:defmacro function-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:function-ref ,let-label)))))


(def-customizable-writer
    (function (stream symbol pathname pathname) t)
  *type-ref-proc*
  def-type-ref-writer)

(cl:defmacro type-ref (label)
  (when *add-documentation*
    (with-gensyms (let-label)
      `(let ((,let-label ,label))
	 (declare (type symbol ,let-label))
	 '(:type-ref ,let-label)))))



(declaim (type hash-table *code-tags*))
(cl:defparameter *code-tags* (make-hash-table))

(declaim (ftype (function (symbol &rest t) t) add-code-tag))
(cl:defun add-code-tag (tag &rest list-code)
  (when (not (gethash tag *code-tags*))
      (setf (gethash tag *code-tags*) (make-array 10 :adjustable t :fill-pointer 0)))
  (loop for code in list-code
	do (vector-push-extend code (gethash tag *code-tags*))))

(declaim (ftype (function (symbol) boolean) code-tagp))
(cl:defun code-tagp (tag)
  (values (gethash tag *code-tags*)))

(declaim (ftype (function (symbol) vector) get-code-tag))
(cl:defun get-code-tag (tag)
  (values (gethash tag *code-tags*)))


(declaim (type symbol *hide-symbol*))
(cl:defparameter *hide-symbol* '#:hide)

(declaim (ftype (function (t) boolean) code-hidep))
(cl:defun code-hidep (code)
  (eq code *hide-symbol*))

(declaim (ftype (function (t) boolean) plistp))
(cl:defun plistp (code)
  (or (null code)
      (and (consp code)
	   (plistp (cdr code)))))

(declaim (ftype (function (t) t) remove-code-tag-exprs))
(cl:defun remove-code-tag-exprs (code)
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
(cl:defun remove-own-code-focus-exprs (code)
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
(cl:defun process-code-tag (tag code)
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

;; (cl:defmacro code-focus (tags &rest code)
;;   `(progn ,@code))

(cl:defmacro code-tag (tags &rest code)
  (with-gensyms (tag)
    `(progn
     ,@(loop for expr in code
	     collect (remove-own-code-focus-exprs expr))
     ,@(when *add-documentation*
	 (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol nor a list" tags)
	 (when (listp tags)
	   (assert (every #'symbolp tags) (tags) "Thare is a non-symbol in ~s" tags))
	 `((loop for ,tag in ',tags
		 do (apply #'add-code-tag ,tag ',code)))))))


(def-customizable-writer
    (function (stream list) t)
  *code-block-proc*
  def-code-block-writer)

(cl:defmacro code-block (tags &rest code)
  (when *add-documentation*
    (with-gensyms (expr)
      `(emplace-adp-element :code-block (loop for ,expr in ',code
					      if (and (symbolp ,expr)
						      (member ,expr ',tags))
						append (process-code-tag ,expr (get-code-tag ,expr))
					      else
						collect (process-code-tag '#:dummy-tag ,expr))))))


(def-customizable-writer
    (function (stream list) t)
  *code-example-proc*
  def-code-example-writer)
 
(cl:defmacro code-example (tags &rest code)
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


(def-customizable-writer
    (function (stream t) t)
  *defclass-proc*
  def-defclass-writer)

(cl:defmacro defclass (&rest defclass-body)
  `(progn
     (cl:defclass ,@defclass-body)
     (push-type-tag (car ',defclass-body))
     (emplace-adp-element :defclass '(cl:defclass ,@defclass-body))))


(def-customizable-writer
    (function (stream t) t)
  *defconstant-proc*
  def-defconstant-writer)

(cl:defmacro defconstant (&rest defconstant-body)
  `(progn
     (cl:defconstant ,@defconstant-body)
     (push-symbol-tag (car ',defconstant-body))
     (emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body))))


(def-customizable-writer
    (function (stream t) t)
  *defgeneric-proc*
  def-defgeneric-writer)

(cl:defmacro defgeneric (&rest defgeneric-body)
  `(progn
     (cl:defgeneric ,@defgeneric-body)
     (push-function-tag (car ',defgeneric-body))
     (emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-compiler-macro-proc*
  def-define-compiler-macro-writer)

(cl:defmacro define-compiler-macro (&rest define-compiler-macro-body)
  `(progn
     (cl:define-compiler-macro ,@define-compiler-macro-body)
     (emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-condition-proc*
  def-define-condition-writer)

(cl:defmacro define-condition (&rest define-condition-body)
  `(progn
     (cl:define-condition ,@define-condition-body)
     (push-type-tag (car ',define-condition-body))
     (emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-method-combination-proc*
  def-define-method-combination-writer)

(cl:defmacro define-method-combination (&rest define-method-combination-body)
  `(progn
     (cl:define-method-combination ,@define-method-combination-body)
     (emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-modify-macro-proc*
  def-define-modify-macro-writer)

(cl:defmacro define-modify-macro (&rest define-modify-macro-body)
  `(progn
     (cl:define-modify-macro ,@define-modify-macro-body)
     (push-function-tag (car ',define-modify-macro-body))
     (emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-setf-expander-proc*
  def-define-setf-expander-writer)

(cl:defmacro define-setf-expander (&rest define-setf-expander-body)
  `(progn
     (cl:define-setf-expander ,@define-setf-expander-body)
     (push-function-tag (list 'cl:setf (car ',define-setf-expander-body)))
     (emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))))


(def-customizable-writer
    (function (stream t) t)
  *define-symbol-macro-proc*
  def-define-symbol-macro-writer)

(cl:defmacro define-symbol-macro (&rest define-symbol-macro-body)
  `(progn
     (cl:define-symbol-macro ,@define-symbol-macro-body)
     (push-symbol-tag (car ',define-symbol-macro-body))
     (emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body))))


(def-customizable-writer
    (function (stream t) t)
  *defmacro-proc*
  def-defmacro-writer)

(cl:defmacro defmacro (&rest defmacro-body)
  `(progn
     (cl:defmacro ,@defmacro-body)
     (push-function-tag (car ',defmacro-body))
     (emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body))))


(def-customizable-writer
    (function (stream t) t)
  *defmethod-proc*
  def-defmethod-writer)

(cl:defmacro defmethod (&rest defmethod-body)
  `(progn
     (cl:defmethod ,@defmethod-body)
     (emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))))


(def-customizable-writer
    (function (stream t) t)
  *defpackage-proc*
  def-defpackage-writer)

(cl:defmacro defpackage (&rest defpackage-body)
  `(progn
     (cl:defpackage ,@defpackage-body)
     (emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))))


(def-customizable-writer
    (function (stream t) t)
  *defparameter-proc*
  def-defparameter-writer)

(cl:defmacro defparameter (&rest defparameter-body)
  `(progn
     (cl:defparameter ,@defparameter-body)
     (push-symbol-tag (car ',defparameter-body))
     (emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body))))


(def-customizable-writer
    (function (stream t) t)
  *defsetf-proc*
  def-defsetf-writer)

(cl:defmacro defsetf (&rest defsetf-body)
  `(progn
     (cl:defsetf ,@defsetf-body)
     (push-function-tag (list 'cl:setf (car ',defsetf-body)))
     (emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))))


(def-customizable-writer
    (function (stream t) t)
  *defstruct-proc*
  def-defstruct-writer)

(cl:defmacro defstruct (&rest defstruct-body)
  `(progn
     (cl:defstruct ,@defstruct-body)
     (push-type-tag (car ',defstruct-body))
     (emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body))))


(def-customizable-writer
    (function (stream t) t)
  *deftype-proc*
  def-deftype-writer)

(cl:defmacro deftype (&rest deftype-body)
  `(progn
     (cl:deftype ,@deftype-body)
     (push-type-tag (car ',deftype-body))
     (emplace-adp-element :deftype '(cl:deftype ,@deftype-body))))


(def-customizable-writer
    (function (stream t) t)
  *defun-proc*
  def-defun-writer)

(cl:defmacro defun (&rest defun-body)
  `(progn
     (cl:defun ,@defun-body)
     (push-function-tag (car ',defun-body))
     (emplace-adp-element :defun '(cl:defun ,@defun-body))))


(def-customizable-writer
    (function (stream t) t)
  *defvar-proc*
  def-defvar-writer)

(cl:defmacro defvar (&rest defvar-body)
  `(progn
     (cl:defvar ,@defvar-body)
     (push-symbol-tag (car ',defvar-body))
     (emplace-adp-element :defvar '(cl:defvar ,@defvar-body))))


;; ----- file functions -----

(def-customizable-writer
    (function () string)
  *get-file-extension-proc*
  def-get-file-extension-writer)


(cl:defmacro add-documentation-in-file (file-path)
  (when *add-documentation*
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (assert (pathname-directory ,file-path) (,file-path) "The ~s pathname has not a directory part." ,file-path)
	   (assert (pathname-name ,file-path) (,file-path) "The ~s pathname has not a name part." ,file-path)
	   (let ((,let-file-path (make-pathname :directory (cons :relative (cdr (pathname-directory ,file-path)))
						:name (pathname-name ,file-path))))
	     (emplace-adp-file ,let-file-path (copy-array *file-adp-elements*))
	     (empty-adp-elements)
	     (loop for (,header-tag . ,header-str) across *header-tags*
		   for ,symbol-tag across *symbol-tags*
		   for ,function-tag across *function-tags*
		   for ,type-tag across *type-tags*
		   do (add-header-tag-path ,header-tag ,header-str ,let-file-path)
		      (add-symbol-tag-path ,symbol-tag ,let-file-path)
		      (add-function-tag-path ,function-tag ,let-file-path)
		      (add-type-tag-path ,type-tag ,let-file-path)
		   finally (empty-header-tags)
			   (empty-symbol-tags)
			   (empty-function-tags)
			   (empty-type-tags))))))))


(cl:defun slice-format (root-path &rest args)
  (with-output-to-string (stream)
    (loop for arg in args
	  do (cond
	       ((listp arg)
		(cond
		  ((eq (car arg) :header-ref)
		   (assert (get-header-tag-path (cadr arg)) ((cadr arg)) "~s is not a header tag." (cadr arg)) 
		   (let* ((header-tag (cadr arg))
			  (header-str-path (get-header-tag-path header-tag))
			  (header-str (car header-str-path))
			  (header-rel-path (cdr header-str-path)))
		     (funcall *header-ref-proc* stream header-tag header-str root-path header-rel-path)))
		  ((eq (car arg) :symbol-ref)
		   (assert (get-header-tag-path (cadr arg)) ((cadr arg)) "~s is not a symbol tag." (cadr arg))
		   (let* ((symbol-tag (cadr arg))
			  (symbol-path (get-symbol-tag-path symbol-tag)))
		     (funcall *symbol-ref-proc* stream symbol-tag root-path symbol-path)))
		  ((eq (car arg) :function-ref)
		   (assert (get-function-tag-path (cadr arg)) ((cadr arg)) "~s is not a function tag." (cadr arg))
		   (let* ((function-tag (cadr arg))
			  (function-path (get-function-tag-path function-tag)))
		     (funcall *function-ref-proc* stream function-tag root-path function-path)))
		  ((eq (car arg) :type-ref)
		   (assert (get-type-tag-path (cadr arg)) ((cadr arg)) "~s is not a type tag." (cadr arg))
		   (let* ((type-tag (cadr arg))
			  (type-path (get-type-tag-path type-tag)))
		     (funcall *type-ref-proc* stream type-tag root-path type-path)))))
	       (t (princ arg stream))))))


(declaim (ftype (function (stream pathname (vector adp-element)) t) write-file-contents))
(cl:defun write-file-contents (stream root-path elements)
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


(def-customizable-writer
    (function (stream) t)
  *file-header-proc*
  def-file-header-writer)

(def-customizable-writer
    (function (stream) t)
  *file-foot-proc*
  def-file-foot-writer)


(declaim (ftype (function (pathname pathname (vector adp-element)) t) write-file))
(cl:defun write-file (root-path rel-path elements)
  (let* ((complete-path (make-pathname :host (pathname-host root-path)
				       :device (pathname-device root-path)
				       :directory (append (pathname-directory root-path) (cdr (pathname-directory rel-path)))
				       :name (pathname-name rel-path)
				       :type (funcall *get-file-extension-proc*))))
    (multiple-value-bind (file-path created) (ensure-directories-exist complete-path :verbose nil)
      (when (not created)
	(error "The directories from ~s could not be created." file-path)))
    (with-open-file (stream complete-path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (funcall *file-header-proc* stream)
      (write-file-contents stream root-path elements)
      (funcall *file-foot-proc* stream))))


(def-customizable-writer
    (function (pathname) t)
  *system-files-proc*
  def-system-files-writer)


(cl:defun write-system-files (root-path)
  (funcall *system-files-proc* root-path)
  (loop for (rel-path . file-contents) across *project-adp-files*
	do (write-file root-path rel-path file-contents)))





(cl:defun load-style (style)
  (asdf:load-system style))


(declaim (type list *style-parameters*))
(cl:defparameter *style-parameters* nil)

(cl:defun add-style-parameter (name key-name required)
  (push (list name key-name required) *style-parameters*))

(cl:defun style-parameterp (key-name)
  (member key-name *style-parameters* :key #'cadr))

(cl:defun style-parameter-requiredp (key-name)
  (cadar (member key-name *style-parameters* :key #'cadr)))

(cl:defun style-required-parameters ()
  (loop for (name key-name requiredp) in *style-parameters*
	if requiredp
	  collect key-name))

(cl:defun set-parameter-value (key-name value)
  (loop for (name key requiredp) in *style-parameters*
	until (eq key key-name)
	finally (setf (symbol-value name) value)))

(cl:defmacro def-style-parameter (name &key (value nil) (key-name nil) (required nil))
  (check-type name symbol "a symbol")
  (check-type key-name keyword "a keyword")
  `(progn
     (cl:defparameter ,name ,value)
     (if ,key-name
	 (add-style-parameter ,name ,key-name ,required)
	 (add-style-parameter ,name (intern (symbol-name ,name) :keyword) *style-parameters*))))

(cl:defun check-style-parameters (style-params)
  (let ((required-parameters (style-required-parameters))
	(style-parameter-key-names (loop for key-name in style-params by #'cddr
					 collect key-name)))
    (loop for style-parameter-key-name in style-parameter-key-names
	  if (not (style-parameterp style-parameter-key-name))
	    do (error "The parameter ~s is not allowed." style-parameter-key-name))
    (loop for required-param in required-parameters
	  if (not (member required-param style-parameter-key-names))
	    do (error "The required param ~s is not used." required-param))))


(cl:defun load-documentation-system (system root-path &rest style-args)
  (check-style-parameters style-args)
  (let ((*add-documentation*))
    (asdf:load-system system :force t))
  (loop for (name value) in style-args by #'cddr
	do (set-parameter-value name value))
  (let ((fixed-root-path (make-pathname :host (pathname-host root-path)
					:device (pathname-device root-path)
					:directory (cons :absolute (cdr (pathname-directory root-path))))))
    (write-system-files fixed-root-path)))






