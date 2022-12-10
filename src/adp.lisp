
(in-package :adp)


;; ----- special variables -----

(cl:defvar *adp* nil)
(cl:defvar *project* nil)


;; ----- Literature -----

(cl:defmacro define-header-macro (name)
  (let ((macro-doc (format nil "Add a ~a with name str. Also, if tag is not nil but a symbol, a new header-tag is created."
			   (string-downcase (symbol-name name)))))
    (with-gensyms (str tag fixed-tag)
      `(cl:defmacro ,name (,str &optional ,tag)
	 ,macro-doc
	 (when *adp*
	   (check-type ,str string "a string")
	   (check-type ,tag (or null symbol) "a symbol or nil")
	   (let ((,fixed-tag (or ,tag (gensym))))
	     `(progn
		(adppvt:add-element *project* (make-instance ',',name
							     :name ,,(string-downcase (symbol-name name))
							     :tag ,,fixed-tag
							     :source-location (relative-truename *project*)))
		(values))))))))

(define-header-macro header)
(define-header-macro subheader)
(define-header-macro subsubheader)


(cl:defmacro text (&rest objects)
  "Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, function-ref and type-ref."
  (when *adp*
    `(progn
       (adppvt:add-element *project* (make-instance 'text
						    :name "text"
						    :text-elements (list ,@objects)
						    :source-location (relative-truename *project*)))
       (values))))


(cl:defmacro cell (&rest objects)
  "Create a cell to place into a table. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, function-ref and type-ref."
  (when *adp*
    `(make-instance 'cell
		    :name "cell"
		    :text-element (list ,@objects)
		    :source-location (relative-truename *project*))))


(cl:defmacro table (&rest rows)
  "Add a table. Each argument must be a list of text macro calls."
  (when *adp*
    (loop with row-length = (length (car rows))
	  for row in rows
	  do (check-type row list "a list")
	     (assert (= row-length (length row)) () "Each row must have the same length."))
    `(progn
       (adppvt:add-element *project* (make-instance 'table
						    :name "table"
						    :rows ,@(loop for row in rows
								  collect (cons 'list row))
						    :source-location (relative-truename *project*)))
       (values))))


(cl:defmacro item (&rest items)
  "Create an item to be placed into a iterate/enumerate form. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, function-ref and type-ref."
  (when *adp*
    `(make-instance 'item
		    :name "item"
		    :text-elements (list ,@items)
		    :source-location (relative-truename *project*))))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (cl:defun process-itemize (itemize-form)
    (case (car itemize-form)
      (itemize   `(make-instance 'itemize
				 :name "itemize"
				 :elements (list ,@(mapcar #'process-itemize (cdr itemize-form)))
				 :source-location (relative-truename *project*)))
      (enumerate `(make-instance 'enumerate
				 :name "enumerate"
				 :elements (list ,@(mapcar #'process-itemize (cdr itemize-form)))
				 :source-location (relative-truename *project*))))))


(cl:defmacro itemize (&whole itemize-form)
  "Add a list of items. Each argument must be a list. Each list must start with the symbol item, itemize or enumerate. If 
item is used, the rest of the elements in that list will be treated as if using the macro text. If itemize or enumerate is used the rest 
of elements must be lists that must start with item, itemize or enumerate. In other words, when itemize or enumerate is used 
a nested list is added. A certain symbol will be printed before each element of the list."
  (when *adp*
    `(progn
       (adppvt:add-element *project* ,(process-itemize itemize-form))
       (values))))


(cl:defmacro enumerate (&whole enumerate-form)
  "Same as itemize, but a number is printed before each element."
  (when *adp*
    `(progn
       (adppvt:add-element *project* ,(process-itemize enumerate-form))
       (values))))


(cl:defmacro table-of-contents ()
  "Add a list of all headers and subheaders used in the system. The headers from different
files are shown in the same order the files are loaded."
  (when *adp*
    '(progn
      (adppvt:add-element (make-instance 'table-of-contents
			   :name "table-of-contents"
			   :source-location (relative-truename *project*)))
      (values))))


(cl:defmacro mini-table-of-contents ()
  "Add a list of all headers, subheaders and subsubheaders used in the current documentation file."
  (when *adp*
    '(progn
      (adppvt:add-element (make-instance 'mini-table-of-contents
			   :name "mini-table-of-contents"
			   :source-location (relative-truename *project*)))
      (values))))


(cl:defmacro table-of-functions ()
  "Add an ordered list of all functions and macros defined using ADP."
  (when *adp*
    '(progn
      (adppvt:add-element (make-instance 'table-of-functions
			    :name "table-of-functions"
			    :source-location (relative-truename *project*)))
      (values))))


(cl:defmacro table-of-symbols ()
  "Add an ordered list of all variables defined using ADP."
  (when *adp*
    '(progn
      (adppvt:add-element (make-instance 'table-of-symbols
			   :name "table-of-symbols"
			   :source-location (relative-truename *project*)))
      (values))))


(cl:defmacro table-of-types ()
  "Add an ordered list of all types defined using ADP."
  (when *adp*
    '(progn
      (adppvt:add-element (make-instance 'table-of-types
			   :name "table-of-types"
			   :source-location (relative-truename *project*)))
      (values))))


(cl:defmacro image (alt-text path)
  "Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located."
  (when *adp*
    (check-type alt-text string "a string")
    (check-type path pathname "a pathname")
    `(progn
       (adppvt:add-element *project* (make-instance 'image
						    :name "image"
						    :path ,path
						    :source-location (relative-truename *project*)))
       (values))))


(cl:defmacro define-text-enrichment-macro (name docstring)
  (with-gensyms (args)
    `(cl:defmacro ,name (&rest ,args)
       ,docstring
       (when *adp*
	 `(make-instance ',',name
			 :name ,(string-downcase ,(symbol-name name))
			 :text-elements (list ,@,args)
			 :source-location (relative-truename *project*))))))

(define-text-enrichment-macro bold
  "Add bold style to text. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro italic
  "Add italic style to text. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro bold-italic
  "Add bold and italic style to text. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro code-inline
  "Add inlined style to text. Each argument is princ-ed and concatenated into a string.")


(cl:defmacro web-link (name link)
  "Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings."
  (when *adp*
    (check-type name string "a string")
    (check-type link string "a string")
    `(make-instance 'web-link
		    :name "web-link"
		    :text ,name
		    :address ,link
		    :source-location (relative-truename *project*))))


(cl:defmacro define-reference-macro (name docstring)
  (with-gensyms (tag)
    `(cl:defmacro ,name (,tag)
       ,docstring
       (when *adp*
	 (check-type ,tag symbol "a symbol")
	 `(make-instance ',',name
			 :name ,,(string-downcase (symbol-name name))
			 :tag ,,tag
			 :source-location (relative-truename *project*))))))

(define-reference-macro header-ref
  "Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.")

(define-reference-macro symbol-ref
  "Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.")

(define-reference-macro function-ref
  "Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.")

(define-reference-macro type-ref
  "Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.")


(cl:defvar *hide-symbol* '#:hide)
(cl:defvar *comment-symbol* '#:comment)

(cl:defun plistp (code)
  "Check if an expression is a proper list."
  (or (null code)
      (and (consp code)
	   (plistp (cdr code)))))

(cl:defun remove-code-tag (expr)
  "Remove the forms recognized by code-tag."
  (labels ((remove-code-tag-exprs-aux (code)
	     (if (plistp code)
		 (cond
		   ((member (car code) '(code-hide code-remove))
		    (mapcan #'remove-code-tag-exprs-aux (cddr code)))
		   ((eq (car code) 'code-tag)
		    (list code))
		   ((member (car code) '(code-quote code-comment))
		    nil)
		   (t
		    (list (mapcan #'remove-code-tag-exprs-aux code))))
		 (list code))))
    (car (remove-code-tag-exprs-aux expr))))

(cl:defun valid-tag-p (tag tags)
  "Check if a tag is accepted by a list of tags."
  (declare (type symbol tag) (type list tags))
  (or (null tags)
      (member tag tags)))

(cl:defun process-code-tag (tag code)
  "Process the forms recognized by code-tag."
  (declare (type symbol tag))
  (labels ((process-aux (tag code)
	     (if (plistp code)
		 (cond
		   ((eq (car code) 'code-hide)
		    (if (valid-tag-p tag (cadr code))
			(list *hide-symbol*)
			(loop for expr in (cddr code)
			      append (process-aux tag expr))))
		   ((eq (car code) 'code-remove)
		    (if (valid-tag-p tag (cadr code))
			nil
			(loop for expr in (cddr code)
			      append (process-aux tag expr))))
		   ((eq (car code) 'code-quote)
		    (loop for expr in (cdr code)
			  append (process-aux tag expr)))
		   ((eq (car code) 'code-comment)
		    (list `(,*comment-symbol* ,(caddr code))))
		   ((eq (car code) 'code-tag)
		    (loop for expr in (cddr code)
			  append (process-aux tag expr)))
		   (t
		    (list (loop for expr in code
				append (process-aux tag expr)))))
		 (list code))))
    (car (process-aux tag code))))

(cl:defmacro code-tag (tags &body exprs)
  "Assign several tags to several forms. The forms are placed into a progn form. The argument tags must be a list
of symbols. If no tags are provided, an error is raised. Each symbol in tags will be a code-tag assigned to code.
The same tag can be used several times in different calls to code-tag.  Inside the code-tag form it is correct to use
the next forms: code-hide, code-remove, code-show and code-comment. 
  - code-hide: It has the syntax (code-hide (&rest tags) &rest forms). code-hide receives a list of tags. If a tag 
               used in code-tag also appears in code-hide, the rest of forms will be hidden when using the macro code-block. 
               If the list of tags in code-hide is empty, the forms will be hidden for every tag used in code-tag.
               Hidding the code means printing \"...\" instead of the forms.
  - code-remove: Same as code-hide, but removes the code instead of printing \"...\"
  - code-quote: It has the same syntax as code-hide and code-remove. The forms in code-quote are never evaluated, but will be
                shown when using code-block if some tag from code-tag is used in code-quote. Otherwise it won't print anything.
  - code-comment: It has the systax (code-comment (&rest tags) comment). The comment will be printed when using code-block as
                  a comment using \";;\" if some tag from code-tag is used in code-comment. Otherwise, it won't print anything."
  `(progn
     ,@(when *adp*
	 (check-type tags list "a list")
	 (assert (not (null tags)) (tags) "The list of tags must have at least one symbol.")
	 (loop for tag in tags
	       do (check-type tag symbol "a symbol"))
	 (with-gensyms (tag expr)
	   `((loop for ,tag in ',tags
		   do (loop for ,expr in ',exprs
			    do (apply #'adppvt:add-code-tag *project* ,tag (make-instance 'tagged-code
											  :name "tagged-code"
											  :hide-symbol *hide-symbol*
											  :comment-symbol *comment-symbol*
											  :tag tag
											  :expr (process-code-tag ,expr))))))))
     ,@(remove-code-tag exprs)))


(cl:defmacro code-block ((&rest tags) &body code)
  "Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is printed instead of the symbol."
  (when *adp*
    (check-type tags list "a list")
    (loop for tag in tags
	  do (check-type tag symbol "a symbol")
	     (assert (member tag code) () "The tag ~s is not present in the code-block form." tag))
    (assert (not (null code)) () "Expected at least one expression in a code-block form.")
    (with-gensyms (expr)
      `(progn
	 (adppvt:add-element *project* (make-instance 'code-block
						      :name "code-block"
						      :code-type "Lisp"
						      :code-elements (loop for ,expr in ',code
									   if (and (symbolp ,expr)
										   (member ,expr ',tags))
									     collect (make-instance 'code-ref
												    :name "code-ref"
												    :tag ,expr
												    :source-location (relative-truename *project*))
									   else
									     collect (make-instance 'code
												    :name "code"
												    :expr ,expr
												    :source-location (relative-truename *project*)))))
	 (values)))))


(cl:defmacro verbatim-code-block (lang-or-text &optional (text nil textp))
  "Add a block of text. It can receive up to two arguments. If only one argument is received it must be a string of text that will be printed inside the block.
If two arguments are received, the text to be printed must be the second one, and the first argument is a string representing the language used for writing
the text."
  (when *adp*
    (check-type lang-or-text string)
    (check-type textp (or null string) "a string or NIL")
    (let ((true-lang (and textp lang-or-text))
	  (true-text (if textp text lang-or-text)))
      `(progn
	 (adppvt:add-element *project* (make-instance 'verbatim-code-block
						      :name "verbatim-code-block"
						      :code-type ,true-lang
						      :code-text ,true-text
						      :source-location (relative-truename *project*)))
	 (values)))))


(cl:defmacro code-example (&body code)
  "Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed."
  (when *adp*
      (assert (not (null code)) () "Expected at least one expression in a code-example form.")
      (with-gensyms (output result expr)
	`(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
		(,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
						,@code))))

	   (adppvt:add-element *project* (make-instance 'code-example
							:name "code-example"
							:code-elements (mapcar (lambda (,expr)
										 (make-instance 'code
												:name "code"
												:expr ,expr
												:source-location (relative-truename *project*)))
									       ,code)
							:output ,output
							:result ,result
							:source-location (relative-truename *project*)))
	   (values)))))


;; ----- API -----

(cl:defmacro define-definition-macro (name type tag-extraction-expr docstring)
  (let ((body (if tag-extraction-expr
		  (car tag-extraction-expr)
		  (gensym)))
	(tag-extraction (if tag-extraction-expr
			    (cadr tag-extraction-expr)
			    nil)))
    `(cl:defmacro ,name (&body ,body)
       ,docstring
       `(progn
	  ,@(when *adp*
	      `((adppvt:add-element *project* (make-instance ',',type
							     :name ,,(string-downcase (symbol-name name))
							     :expr '(,',(find-symbol (symbol-name name) "CL") ,@,body)
							     ,@,(when tag-extraction-expr
								  `'(:tag ,tag-extraction))
							     :source-location (relative-truename *project*)))))
	  (,(find-symbol (symbol-name ',name) "CL") ,@,body)))))

(define-definition-macro defclass defclass-definition (body (car body))
  "Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag.")
(define-definition-macro defconstant defconstant-definition (body (car body))
  "Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag.")
(define-definition-macro defgeneric defgeneric-definition (body (car body))
  "Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag.")
(define-definition-macro define-compiler-macro define-compiler-macro-definition nil
  "Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro.")
(define-definition-macro define-condition define-condition-definition (body (car body))
  "Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag.")
(define-definition-macro define-method-combination define-method-combination-definition nil
  "Add a define-method-combination declaration. The macro expands to cl:define-method-combination.")
(define-definition-macro define-modify-macro define-modify-macro-definition (body (car body))
  "Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag.")
(define-definition-macro define-setf-expander define-setf-expander-definition nil
  "Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander.")
(define-definition-macro define-symbol-macro define-symbol-macro-definition (body (car body))
  "Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag.")
(define-definition-macro defmacro defmacro-definition (body (car body))
  "Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag.")
(define-definition-macro defmethod defmethod-definition nil
  "Add a defmethod declaration. The macro expands to cl:defmethod.")
(define-definition-macro defpackage defpackage-definition nil
  "Add a defpackage declaration. The macro expands to cl:defpackage.")
(define-definition-macro defparameter defparameter-definition (body (car body))
  "Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag.")
(define-definition-macro defsetf defsetf-definition nil
  "Add a defsetf declaration. The macro expands to cl:defsetf.")
(define-definition-macro defstruct defstruct-definition (body (car body))
  "Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag.")
(define-definition-macro deftype deftype-definition (body (car body))
  "Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag.")
(define-definition-macro defun defun-definition (body (if (symbolp (car body))
					 (car body)
					 nil))
  "Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag.")
(define-definition-macro defvar defvar-definition (body (car body))
  "Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag.")


;; ----- Writers -----

(cl:defmacro in-file (path)
  (when *adp*
    (check-type path pathname)
    (let* ((true-path (truename path))
	   (fixed-true-path (make-pathname :directory (cons :relative (cdr (pathname-directory true-path)))
					   :name (pathname-name path)
					   :type (pathname-type path))))
      `(select-file *project* ,fixed-true-path))))


(uiop:with-upgradability ()
  (cl:defclass load-doc-source-op (asdf/lisp-action:basic-load-op asdf/action:downward-operation asdf/action:selfward-operation)
    ((asdf/action:selfward-operation :initform 'asdf:prepare-source-op :allocation :class))
    (:documentation "Operation for loading a Lisp file as source with ADP documentation.")))

(cl:defvar *doc-system* nil)
(cl:defvar *already-visited* nil)

(uiop:with-upgradability ()
  (cl:defmethod asdf:action-description ((o load-doc-source-op) (c asdf:component))
    (format nil (uiop:compatfmt "~@<Loading source of ~3i~_~A~@:>") c))
  (cl:defmethod asdf:action-description ((o load-doc-source-op) (c asdf:parent-component))
    (format nil (uiop:compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))
  (cl:defun perform-lisp-load-source (o c)
    "Perform the loading of a Lisp file as associated to specified action (O . C)"
    (asdf/lisp-action:call-with-around-compile-hook
     c #'(lambda ()
           (let ((*adp* (equal (asdf:component-system c) *doc-system*)))
	     (when (not *already-visited*)
	       (setf *already-visited* t)
	       (setf *gensym-counter* 0))
	     (uiop:load* (first (asdf:input-files o c))
			 :external-format (asdf:component-external-format c))))))

  (cl:defmethod asdf:perform ((o load-doc-source-op) (c asdf:cl-source-file))
    (perform-lisp-load-source o c))
  (cl:defmethod asdf:perform ((o load-doc-source-op) (c asdf:static-file))
    nil))


(cl:defun load-style (style)
  "Load an adp style."
  (declare (type symbol style))
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (assert (asdf:find-system style-system) (style-system) "ADP error: The style ~s was not found." style-system)
    (format t "~%Loading the style ~s" style)
    (asdf:operate 'asdf:load-source-op style-system :force t)))

(cl:defun load-project (system)
  (format t "~%Loading the system ~s" system)
  (asdf:operate 'load-doc-source-op system :force t))

(cl:defun load-system (system style &rest style-args)
  "Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed."
  (declare (type symbol style))
  (assert (asdf:find-system system) (system) "ADP error: The system ~s was not found." system)

  (adppvt:with-special-writers

    (adppvt:with-new-style-parameter-list

      (load-style style)
    
      (adppvt:check-special-writers)
    
      (adppvt:with-style-parameters style-args
	
	(let* ((*adp* t)
	       (root-path (truename (asdf:system-source-directory system)))
	       (fixed-root-path (make-pathname :host (pathname-host root-path)
					       :device (pathname-device root-path)
					       :directory (pathname-directory root-path)))
	       (*project* (make-instance 'project :root-directiory fixed-root-path))
	       (*gensym-counter* 0))

	  (load-project system)

	  (format t "~%Writing documentation")
	  (adppvt:project-print *project*)
	  (format t "~%~a" "Done!")))))

  (values))
