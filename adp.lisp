

(in-package :adp)


(cl:defmacro adv-header (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :header ,str ',label)
       (values))))


(cl:defmacro adv-subheader (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',label)
       (values))))


(cl:defmacro adv-defmacro (&body defmacro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body))))
     (cl:defmacro ,@defmacro-body)))

(cl:defmacro adv-defun (&body defun-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defun-body))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (car ',defun-body))))
     (cl:defun ,@defun-body)))


(cl:defmacro adv-write-in-file (file-path)
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-directory file-path) (file-path) "The ~s pathname has not a directory part." file-path)
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (cons :relative (cdr (pathname-directory ,file-path)))
						:name (pathname-name ,file-path))))
	     (adppvt:push-adp-file ,let-file-path)
	     (adppvt:push-file-tag ,let-file-path)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		   finally (adppvt:empty-header-tags))
	     (loop for ,symbol-tag across adppvt:*symbol-tags*
		   do (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		   finally (adppvt:empty-symbol-tags))
	     (loop for ,function-tag across adppvt:*function-tags*
		   do (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		   finally (adppvt:empty-function-tags))
	     (loop for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-type-tags)))
	   (values))))))


;; ----- ADP interface -----

(adv-header "ADP User Interface" user-api-header)


;; ----- Guide functions -----

(adv-subheader "Literate programming functions" literate-subheader)

(adv-defmacro header (str &optional tag)
  "Add a header with name str. Also, if tag is not nil but a symbol, a new header-tag is created."	      
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    `(progn
       ,@(when tag
	   `((adppvt:push-header-tag ',tag ,str)))
       (adppvt:emplace-adp-element :header ,str ',tag)
       (values))))


(adv-defmacro subheader (str &optional tag)
  "Same as header, but add a subheader."
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    `(progn
       ,@(when tag
	   `((adppvt:push-header-tag ',tag ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',tag)
       (values))))


(adv-defmacro subsubheader (str &optional tag)
  "Same as header, but add a subsubheader."
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    `(progn
       ,@(when tag
	   `((adppvt:push-header-tag ',tag ,str)))
       (adppvt:emplace-adp-element :subsubheader ,str ',tag)
       (values))))


(adv-defmacro text (&rest objects)
  "Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, 
function-ref, type-ref and file-ref."
  (when adppvt:*add-documentation*
    `(progn
       (adppvt:emplace-adp-element :text ,@objects)
       (values))))


(adv-defmacro table (&rest rows)
  "Add a table. Each argument must be a list of lists. Each inner list must have as first element the keyword :cell and the rest 
are treated as if using the macro text."
  (when adppvt:*add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(progn
       (adppvt:emplace-adp-element :table ,@(loop for row in rows
						  collect (cons 'list (loop for elem in row
									    collect (cons 'list elem)))))
       (values))))


(adv-defmacro itemize (&rest items)
  "Add a list of items. Each argument must be a list. Each list must have as first argument the keyword :item or :itemize. If 
:item is used, the rest of the elements in that list will be treated as if using the macro text. If :itemize is used the rest 
of elements must be lists where its first elements are the keywords :item or :itemize. In other words, when :itemize is used 
a nested list is added."
  (when adppvt:*add-documentation*
    (labels ((check-items (item-list)
	       (loop for item in item-list
		     if (not (eq (car item) :item))
		       do (if (eq (car item) :itemize) 
			      (check-items (cdr item))
			      (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" item)))))
      (check-items items))
    (labels ((process-itemize-items (item-list)
	       (loop for item in item-list
		     if (eq (car item) :item)
		       collect (cons 'list item)
		     else
		       collect (list* 'list :itemize (process-itemize-items (cdr item))))))
      `(progn
	 (adppvt:emplace-adp-element :itemize ,@(process-itemize-items items))
	 (values)))))


(adv-defmacro image (alt-text path)
  "Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located."
  (when adppvt:*add-documentation*
    (check-type alt-text string "a string")
    (check-type path pathname "a pathname")
    `(progn
       (adppvt:emplace-adp-element :image ,alt-text ,path)
       (values))))


(adv-defmacro bold (&rest args)
  "Add bold style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(adppvt:create-bold-text ,@args)))


(adv-defmacro italic (&rest args)
  "Add italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(adppvt:create-italic-text ,@args)))


(adv-defmacro bold-italic (&rest args)
  "Add bold and italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(adppvt:create-bold-italic-text ,@args)))


(adv-defmacro code-inline (&rest code)
  "Add inlined style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(adppvt:create-code-inline-text ,@code)))


(adv-defmacro web-link (name link)
  "Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings."
  (when adppvt:*add-documentation*
    (check-type name string "a string")
    (check-type link string "a string")
    `(adppvt:create-web-link-text ,name ,link)))


(adv-defmacro file-ref (path)
  "Add a reference to a documentation file when using the macros text, table or itemize. The argument is the pathname, relative 
to the system's root directory, to the referenced file. Only pathnames used with write-in-file are valid. "
  (when adppvt:*add-documentation*
    (check-type path pathname "a pathname")
    `(adppvt:create-file-ref-text ,path)))


(adv-defmacro header-ref (tag)
  "Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(adppvt:create-header-ref-text ',tag)))


(adv-defmacro symbol-ref (tag)
  "Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(adppvt:create-symbol-ref-text ',tag)))


(adv-defmacro function-ref (tag)
  "Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(adppvt:create-function-ref-text ',tag)))


(adv-defmacro type-ref (tag)
  "Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(adppvt:create-type-ref-text ',tag)))


(adv-defmacro code-tag (tags &body code)
  "Assign several tags to a piece of code. The code is placed into a progn form. The argument tags must be a list
of symbols. If no tags are provided, then code-tag will do nothing. Each symbol in tags will be a code-tag assigned to code. 
Inside the code-tag form it is correct to use a (code-hide tags &rest forms) form. It only indicates to code-tag 
which parts of code can be hidden when using the tag in the macro code-block. code-hide accepts also a list of tags. If a tag 
used in code-tag also appears in code-hide, that piece of code will be hidden when using the macro code-block. If the list of 
tags in code-hide is empty, the that piece of code will be hidden for every tag used in code-tag."
  (with-gensyms (tag)
    `(progn
       ,@(when adppvt:*add-documentation*
	   (check-type tags list "a list")
	   (loop for tag in tags
		 do (check-type tag symbol "a symbol"))
	   `((loop for ,tag in ',tags
		   do (apply #'adppvt:add-code-tag ,tag (adppvt:process-code-tag ,tag ',code)))))
       ,@(adppvt:remove-own-code-hide-exprs code))))


(adv-defmacro code-block (tags &body code)
  "Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is prin1-ed instead of the symbol."
  (when adppvt:*add-documentation*
    (check-type tags list "a list")
    (loop for tag in tags
	  do (check-type tag symbol "a symbol"))
    (with-gensyms (expr)
      `(progn
	 (adppvt:emplace-adp-element :code-block (loop for ,expr in ',code
						     if (and (symbolp ,expr)
							     (member ,expr ',tags))
						       append (adppvt:create-code-block-tag ,expr)
						     else
						       collect (adppvt:process-code-tag '#:dummy-tag ,expr)))
	 (values)))))


(adv-defmacro code-example (&body code)
  "Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the results of each piece of code are also printed."
  (when adppvt:*add-documentation*
    (let ((evaluated-code (loop for expr in code
				collect (with-gensyms (output result)
					  `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
										  ,(adppvt:remove-code-tag-exprs expr)))))
					     (list ',(adppvt:process-code-tag '#:dummy-tag expr)
						   ,output
						   ,result))))))
      `(progn
	 (adppvt:emplace-adp-element :code-example (list ,@evaluated-code))
	 (values)))))


;; ----- API functions -----

(adv-subheader "API documentation functions" api-subheader)

(adv-defmacro defclass (&body defclass-body)
  "Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defclass-body))
	   (adppvt:emplace-adp-element :defclass '(cl:defclass ,@defclass-body) (car ',defclass-body))))
     (cl:defclass ,@defclass-body)))


(adv-defmacro defconstant (&body defconstant-body)
  "Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defconstant-body))
	   (adppvt:emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body) (car ',defconstant-body))))
     (cl:defconstant ,@defconstant-body)))


(adv-defmacro defgeneric (&body defgeneric-body)
  "Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defgeneric-body))
	   (adppvt:emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body) (car ',defgeneric-body))))
     (cl:defgeneric ,@defgeneric-body)))


(adv-defmacro define-compiler-macro (&body define-compiler-macro-body)
  "Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))))
     (cl:define-compiler-macro ,@define-compiler-macro-body)))


(adv-defmacro define-condition (&body define-condition-body)
  "Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',define-condition-body))
	   (adppvt:emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body) (car ',define-condition-body))))
     (cl:define-condition ,@define-condition-body)))


(adv-defmacro define-method-combination (&body define-method-combination-body)
  "Add a define-method-combination declaration. The macro expands to cl:define-method-combination."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))))
     (cl:define-method-combination ,@define-method-combination-body)))


(adv-defmacro define-modify-macro (&body define-modify-macro-body)
  "Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',define-modify-macro-body))
	   (adppvt:emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body) (car ',define-modify-macro-body))))
     (cl:define-modify-macro ,@define-modify-macro-body)))


(adv-defmacro define-setf-expander (&body define-setf-expander-body)
  "Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))))
     (cl:define-setf-expander ,@define-setf-expander-body)))


(adv-defmacro define-symbol-macro (&body define-symbol-macro-body)
  "Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',define-symbol-macro-body))
	   (adppvt:emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body) (car ',define-symbol-macro-body))))
     (cl:define-symbol-macro ,@define-symbol-macro-body)))


(adv-defmacro defmacro (&body defmacro-body)
  "Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body))))
     (cl:defmacro ,@defmacro-body)))


(adv-defmacro defmethod (&body defmethod-body)
  "Add a defmethod declaration. The macro expands to cl:defmethod."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))))
     (cl:defmethod ,@defmethod-body)))


(adv-defmacro defpackage (&body defpackage-body)
  "Add a defpackage declaration. The macro expands to cl:defpackage."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))))
     (cl:defpackage ,@defpackage-body)))


(adv-defmacro defparameter (&body defparameter-body)
  "Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defparameter-body))
	   (adppvt:emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body) (car ',defparameter-body))))
     (cl:defparameter ,@defparameter-body)))


(adv-defmacro defsetf (&body defsetf-body)
  "Add a defsetf declaration. The macro expands to cl:defsetf."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))))
     (cl:defsetf ,@defsetf-body)))


(adv-defmacro defstruct (&body defstruct-body)
  "Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defstruct-body))
	   (adppvt:emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body) (car ',defstruct-body))))
     (cl:defstruct ,@defstruct-body)))


(adv-defmacro deftype (&body deftype-body)
  "Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',deftype-body))
	   (adppvt:emplace-adp-element :deftype '(cl:deftype ,@deftype-body) (car ',deftype-body))))
     (cl:deftype ,@deftype-body)))


(adv-defmacro defun (&body defun-body)
  "Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((when (symbolp (car ',defun-body))
	     (adppvt:push-function-tag (car ',defun-body)))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (if (symbolp (car ',defun-body))
									   (car ',defun-body)
									   nil))))
     (cl:defun ,@defun-body)))


(adv-defmacro defvar (&body defvar-body)
  "Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defvar-body))
	   (adppvt:emplace-adp-element :defvar '(cl:defvar ,@defvar-body) (car ',defvar-body))))
     (cl:defvar ,@defvar-body)))


;; ----- Writer functions -----

(adv-subheader "Documentation writer function" writer-subheader)


(adv-defmacro write-in-file (file-path)
  "Associate all the information gathered so far with the pathname file-path. This will cause the creation of a file 
where all the information will be printed in. The pathname will be considered relative to the system's root directory.
Only the directory and name parts of file-path are considered. The rest are ignored (including the extension). This 
macro can be used multiple times."
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (if (pathname-directory ,file-path)
							       (cons :relative (cdr (pathname-directory ,file-path)))
							       nil)
						:name (pathname-name ,file-path))))
	     (adppvt:push-adp-file ,let-file-path)
	     (adppvt:push-file-tag ,let-file-path)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		   finally (adppvt:empty-header-tags))
	     (loop for ,symbol-tag across adppvt:*symbol-tags*
		   do (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		   finally (adppvt:empty-symbol-tags))
	     (loop for ,function-tag across adppvt:*function-tags*
		   do (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		   finally (adppvt:empty-function-tags))
	     (loop for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-type-tags)))
	   (values))))))


(declaim (ftype (function (t symbol &rest t) t) load-documentation-system))
(adv-defun load-documentation-system (system style &rest style-args)
  "Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed."
  (assert (asdf:find-system system) (system) "The system ~s was not found." system)
  (adppvt:remove-current-procs)
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (assert (asdf:find-system style-system) (style-system) "The style ~s was not found." style-system)
    (asdf:load-system style-system :force t))
  (adppvt:check-current-procs)
  (adppvt:check-style-parameters style-args)
  (adppvt:remove-current-data)
  (let ((adppvt:*add-documentation* t))
    (asdf:load-system system :force t))
  (loop for (name value) in style-args by #'cddr
	do (adppvt:set-parameter-value name value))
  (let* ((root-path (asdf:system-source-directory system))
	 (fixed-root-path (make-pathname :host (pathname-host root-path)
					 :device (pathname-device root-path)
					 :directory (pathname-directory root-path))))
    (adppvt:write-system-files fixed-root-path)))


;; ----- Additional functions -----

(adv-subheader "Additional functions")

(adv-defmacro cl-ref (sym)
  "Add a reference to a Common Lisp symbol when using the macros text, table or itemize."
  (let ((result (hyperspec:lookup sym)))
    (assert result () "The symbol ~s is not a valid Common Lisp symbol." sym)
    `(web-link ,(prin1-to-string sym) ,result)))

(adv-write-in-file #P"docs/user-api")
