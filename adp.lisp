

(in-package :adp)


(cl:defmacro adv-header (str &optional tag)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    (let ((fixed-tag (or tag (gensym))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:add-header-tag ',fixed-tag ,str)
	     (adppvt:emplace-adp-element :header ,str ',fixed-tag)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(cl:defmacro adv-subheader (str &optional tag)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    (let ((fixed-tag (or tag (gensym))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:add-header-tag ',fixed-tag ,str)
	     (adppvt:emplace-adp-element :subheader ,str ',fixed-tag)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))

(cl:defmacro adv-defmacro (&body defmacro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-function-tag (car ',defmacro-body))
		 (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defmacro ,@defmacro-body)))

(cl:defmacro adv-defun (&body defun-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-function-tag (car ',defun-body))
		 (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (car ',defun-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defun ,@defun-body)))

(cl:defmacro adv-write-in-file (file-path)
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path)
      (once-only (file-path)
	`(if adppvt:*add-documentation*
	     (progn
	       (let ((,let-file-path (make-pathname :directory (if (pathname-directory ,file-path)
								   (cons :relative (cdr (pathname-directory ,file-path)))
								   nil)
						    :name (pathname-name ,file-path))))
		 (adppvt:push-adp-file ,let-file-path))
	       (values))
	     (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))))


;; ----- ADP interface -----

(adv-write-in-file #P"docs/user-api")

(adv-header "ADP User Interface" user-api-header)


;; ----- Guide functions -----

(adv-subheader "Literate programming functions")

(adv-defmacro header (str &optional tag)
  "Add a header with name str. Also, if tag is not nil but a symbol, a new header-tag is created."	      
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    (let ((fixed-tag (or tag (gensym))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:add-header-tag ',fixed-tag ,str)
	     (adppvt:emplace-adp-element :header ,str ',fixed-tag)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(adv-defmacro subheader (str &optional tag)
  "Same as header, but add a subheader."
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    (let ((fixed-tag (or tag (gensym))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:add-header-tag ',fixed-tag ,str)
	     (adppvt:emplace-adp-element :subheader ,str ',fixed-tag)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(adv-defmacro subsubheader (str &optional tag)
  "Same as header, but add a subsubheader."
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type tag (or null symbol) "a symbol")
    (let ((fixed-tag (or tag (gensym))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:add-header-tag ',fixed-tag ,str)
	     (adppvt:emplace-adp-element :subsubheader ,str ',fixed-tag)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(adv-defmacro text (&rest objects)
  "Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, function-ref and type-ref."
  (when adppvt:*add-documentation*
    `(if adppvt:*add-documentation*
	 (progn
	   (adppvt:emplace-adp-element :text ,@objects)
	   (values))
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro table (&rest rows)
  "Add a table. Each argument must be a list of lists. Each inner list must have as first element the keyword :cell and the rest 
are treated as if using the macro text."
  (when adppvt:*add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(if adppvt:*add-documentation*
	 (progn
	   (adppvt:emplace-adp-element :table ,@(loop for row in rows
						      collect (cons 'list (loop for elem in row
										collect (cons 'list elem)))))
	   (values))
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro itemize (&rest items)
  "Add a list of items. Each argument must be a list. Each list must have as first argument the keyword :item or :itemize. If 
:item is used, the rest of the elements in that list will be treated as if using the macro text. If :itemize is used the rest 
of elements must be lists where its first elements are the keywords :item or :itemize. In other words, when :itemize is used 
a nested list is added."
  (when adppvt:*add-documentation*
    (labels ((check-items (item-list)
	       (assert (not (null item-list)) () "Expected at least one expression in a itemize/:itemize form.")
	       (assert (and (listp (car item-list)) (not (eq (caar item-list) :itemize))) () "The first element of itemize/:itemize must be a list starting with :item.")
	       (loop for item in (cdr item-list)
		     if (not (and (listp item)
				  (member (car item) '(:item :itemize))))
		       do (error "Each item of itemize/:itemize must be a list starting with :item ot :itemize.")
		     if (eq (car item) :itemize)
		       do (check-items (cdr item)))))
      (check-items items))
    (labels ((process-itemize-items (item-list)
	       (loop for item in item-list
		     if (eq (car item) :item)
		       collect (cons 'list item)
		     else
		       collect (list* 'list :itemize (process-itemize-items (cdr item))))))
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:emplace-adp-element :itemize ,@(process-itemize-items items))
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(adv-defmacro image (alt-text path)
  "Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located."
  (when adppvt:*add-documentation*
    (check-type alt-text string "a string")
    (check-type path pathname "a pathname")
    `(if adppvt:*add-documentation*
	 (progn
	   (adppvt:emplace-adp-element :image ,alt-text ,path)
	   (values))
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro bold (&rest args)
  "Add bold style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(if adppvt:*add-documentation*
	 (adppvt:create-bold-text ,@args)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro italic (&rest args)
  "Add italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(if adppvt:*add-documentation*
	 (adppvt:create-italic-text ,@args)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro bold-italic (&rest args)
  "Add bold and italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(if adppvt:*add-documentation*
	 (adppvt:create-bold-italic-text ,@args)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro code-inline (&rest code)
  "Add inlined style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string."
  (when adppvt:*add-documentation*
    `(if adppvt:*add-documentation*
	 (adppvt:create-code-inline-text ,@code)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro web-link (name link)
  "Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings."
  (when adppvt:*add-documentation*
    (check-type name string "a string")
    (check-type link string "a string")
    `(if adppvt:*add-documentation*
	 (adppvt:create-web-link-text ,name ,link)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro header-ref (tag)
  "Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(if adppvt:*add-documentation*
	 (adppvt:create-header-ref-text ',tag)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro symbol-ref (tag)
  "Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(if adppvt:*add-documentation*
	 (adppvt:create-symbol-ref-text ',tag)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro function-ref (tag)
  "Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(if adppvt:*add-documentation*
	 (adppvt:create-function-ref-text ',tag)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


(adv-defmacro type-ref (tag)
  "Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype."
  (when adppvt:*add-documentation*
    (check-type tag symbol "a symbol")
    `(if adppvt:*add-documentation*
	 (adppvt:create-type-ref-text ',tag)
	 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))


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
	   `((if adppvt:*add-documentation*
		 (loop for ,tag in ',tags
		       do (apply #'adppvt:add-code-tag ,tag (adppvt:process-code-tag ,tag ',code)))
		 (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
       ,@(adppvt:remove-own-code-hide-exprs code))))


(adv-defmacro code-block ((&rest tags) &body code)
  "Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is prin1-ed instead of the symbol."
  (when adppvt:*add-documentation*
    (check-type tags list "a list")
    (loop for tag in tags
	  do (check-type tag symbol "a symbol")
	     (assert (member tag code) () "The tag ~s is not present in code-block code." tag))
    (assert (not (null code)) () "Expected at least one expression in a code-block form.")
    (with-gensyms (expr)
      `(if adppvt:*add-documentation*
	   (progn
	     (adppvt:emplace-adp-element :code-block (loop for ,expr in ',code
							   if (and (symbolp ,expr)
								   (member ,expr ',tags))
							     collect (adppvt:create-code-block-tag ,expr)
							   else
							     collect (adppvt:process-code-tag '#:dummy-tag ,expr)))
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


(adv-defmacro code-example (&body code)
  "Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed."
  (when adppvt:*add-documentation*
    (assert (not (null code)) () "Expected at least one expression in a code-example form.")
    (with-gensyms (output result)
      `(if adppvt:*add-documentation*
	   (let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
		  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
						  ,@(adppvt:remove-code-tag-exprs code)))))

	     (adppvt:emplace-adp-element :code-example (adppvt:process-code-tag '#:dummy-tag ',code) ,output ,result)
	     (values))
	   (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process.")))))


;; ----- API functions -----

(adv-subheader "API documentation functions" api-subheader)

(adv-defmacro defclass (&body defclass-body)
  "Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-type-tag (car ',defclass-body))
		 (adppvt:emplace-adp-element :defclass '(cl:defclass ,@defclass-body) (car ',defclass-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defclass ,@defclass-body)))


(adv-defmacro defconstant (&body defconstant-body)
  "Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-symbol-tag (car ',defconstant-body))
		 (adppvt:emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body) (car ',defconstant-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defconstant ,@defconstant-body)))


(adv-defmacro defgeneric (&body defgeneric-body)
  "Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-function-tag (car ',defgeneric-body))
		 (adppvt:emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body) (car ',defgeneric-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defgeneric ,@defgeneric-body)))


(adv-defmacro define-compiler-macro (&body define-compiler-macro-body)
  "Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-compiler-macro ,@define-compiler-macro-body)))


(adv-defmacro define-condition (&body define-condition-body)
  "Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-type-tag (car ',define-condition-body))
		 (adppvt:emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body) (car ',define-condition-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-condition ,@define-condition-body)))


(adv-defmacro define-method-combination (&body define-method-combination-body)
  "Add a define-method-combination declaration. The macro expands to cl:define-method-combination."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-method-combination ,@define-method-combination-body)))


(adv-defmacro define-modify-macro (&body define-modify-macro-body)
  "Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-function-tag (car ',define-modify-macro-body))
		 (adppvt:emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body) (car ',define-modify-macro-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-modify-macro ,@define-modify-macro-body)))


(adv-defmacro define-setf-expander (&body define-setf-expander-body)
  "Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-setf-expander ,@define-setf-expander-body)))


(adv-defmacro define-symbol-macro (&body define-symbol-macro-body)
  "Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-symbol-tag (car ',define-symbol-macro-body))
		 (adppvt:emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body) (car ',define-symbol-macro-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:define-symbol-macro ,@define-symbol-macro-body)))


(adv-defmacro defmacro (&body defmacro-body)
  "Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-function-tag (car ',defmacro-body))
		 (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defmacro ,@defmacro-body)))


(adv-defmacro defmethod (&body defmethod-body)
  "Add a defmethod declaration. The macro expands to cl:defmethod."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defmethod ,@defmethod-body)))


(adv-defmacro defpackage (&body defpackage-body)
  "Add a defpackage declaration. The macro expands to cl:defpackage."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defpackage ,@defpackage-body)))


(adv-defmacro defparameter (&body defparameter-body)
  "Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-symbol-tag (car ',defparameter-body))
		 (adppvt:emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body) (car ',defparameter-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defparameter ,@defparameter-body)))


(adv-defmacro defsetf (&body defsetf-body)
  "Add a defsetf declaration. The macro expands to cl:defsetf."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (adppvt:emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defsetf ,@defsetf-body)))


(adv-defmacro defstruct (&body defstruct-body)
  "Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-type-tag (car ',defstruct-body))
		 (adppvt:emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body) (car ',defstruct-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defstruct ,@defstruct-body)))


(adv-defmacro deftype (&body deftype-body)
  "Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-type-tag (car ',deftype-body))
		 (adppvt:emplace-adp-element :deftype '(cl:deftype ,@deftype-body) (car ',deftype-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:deftype ,@deftype-body)))


(adv-defmacro defun (&body defun-body)
  "Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (when (symbolp (car ',defun-body))
		   (adppvt:add-function-tag (car ',defun-body)))
		 (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (if (symbolp (car ',defun-body))
										 (car ',defun-body)
										 nil)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defun ,@defun-body)))


(adv-defmacro defvar (&body defvar-body)
  "Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag."
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((if adppvt:*add-documentation*
	       (progn
		 (adppvt:add-symbol-tag (car ',defvar-body))
		 (adppvt:emplace-adp-element :defvar '(cl:defvar ,@defvar-body) (car ',defvar-body)))
	       (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))
     (cl:defvar ,@defvar-body)))


;; ----- Writer functions -----

(adv-subheader "Documentation writer function")


(adv-defmacro write-in-file (file-path)
  "Associate all the information gathered so far with the pathname file-path. This will cause the creation of a file 
where all the information will be printed in. The pathname will be considered relative to the system's root directory.
Only the directory and name parts of file-path are considered. The rest are ignored (including the extension). This 
macro can be used multiple times."
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path)
      (once-only (file-path)
	`(if adppvt:*add-documentation*
	     (progn
	       (let ((,let-file-path (make-pathname :directory (if (pathname-directory ,file-path)
								   (cons :relative (cdr (pathname-directory ,file-path)))
								   nil)
						    :name (pathname-name ,file-path))))
		 (adppvt:push-adp-file ,let-file-path))
	       (values))
	     (warn "ADP is trying to gather information even being disabled. Reload every file from the affected system or restart the Lisp process."))))))


(declaim (ftype (function (t symbol &rest t) (values &optional)) load-documentation-system))
(adv-defun load-documentation-system (system style &rest style-args)
  "Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed."
  (adppvt:remove-current-procs)
  (adppvt:remove-current-data)
  (assert (asdf:find-system system) (system) "The system ~s was not found." system)
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (assert (asdf:find-system style-system) (style-system) "The style ~s was not found." style-system)
    (asdf:operate 'asdf:load-source-op style-system :force t))
  (adppvt:check-current-procs)
  (adppvt:check-style-parameters style-args)
  (let ((adppvt:*add-documentation* t))
    (asdf:operate 'asdf:load-source-op system :force t))
  (loop for (name value) in style-args by #'cddr
	do (adppvt:set-parameter-value name value))
  (let* ((root-path (asdf:system-source-directory system))
	 (fixed-root-path (make-pathname :host (pathname-host root-path)
					 :device (pathname-device root-path)
					 :directory (pathname-directory root-path))))
    (adppvt:write-system-files fixed-root-path)
    (format t "~%~a" "Done!"))
  (values))


;; ----- Additional functions -----

(subheader "Additional functions" additional-functions-subheader)

(defmacro cl-ref (sym)
  "Add a reference to a Common Lisp symbol when using the macros text, table or itemize."
  (let ((result (hyperspec:lookup sym)))
    (assert result () "The symbol ~s is not a valid Common Lisp symbol." sym)
    `(web-link ,(prin1-to-string sym) ,result)))


(defmacro table-of-contents ()
  "Add a list of all headers and subheaders used in the system. The headers from different
files are shown in the same order the files are loaded."
  (when adppvt:*add-documentation*
    '(progn
      (adppvt:emplace-adp-element :table-of-contents)
      (values))))


(defmacro mini-table-of-contents ()
  "Add a list of all headers, subheaders and subsubheaders used in the current documentation file."
  (when adppvt:*add-documentation*
    '(progn
      (adppvt:emplace-adp-element :mini-table-of-contents)
      (values))))


(defmacro table-of-functions ()
  "Add an ordered list of all functions and macros defined using ADP."
  (when adppvt:*add-documentation*
    '(progn
      (adppvt:emplace-adp-element :table-of-functions)
      (values))))


(defmacro table-of-symbols ()
  "Add an ordered list of all variables defined using ADP."
  (when adppvt:*add-documentation*
    '(progn
      (adppvt:emplace-adp-element :table-of-symbols)
      (values))))


(defmacro table-of-types ()
  "Add an ordered list of all types defined using ADP."
  (when adppvt:*add-documentation*
    '(progn
      (adppvt:emplace-adp-element :table-of-types)
      (values))))


(defmacro with-made-symbols (names &body forms)
  "Same as with-gensyms, but it uses make-symbol instead. This is intended for using when defining a macro that
expands to some form that defines something (like adp:defun or adp:defmacro). If your macro expands to some of that forms the generated symbols may be printed in the documentation. And the symbols from with-gensyms have a different number suffix each time you use it, so the printed documentation could change each time you generate it. Using with-made-symbols avoids that. In other words, the printed documentation remains the same if you don't change the code."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (make-symbol ,string))))
                 names)
     ,@forms))


;; ----- Macro characters -----

(subheader "Macro characters")

(text "The next table shows what macro characters can be used and what they expand to:")

(table ((:cell "Character") (:cell "Macro") (:cell "Example"))
       ((:cell "@b") (:cell (function-ref bold)) (:cell (code-inline "@b(\"This text is bold\")")))
       ((:cell "@i") (:cell (function-ref italic)) (:cell (code-inline "@i(\"This text is italic\")")))
       ((:cell "@e") (:cell (function-ref bold-italic)) (:cell (code-inline "@e(\"This text is emphasized\")")))
       ((:cell "@c") (:cell (function-ref code-inline)) (:cell (code-inline "@c(\"This text is inlined\")")))
       ((:cell "@w") (:cell (function-ref web-link)) (:cell (code-inline "@w(\"Name of link\" \"www.example.com\")")))
       ((:cell "@h") (:cell (function-ref header-ref)) (:cell (code-inline "@h(header)")))
       ((:cell "@f") (:cell (function-ref function-ref)) (:cell (code-inline "@f(function)")))
       ((:cell "@s") (:cell (function-ref symbol-ref)) (:cell (code-inline "@s(variable)")))
       ((:cell "@t") (:cell (function-ref type-ref)) (:cell (code-inline "@t(type)")))
       ((:cell "@l") (:cell (function-ref cl-ref)) (:cell (code-inline "@l(princ)"))))

(make-dispatch-macro-character #\@ t)

(cl:defun adp-reader-macro-dispatch (stream char num-arg)
  (declare (ignore num-arg))
  (let ((list (read stream))
	(macro (case char
		 (#\b 'bold)
		 (#\i 'italic)
		 (#\e 'bold-italic)
		 (#\c 'code-inline)
		 (#\w 'web-link)
		 (#\h 'header-ref)
		 (#\f 'function-ref)
		 (#\s 'symbol-ref)
		 (#\t 'type-ref)
		 (#\l 'cl-ref))))
    (cons macro list)))

(loop for char in '(#\b #\i #\e #\c #\w #\h #\f #\s #\t #\p #\l)
      do (set-dispatch-macro-character #\@ char #'adp-reader-macro-dispatch))
