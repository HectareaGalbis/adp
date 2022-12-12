
(in-package :adpsm)


(adp:in-file #P"docs/style-maker-api")


(adp:header "Style-maker API" style-maker-api-header)


(adp:subheader "ADP pprint dispatch")

(defvar *adp-pprint-dispatch* adppvt:*adp-pprint-dispatch*)


(adp:subheader "Style parameters")

(defmacro define-style-parameter (name &key (value nil) (key nil) (required nil))
  `(adppvt:define-style-parameter ,name :value ,value :key ,key :required ,required))


(adp:subheader "Writers")

(defmacro define-writer-definer (writer-proc writer-definer num-args docstring)
  (check-type writer-definer symbol)
  (check-type num-args unsigned-byte)
  (let ((writer-args (loop repeat num-args collect (gensym))))
    (with-gensyms (body proc-args)
      `(adp:defmacro ,writer-definer (,writer-args &body ,body)
	 ,docstring
	 (let ((,proc-args (list ,@writer-args)))
	   `(setf ,',writer-proc (lambda ,,proc-args
				   ,@,body)))))))


;; file
(define-writer-definer adppvt:*begin-file-writer* define-begin-file-writer 1
  "Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.")
(define-writer-definer adppvt:*end-file-writer* define-end-file-writer 1
  "Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.")
(define-writer-definer adppvt:*file-extension* define-file-extension 0
  "Define a function that must return a string indicating the extension of the files that ADP will create.")

;; project
(define-writer-definer adppvt:*begin-project-writer* define-begin-project-writer 1
  "Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.")
(define-writer-definer adppvt:*end-project-writer* define-end-project-writer 1
  "Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.")

;; header
(define-writer-definer adppvt:*header-writer* define-header-writer 3
  "Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.")
(define-writer-definer adppvt:*subheader-writer* define-subheader-writer 3
  "Same as define-header-writer but it must print a subheader element.")
(define-writer-definer adppvt:*subsubheader-writer* define-subsubheader-writer 3
  "Same as define-header-writer but it must print a subsubheader element.")

;; text
(define-writer-definer adppvt:*text-writer* define-text-writer 2
  "Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.")
(define-writer-definer adppvt:*escape-text* define-escape-text 1
  "Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.")

;; text enrichment
(define-writer-definer adppvt:*bold-writer* define-bold-writer 2
  "Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.")
(define-writer-definer adppvt:*italic-writer* define-italic-writer 2
  "Same as define-bold-writer, but with an italic style.")
(define-writer-definer adppvt:*emphasis-writer* define-emphasis-writer 2
  "Same as define-bold-writer, but with both bold and italic style.")
(define-writer-definer adppvt:*inline-code-writer* define-inline-code-writer 2
  "Same as define-bold-writer, but with a code-inline style.")

;; text reference
(define-writer-definer adppvt:*header-ref-writer* define-header-ref-writer 4
  "Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.")
(define-writer-definer adppvt:*symbol-ref-writer* define-symbol-ref-writer 3
  "Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.")
(define-writer-definer adppvt:*function-ref-writer* define-function-ref-writer 3
  "Same as define-symbol-ref-writer, but it prints a function reference.")
(define-writer-definer adppvt:*type-ref-writer* define-type-ref-writer 3
  "Sama as define-symbol-ref-writer, but it prints a type reference.")

;; web link
(define-writer-definer adppvt:*web-link-writer* define-web-link-writer 3
  "Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.")

;; image
(define-writer-definer adppvt:*image-writer* define-image-writer 3
  "Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.")

;; table
(define-writer-definer adppvt:*table-writer* define-table-writer 2
  "Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.")

;; itemize
(define-writer-definer adppvt:*itemize-writer* define-itemize-writer 2
  "Define a function to print an itemize element. It receives the stream and a list. The first element of the
list can be :iterate or :enumerate indicating if you should use numbers. Each element of the rest of the list can be an item or a nested list element. The item has the form (:item string). A nested list element is a list starting with :itemize or :enumerate and the rest of elements are again items or nested list elements.
  For example, you could receive something like:
    (:itemize (:item \"Animals\")
              (:enumerate (:item \"Dog\")
                          (:item \"Cat\")))")

;; code
(define-writer-definer adppvt:*code-block-writer* define-code-block-writer 3
  "Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.")
(define-writer-definer adppvt:*code-example-writer* define-code-example-writer 4
  "Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.")

;; definition
(define-writer-definer adppvt:*defclass-writer* define-defclass-writer 2
  "Define a function to print a defclass definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defconstant-writer* define-defconstant-writer 2
  "Define a function to print a defconstant definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defgeneric-writer* define-defgeneric-writer 2
  "Define a function to print a defgeneric definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-compiler-macro-writer* define-define-compiler-macro-writer 2
  "Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-condition-writer* define-define-condition-writer 2
  "Define a function to print a define-condition definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-method-combination-writer* define-define-method-combination-writer 2
  "Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-modify-macro-writer* define-define-modify-macro-writer 2
  "Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-setf-expander-writer* define-define-setf-expander-writer 2
  "Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*define-symbol-macro-writer* define-define-symbol-macro-writer 2
  "Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defmacro-writer* define-defmacro-writer 2
  "Define a function to print a defmacro definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defmethod-writer* define-defmethod-writer 2
  "Define a function to print a defmethod definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defpackage-writer* define-defpackage-writer 2
  "Define a function to print a defpackage definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defparameter-writer* define-defparameter-writer 2
  "Define a function to print a defparameter definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defsetf-writer* define-defsetf-writer 2
  "Define a function to print a defsetf definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defstruct-writer* define-defstruct-writer 2
  "Define a function to print a defstruct definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*deftype-writer* define-deftype-writer 2
  "Define a function to print a deftype definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defun-writer* define-defun-writer 2
  "Define a function to print a defun definition. It receives the stream, and the definition expression.")
(define-writer-definer adppvt:*defvar-writer* define-defvar-writer 2
  "Define a function to print a defvar definition. It receives the stream, and the definition expression.")


(adp:subheader "API function components")

(defun documentationp (expr)
  (stringp expr))

(defun declarationp (expr)
  (and (listp expr)
       (eq (car expr) 'declare)))

(defun check-component-symbols (symbols components)
  (loop for sym in symbols
	do (unless (symbolp sym)
	     (error "Expected a symbol. Found: ~s" sym))
	   (unless (member sym components :test #'string=)
	     (error "Expected one of the following symbols: ~s~%Found: ~s" components sym))))

(defmacro def-with-components (name &rest components)
  (let ((name-arg (intern (concatenate 'string "WITH-" (symbol-name name) "-COMPONENTS"))))
    (with-gensyms (component-rest-args function-body-arg body-arg function-body-value-var let-bindings-var lambda-var)
      `(adp:defmacro ,name-arg (((&rest ,component-rest-args) ,function-body-arg) &body ,body-arg)
	 (check-component-symbols ,component-rest-args ',components)
	 (let* ((,function-body-value-var (gensym))
		(,let-bindings-var (mapcar (lambda (,lambda-var)
					     (list ,lambda-var (list (find-symbol (concatenate 'string ,(symbol-name name) "-" (symbol-name ,lambda-var)) "ADPSM") ,function-body-value-var)))
					   ,component-rest-args)))
	   `(let ((,,function-body-value-var ,,function-body-arg))
	      (let ,,let-bindings-var
		,@,body-arg)))))))


;; ----- defclass components -----

(adp:subsubheader "Defclass components")

(defun defclass-class-name (defclass-source)
  (cadr defclass-source))

(defun defclass-superclass-names (defclass-source)
  (caddr defclass-source))

(defun defclass-slot-specifiers (defclass-source)
  (cadddr defclass-source))

(defun defclass-slot-names (defclass-source)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-source)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (car slot-specifier)
			   slot-specifier)))))

(defun defclass-slot-options (defclass-source)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-source)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (cdr slot-specifier)
			   nil)))))

(defun defclass-reader-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(defun defclass-writer-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(defun defclass-accessor-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(defun defclass-allocation-types (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(defun defclass-initarg-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(defun defclass-initforms (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(defun defclass-type-specifiers (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(defun defclass-slot-documentations (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :documentation slot-option)))))))

(defun defclass-class-options (defclass-source)
  (cdddr defclass-source))

(defun defclass-default-initargs (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cdr (member :default-initargs class-options :key #'car)))))

(defun defclass-documentation (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cadr (member :documentation class-options :key #'car)))))

(defun defclass-metaclass (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cadr (member :metaclass class-options :key #'car)))))

(def-with-components defclass class-name superclass-names slot-specifiers slot-names slot-options
  reader-function-names writer-function-names accessor-function-names allocation-types
  initarg-names initforms type-specifiers slot-documentations class-options default-initargs
  documentation metaclass)


;; ----- defconstant components -----

(adp:subsubheader "Defconstant components")

(defun defconstant-name (defconstant-source)
  (cadr defconstant-source))

(defun defconstant-initial-value (defconstant-source)
  (caddr defconstant-source))

(defun defconstant-documentation (defconstant-source)
  (cadddr defconstant-source))

(def-with-components defconstant name initial-value documentation)


;; ----- defgeneric components -----

(adp:subsubheader "Defgeneric components")

(defun defgeneric-function-name (defgeneric-source)
  (cadr defgeneric-source))

(defun defgeneric-gf-lambda-list (defgeneric-source)
  (caddr defgeneric-source))

(defun defgeneric-options (defgeneric-source)
  (remove-if (lambda (x)
	       (eq (car x) :method))
	     (cdddr defgeneric-source)))

(defun defgeneric-argument-precedence-order (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member :argument-precedence-order options :key #'car)))))

(defun defgeneric-gf-declarations (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member 'cl:declare options :key #'car)))))

(defun defgeneric-documentation (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :documentation options :key #'car)))))

(defun defgeneric-method-combination (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member :method-combination options :key #'car)))))

(defun defgeneric-generic-function-class (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :generic-function-class options :key #'car)))))

(defun defgeneric-method-class (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :method-class options :key #'car)))))

(defun defgeneric-method-descriptions (defgeneric-source)
  (remove-if-not (lambda (x)
		   (eq (car x) :method))
		 (cdddr defgeneric-source)))

(def-with-components defgeneric function-name gf-lambda-list options argument-precedence-order gf-declarations
  documentation method-combination generic-function-class method-class method-descriptions)


;; ----- define-compiler-macro components -----

(adp:subsubheader "Define-compiler-macro components")

(defun define-compiler-macro-name (define-compiler-macro-source)
  (cadr define-compiler-macro-source))

(defun define-compiler-macro-lambda-list (define-compiler-macro-source)
  (caddr define-compiler-macro-source))

(defun define-compiler-macro-declarations (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun define-compiler-macro-documentation (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun define-compiler-macro-forms (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components define-compiler-macro name lambda-list declarations documentation forms)


;; ----- define-condition components -----

(adp:subsubheader "Define-condition components")

(defun define-condition-name (define-condition-source)
  (cadr define-condition-source))

(defun define-condition-parent-types (define-condition-source)
  (caddr define-condition-source))

(defun define-condition-slot-specs (define-condition-source)
  (cadddr define-condition-source))

(defun define-condition-slot-names (define-condition-source)
  (let ((slot-specs (define-condition-slot-specs define-condition-source)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect slot-spec
	       else
		 collect (car slot-spec)))))

(defun define-condition-slot-options (define-condition-source)
  (let ((slot-specs (define-condition-slot-specs define-condition-source)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect nil
	       else
		 collect (cdr slot-spec)))))

(defun define-condition-slot-readers (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-writers (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-accessors (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-allocations (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(defun define-condition-slot-initargs (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-initforms (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(defun define-condition-slot-types (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(defun define-condition-options (define-condition-source)
  (cddddr define-condition-source))

(defun define-condition-default-initargs (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cdr (member :default-initargs options :key #'car)))))

(defun define-condition-documentation (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cadr (member :documentation options :key #'car)))))

(defun define-condition-report-name (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cadr (member :report options :key #'car)))))

(def-with-components define-condition name parent-types slot-specs slot-names slot-options slot-readers
  slot-writers slot-accessors slot-allocations slot-initargs slot-initforms slot-types options
  default-initargs documentation report-name)


;; ----- define-method-combination components -----

(adp:subsubheader "Define-method-combination components")

(defun define-method-combination-name (define-method-combination-source)
  (cadr define-method-combination-source))

(defun define-method-combination-short-form-options (define-method-combination-source)
  (let ((possible-short-form-options (cddr define-method-combination-source)))
    (if (keywordp (car possible-short-form-options))
	possible-short-form-options
	nil)))

(defun define-method-combination-identity-with-one-argument (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (and short-form-options
	 (cadr (member :identity-with-one-argument short-form-options)))))

(defun define-method-combination-operator (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (and short-form-options
	 (cadr (member :operator short-form-options)))))

(defun define-method-combination-lambda-list (define-method-combination-source)
  (caddr define-method-combination-source))

(defun define-method-combination-method-group-specifiers (define-method-combination-source)
  (cadddr define-method-combination-source))

(defun define-method-combination-method-group-specifier-names (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (mapcar #'car method-group-specifiers))))

(defun define-method-combination-qualifier-patterns (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (loop for possible-qualifier-pattern in (cdr method-group-specifier)
			     while (listp possible-qualifier-pattern)
			     collect possible-qualifier-pattern)))))

(defun define-method-combination-predicates (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (let ((possible-predicate (cadr method-group-specifier)))
			 (if (listp possible-predicate)
			     nil
			     possible-predicate))))))

(defun define-method-combination-long-form-options (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (if (symbolp (cadr method-group-specifier))
			   (cddr method-group-specifier)
			   (loop for rest-method-group-specifier on (cdr method-group-specifier)
				 while (listp (car rest-method-group-specifier))
				 finally (return rest-method-group-specifier)))))))

(defun define-method-combination-descriptions (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :description long-form-option))))))

(defun define-method-combination-orders (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :order long-form-option))))))

(defun define-method-combination-requireds (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :required long-form-option))))))

(defun define-method-combination-arguments (define-method-combination-source)
  (let ((possible-arguments (car (cddddr define-method-combination-source))))
    (if (and (listp possible-arguments)
	     (eq (car possible-arguments) :arguments))
	(cdr possible-arguments)
	nil)))

(defun define-method-combination-generic-function (define-method-combination-source)
  (let ((rest-possible-generic-function (cddddr define-method-combination-source)))
    (if (and (listp (car rest-possible-generic-function))
	     (eq (caar rest-possible-generic-function) :generic-function))
	(cadar rest-possible-generic-function)
	(let ((last-possible-generic-function (cadr rest-possible-generic-function)))
	  (if (and (listp last-possible-generic-function)
		   (eq (car last-possible-generic-function) :generic-function))
	      (cadr last-possible-generic-function))))))

(defun define-method-combination-declarations (define-method-combination-source)
  (loop for possible-declaration in (cddddr define-method-combination-source)
	while (or (declarationp possible-declaration)
		  (documentationp possible-declaration)
		  (and (listp possible-declaration)
		       (member (car possible-declaration) '(:arguments :generic-function))))
	when (declarationp possible-declaration)
	  collect possible-declaration))

(defun define-method-combination-documentation (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (if short-form-options
	(cadr (member :documentation short-form-options))
	(loop for possible-documentation in (cdddr define-method-combination-source)
	      while (or (declarationp possible-documentation)
			(documentationp possible-documentation)
			(and (listp possible-documentation)
			     (member (car possible-documentation) '(:arguments :generic-function))))
	      when (documentationp possible-documentation)
		return possible-documentation))))

(defun define-method-combination-forms (define-method-combination-source)
  (loop for possible-forms on (cddddr define-method-combination-source)
	while (or (declarationp (car possible-forms))
		  (documentationp (car possible-forms))
		  (and (listp (car possible-forms))
		       (member (caar possible-forms) '(:arguments :generic-function))))
	finally (return possible-forms)))

(def-with-components define-method-combination name short-form-options identity-with-one-argument operator
  lambda-list method-group-specifiers method-group-specifier-names qualifier-patterns predicates
  long-form-options descriptions orders requireds arguments generic-function declarations documentation forms)


;; ----- define-modify-macro components -----

(adp:subsubheader "Define-modify-macro components")

(defun define-modify-macro-name (define-modify-macro-source)
  (cadr define-modify-macro-source))

(defun define-modify-macro-lambda-list (define-modify-macro-source)
  (caddr define-modify-macro-source))

(defun define-modify-macro-function (define-modify-macro-source)
  (cadddr define-modify-macro-source))

(defun define-modify-macro-documentation (define-modify-macro-source)
  (car (cddddr define-modify-macro-source)))

(def-with-components define-modify-macro name lambda-list function documentation) 


;; ----- define-setf-expander components -----

(adp:subsubheader "Define-setf-expander components")

(defun define-setf-expander-access-fn (define-setf-expander-source)
  (cadr define-setf-expander-source))

(defun define-setf-expander-lambda-list (define-setf-expander-source)
  (caddr define-setf-expander-source))

(defun define-setf-expander-declarations (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun define-setf-expander-documentation (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun define-setf-expander-forms (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components define-setf-expander access-fn lambda-list declarations documentation forms)


;; ----- define-symbol-macro components -----

(adp:subsubheader "Define-symbol-macro components")

(defun define-symbol-macro-symbol (define-symbol-macro-source)
  (cadr define-symbol-macro-source))

(defun define-symbol-macro-expansion (define-symbol-macro-source)
  (caddr define-symbol-macro-source))

(def-with-components define-symbol-macro symbol expansion)


;; ----- defmacro components -----

(adp:subsubheader "Defmacro components")

(defun defmacro-name (defmacro-source)
  (cadr defmacro-source))

(defun defmacro-lambda-list (defmacro-source)
  (caddr defmacro-source))

(defun defmacro-declarations (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun defmacro-documentation (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun defmacro-forms (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components defmacro name lambda-list declarations documentation forms)


;; ----- defmethod components -----

(adp:subsubheader "defmethod components")

(defun defmethod-function-name (defmethod-source)
  (cadr defmethod-source))

(defun defmethod-method-qualifiers (defmethod-source)
  (loop for qualifier in (cddr defmethod-source)
	while (not (listp qualifier))
	collect qualifier))

(defun defmethod-specialized-lambda-list (defmethod-source)
  (loop for possible-lambda-list in (cddr defmethod-source)
	when (listp possible-lambda-list)
	  return possible-lambda-list))

(defun defmethod-declarations (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for possible-declaration in (cdr rest-defmethod-source)
		       while (or (declarationp possible-declaration)
				 (documentationp possible-declaration))
		       if (declarationp possible-declaration)
			 collect possible-declaration)))

(defun defmethod-documentation (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for possible-documentation in (cdr rest-defmethod-source)
		       while (or (declarationp possible-documentation)
				 (documentationp possible-documentation))
		       if (documentationp possible-documentation)
			 return possible-documentation)))

(defun defmethod-forms (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for rest-possible-forms on (cdr rest-defmethod-source)
		       while (or (declarationp (car rest-possible-forms))
				 (documentationp (car rest-possible-forms)))
		       finally (return rest-possible-forms))))

(def-with-components defmethod function-name method-qualifiers specialized-lambda-list declarations
  documentation forms)


;; ----- defpackage components -----

(adp:subsubheader "Defpackage components")

(defun defpackage-options (defpackage-source)
  (cddr defpackage-source))

(defun defpackage-defined-package-name (defpackage-source)
  (cadr defpackage-source))

(defun defpackage-nicknames (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :nicknames))
				 options))))

(defun defpackage-documentation (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (and options
	 (cadr (member :documentation options)))))

(defun defpackage-use-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :use))
				 options))))

(defun defpackage-shadow-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :shadow))
				 options))))

(defun defpackage-shadowing-import-from-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				  options))))

(defun defpackage-shadowing-import-from-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				 options))))

(defun defpackage-import-from-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				  options))))

(defun defpackage-import-from-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				  options))))

(defun defpackage-export-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(defun defpackage-import-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(defun defpackage-size (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (cadar (member :size options :key #'car))))

(def-with-components defpackage options defined-package-name nicknames documentation use-package-names
  shadow-symbol-names shadowing-import-from-package-names shadowing-import-from-symbol-names
  import-from-package-names import-from-symbol-names export-symbol-names import-symbol-names size)


;; ----- defparameter components -----

(adp:subsubheader "Defparameter components")

(defun defparameter-name (defparameter-source)
  (cadr defparameter-source))

(defun defparameter-initial-value (defparameter-source)
  (caddr defparameter-source))

(defun defparameter-documentation (defparameter-source)
  (cadddr defparameter-source))

(def-with-components defparameter name initial-value documentation)


;; ----- defsetf components -----

(adp:subsubheader "Defsetf components")

(defun defsetf-access-fn (defsetf-source)
  (cadr defsetf-source))

(defun defsetf-update-fn (defsetf-source)
  (let ((possible-update-fn (caddr defsetf-source)))
    (if (symbolp possible-update-fn)
	possible-update-fn)))

(defun defsetf-documentation (defsetf-source)
  (cadddr defsetf-source))

(def-with-components defsetf access-fn update-fn documentation)


;; ----- defstruct -----

(adp:subsubheader "Defstruct components")

(defun defstruct-name-and-options (defstruct-source)
  (cadr defstruct-source))

(defun defstruct-structure-name (defstruct-source)
  (let ((name-and-options (defstruct-name-and-options defstruct-source)))
    (if (listp name-and-options)
	(car name-and-options)
	name-and-options)))

(defun defstruct-options (defstruct-source)
  (let ((name-and-options (defstruct-name-and-options defstruct-source)))
    (if (listp name-and-options)
	(cdr name-and-options)
	nil)))

(defun defstruct-conc-name-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :conc-name options))
	    (car (member :conc-name options :key #'car)))
	nil)))

(defun defstruct-constructor-options (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (remove-if-not (lambda (x)
		     (if (listp x)
			 (eq (car x) :constructor)
			 (eq x :constructor)))
		   options)))

(defun defstruct-copier-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :copier options))
	    (car (member :copier options :key #'car)))
	nil)))

(defun defstruct-predicate-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :predicate options))
	    (car (member :predicate options :key #'car)))
	nil)))

(defun defstruct-include-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :include options))
	    (car (member :include options :key #'car)))
	nil)))

(defun defstruct-printer-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :print-object options :key #'car))
	    (car (member :print-function options :key #'car)))
	nil)))

(defun defstruct-print-object-option (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-object))
	printer-option
	nil)))

(defun defstruct-print-function-option (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-function))
	printer-option
	nil)))

(defun defstruct-type-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :type options :key #'car))
	nil)))

(defun defstruct-named-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :named options))
	nil)))

(defun defstruct-initial-offset-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :initial-offset options :key #'car))
	nil)))

(defun defstruct-slot-descriptions (defstruct-source)
  (let ((possible-descriptions (cddr defstruct-source)))
    (if (documentationp (car possible-descriptions))
	(cdr possible-descriptions)
	possible-descriptions)))

(defun defstruct-slot-options (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (loop for slot-description in slot-descriptions
	  collect (if (listp slot-description)
		      (cddr slot-description)
		      nil))))

(defun defstruct-conc-name (defstruct-source)
  (let ((conc-name-option (defstruct-conc-name-option defstruct-source)))
    (and conc-name-option
	 (if (listp conc-name-option)
	     (cadr conc-name-option)
	     nil))))

(defun defstruct-constructor-arglists (defstruct-source)
  (let ((constructor-options (defstruct-constructor-options defstruct-source)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (caddr constructor-option)
			   nil)))))

(defun defstruct-constructor-names (defstruct-source)
  (let ((constructor-options (defstruct-constructor-options defstruct-source)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (cadr constructor-option)
			   nil)))))

(defun defstruct-copier-name (defstruct-source)
  (let ((copier-option (defstruct-copier-option defstruct-source)))
    (and copier-option
	 (if (listp copier-option)
	     (cadr copier-option)
	     nil))))

(defun defstruct-included-structure-name (defstruct-source)
  (let ((include-option (defstruct-include-option defstruct-source)))
    (and include-option
	 (cadr include-option))))

(defun defstruct-include-option-slot-descriptors (defstruct-source)
  (let ((include-option (defstruct-include-option defstruct-source)))
    (and include-option
	 (cddr include-option))))

(defun defstruct-initial-offset (defstruct-source)
  (let ((initial-offset-option (defstruct-initial-offset-option defstruct-source)))
    (and initial-offset-option
	 (cadr initial-offset-option))))

(defun defstruct-predicate-name (defstruct-source)
  (let ((predicate-option (defstruct-predicate-option defstruct-source)))
    (and predicate-option
	 (cadr predicate-option))))

(defun defstruct-printer-name (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (and printer-option
	 (cadr printer-option))))

(defun defstruct-slot-names (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (car slot-description)
			   slot-description)))))

(defun defstruct-slot-initforms (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (cadr slot-description)
			   nil)))))

(defun defstruct-slot-types (defstruct-source)
  (let ((slot-options (defstruct-slot-options defstruct-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :type slot-option))))))

(defun defstruct-slot-read-only-ps (defstruct-source)
  (let ((slot-options (defstruct-slot-options defstruct-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :read-only slot-option))))))

(defun defstruct-type (defstruct-source)
  (let ((type-option (defstruct-type-option defstruct-source)))
    (and type-option
	 (cadr type-option))))

(defun defstruct-documentation (defstruct-source)
  (let ((possible-documentation (caddr defstruct-source)))
    (when (documentationp possible-documentation)
      possible-documentation)))

(def-with-components defstruct name-and-options options conc-name-option constructor-option
  copier-option predicate-option include-option printer-option print-object-option
  print-function-option type-option named-option initial-offset-option slot-descriptions
  slot-options conc-name constructor-arglists constructor-names copier-name
  included-structure-name include-option-slot-descriptions initial-offset predicate-name
  printer-name slot-names slot-initforms slot-read-only-ps slot-types structure-name type
  documentation)


;; ----- deftype components -----

(adp:subsubheader "Deftype components")

(defun deftype-name (deftype-source)
  (cadr deftype-source))

(defun deftype-lambda-list (deftype-source)
  (caddr deftype-source))

(defun deftype-declarations (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun deftype-documentation (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun deftype-forms (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components deftype name lambda-list declarations documentation forms)


;; ----- defun components -----

(adp:subsubheader "Defun components")

(defun defun-function-name (defun-source)
  (cadr defun-source))

(defun defun-lambda-list (defun-source)
  (caddr defun-source))

(defun defun-declarations (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun defun-documentation (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun defun-forms (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components defun function-name lambda-list declarations documentation forms)


;; ----- defvar components -----

(adp:subsubheader "Defvar components")

(defun defvar-name (defvar-source)
  (cadr defvar-source))

(defun defvar-initial-value (defvar-source)
  (caddr defvar-source))

(defun defvar-documentation (defvar-source)
  (cadddr defvar-source))

(def-with-components defvar name initial-value documentation)
