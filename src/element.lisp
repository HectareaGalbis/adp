
(in-package :adppvt)


;; ----- element -----

(defclass element ()
  ((name :initarg :name)
   (source-location :initarg :source-location
		    :type pathname)
   (file-location :type pathname))
  (:documentation
   "Represent the most basic unit of documentation."))

(defclass tagged-element (element)
  ((tag :initarg :tag
	:type symbol))
  (:documentation
   "Represent an element that can be associated with a tag."))


;; ----- header -----

(defclass header-type (tagged-element)
  ((title :initarg :title
	  :type string)
   (user-tag-p :initarg :user-tag-p
	       :type boolean))
  (:documentation
   "Represents a header type element."))

(defclass header (header-type) ()
  (:documentation
   "Represent a header element."))

(defclass subheader (header-type) ()
  (:documentation
   "Represent a subheader element."))

(defclass subsubheader (header-type) ()
  (:documentation
   "Represent a subsubheader element."))


;; ----- text -----

(defclass text-type (element)
  ((text-elements :initarg :text-elements
		  :type list))
  (:documentation
   "Represents a text type element."))

(defclass text-subelement-type (element) ()
  (:documentation
   "Repreents a text subelement type."))

(defclass text-element (text-type) ()
  (:documentation
   "Represent a top level text element."))

(defclass text (text-element) ()
  (:documentation
   "Represents a text element."))

(defclass text-subelement (text-subelement-type text-type) ()
  (:documentation
   "Represent a text subelement type element."))


;; ----- text enrichment -----

(defclass bold (text-subelement) ()
  (:documentation
   "Represent a bold element."))

(defclass italic (text-subelement) ()
  (:documentation
   "Represent a italic element."))

(defclass emphasis (text-subelement) ()
  (:documentation
   "Represent a bold and italic element."))

(defclass inline-code (text-subelement) ()
  (:documentation
   "Represent an inline code element."))


;; ----- text reference -----

(defclass text-reference (tagged-element text-subelement-type) ()
  (:documentation
   "Represent a text reference element."))

(defclass header-ref (text-reference) ()
  (:documentation
   "Represent a header reference element."))

(defclass symbol-ref (text-reference) ()
  (:documentation
   "Represent a symbol reference element."))

(defclass function-ref (text-reference) ()
  (:documentation
   "Represent a function reference element."))

(defclass type-ref (text-reference) ()
  (:documentation
   "Represent a type reference element."))


;; ----- web link -----

(defclass web-link (text-subelement-type)
  ((text :initarg :text
	 :type string)
   (address :initarg :address
	    :type string))
  (:documentation
   "Represents a web-link element."))


;; ----- image -----

(defclass image (element)
  ((alt-text :initarg :alt-text
	     :type string)
   (path :initarg :path
	 :type pathname))
  (:documentation
   "Represent an image element."))


;; ----- table -----

(defclass cell (text-element) ()
  (:documentation
   "Represent a cell table element."))

(defclass table (element)
  ((rows :initarg :rows
	 :type list))
  (:documentation
   "Represent a table element. Each row is a list of text elements."))


;; ----- itemize -----

(defclass item (text-element) ()
  (:documentation
   "Represents an item element."))

(defclass itemize-type (element)
  ((elements :initarg :elements
	     :type list))
  (:documentation
   "Represents an itemize element."))

(defclass itemize (itemize-type) ()
  (:documentation
   "Represent an itemize element."))

(defclass enumerate (itemize-type) ()
  (:documentation
   "Represent an enumerate element."))


;; ----- table-of-contents -----

(defclass table-of-contents (element)
  ((project :type project))
  (:documentation
   "Represent a table of contents element."))

(defclass file-table-of-contents (element)
  ((file :type file))
  (:documentation
   "Represent a file related table of contents element."))

(defclass mini-table-of-contents (file-table-of-contents) ()
  (:documentation
   "Represent a table of contents element."))

(defclass table-of-functions (file-table-of-contents) ()
  (:documentation
   "Represent a table of functions element."))

(defclass table-of-symbols (file-table-of-contents) ()
  (:documentation
   "Represent a table of symbols element."))

(defclass table-of-types (file-table-of-contents) ()
  (:documentation
   "Represent a table of types element."))


;; ----- code -----

(defclass code (element)
  ((expr :initarg :expr))
  (:documentation
   "Represent a code element."))

(defclass code-hide () ()
  (:documentation
   "Represent a code hide symbol."))

(defmethod print-object ((object code-hide) stream)
  (princ "..." stream))

(defclass code-comment ()
  ((comment :initarg :comment
	    :type string)
   (expr :initarg :expr))
  (:documentation
   "Represent a code hide symbol."))

(defmethod print-object ((object code-comment) stream)
  (with-slots (comment expr) object
    (pprint-logical-block (stream nil)
      (format stream ";; ~a" comment)
      (pprint-newline :mandatory stream)
      (write expr :stream stream))))

(defclass tagged-code (code tagged-element) ()
  (:documentation
   "Represent a tagged code element."))


(defclass code-block-type (element)
  ((code-type :initarg :code-type
	      :type string))
  (:documentation
   "Represent a code block type element."))

(defclass code-block (code-block-type)
  ((code-elements :initarg :code-elements
		  :type list))
  (:documentation
   "Represent a code block element."))

(defclass code-ref (tagged-element)
  ((code-tags :type tag-table))
  (:documentation
   "Represent a code reference element."))

(defclass verbatim-code-block (code-block-type)
  ((code-text :initarg :code-text
	      :type string))
  (:documentation
   "Represent a verbatim code block element."))

(defclass code-example (element)
  ((code-elements :initarg :code-elements
		  :type list)
   (output :initarg :output
	   :type string)
   (result :initarg :result
	   :type list))
  (:documentation
   "Represent a code example element."))


;; ----- definition -----

(defclass definition (element)
  ((expr :initarg :expr))
  (:documentation
   "Represent a definition element."))

(defclass tagged-definition (definition tagged-element) ()
  (:documentation
   "Represent a tagged definition element."))

(defclass symbol-definition (tagged-definition) ()
  (:documentation
   "Represent a symbol tag definition element."))

(defclass function-definition (tagged-definition) ()
  (:documentation
   "Represent a function tag definition element."))

(defclass type-definition (tagged-definition) ()
  (:documentation
   "Represent a type tag definition element."))

(defmacro define-definition-class (name super docstring)
  `(defclass ,name (,super) ()
     (:documentation
      ,docstring)))

(define-definition-class defclass-definition                  type-definition     "Represent a defclass element.")
(define-definition-class defconstant-definition               symbol-definition   "Represent a defconstant element.")
(define-definition-class defgeneric-definition                function-definition "Represent a defgeneric element.")
(define-definition-class define-compiler-macro-definition     defintion           "Represent a define-compiler-macro element.")
(define-definition-class define-condition-definition          type-definition     "Represent a define-condition element.")
(define-definition-class define-method-combination-definition definition          "Represent a define-method-combination element.")
(define-definition-class define-modify-macro-definition       function-definition "Represent a define-modify-macro element.")
(define-definition-class define-setf-expander-definition      definition          "Represent a define-setf-expander element.")
(define-definition-class define-symbol-macro-definition       symbol-definition   "Represent a define-symbol-macro element.")
(define-definition-class defmacro-definition                  function-definition "Represent a defmacro element.")
(define-definition-class defmethod-definition                 definition          "Represent a defmethod element.")
(define-definition-class defpackage-definition                definition          "Represent a defpackage element.")
(define-definition-class defparameter-definition              symbol-definition   "Represent a defparameter element.")
(define-definition-class defsetf-definition                   definition          "Represent a defsetf element.")
(define-definition-class defstruct-definition                 type-definition     "Represent a defstruct element.")
(define-definition-class deftype-definition                   type-definition     "Represent a deftype element.")
(define-definition-class defun-definition                     function-definition "Represent a defun element.")
(define-definition-class defvar-definition                    symbol-definition   "Represent a defvar element.")
