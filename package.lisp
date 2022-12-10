

(defpackage #:adp
  (:use #:cl #:alexandria)
  (:shadow #:defclass #:defconstant #:defgeneric #:define-compiler-macro #:define-condition
	   #:define-method-combination #:define-modify-macro #:define-setf-expander #:define-symbol-macro
	   #:defmacro #:defmethod #:defpackage #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar)
  (:export #:header #:subheader #:subsubheader #:text #:table #:cell #:itemize #:enumerate #:item
	   #:table-of-contents #:mini-table-of-contents #:table-of-functions #:table-of-symbols #:table-of-types
	   #:image #:bold #:italic #:bold-italic #:code-inline #:web-link #:header-ref #:symbol-ref
	   #:function-ref #:type-ref #:code-tag #:code-quote #:code-comment #:code-hide #:code-remove
	   #:code-block #:verbatim-code-block #:code-example #:defclass #:defconstant #:defgeneric
	   #:define-compiler-macro #:define-condition #:define-method-combination #:define-modify-macro
	   #:define-setf-expander #:define-symbol-macro #:defmacro #:defmethod #:defpackage #:defparameter
	   #:defsetf #:defstruct #:deftype #:defun #:defvar #:in-file #:load-system #:cl-ref))


(defpackage #:adp-private
  (:use #:cl #:alexandria)
  (:nicknames #:adppvt)
  (:export #:add-element #:project-print #:add-code-tag #:*adp-pprint-dispatch* #:with-special-writers
	   #:check-special-writers #:with-new-style-parameter-list #:with-style-parameters

	   #:header #:subheader #:subsubheader #:text #:table #:cell #:itemize #:enumerate #:item
	   #:table-of-contents #:mini-table-of-contents #:table-of-functions #:table-of-symbols #:table-of-types
	   #:image #:bold #:italic #:bold-italic #:code-inline #:web-link #:header-ref #:symbol-ref
	   #:function-ref #:type-ref #:tagged-code #:code-ref #:code-comment #:code-hide #:code-block #:code
	   #:verbatim-code-block #:code-example #:defclass-definition #:defconstant-definition
	   #:defgeneric-definition #:define-compiler-macro-definition #:define-condition-definition
	   #:define-method-combination-definition #:define-modify-macro-definition
	   #:define-setf-expander-definition #:define-symbol-macro-definition #:defmacro-definition
	   #:defmethod-definition #:defpackage-definition #:defparameter-definition #:defsetf-definition
	   #:defstruct-definition #:deftype-definition #:defun-definition #:defvar-definition #:project

	   #:define-style-parameter #:define-header-writer #:define-subheader-writer #:define-subsubheader-writer
	   #:define-escape-text #:define-text-writer #:define-table-writer #:define-itemize-writer
	   #:define-image-writer #:define-bold-writer #:define-italic-writer #:define-bold-italic-writer
	   #:define-code-inline-writer #:define-web-link-writer #:define-file-ref-writer
	   #:define-header-ref-writer #:define-symbol-ref-writer #:define-function-ref-writer
	   #:define-type-ref-writer #:define-code-block-writer #:define-code-example-writer
	   #:define-defclass-writer #:define-defconstant-writer #:define-defgeneric-writer
	   #:define-define-compiler-macro-writer #:define-define-condition-writer
	   #:define-define-method-combination-writer #:define-define-modify-macro-writer
	   #:define-define-setf-expander-writer #:define-define-symbol-macro-writer #:define-defmacro-writer
	   #:define-defmethod-writer #:define-defpackage-writer #:define-defparameter-writer
	   #:define-defsetf-writer #:define-defstruct-writer #:define-deftype-writer #:define-defun-writer
	   #:define-defvar-writer #:define-file-extension #:def-file-head-writer #:define-file-foot-writer
	   #:def-system-files-writer

	   #:defclass-class-name #:defclass-superclass-names #:defclass-slot-specifiers #:defclass-slot-names
	   #:defclass-slot-options #:defclass-reader-function-names #:defclass-writer-function-names
	   #:defclass-accessor-function-names #:defclass-allocation-types #:defclass-initarg-names
	   #:defclass-initforms #:defclass-type-specifiers #:defclass-slot-documentations
	   #:defclass-class-options #:defclass-default-initargs #:defclass-documentation #:defclass-metaclass
	   #:defconstant-name #:defconstant-initial-value #:defconstant-documentation #:defgeneric-function-name
	   #:defgeneric-gf-lambda-list #:defgeneric-options #:defgeneric-argument-precedence-order
	   #:defgeneric-gf-declarations #:defgeneric-documentation #:defgeneric-method-combination
	   #:defgeneric-generic-function-class #:defgeneric-method-class #:defgeneric-method-descriptions
	   #:define-compiler-macro-name #:define-compiler-macro-lambda-list #:define-compiler-macro-declarations
	   #:define-compiler-macro-documentation #:define-compiler-macro-forms #:define-condition-name
	   #:define-condition-parent-types #:define-condition-slot-specs #:define-condition-slot-names
	   #:define-condition-slot-options #:define-condition-slot-readers #:define-condition-slot-writers
	   #:define-condition-slot-accessors #:define-condition-slot-allocations
	   #:define-condition-slot-initargs #:define-condition-slot-initforms #:define-condition-slot-types
	   #:define-condition-options #:define-condition-default-initargs #:define-condition-documentation
	   #:define-condition-report-name #:define-method-combination-name
	   #:define-method-combination-short-form-options #:define-method-combination-identity-with-one-argument
	   #:define-method-combination-operator #:define-method-combination-lambda-list
	   #:define-method-combination-method-group-specifiers
	   #:define-method-combination-method-group-specifier-names
	   #:define-method-combination-qualifier-patterns #:define-method-combination-predicates
	   #:define-method-combination-long-form-options #:define-method-combination-descriptions
	   #:define-method-combination-orders #:define-method-combination-requireds
	   #:define-method-combination-arguments #:define-method-combination-generic-function
	   #:define-method-combination-declarations #:define-method-combination-documentation
	   #:define-method-combination-forms #:define-modify-macro-name #:define-modify-macro-lambda-list
	   #:define-modify-macro-function #:define-modify-macro-documentation #:define-setf-expander-access-fn
	   #:define-setf-expander-lambda-list #:define-setf-expander-declarations
	   #:define-setf-expander-documentation #:define-setf-expander-forms #:define-symbol-macro-symbol
	   #:define-symbol-macro-expansion #:defmacro-name #:defmacro-lambda-list #:defmacro-declarations
	   #:defmacro-documentation #:defmacro-forms #:defmethod-function-name #:defmethod-method-qualifiers
	   #:defmethod-specialized-lambda-list #:defmethod-declarations #:defmethod-documentation
	   #:defmethod-forms #:defpackage-options #:defpackage-defined-package-name #:defpackage-nicknames
	   #:defpackage-use-package-names #:defpackage-shadow-symbol-names
	   #:defpackage-shadowing-import-from-package-names #:defpackage-shadowing-import-from-symbol-names
	   #:defpackage-import-from-package-names #:defpackage-import-from-symbol-names
	   #:defpackage-export-symbol-names #:defpackage-import-symbol-names #:defpackage-size
	   #:defparameter-name #:defparameter-initial-value #:defparameter-documentation #:defsetf-access-fn
	   #:defsetf-update-fn #:defsetf-documentation #:defstruct-name-and-options #:defstruct-options
	   #:defstruct-conc-name-option #:defstruct-constructor-options #:defstruct-copier-option
	   #:defstruct-predicate-option #:defstruct-include-option #:defstruct-printer-option
	   #:defstruct-print-object-option #:defstruct-print-function-option #:defstruct-type-option
	   #:defstruct-named-option #:defstruct-initial-offset-option #:defstruct-slot-descriptions
	   #:defstruct-slot-options #:defstruct-conc-name #:defstruct-constructor-arglists
	   #:defstruct-constructor-names #:defstruct-copier-name #:defstruct-included-structure-name
	   #:defstruct-include-option-slot-descriptors #:defstruct-initial-offset #:defstruct-predicate-name
	   #:defstruct-printer-name #:defstruct-slot-names #:defstruct-slot-initforms #:defstruct-slot-types
	   #:defstruct-slot-read-only-ps #:defstruct-type #:defstruct-documentation #:deftype-name
	   #:deftype-lambda-list #:deftype-declarations #:deftype-documentation #:deftype-forms
	   #:defun-function-name #:defun-lambda-list #:defun-declarations #:defun-documentation #:defun-forms
	   #:defvar-name #:defvar-initial-value #:defvar-documentation #:with-defclass-components
	   #:with-defconstant-components #:with-defgeneric-components
	   #:with-define-compiler-macro-components #:with-define-condition-components
	   #:with-define-method-combination-components #:with-define-modify-macro-components
	   #:with-define-setf-expander-components #:with-define-symbol-macro-components
	   #:with-defmacro-components #:with-defmethod-components #:with-defpackage-components
	   #:with-defparameter-components #:with-defsetf-components #:with-defstruct-components
	   #:with-deftype-components #:with-defun-components #:with-defvar-components))
