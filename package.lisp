

(defpackage #:adp
  (:use #:cl #:alexandria)
  (:shadow #:defclass #:defconstant #:defgeneric #:define-compiler-macro #:define-condition
	   #:define-method-combination #:define-modify-macro #:define-setf-expander #:define-symbol-macro
	   #:defmacro #:defmethod #:defpackage #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar)
  (:export #:header #:subheader #:subsubheader #:text #:table #:cell #:itemize #:enumerate #:item #:image #:bold
	   #:italic #:bold-italic #:code-inline #:web-link #:file-ref #:header-ref #:symbol-ref #:function-ref
	   #:type-ref #:code-tag #:code-block #:verbatim-code-block #:code-example #:defclass #:defconstant
	   #:defgeneric #:define-compiler-macro #:define-condition #:define-method-combination
	   #:define-modify-macro #:define-setf-expander #:define-symbol-macro #:defmacro #:defmethod
	   #:defpackage #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar #:write-in-file
	   #:load-documentation-system #:cl-ref #:table-of-contents #:mini-table-of-contents #:table-of-functions
	   #:table-of-symbols #:table-of-types))


(defpackage #:adp-private
  (:use #:cl #:alexandria)
  (:nicknames #:adppvt)
  (:export #:*add-documentation* #:current-style #:*file-adp-elements* #:*header-tags* #:*symbol-tags*
	   #:*function-tags* #:*type-tags* #:push-file-tag #:add-header-tag #:add-symbol-tag
	   #:add-function-tag #:add-type-tag #:empty-header-tags #:empty-symbol-tags
	   #:empty-function-tags #:empty-type-tags #:emplace-adp-element #:push-adp-file #:empty-adp-elements
	   #:check-style-parameters #:set-parameter-value #:write-system-files #:add-code-tag #:get-code-tag
	   #:process-code-tag #:remove-code-tag-exprs #:remove-own-code-tag-exprs #:remove-current-data
	   #:remove-current-procs #:check-current-procs #:hide-symbolp #:create-bold-text #:create-italic-text
	   #:create-bold-italic-text #:create-code-inline-text #:create-web-link-text #:create-file-ref-text
	   #:create-header-ref-text #:create-symbol-ref-text #:create-function-ref-text #:create-type-ref-text
	   #:create-code-block-tag

	   #:def-style-parameter #:def-header-writer #:def-subheader-writer #:def-subsubheader-writer
	   #:def-escape-text-writer #:def-text-writer #:def-table-writer #:def-itemize-writer #:def-image-writer
	   #:def-bold-writer #:def-italic-writer #:def-bold-italic-writer #:def-code-inline-writer
	   #:def-web-link-writer #:def-file-ref-writer #:def-header-ref-writer #:def-symbol-ref-writer
	   #:def-function-ref-writer #:def-type-ref-writer #:def-code-block-writer #:def-code-example-writer
	   #:def-defclass-writer #:def-defconstant-writer #:def-defgeneric-writer
	   #:def-define-compiler-macro-writer #:def-define-condition-writer
	   #:def-define-method-combination-writer #:def-define-modify-macro-writer
	   #:def-define-setf-expander-writer #:def-define-symbol-macro-writer #:def-defmacro-writer
	   #:def-defmethod-writer #:def-defpackage-writer #:def-defparameter-writer #:def-defsetf-writer
	   #:def-defstruct-writer #:def-deftype-writer #:def-defun-writer #:def-defvar-writer
	   #:def-get-file-extension-writer #:def-file-header-writer #:def-file-foot-writer
	   #:def-system-files-writer #:style-maker-api-header #:style-maker-helper-header

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
	   #:with-deftype-components #:with-defun-components #:with-defvar-components #:custom-prin1
	   #:*custom-pprint-dispatch*))
