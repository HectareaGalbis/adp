

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
	   #:check-special-writers #:with-new-style-parameter-list #:with-style-parameters #:with-tag-tables
	   #:define-style-parameter #:select-file #:relative-truename

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

	   #:*begin-file-writer* #:*end-file-writer* #:*file-extension* #:*begin-project-writer*
	   #:*end-project-writer* #:*header-writer* #:*subheader-writer* #:*subsubheader-writer* #:*text-writer*
	   #:*escape-text* #:*bold-writer* #:*italic-writer* #:*bold-italic-writer* #:*code-inline-writer*
	   #:*header-ref-writer* #:*symbol-ref-writer* #:*function-ref-writer* #:*type-ref-writer*
	   #:*web-link-writer* #:*image-writer* #:*table-writer* #:*itemize-writer* #:*code-block-writer*
	   #:*code-example-writer* #:*defclass-writer* #:*defconstant-writer* #:*defgeneric-writer*
	   #:*define-compiler-macro-writer* #:*define-condition-writer* #:*define-method-combination-writer*
	   #:*define-modify-macro-writer* #:*define-setf-expander-writer* #:*define-symbol-macro-writer*
	   #:*defmacro-writer* #:*defmethod-writer* #:*defpackage-writer* #:*defparameter-writer*
	   #:*defsetf-writer* #:*defstruct-writer* #:*deftype-writer* #:*defun-writer* #:*defvar-writer*))


(defpackage #:adp-style-maker
  (:use :cl :alexandria)
  (:nicknames :adpsm)
  (:export #:*adp-pprint-dispatch* #:define-style-parameter #:define-header-writer #:define-subheader-writer
	   #:define-subsubheader-writer #:define-escape-text #:define-text-writer #:define-table-writer
	   #:define-itemize-writer #:define-image-writer #:define-bold-writer #:define-italic-writer
	   #:define-bold-italic-writer #:define-code-inline-writer #:define-web-link-writer
	   #:define-file-ref-writer #:define-header-ref-writer #:define-symbol-ref-writer
	   #:define-function-ref-writer #:define-type-ref-writer #:define-code-block-writer
	   #:define-code-example-writer #:define-defclass-writer #:define-defconstant-writer
	   #:define-defgeneric-writer #:define-define-compiler-macro-writer #:define-define-condition-writer
	   #:define-define-method-combination-writer #:define-define-modify-macro-writer
	   #:define-define-setf-expander-writer #:define-define-symbol-macro-writer #:define-defmacro-writer
	   #:define-defmethod-writer #:define-defpackage-writer #:define-defparameter-writer
	   #:define-defsetf-writer #:define-defstruct-writer #:define-deftype-writer #:define-defun-writer
	   #:define-defvar-writer #:define-file-extension #:def-file-head-writer #:define-file-foot-writer
	   #:def-system-files-writer

	   #:with-defclass-components #:with-defconstant-components #:with-defgeneric-components
	   #:with-define-compiler-macro-components #:with-define-condition-components
	   #:with-define-method-combination-components #:with-define-modify-macro-components
	   #:with-define-setf-expander-components #:with-define-symbol-macro-components
	   #:with-defmacro-components #:with-defmethod-components #:with-defpackage-components
	   #:with-defparameter-components #:with-defsetf-components #:with-defstruct-components
	   #:with-deftype-components #:with-defun-components #:with-defvar-components

	   #:style-maker-api-header))
