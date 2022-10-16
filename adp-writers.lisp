
(in-package :adppvt)


(adp:header "Style-maker interface" style-header)


;; ----- style parameters -----

(adp:subheader "Style parameters" style-parameters-subheader)

(adp:defmacro def-style-parameter (name &key (value nil) (key-name nil) (required nil))
  (check-type name symbol "a symbol")
  (check-type key-name keyword "a keyword")
  `(progn
     (defparameter ,name ,value)
     (if ,key-name
	 (add-style-parameter ,name ,key-name ,required)
	 (add-style-parameter ,name (intern (symbol-name ,name) :keyword) *style-parameters*))))


;; ----- def-writer macros ----- Añadir doc string como argumento

(defmacro def-customizable-writer (writer-name global-proc-name)
  (with-gensyms (writer-arg writer-args body-arg)
    `(adp:defmacro ,writer-name (,writer-args &body ,body-arg)
       (check-type ,writer-args list)
       (loop for ,writer-arg in ,writer-args
	     do (check-type ,writer-arg symbol))
       `(setq ,',global-proc-name (lambda ,,writer-args
				    ,@,body-arg)))))


;; ----- guide functions -----

(adp:subheader "Customizable writer macros" customizable-subheader)

(def-customizable-writer def-header-writer *header-proc*)
(def-customizable-writer def-subheader-writer *subheader-proc*)
(def-customizable-writer def-subsubheader-writer *subsubheader-proc*)
(def-customizable-writer def-text-writer *text-proc*)
(def-customizable-writer def-table-writer *table-proc*)
(def-customizable-writer def-itemize-writer *itemize-proc*)
(def-customizable-writer def-image-writer *image-proc*)
(def-customizable-writer def-bold-writer *bold-proc*)
(def-customizable-writer def-italic-writer *italic-proc*)
(def-customizable-writer def-code-inline-writer *code-inline-proc*)
(def-customizable-writer def-web-link-writer *web-link-proc*)
(def-customizable-writer def-header-ref-writer *header-ref-proc*)
(def-customizable-writer def-symbol-ref-writer *symbol-ref-proc*)
(def-customizable-writer def-function-ref-writer *function-ref-proc*)
(def-customizable-writer def-type-ref-writer *type-ref-proc*)
(def-customizable-writer def-code-block-writer *code-block-proc*)
(def-customizable-writer def-code-example-writer *code-example-proc*)

(def-customizable-writer def-defclass-writer *defclass-proc*)
(def-customizable-writer def-defconstant-writer *defconstant-proc*)
(def-customizable-writer def-defgeneric-writer *defgeneric-proc*)
(def-customizable-writer def-define-compiler-macro-writer *define-compiler-macro-proc*)
(def-customizable-writer def-define-condition-writer *define-condition-proc*)
(def-customizable-writer def-define-method-combination-writer *define-method-combination-proc*)
(def-customizable-writer def-define-modify-macro-writer *define-modify-macro-proc*)
(def-customizable-writer def-define-setf-expander-writer *define-setf-expander-proc*)
(def-customizable-writer def-define-symbol-macro-writer *define-symbol-macro-proc*)
(def-customizable-writer def-defmacro-writer *defmacro-proc*)
(def-customizable-writer def-defmethod-writer *defmethod-proc*)
(def-customizable-writer def-defpackage-writer *defpackage-proc*)
(def-customizable-writer def-defparameter-writer *defparameter-proc*)
(def-customizable-writer def-defsetf-writer *defsetf-proc*)
(def-customizable-writer def-defstruct-writer *defstruct-proc*)
(def-customizable-writer def-deftype-writer *deftype-proc*)
(def-customizable-writer def-defun-writer *defun-proc*)
(def-customizable-writer def-defvar-writer *defvar-proc*)

(def-customizable-writer def-get-file-extension-writer *get-file-extension-proc*)
(def-customizable-writer def-file-header-writer *file-header-proc*)
(def-customizable-writer def-file-foot-writer *file-foot-proc*)
(def-customizable-writer def-system-files-writer *system-files-proc*)


(adp:write-in-file #P"docs/style-maker-api")
