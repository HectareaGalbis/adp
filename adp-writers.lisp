
(in-package :adppvt)

(adp:write-in-file #P"docs/style-maker-api")

(adp:header "Style-maker interface" style-maker-api-header)


;; ----- style parameters -----

(adp:subheader "Style parameters")

(adp:defmacro def-style-parameter (name &key (value nil) (key-name nil) (required nil))
  (check-type name symbol "a symbol")
  (check-type key-name keyword "a keyword")
  `(progn
     (defparameter ,name ,value)
     (if ,key-name
	 (add-style-parameter ,name ,key-name ,required)
	 (add-style-parameter ,name (intern (symbol-name ,name) :keyword) *style-parameters*))))


;; ----- def-writer macros ----- Añadir doc string como argumento

(defmacro def-customizable-writer (writer-name global-proc-name num-args)
  (adp:with-made-symbols (writer-arg writer-args body-arg)
    `(adp:defmacro ,writer-name (,writer-args &body ,body-arg)
       (check-type ,writer-args list)
       (assert (= (length ,writer-args) ,num-args) () "The writer must receive ~s arguments." ,num-args)
       (loop for ,writer-arg in ,writer-args
	     do (check-type ,writer-arg symbol))
       `(setq ,',global-proc-name (lambda ,,writer-args
				    ,@,body-arg)))))


;; ----- guide functions -----

(adp:subheader "Customizable writer macros")

(def-customizable-writer def-header-writer *header-proc* 3)
(def-customizable-writer def-subheader-writer *subheader-proc* 3)
(def-customizable-writer def-subsubheader-writer *subsubheader-proc* 3)
(def-customizable-writer def-text-writer *text-proc* 2)
(def-customizable-writer def-table-writer *table-proc* 2)
(def-customizable-writer def-itemize-writer *itemize-proc* 2)
(def-customizable-writer def-image-writer *image-proc* 3)
(def-customizable-writer def-bold-writer *bold-proc* 2)
(def-customizable-writer def-italic-writer *italic-proc* 2)
(def-customizable-writer def-bold-italic-writer *bold-italic-proc* 2)
(def-customizable-writer def-code-inline-writer *code-inline-proc* 2)
(def-customizable-writer def-web-link-writer *web-link-proc* 3)
(def-customizable-writer def-header-ref-writer *header-ref-proc* 4)
(def-customizable-writer def-symbol-ref-writer *symbol-ref-proc* 3)
(def-customizable-writer def-function-ref-writer *function-ref-proc* 3)
(def-customizable-writer def-type-ref-writer *type-ref-proc* 3)
(def-customizable-writer def-code-block-writer *code-block-proc* 2)
(def-customizable-writer def-code-example-writer *code-example-proc* 4)

(def-customizable-writer def-defclass-writer *defclass-proc* 3)
(def-customizable-writer def-defconstant-writer *defconstant-proc* 3)
(def-customizable-writer def-defgeneric-writer *defgeneric-proc* 3)
(def-customizable-writer def-define-compiler-macro-writer *define-compiler-macro-proc* 2)
(def-customizable-writer def-define-condition-writer *define-condition-proc* 3)
(def-customizable-writer def-define-method-combination-writer *define-method-combination-proc* 2)
(def-customizable-writer def-define-modify-macro-writer *define-modify-macro-proc* 3)
(def-customizable-writer def-define-setf-expander-writer *define-setf-expander-proc* 2)
(def-customizable-writer def-define-symbol-macro-writer *define-symbol-macro-proc* 3)
(def-customizable-writer def-defmacro-writer *defmacro-proc* 3)
(def-customizable-writer def-defmethod-writer *defmethod-proc* 2)
(def-customizable-writer def-defpackage-writer *defpackage-proc* 2)
(def-customizable-writer def-defparameter-writer *defparameter-proc* 3)
(def-customizable-writer def-defsetf-writer *defsetf-proc* 2)
(def-customizable-writer def-defstruct-writer *defstruct-proc* 3)
(def-customizable-writer def-deftype-writer *deftype-proc* 3)
(def-customizable-writer def-defun-writer *defun-proc* 3)
(def-customizable-writer def-defvar-writer *defvar-proc* 3)

(def-customizable-writer def-get-file-extension-writer *get-file-extension-proc* 0)
(def-customizable-writer def-file-header-writer *file-header-proc* 1)
(def-customizable-writer def-file-foot-writer *file-foot-proc* 1)
(def-customizable-writer def-system-files-writer *system-files-proc* 1)
