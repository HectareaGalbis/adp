
(in-package :adppvt)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct writer
    proc
    definer
    optional)
  
  (let ((writers nil))
    
    (defmacro with-special-writers (&body body)
      (let ((let-proc-binds (mapcar (lambda (writer)
				      (list (writer-proc writer) nil))
				    writers)))
	`(let ,let-proc-binds
	   ,@body)))

    (defmacro check-special-writers ()
      (let ((unless-exprs (mapcar (lambda (writer)
				    `(when ,(and (not (writer-optional writer))
						 (not (writer-proc writer)))
				       (error ,(concatenate 'string "ADP error: The function " (symbol-name (writer-definer writer)) " is not used in the current style."))))
				  writers)))
	`(progn
	   ,@unless-exprs)))
    
    (defmacro define-customizable-writer (writer-proc writer-definer num-args &optional optionalp)
      (check-type name symbol)
      (check-type args unsigned-byte)
      (let ((writer-args (loop repeat num-args do (gensym))))
	(push (make-writer :proc writer-proc :definer writer-definer :optional optionalp) writers)
	(with-gensyms (body proc-args)
	  `(progn
	     (defvar ,writer-proc nil)
	     (defmacro ,writer-definer (,writer-args &body ,body)
	       (let ((,proc-args (list ,@writer-args)))
		 `(setf ,',writer-proc (lambda ,@,proc-args
					 ,@,body))))))))))

;; file
(define-customizable-writer *file-head-writer* define-file-header-writer 1 t)
(define-customizable-writer *file-foot-writer* define-file-footer-writer 1 t)

;; project
(define-customizable-writer *file-extension* define-file-extension 0)
(define-customizable-writer *general-files-writer* define-general-files-writer 1 t)

;; header
(define-customizable-writer *header-writer* define-header-writer 3)
(define-customizable-writer *subheader-writer* define-subheader-writer 3)
(define-customizable-writer *subsubheader-writer* define-subsubheader-writer 3)

;; text
(define-customizable-writer *text-writer* define-text-writer 2)
(define-customizable-writer *escape-text* define-escape-text 1 t)

;; text enrichment
(define-customizable-writer *bold-writer* define-bold-writer 2)
(define-customizable-writer *italic-writer* define-italic-writer 2)
(define-customizable-writer *bold-italic-writer* define-bold-italic-writer 2)
(define-customizable-writer *code-inline-writer* define-code-inline-writer 2)

;; text reference
(define-customizable-writer *header-ref-writer* define-header-ref-writer 4)
(define-customizable-writer *symbol-ref-writer* define-symbol-ref-writer 3)
(define-customizable-writer *function-ref-writer* define-function-ref-writer 3)
(define-customizable-writer *type-ref-writer* define-type-ref-writer 3)

;; web link
(define-customizable-writer *web-link-writer* define-web-link-writer 3)

;; image
(define-customizable-writer *image-writer* define-image-writer 2)

;; table
(define-customizable-writer *table-writer* define-table-writer 2)

;; itemize
(define-customizable-writer *itemize-writer* define-itemize-writer 2)

;; code
(define-customizable-writer *code-block-writer* define-code-block-writer 3)
(define-customizable-writer *code-example-writer* define-code-example-writer 4)

;; definition
(define-customizable-writer *defclass-writer* define-defclass-writer 2)
(define-customizable-writer *defconstant-writer* define-defconstant-writer 2)
(define-customizable-writer *defgeneric-writer* define-defgeneric-writer 2)
(define-customizable-writer *define-compiler-macro-writer* define-define-compiler-macro-writer 2)
(define-customizable-writer *define-condition-writer* define-define-condition-writer 2)
(define-customizable-writer *define-method-combination-writer* define-define-method-combination-writer 2)
(define-customizable-writer *define-modify-macro-writer* define-define-modify-macro-writer 2)
(define-customizable-writer *define-setf-expander-writer* define-define-setf-expander-writer 2)
(define-customizable-writer *define-symbol-macro-writer* define-define-symbol-macro-writer 2)
(define-customizable-writer *defmacro-writer* define-defmacro-writer 2)
(define-customizable-writer *defmethod-writer* define-defmethod-writer 2)
(define-customizable-writer *defpackage-writer* define-defpackage-writer 2)
(define-customizable-writer *defparameter-writer* define-defparameter-writer 2)
(define-customizable-writer *defsetf-writer* define-defsetf-writer 2)
(define-customizable-writer *defstruct-writer* define-defstruct-writer 2)
(define-customizable-writer *deftype-writer* define-deftype-writer 2)
(define-customizable-writer *defun-writer* define-defun-writer 2)
(define-customizable-writer *defvar-writer* define-defvar-writer 2)
