
(in-package :adppvt)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct writer
    proc
    optional)
  
  (let ((writers nil))
    
    (defmacro with-special-writers (&body body)
      (let ((let-proc-binds (mapcar (lambda (writer)
				      (list (writer-proc writer) nil))
				    writers)))
	`(let ,let-proc-binds
	   ,@body)))

    (defmacro check-special-writers ()
      (let ((unless-exprs (loop for writer in writers
				if (not (writer-optional writer))
				  collect `(unless ,(writer-proc writer)
					     (error "ADP error: The function ~a is not defined in the current style."
						    ,(symbol-name (writer-proc writer)))))))
	`(progn
	   ,@unless-exprs)))
    
    (defmacro define-customizable-writer (writer-proc &optional optionalp)
      (check-type writer-proc symbol)
      (check-type optionalp boolean)
      (when (not (boundp writer-proc))
	(push (make-writer :proc writer-proc :optional optionalp) writers))
      `(defvar ,writer-proc nil))))

;; file
(define-customizable-writer *begin-file-writer* t)
(define-customizable-writer *end-file-writer* t)
(define-customizable-writer *file-extension*)

;; project
(define-customizable-writer *begin-project-writer* t)
(define-customizable-writer *end-project-writer* t)

;; header
(define-customizable-writer *header-writer*)
(define-customizable-writer *subheader-writer*)
(define-customizable-writer *subsubheader-writer*)

;; text
(define-customizable-writer *text-writer*)
(define-customizable-writer *escape-text* t)

;; text enrichment
(define-customizable-writer *bold-writer*)
(define-customizable-writer *italic-writer*)
(define-customizable-writer *emphasis-writer*)
(define-customizable-writer *inline-code-writer*)

;; text reference
(define-customizable-writer *header-ref-writer*)
(define-customizable-writer *symbol-ref-writer*)
(define-customizable-writer *function-ref-writer*)
(define-customizable-writer *type-ref-writer*)

;; web link
(define-customizable-writer *web-link-writer*)

;; image
(define-customizable-writer *image-writer*)

;; table
(define-customizable-writer *table-writer*)

;; itemize
(define-customizable-writer *itemize-writer*)

;; code
(define-customizable-writer *code-block-writer*)
(define-customizable-writer *code-example-writer*)

;; definition
(define-customizable-writer *defclass-writer*)
(define-customizable-writer *defconstant-writer*)
(define-customizable-writer *defgeneric-writer*)
(define-customizable-writer *define-compiler-macro-writer*)
(define-customizable-writer *define-condition-writer*)
(define-customizable-writer *define-method-combination-writer*)
(define-customizable-writer *define-modify-macro-writer*)
(define-customizable-writer *define-setf-expander-writer*)
(define-customizable-writer *define-symbol-macro-writer*)
(define-customizable-writer *defmacro-writer*)
(define-customizable-writer *defmethod-writer*)
(define-customizable-writer *defpackage-writer*)
(define-customizable-writer *defparameter-writer*)
(define-customizable-writer *defsetf-writer*)
(define-customizable-writer *defstruct-writer*)
(define-customizable-writer *deftype-writer*)
(define-customizable-writer *defun-writer*)
(define-customizable-writer *defvar-writer*)
