
(in-package :adppvt)


(defstruct writer
  proc
  optional)
  
(defmacro define-with-writers (name &body writer-names)
  (with-gensyms (body let-bindings writer-name)
    `(defmacro ,name (&body ,body)
       (let ((,let-bindings (mapcar (lambda (,writer-name)
				      `(,,writer-name nil))
				    ',writer-names)))
	 `(let ,,let-bindings
	    ,@,body)))))

(defmacro define-check-writers (name &body writer-name-optionals)
  (with-gensyms (unless-exprs writer-name optionalp)
    `(defmacro ,name ()
       (let ((,unless-exprs (loop for (,writer-name ,optionalp) in ',writer-name-optionals
				  if (not ,optionalp)
				    collect `(unless ,,writer-name
					       (error "ADP error: The function ~a is not defined in the current style."
						      ,(symbol-name ,writer-name))))))
	 `(progn
	    ,@,unless-exprs)))))

(defmacro define-writers (with-writers-name check-writers-name &body writer-name-optionals)
  (let ((definitions (mapcar (lambda (name-optional)
			       `(defvar ,(car name-optional) nil))
			     writer-name-optionals)))
    `(progn
       ,@definitions
       (define-with-writers ,with-writers-name
	 ,@(mapcar #'car writer-name-optionals))
       (define-check-writers ,check-writers-name
	 ,@writer-name-optionals))))


(define-writers with-writers check-writers

  ;; file
  (*begin-file-writer* t)
  (*end-file-writer* t)
  (*file-extension* nil)

  ;; project
  (*begin-project-writer* t)
  (*end-project-writer* t)

  ;; header
  (*header-writer* nil)
  (*subheader-writer* nil)
  (*subsubheader-writer* nil)

  ;; text
  (*text-writer* nil)
  (*escape-text* t)

  ;; text enrichment
  (*bold-writer* nil)
  (*italic-writer* nil)
  (*emphasis-writer* nil)
  (*inline-code-writer* nil)

  ;; text reference
  (*header-ref-writer* nil)
  (*symbol-ref-writer* nil)
  (*function-ref-writer* nil)
  (*type-ref-writer* nil)

  ;; web link
  (*web-link-writer* nil)

  ;; image
  (*image-writer* nil)

  ;; table
  (*table-writer* nil)

  ;; itemize
  (*itemize-writer* nil)

  ;; code
  (*code-block-writer* nil)
  (*code-example-writer* nil)

  ;; definition
  (*defclass-writer* nil)
  (*defconstant-writer* nil)
  (*defgeneric-writer* nil)
  (*define-compiler-macro-writer* nil)
  (*define-condition-writer* nil)
  (*define-method-combination-writer* nil)
  (*define-modify-macro-writer* nil)
  (*define-setf-expander-writer* nil)
  (*define-symbol-macro-writer* nil)
  (*defmacro-writer* nil)
  (*defmethod-writer* nil)
  (*defpackage-writer* nil)
  (*defparameter-writer* nil)
  (*defsetf-writer* nil)
  (*defstruct-writer* nil)
  (*deftype-writer* nil)
  (*defun-writer* nil)
  (*defvar-writer* nil))
