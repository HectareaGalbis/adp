
(in-package :adppvt)


(defclass code (element)
  ((expr :initarg :expr)
   (hide-symbol :initform '#:hide
		:allocation :class
		:type symbol)
   (comment-symbol :initform ))
  (:documentation
   "Represent a code element."))

(defclass tagged-code (code tagged-element)
  (:documentation
   "Represent a tagged code element."))


(intern "CODE-TAG"     :adp) ; Advance intern
(intern "CODE-QUOTE"   :adp) ; Advance intern
(intern "CODE-HIDE"    :adp) ; Advance intern
(intern "CODE-REMOVE"  :adp) ; Advance intern
(intern "CODE-COMMENT" :adp) ; Advance intern

;; (declaim (ftype (function (t) t) remove-code-tag-exprs))
;; (defun remove-code-tag-exprs (code)
;;   (labels ((remove-code-tag-exprs-aux (code)
;; 	     (if (plistp code)
;; 		 (cond
;; 		   ((member (car code) '(adp::code-tag adp::code-hide adp::code-remove adp::code-comment))
;; 		    (mapcan #'remove-code-tag-exprs-aux (cddr code)))
;; 		   ((eq (car code) 'adp::code-quote)
;; 		    nil)
;; 		   (t
;; 		    (list (mapcan #'remove-code-tag-exprs-aux code))))
;; 		 (list code))))
;;     (car (remove-code-tag-exprs-aux code))))

(defun expr-remove-own-tag-exprs (expr)
  (labels ((remove-own-code-tag-exprs-aux (expr)
	     (if (plistp expr)
		 (cond
		   ((member (car expr) '(adp::code-hide adp::code-remove))
		    (mapcan #'remove-own-code-tag-exprs-aux (cddr expr)))
		   ((eq (car expr) 'adp::code-tag)
		    (list expr))
		   ((member (car expr) '(adp::code-quote adp::code-comment))
		    nil)
		   (t
		    (list (mapcan #'remove-own-code-tag-exprs-aux expr))))
		 (list expr))))
    (car (remove-own-code-tag-exprs-aux expr))))

(defun valid-tag-p (tag tags)
  (declare (type symbol tag) (type list tags))
  (or (null tags)
      (member tag tags)))

(defun expr-process-tag-exprs (tag expr)
  (labels ((process-aux (tag expr)
	     (if (plistp expr)
		 (cond
		   ((eq (car expr) 'adp::code-hide)
		    (if (valid-tag-p tag (cadr expr))
			(list hide-symbol)
			(loop for expr in (cddr expr)
			      append (process-aux tag expr))))
		   ((eq (car expr) 'adp::code-remove)
		    (if (valid-tag-p tag (cadr expr))
			nil
			(loop for expr in (cddr expr)
			      append (process-aux tag expr))))
		   ((eq (car expr) 'adp::code-quote)
		    (if (valid-tag-p tag (cadr expr))
			(loop for expr in (cddr expr)
			      append (process-aux tag expr))
			nil))
		   ((eq (car expr) 'adp::code-comment)
		    (if (valid-tag-p tag (cadr expr))
			(list `(,comment-symbol ,(caddr expr)))))
		   ((eq (car expr) 'adp::code-tag)
		    (loop for expr in (cddr expr)
			  append (process-aux tag expr)))
		   (t
		    (list (loop for expr in expr
				append (process-aux tag expr)))))
		 (list expr))))
    (car (process-aux tag expr))))

(defmethod initialize-instance :after ((code tagged-code) &rest initargs)
  (with-slots (tag expr) code
    (setf expr (expr-process-tag-exprs tag expr))))


(defun make-custom-pprint-dispatch (hide-symbol hide-str comment-symbol)
  (let ((normal-pprint-dispatch *print-pprint-dispatch*)
	(custom-pprint-dispatch (copy-pprint-dispatch)))    
    (flet ((find-shortest-string (strings)
	     (loop for str in strings
		   for shortest = str then (if (< (length str) (length shortest))
					       str
					       shortest)
		   finally (return shortest))))
      (set-pprint-dispatch '(cons (member adp:defclass)) (pprint-dispatch '(defclass)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defconstant)) (pprint-dispatch '(defconstant)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defgeneric)) (pprint-dispatch '(defgeneric)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-compiler-macro)) (pprint-dispatch '(define-compiler-macro)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-condition)) (pprint-dispatch '(define-condition)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-method-combination)) (pprint-dispatch '(define-method-combination)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-modify-macro)) (pprint-dispatch '(define-modify-macro)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-setf-expander)) (pprint-dispatch '(define-setf-expander)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:define-symbol-macro)) (pprint-dispatch '(define-symbol-macro)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defmacro)) (pprint-dispatch '(defmacro)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defmethod)) (pprint-dispatch '(defmethod)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defpackage)) (pprint-dispatch '(defpackage)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defparameter)) (pprint-dispatch '(defparameter)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defsetf)) (pprint-dispatch '(defsetf)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defstruct)) (pprint-dispatch '(defstruct)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:deftype)) (pprint-dispatch '(deftype)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defun)) (pprint-dispatch '(defun)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch '(cons (member adp:defvar)) (pprint-dispatch '(defvar)) 0 custom-pprint-dispatch)
      (set-pprint-dispatch 'symbol (lambda (stream sym)
				     (if (eq sym hide-symbol)
					 (format stream hide-str)
					 (let* ((sym-package (symbol-package sym))
						(nickname (and sym-package
							       (find-shortest-string (package-nicknames sym-package))))
						(print-package-mode (and sym-package
									 (not (equal sym-package (find-package "CL")))
									 (case (nth-value 1 (find-symbol (symbol-name sym) sym-package))
									   (:external :external)
									   (:internal (if (or (boundp sym) (fboundp sym))
											  :internal
											  nil))
									   (t nil))))
						(package-to-print (and print-package-mode
								       (or nickname
									   (and (keywordp sym) "")
									   (package-name sym-package))))
						(*print-escape* nil)
						(*print-pprint-dispatch* normal-pprint-dispatch))
					   (case print-package-mode
					     (:external (format stream "~a:~a" package-to-print (symbol-name sym)))
					     (:internal (format stream "~a::~a" package-to-print (symbol-name sym)))
					     (t (format stream "~a" (symbol-name sym)))))))
			   0 custom-pprint-dispatch)
      (set-pprint-dispatch `(cons (member ,comment-symbol)) (lambda (stream code-comment)
							      (format stream ";; ~a" (cadr code-comment)))
			   0 custom-pprint-dispatch))))


(defun code-custom-prin1 (code stream hide-str)
  "It is like prin1, but uses a custom pprint dispatch. Also, if a hide symbol is found, then hide-str is princ-ed."
  (declare (type code code) (type stream stream) (type string hide-str))
  (with-slots (expr hide-symbol comment-symbol) code
    (let ((custom-pprint-dispatch (make-custom-pprint-dispatch hide-symbol hide-str comment-symbol)))
      (let ((*print-pprint-dispatch* custom-pprint-dispatch))
	(prin1 expr stream)))))


(defmethod element-print ((element code) stream)
  (code-custom-prin1 element stream "..."))


(defclass code-block (element)
  ((code-tags :initarg :code-tags
	      :type tag-table)
   (code-elements :initarg :code-elements
		  :type list))
  (:documentation
   "Represent a code block element."))

(define-customizable-writer *code-block-writer* define-code-block-writer 3)

(defmethod element-print ((element code-block) stream)
  (with-slots (code-elements code-tags) element
    (let ((complete-exprs (mapcan (lambda (expr)
				    (if (typep expr 'code-reference)
					(with-slots (code-tag source-location) expr
					  (multiple-value-bind (tag-exprs tag-foundp) (tag-table-find-elements code-tags code-tag)
					    (if tag-foundp
						(coerce tag-exprs 'list)
						(error "The tag ~s used at '~a' is not a code tag."
						       expr source-location))))
					(list expr)))
				  code-exprs))
	  (exprs-string (with-output-to-string (str-stream)
			  (element-print str-stream "~{~a~^~%~%~}" complete-exprs)))) ; Usar element-print
      (funcall *code-block-writer* stream "Lisp" exprs-string))))


(defclass verbatim-code-block (element)
  ((code-type :initarg :code-type
	      :type string)
   (code-text :initarg :code-text
	      :type string))
  (:documentation
   "Represent a verbatim code block element."))

(defmethod element-print ((element verbatim-code-block) stream)
  (with-slots (code-type code-text) element
    (funcall *code-block-writer* stream code-type code-text)))


(defclass code-example (element)
  ((code :initarg :code)
   (output :initarg :output
	   :type string)
   (result :initarg
	   :type list))
  (:documentation
   "Represent a code example element."))

(define-customizable-writer *code-example-writer* define-code-example-writer 4)

(defmethod element-print ((element code-example) stream)
  (with-slots (code output result) element
    (funcall *code-example-writer* stream code )))
