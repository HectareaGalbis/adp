
(in-package :adp/markdown)


;; ----- guide functions -----

(adppvt:def-header-writer (stream text tag)
  (declare (ignore tag))
  (format stream "# ~a~%~%" text))

(adppvt:def-subheader-writer (stream text tag)
  (declare (ignore tag))
  (format stream "## ~a~%~%" text))

(adppvt:def-subsubheader-writer (stream text tag)
  (declare (ignore tag))
  (format stream "### ~a~%~%" text))

(adppvt:def-text-writer (stream text)
  (format stream "~a~%~%" text))

(adppvt:def-table-writer (stream table)
  (format stream "~{| ~a ~}|~%" (car table))
  (format stream "~v@{| --- ~}|~%" (length (car table)) nil)
  (format stream "~{~{| ~a ~}|~%~}~%~%" (cdr table)))

(adppvt:def-itemize-writer (stream items)
  (labels ((itemize-aux (item-list level)
	     (loop for item in item-list
		   if (eq (car item) :item)
		     do (format stream "~v@{  ~}* ~a~%" level (cadr item))
		   else if (eq (car item) :itemize)
			  do (itemize-aux (cdr item) (1+ level))
		   finally (when (zerop level)
			     (format stream "~%")))))
    (itemize-aux items 0)))

(adppvt:def-image-writer (stream alt-text root-path rel-image-path)
  (format stream "![~a](~a)~%~%" alt-text (merge-pathnames root-path rel-image-path)))

(adppvt:def-bold-writer (stream text)
  (format stream "**~a**" text))

(adppvt:def-italic-writer (stream text)
  (format stream "_~a_" text))

(adppvt:def-bold-italic-writer (stream text)
  (format stream "***~a***" text))

(adppvt:def-code-inline-writer (stream code)
  (let ((*print-pretty* nil))
    (format stream "`~a`" code)))

(adppvt:def-web-link-writer (stream name link)
  (format stream "[~a](~a)" name link))

(adppvt:def-file-ref-writer (stream root-path file-path)
    (declare (ignore root-path))
  (format stream "[~a](~a.md)" file-path file-path))


(defun convert-to-github-header-anchor (str)
  (let ((down-str (string-downcase str))
	(simple-str (make-array 100 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for down-char across down-str
	  do (when (or (alphanumericp down-char)
		       (char= down-char #\space)
		       (char= down-char #\-))
	       (vector-push-extend down-char simple-str)))
    (loop for i from 0 below (fill-pointer simple-str)
	  do (when (char= (aref simple-str i) #\space)
	       (setf (aref simple-str i) #\-)))
    (values simple-str)))


(defun symbol-github-name (sym)
  (let* ((sym-name (symbol-name sym))
	 (length-name (length sym-name)))
    (if (and (char= (aref sym-name 0) #\*)
	     (char= (aref sym-name (1- length-name)) #\*))
	(format nil "\\*~a\\*" (subseq sym-name 1 (1- length-name)))
	(format nil "~a" sym))))


(adppvt:def-header-ref-writer (stream tag header-text root-path file-path)
  (declare (ignore tag root-path))
  (format stream "[~a](~a.md#~a)" header-text file-path (convert-to-github-header-anchor header-text)))

(defun symbol-macro-p (sym &optional env)
  (let ((*macroexpand-hook* (constantly nil)))
    (nth-value 1 (macroexpand-1 sym env))))

(adppvt:def-symbol-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path))
  (let* ((symbol-header (cond
			  ((symbol-macro-p tag)
			   (format nil "Symbol macro: ~a" (symbol-github-name tag)))
			  ((constantp tag)
			   (format nil "Constant: ~a" (symbol-github-name tag)))
			  (t (format nil "Variable: ~a" (symbol-github-name tag)))))
	 (symbol-anchor (convert-to-github-header-anchor symbol-header)))
    (format stream "[~a](~a.md#~a)" (symbol-github-name tag) file-path symbol-anchor)))

(adppvt:def-function-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path))
  (let* ((function-header (cond
			    ((macro-function tag)
			     (format nil "Macro: ~a" tag))
			    ((subtypep tag 'generic-function)
			     (format nil "Generic function: ~a" tag))
			    (t (format nil "Function: ~a" tag))))
	 (function-anchor (convert-to-github-header-anchor function-header)))
    (format stream "[~a](~a.md#~a)" tag file-path function-anchor)))

(adppvt:def-type-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path))
  (let* ((type-header (cond
			((subtypep tag 'condition)
			 (format nil "Condition: ~a" tag))
			((subtypep tag 'structure-class)
			 (format nil "Struct: ~a" tag))
			((subtypep tag 'class)
			 (format nil "Class: ~a" tag))
			(t (format nil "Type: ~a" tag))))
	 (type-anchor (convert-to-github-header-anchor type-header)))
    (format stream "[~a](~a.md#~a)" tag file-path type-anchor)))

(adppvt:def-code-block-writer (stream code-list)
    (format stream "```")
  (loop for code in code-list
	do (terpri stream)
	   (adppvt:custom-prin1 code stream "...")
	   (terpri stream))
  (format stream "```~%~%"))

(adppvt:def-code-example-writer (stream code-list)
  (format stream "```")
  (loop for (code output result) (t string list) in code-list
	do (terpri stream)
	   (adppvt:custom-prin1 code stream "...")
	   (format stream "~a~{~%~s~}~%" output result))
  (format stream "```~%~%"))


;; ----- api functions -----

(adppvt:def-defclass-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defclass-components ((class-name superclass-names slot-specifiers documentation default-initargs metaclass) source)
    (format stream "#### Class: ~a~%~%" class-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defclass ~s ~a~%  ~a~@[~%  (:default-initargs~{ ~a~})~]~[~%  (:metaclass ~a)~])~%```~%~%"
	      class-name superclass-names slot-specifiers default-initargs metaclass documentation))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defconstant-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defconstant-components ((name initial-value documentation) source)
    (format stream "#### Constant: ~a~%~%" (symbol-github-name name))
    (format stream "```Lisp~%(defconstant ~s ~s)~%```~%~%" name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defgeneric-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defgeneric-components ((function-name gf-lambda-list documentation) source)
    (format stream "#### Generic function: ~a~%~%" function-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defgeneric ~s ~a~%  ...)~%```~%~%" function-name gf-lambda-list))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-compiler-macro-writer (stream source)
  (adppvt:with-define-compiler-macro-components ((name documentation) source)
    (format stream "#### Compiler macro: ~a~%~%" name)
    (format stream "```Lisp~%(define-compiler-macro ~s~%  ...)~%```~%~%" name)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-condition-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-condition-components ((name parent-types slot-specs default-initargs report-name documentation) source)
    (format stream "#### Condition: ~a~%~%" name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defcondition ~s ~s~%  ~a~@[~%  (:default-initargs~{ ~a~})~]~@[~%  (:report ~a)~])~%```~%~%"
	      name parent-types slot-specs default-initargs report-name))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-method-combination-writer (stream source)
  (adppvt:with-define-method-combination-components ((name documentation) source)
    (format stream "#### Method combination: ~a~%~%" name)
    (format stream "```Lisp~%(define-method-combination ~s~%  ...)~%```~%~%" name)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-modify-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-modify-macro-components ((name lambda-list function documentation) source)
    (format stream "#### Macro: ~a~%~%" name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-modify-macro ~s ~a ~s)~%```~%~%" name lambda-list function))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-setf-expander-writer (stream source)
  (adppvt:with-define-setf-expander-components ((access-fn lambda-list documentation) source)
    (format stream "#### Setf expander: ~a~%~%" access-fn)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-setf-expander ~s ~a~%  ...)~%```~%~%" access-fn lambda-list))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-symbol-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-symbol-macro-components ((symbol expansion) source)
    (format stream "#### Symbol macro: ~a~%~%" (symbol-github-name symbol))
    (format stream "```Lisp~%(define-symbol-macro ~s ~s)~%```~%~%" symbol expansion)))

(adppvt:def-defmacro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defmacro-components ((name lambda-list documentation) source)
    (format stream "#### Macro: ~a~%~%" name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defmacro ~s ~a~%  ...)~%```~%~%" name lambda-list))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defmethod-writer (stream source)
  (adppvt:with-defmethod-components ((function-name method-qualifiers specialized-lambda-list documentation) source)
    (format stream "#### Method: ~a~%~%" function-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defmethod ~s~{ ~s~} ~a~%  ...)~%```~%~%" function-name method-qualifiers specialized-lambda-list))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defpackage-writer (stream source)
  (adppvt:with-defpackage-components ((defined-package-name nicknames use-package-names documentation) source)
    (format stream "#### Package: ~a~%~%" defined-package-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defpackage ~a~@[~%  (:nicknames~{ ~a~})~]~@[~%  (:use~{ ~a~})~]~%  ...)~%```~%~%"
	      defined-package-name nicknames use-package-names))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defparameter-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defparameter-components ((name initial-value documentation) source)
    (format stream "#### Variable: ~a~%~%" (symbol-github-name name))
    (format stream "```Lisp~%(defparameter ~s ~s)~%```~%~%"
	    name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defsetf-writer (stream source)
  (adppvt:with-defsetf-components ((access-fn update-fn documentation) source)
    (format stream "#### Defsetf: ~a~%~%" access-fn)
    (format stream "```Lisp~%(defsetf ~s ~s)" access-fn update-fn)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defstruct-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defstruct-components ((structure-name name-and-options slot-descriptions documentation) source)
    (format stream "#### Struct: ~a~%~%" structure-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defstruct ~s~%  ~a)~%```~%~%" name-and-options slot-descriptions))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-deftype-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-deftype-components ((name lambda-list documentation) source)
    (format stream "#### Type: ~a~%~%" name)
    (format stream "```Lisp~%(deftype ~s ~s)~%```~%~%" name lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defun-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defun-components ((function-name lambda-list documentation) source)
    (format stream "#### Function: ~a~%~%" function-name)
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defun ~s ~a)~%```~%~%" function-name lambda-list))
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defvar-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defvar-components ((name initial-value documentation) source)
    (format stream "#### Variable: ~a~%~%" (symbol-github-name name))
    (format stream "```Lisp~%(defvar ~s~@[ ~s~])~%```~%~%" name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))


;; ----- file functions -----

(adppvt:def-get-file-extension-writer ()
  "md")
