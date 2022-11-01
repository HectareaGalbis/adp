
(in-package :adp/github-md)

;; ----- Aux functions -----

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

(defun escape-characters (text)
  (let ((problematic-chars '(#\* #\_ #\~ #\`))
	(fixed-text (make-array (length text) :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for char across text
	  when (member char problematic-chars :test #'char=)
	    do (vector-push-extend #\\ fixed-text)
	  do (vector-push-extend char fixed-text))
    (values fixed-text)))

(defun symbol-macro-p (sym &optional env)
  (let ((*macroexpand-hook* (constantly nil)))
    (nth-value 1 (macroexpand-1 sym env))))


;; ----- guide functions -----

(adppvt:def-header-writer (stream text tag)
  (declare (ignore tag))
  (format stream "# ~a~%~%" (escape-characters text)))

(adppvt:def-subheader-writer (stream text tag)
  (declare (ignore tag))
  (format stream "## ~a~%~%" (escape-characters text)))

(adppvt:def-subsubheader-writer (stream text tag)
  (declare (ignore tag))
  (format stream "### ~a~%~%" (escape-characters text)))

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

(adppvt:def-image-writer (stream alt-text rel-image-path)
  (format stream "![~a](/~a)~%~%" (escape-characters alt-text) rel-image-path))

(adppvt:def-bold-writer (stream text)
  (format stream "**~a**" (escape-characters text)))

(adppvt:def-italic-writer (stream text)
  (format stream "_~a_" (escape-characters text)))

(adppvt:def-bold-italic-writer (stream text)
  (format stream "***~a***" (escape-characters text)))

(adppvt:def-code-inline-writer (stream text)
  (let ((*print-pretty* nil))
    (format stream "`~a`" (escape-characters text))))

(adppvt:def-web-link-writer (stream name link)
  (format stream "[~a](~a)" (escape-characters name) link))

(adppvt:def-header-ref-writer (stream tag header-text file-path)
  (declare (ignore tag))
  (format stream "[~a](/~a.md#~a)" (escape-characters header-text) file-path (convert-to-github-header-anchor header-text)))

(adppvt:def-symbol-ref-writer (stream tag file-path)
  (let* ((symbol-header (cond
			  ((symbol-macro-p tag)
			   (format nil "Symbol macro: ~a" (escape-characters (princ-to-string tag))))
			  ((constantp tag)
			   (format nil "Constant: ~a" (escape-characters (princ-to-string tag))))
			  (t (format nil "Variable: ~a" (escape-characters (princ-to-string tag))))))
	 (symbol-anchor (convert-to-github-header-anchor symbol-header))
	 (*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
    (format stream "[~a](/~a.md#~a)" (escape-characters (prin1-to-string tag)) file-path symbol-anchor)))

(adppvt:def-function-ref-writer (stream tag file-path)
  (let* ((function-header (cond
			    ((macro-function tag)
			     (format nil "Macro: ~a" (escape-characters (princ-to-string tag))))
			    ((subtypep tag 'generic-function)
			     (format nil "Generic function: ~a" (escape-characters (princ-to-string tag))))
			    (t (format nil "Function: ~a" (escape-characters (princ-to-string tag))))))
	 (function-anchor (convert-to-github-header-anchor function-header))
	 (*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
    (format stream "[~a](/~a.md#~a)" (escape-characters (prin1-to-string tag)) file-path function-anchor)))

(adppvt:def-type-ref-writer (stream tag file-path)
  (let* ((type-header (cond
			((subtypep tag 'condition)
			 (format nil "Condition: ~a" (escape-characters (princ-to-string tag))))
			((subtypep tag 'structure-class)
			 (format nil "Struct: ~a" (escape-characters (princ-to-string tag))))
			((subtypep tag 'class)
			 (format nil "Class: ~a" (escape-characters (princ-to-string tag))))
			(t (format nil "Type: ~a" (escape-characters (princ-to-string tag))))))
	 (type-anchor (convert-to-github-header-anchor type-header))
	 (*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
    (format stream "[~a](/~a.md#~a)" (escape-characters (prin1-to-string tag)) file-path type-anchor)))

(adppvt:def-code-block-writer (stream code-list)
    (format stream "```")
  (loop for code in code-list
	do (terpri stream)
	   (adppvt:custom-prin1 code stream "...")
	   (terpri stream))
  (format stream "```~%~%"))

(adppvt:def-code-example-writer (stream code output results)
  (format stream "```")
  (loop for expr in code
	do (terpri stream)
	   (adppvt:custom-prin1 expr stream "...")
	   (terpri stream))
  (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
    (format stream "~a~{~%~s~}~%" output results))
  (format stream "```~%~%"))


;; ----- api functions -----

(adppvt:def-defclass-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defclass-components ((class-name superclass-names slot-specifiers documentation default-initargs metaclass) source)
    (format stream "#### Class: ~a~%~%" (escape-characters (princ-to-string class-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defclass ~s ~s~%  ~s~@[~%  (:default-initargs~{ ~s~})~]~[~%  (:metaclass ~s)~])~%```~%~%"
	      class-name superclass-names slot-specifiers default-initargs metaclass documentation))
    (when documentation
      (format stream "````~%~a~%````~%~%" (escape-characters documentation)))))

(adppvt:def-defconstant-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defconstant-components ((name initial-value documentation) source)
    (format stream "#### Constant: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defconstant ~s ~s)~%```~%~%" name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defgeneric-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defgeneric-components ((function-name gf-lambda-list documentation) source)
    (format stream "#### Generic function: ~a~%~%" (escape-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defgeneric ~s ~s~%  ...)~%```~%~%" function-name gf-lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-compiler-macro-writer (stream source)
  (adppvt:with-define-compiler-macro-components ((name documentation) source)
    (format stream "#### Compiler macro: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-compiler-macro ~s~%  ...)~%```~%~%" name))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-condition-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-condition-components ((name parent-types slot-specs default-initargs report-name documentation) source)
    (format stream "#### Condition: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defcondition ~s ~s~%  ~s~@[~%  (:default-initargs~{ ~s~})~]~@[~%  (:report ~s)~])~%```~%~%"
	      name parent-types slot-specs default-initargs report-name))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-method-combination-writer (stream source)
  (adppvt:with-define-method-combination-components ((name documentation) source)
    (format stream "#### Method combination: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-method-combination ~s~%  ...)~%```~%~%" name))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-modify-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-modify-macro-components ((name lambda-list function documentation) source)
    (format stream "#### Macro: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-modify-macro ~s ~s ~s)~%```~%~%" name lambda-list function))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-setf-expander-writer (stream source)
  (adppvt:with-define-setf-expander-components ((access-fn lambda-list documentation) source)
    (format stream "#### Setf expander: ~a~%~%" (escape-characters (princ-to-string access-fn)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-setf-expander ~s ~s~%  ...)~%```~%~%" access-fn lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-define-symbol-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-symbol-macro-components ((symbol expansion) source)
    (format stream "#### Symbol macro: ~a~%~%" (escape-characters (princ-to-string symbol)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(define-symbol-macro ~s ~s)~%```~%~%" symbol expansion))))

(adppvt:def-defmacro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defmacro-components ((name lambda-list documentation) source)
    (format stream "#### Macro: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defmacro ~s ~s~%  ...)~%```~%~%" name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defmethod-writer (stream source)
  (adppvt:with-defmethod-components ((function-name method-qualifiers specialized-lambda-list documentation) source)
    (format stream "#### Method: ~a~%~%" (escape-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defmethod ~s~{ ~s~} ~s~%  ...)~%```~%~%" function-name method-qualifiers specialized-lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defpackage-writer (stream source)
  (adppvt:with-defpackage-components ((defined-package-name nicknames use-package-names documentation) source)
    (format stream "#### Package: ~a~%~%" (escape-characters (princ-to-string defined-package-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defpackage ~s~@[~%  (:nicknames~{ ~s~})~]~@[~%  (:use~{ ~s~})~]~%  ...)~%```~%~%"
	      defined-package-name nicknames use-package-names))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defparameter-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defparameter-components ((name initial-value documentation) source)
    (format stream "#### Variable: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defparameter ~s ~s)~%```~%~%"
	      name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defsetf-writer (stream source)
  (adppvt:with-defsetf-components ((access-fn update-fn documentation) source)
    (format stream "#### Defsetf: ~a~%~%" (escape-characters (princ-to-string access-fn)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defsetf ~s ~s)" access-fn update-fn))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defstruct-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defstruct-components ((structure-name name-and-options slot-descriptions documentation) source)
    (format stream "#### Struct: ~a~%~%" (escape-characters (princ-to-string structure-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defstruct ~s~%  ~s)~%```~%~%" name-and-options slot-descriptions))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-deftype-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-deftype-components ((name lambda-list documentation) source)
    (format stream "#### Type: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(deftype ~s ~s~%  ...)~%```~%~%" name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defun-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defun-components ((function-name lambda-list documentation) source)
    (format stream "#### Function: ~a~%~%" (escape-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defun ~s ~s~%  ...)~%```~%~%" function-name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adppvt:def-defvar-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defvar-components ((name initial-value documentation) source)
    (format stream "#### Variable: ~a~%~%" (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adppvt:*custom-pprint-dispatch*))
      (format stream "```Lisp~%(defvar ~s~@[ ~s~])~%```~%~%" name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))


;; ----- file functions -----

(adppvt:def-get-file-extension-writer ()
  "md")
