
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


(adppvt:def-header-ref-writer (stream tag header-text root-path file-path)
  (declare (ignore tag root-path))
  (format stream "[~a](~a.md#~a)" header-text file-path (convert-to-github-header-anchor header-text)))

(defun symbol-macro-p (sym &optional env)
  (let ((*macroexpand-hook* (constantly nil)))
    (nth-value 1 (macroexpand-1 sym env))))

(adppvt:def-symbol-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path file-path))
  (format stream "`~a`" tag))

(adppvt:def-function-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path file-path))
  (format stream "`~a`" tag))

(adppvt:def-type-ref-writer (stream tag root-path file-path)
  (declare (ignore root-path file-path))
  (format stream "`~a`" tag))

(defun prin1-with-hide-string (stream code str)
  (let ((normal-pprint-dispatch *print-pprint-dispatch*)
	(custom-pprint-dispatch (copy-pprint-dispatch nil)))
    (labels ((custom-hide-print (stream sym)
	       (if (adppvt:hide-symbolp sym)
		   (princ str stream)
		   (let ((*print-pprint-dispatch* normal-pprint-dispatch))
		     (prin1 sym stream)))))
      (set-pprint-dispatch 'symbol #'custom-hide-print 0 custom-pprint-dispatch)
      (let ((*print-pprint-dispatch* custom-pprint-dispatch))
	(prin1 code stream)))))

(adppvt:def-code-block-writer (stream code-list)
    (format stream "```")
  (loop for code in code-list
	do (terpri stream)
	   (prin1-with-hide-string stream code "...")
	   (terpri stream))
  (format stream "```~%~%"))

(adppvt:def-code-example-writer (stream code-list)
  (format stream "```")
  (loop for (code output result) (t string list) in code-list
	do (terpri stream)
	   (prin1-with-hide-string stream code "...")
	   (format stream "~a~{~%~s~}~%" output result))
  (format stream "```~%~%"))


;; ----- api functions -----

(adppvt:def-defclass-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defclass-components ((class-name superclass-names slot-specifiers documentation default-initargs metaclass) source)
    (format stream "Type: ~a~%~%" class-name)
    (format stream "```Lisp~%(defclass ~s ~s~%  ~s~@[~%  (:default-initargs~{ ~s~})~]~[~%  (:metaclass ~s)~])~%```~%~%"
	    class-name superclass-names slot-specifiers default-initargs metaclass documentation)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defconstant-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defconstant-components ((name initial-value documentation) source)
    (format stream "Variable: ~a~%~%" name)
    (format stream "```Lisp~%(defconstant ~s ~s)~%```~%~%" name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defgeneric-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defgeneric-components ((function-name gf-lambda-list documentation) source)
    (format stream "Generic function: ~a~%~%" function-name)
    (format stream "```Lisp~%(defgeneric ~s ~s)~%```~%~%" function-name gf-lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-compiler-macro-writer (stream source)
  (adppvt:with-define-compiler-macro-components ((name documentation) source)
    (format stream "Compiler macro: ~a~%~%" name)
    (format stream "```Lisp~%(define-compiler-macro ~s)~%```~%~%" name)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-condition-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-condition-components ((name parent-types slot-specs default-initargs report-name documentation) source)
    (format stream "Condition: ~a~%~%" name)
    (format stream "```Lisp~%(defcondition ~s ~s~%  ~s~@[~%  (:default-initargs~{ ~s~})~]~@[~%  (:report ~s)~])~%```~%~%"
	    name parent-types slot-specs default-initargs report-name)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-method-combination-writer (stream source)
  (adppvt:with-define-method-combination-components ((name documentation) source)
    (format stream "Method combination: ~a~%~%" name)
    (format stream "```Lisp~%(define-method-combination ~s)~%```~%~%" name)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-modify-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-modify-macro-components ((name lambda-list function documentation) source)
    (format stream "Macro: ~a~%~%" name)
    (format stream "```Lisp~%(define-modify-macro ~s ~s ~s)~%```~%~%" name lambda-list function)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-setf-expander-writer (stream source)
  (adppvt:with-define-setf-expander-components ((access-fn lambda-list documentation) source)
    (format stream "Setf expander: ~a~%~%" access-fn)
    (format stream "```Lisp~%(define-setf-expander ~s ~s)~%```~%~%" access-fn lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-define-symbol-macro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-define-symbol-macro-components ((symbol expansion) source)
    (format stream "Symbol macro: ~a~%~%" symbol)
    (format stream "```Lisp~%(define-symbol-macro ~s ~s)~%```~%~%" symbol expansion)))

(adppvt:def-defmacro-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defmacro-components ((name lambda-list documentation) source)
    (format stream "Macro: ~a~%~%" name)
    (format stream "```Lisp~%(defmacro ~s ~s)~%```" name lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defmethod-writer (stream source)
  (adppvt:with-defmethod-components ((function-name method-qualifiers specialized-lambda-list documentation) source)
    (format stream "Method: ~a~%~%" function-name)
    (format stream "```Lisp~%(defmethod ~s~{ ~s~} ~s)~%```~%~%" function-name method-qualifiers specialized-lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defpackage-writer (stream source)
  (adppvt:with-defpackage-components ((defined-package-name nicknames use-package-names documentation) source)
    (format stream "Package: ~a~%~%" defined-package-name)
    (format stream "```Lisp~%(defpackage ~s~@[~%  (:nicknames~{ ~s~})~]~@[~%  (:use~{ ~s~})~])~%```~%~%"
	    defined-package-name nicknames use-package-names)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defparameter-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defparameter-components ((name initial-value documentation) source)
    (format stream "Variable: ~a~%~%" name)
    (format stream "```Lisp~%(defparameter ~s ~s)~%```~%~%"
	    name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defsetf-writer (stream source)
  (adppvt:with-defsetf-components ((access-fn update-fn documentation) source)
    (format stream "Defsetf: ~a~%~%" access-fn)
    (format stream "```Lisp~%(defsetf ~s ~s)" access-fn update-fn)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defstruct-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defstruct-components ((structure-name name-and-options slot-descriptions documentation) source)
    (format stream "Struct: ~a~%~%" structure-name)
    (format stream "```Lisp~%(defstruct ~s~%  ~s)~%```~%~%" name-and-options slot-descriptions)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-deftype-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-deftype-components ((name lambda-list documentation) source)
    (format stream "Type: ~a~%~%" name)
    (format stream "```Lisp~%(deftype ~s ~s)~%```~%~%" name lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defun-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defun-components ((function-name lambda-list documentation) source)
    (format stream "Function: ~a~%~%" function-name)
    (format stream "```Lisp~%(defun ~s ~s)~%```~%~%" function-name lambda-list)
    (when documentation
      (format stream "~a~%~%" documentation))))

(adppvt:def-defvar-writer (stream source tag)
  (declare (ignore tag))
  (adppvt:with-defvar-components ((name initial-value documentation) source)
    (format stream "Variable: ~a~%~%" name)
    (format stream "```Lisp~%(defvar ~s~@[ ~s~])~%```~%~%" name initial-value)
    (when documentation
      (format stream "~a~%~%" documentation))))


;; ----- file functions -----

(adppvt:def-get-file-extension-writer ()
  "md")
