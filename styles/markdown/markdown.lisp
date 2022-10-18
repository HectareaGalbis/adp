
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

(adppvt:def-code-inline-writer (stream code)
  (let ((*print-pretty* nil))
    (format stream "`~s`" code)))

(adppvt:def-web-link-writer (stream name link)
  (format stream "[~a](~a)" name link))

(adppvt:def-header-ref-writer (stream tag header-text root-path file-path)
  (declare (ignore tag root-path file-path))
  (format stream "***~a***" header-text))

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
  (loop for (code output result) in code-list
	do (terpri stream)
	   (prin1-with-hide-string stream code "...")
	   (format stream "~%~a~{~s~^~%~}~%" output result))
  (format stream "```~%~%"))


;; ----- api functions -----

(defmacro def-general-writer (definition title name documentation)
  (let ((writer-name (find-symbol (concatenate 'string "DEF-" (symbol-name definition) "-WRITER") :adppvt))
	(with-components-name (find-symbol (concatenate 'string "WITH-" (symbol-name definition) "-COMPONENTS") :adppvt)))
    (with-gensyms (stream source)
      `(,writer-name (,stream ,source)
		     (,with-components-name ((,name ,@(when documentation `(,documentation))) ,source)
		       (format ,stream "#### ***~a*** ~s~%~%" ,title ,name)
		       ,@(when documentation
			   `((when ,documentation
			       (format ,stream "~a~%~%" ,documentation))))
		       (format ,stream "```Lisp~%~s~%```~%~%" ,source))))))

(def-general-writer defclass "Class" class-name documentation)
(def-general-writer defconstant "Constant" name documentation)
(def-general-writer defgeneric "Generic function" function-name documentation)
(def-general-writer define-compiler-macro "Compiler macro" name documentation)
(def-general-writer define-condition "Condition" name documentation)
(def-general-writer define-method-combination "Method combination" name documentation)
(def-general-writer define-modify-macro "Modify macro" name documentation)
(def-general-writer define-setf-expander "Setf expander" access-fn documentation)
(def-general-writer define-symbol-macro "Symbol macro" symbol nil)
(def-general-writer defmacro "Macro" name documentation)
(def-general-writer defmethod "Method" function-name documentation)
(def-general-writer defpackage "Package" defined-package-name documentation)
(def-general-writer defparameter "Parameter" name documentation)
(def-general-writer defsetf "Setf" access-fn documentation)
(def-general-writer defstruct "Structure" structure-name documentation)
(def-general-writer deftype "Type" name documentation)
(def-general-writer defun "Function" function-name documentation)
(def-general-writer defvar "Var" name documentation)


;; ----- file functions -----

(adppvt:def-get-file-extension-writer ()
  "md")
