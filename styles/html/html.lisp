

(in-package :adp/html)



(adppvt:def-header-writer (stream text tag)
  (format stream "<h1>~a</h1>~%~%" text))

(adppvt:def-subheader-writer (stream text tag)
  )

(adppvt:def-subsubheader-writer (stream text tag)
  )

(adppvt:def-text-writer (stream text)
  )

(adppvt:def-table-writer (stream table)
  )

(adppvt:def-itemize-writer (stream items)
  )

(adppvt:def-image-writer (stream alt-text root-path rel-image-path)
  )

(adppvt:def-bold-writer (stream text)
  (format stream "<strong>~a</strong>"))

(adppvt:def-italic-writer (stream text)
  )

(adppvt:def-bold-italic-writer (stream text)
  )

(adppvt:def-code-inline-writer (stream code)
  )

(adppvt:def-web-link-writer (stream name link)
  )

(adppvt:def-file-ref-writer (stream root-path file-path)
  )

(adppvt:def-header-ref-writer (stream tag header-text root-path file-path)
  )









(adppvt:def-symbol-ref-writer (stream tag root-path file-path)
  )

(adppvt:def-function-ref-writer (stream tag root-path file-path)
  )

(adppvt:def-type-ref-writer (stream tag root-path file-path)
  )

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
  )

(adppvt:def-code-example-writer (stream code-list)
  )


;; ----- api functions -----

(adppvt:def-defclass-writer (stream source)
  )


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
  "html")

(adppvt:def-file-header-writer (stream)
  )

(adppvt:def-file-foot-writer (stream)
  )

(adppvt:def-system-files-writer (root-path)
  )
