
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
  (let ((punctuation-chars '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~))
	(fixed-text (make-array (length text) :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for char across text
	  when (member char punctuation-chars :test #'char=)
	    do (vector-push-extend #\\ fixed-text)
	  do (vector-push-extend char fixed-text))
    (values fixed-text)))

(defun escape-html-characters (text)
  (let ((punctuation-chars '((#\< . "&#60;")
			     (#\> . "&#62;")))
	(fixed-text (make-array (length text) :adjustable t :fill-pointer 0 :element-type 'character)))
    (with-output-to-string (stream fixed-text)
      (loop for char across text
	    for code = (cdr (assoc char punctuation-chars))
	    if code
	      do (princ code stream)
	    else
	      do (princ char stream)))    
    (values fixed-text)))

(defun symbol-macro-p (sym &optional env)
  (let ((*macroexpand-hook* (constantly nil)))
    (nth-value 1 (macroexpand-1 sym env))))


(defun get-symbol-id (sym type)
  (let ((type-str (ecase type
		    (:header "header")
		    (:symbol "symbol")
		    (:function "function")
		    (:type "type"))))
    (format nil "~a:~a:~a" type-str (package-name (symbol-package sym)) (symbol-name sym))))



;; ----- guide functions -----

(adpsm:define-header-writer (stream text tag)
  (format stream "<h1 id=~s>~a</h1>~%~%" (get-symbol-id tag :header) (escape-html-characters text)))

(adpsm:define-subheader-writer (stream text tag)
  (format stream "<h2 id=~s>~a</h2>~%~%" (get-symbol-id tag :header) (escape-html-characters text)))

(adpsm:define-subsubheader-writer (stream text tag)
  (format stream "<h3 id=~s>~a</h3>~%~%" (get-symbol-id tag :header) (escape-html-characters text)))

(adpsm:define-escape-text (text)
  (escape-characters text))

(adpsm:define-text-writer (stream text)
  (format stream "~a~%~%" text))

(adpsm:define-table-writer (stream table)
  (format stream "~{| ~a ~}|~%" (car table))
  (format stream "~v@{| --- ~}|~%" (length (car table)) nil)
  (format stream "~{~{| ~a ~}|~%~}~%~%" (cdr table)))

(adpsm:define-itemize-writer (stream items)
  (labels ((digits (n)
	     (if (< n 10)
		 1
		 (1+ (digits (truncate n 10)))))
	   (itemize-aux (item-list numbersp indent-space)
	     (loop for item in item-list
		   for index = 0 then (if (eq (car item) :item) (1+ index) index)
		   do (case (car item)
			(:item (if numbersp
				   (format stream "~v@{ ~}~s. ~a~%" indent-space (1+ index) (cadr item))
				   (format stream "~v@{ ~}* ~a~%" indent-space (cadr item))))
			((:itemize :enumerate) (itemize-aux (cdr item) (eq (car item) :enumerate) (if numbersp
												      (+ indent-space (digits index) 2)
												      (+ indent-space 2))))))))
    (itemize-aux (cdr items) (eq (car items) :enumerate) 0))
  (terpri stream))

(adpsm:define-image-writer (stream alt-text rel-image-path)
  (format stream "![~a](/~a)~%~%" (escape-characters alt-text) rel-image-path))

(adpsm:define-bold-writer (stream text)
  (format stream "<strong>~a</strong>" (escape-characters text)))

(adpsm:define-italic-writer (stream text)
  (format stream "<em>~a</em>" (escape-characters text)))

(adpsm:define-emphasis-writer (stream text)
  (format stream "<strong><em>~a</em></strong>" (escape-characters text)))

(adpsm:define-inline-code-writer (stream text)
  (let ((*print-pretty* nil))
    (format stream "``` ~a ```" text)))

(adpsm:define-web-link-writer (stream name link)
  (format stream "[~a](~a)" (escape-characters name) link))

(adpsm:define-header-ref-writer (stream tag header-text file-path)
  (format stream "<a href=\"/~a.md#~a\">~a</a>" file-path (get-symbol-id tag :header) (escape-html-characters header-text)))

(adpsm:define-symbol-ref-writer (stream tag file-path)
  (format stream "<a href=\"/~a.md#~a\">~a</a>" file-path (get-symbol-id tag :symbol) (escape-html-characters (prin1-to-string tag))))

(adpsm:define-function-ref-writer (stream tag file-path)
  (format stream "<a href=\"/~a.md#~a\">~a</a>" file-path (get-symbol-id tag :function) (escape-html-characters (prin1-to-string tag))))

(adpsm:define-type-ref-writer (stream tag file-path)
  (format stream "<a href=\"/~a.md#~a\">~a</a>" file-path (get-symbol-id tag :type) (escape-html-characters (prin1-to-string tag))))

(adpsm:define-code-block-writer (stream lang text-code)
  (format stream "`````~@[~a~]~%~a~%`````~%~%" lang text-code))

(adpsm:define-code-example-writer (stream text-code output results)
  (format stream "```Lisp~%~a~%~a~{~%~s~}~%```~%~%" text-code output results))


;; ----- api functions -----

(adpsm:define-defclass-writer (stream source tag)
  (adpsm:with-defclass-components ((class-name superclass-names slot-specifiers documentation default-initargs metaclass) source)
    (format stream "<h4 id=~s>Class: ~a</h4>~%~%"
	    (get-symbol-id tag :type) (escape-html-characters (princ-to-string class-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defclass ~s ~s~%  ~s~@[~%  (:default-initargs~{ ~s~})~]~[~%  (:metaclass ~s)~])~%```~%~%"
	      class-name superclass-names slot-specifiers default-initargs metaclass documentation))
    (when documentation
      (format stream "````~%~a~%````~%~%" (escape-characters documentation)))))

(adpsm:define-defconstant-writer (stream source tag)
  (adpsm:with-defconstant-components ((name initial-value documentation) source)
    (format stream "<h4 id=~s>Constant: ~a</h4>~%~%"
	    (get-symbol-id tag :symbol) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defconstant ~s ~s)~%```~%~%" name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defgeneric-writer (stream source tag)
  (adpsm:with-defgeneric-components ((function-name gf-lambda-list documentation) source)
    (format stream "<h4 id=~s>Generic function: ~a</h4>~%~%"
	    (get-symbol-id tag :function) (escape-html-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defgeneric ~s ~s~%  ...)~%```~%~%" function-name gf-lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-compiler-macro-writer (stream source)
  (adpsm:with-define-compiler-macro-components ((name documentation) source)
    (format stream "<h4>Compiler macro: ~a</h4>~%~%" (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(define-compiler-macro ~s~%  ...)~%```~%~%" name))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-condition-writer (stream source tag)
  (adpsm:with-define-condition-components ((name parent-types slot-specs documentation) source)
    (format stream "<h4 id=~s>Condition: ~a</h4>~%~%"
	    (get-symbol-id tag :type) (escape-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defcondition ~s ~s~%  ~s~%```~%~%"
	      name parent-types slot-specs))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-method-combination-writer (stream source)
  (adpsm:with-define-method-combination-components ((name documentation) source)
    (format stream "<h4>Method combination: ~a</h4>~%~%" (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(define-method-combination ~s~%  ...)~%```~%~%" name))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-modify-macro-writer (stream source tag)
  (adpsm:with-define-modify-macro-components ((name lambda-list function documentation) source)
    (format stream "<h4 id=~s>Modify macro: ~a</h4>~%~%"
	    (get-symbol-id tag :function) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(define-modify-macro ~s ~s ~s)~%```~%~%"
	      name lambda-list function))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-setf-expander-writer (stream source)
  (adpsm:with-define-setf-expander-components ((access-fn lambda-list documentation) source)
    (format stream "<h4>Setf expander: ~a</h4>~%~%" (escape-html-characters (princ-to-string access-fn)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(define-setf-expander ~s ~s~%  ...)~%```~%~%" access-fn lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-define-symbol-macro-writer (stream source tag)
  (adpsm:with-define-symbol-macro-components ((symbol expansion) source)
    (format stream "<h4 id=~s>Symbol macro: ~a</h4>~%~%"
	    (get-symbol-id tag :symbol) (escape-html-characters (princ-to-string symbol)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(define-symbol-macro ~s ~s)~%```~%~%" symbol expansion))))

(adpsm:define-defmacro-writer (stream source tag)
  (adpsm:with-defmacro-components ((name lambda-list documentation) source)
    (format stream "<h4 id=~s>Macro: ~a</h4>~%~%"
	    (get-symbol-id tag :function) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defmacro ~s ~s~%  ...)~%```~%~%" name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defmethod-writer (stream source)
  (adpsm:with-defmethod-components ((function-name method-qualifiers specialized-lambda-list documentation) source)
    (format stream "<h4>Method: ~a</h4>~%~%" (escape-html-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defmethod ~s~{ ~s~} ~s~%  ...)~%```~%~%" function-name method-qualifiers specialized-lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defpackage-writer (stream source)
  (adpsm:with-defpackage-components ((defined-package-name nicknames use-package-names documentation) source)
    (format stream "<h4>Package: ~a</h4>~%~%" (escape-html-characters (princ-to-string defined-package-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defpackage ~s~@[~%  (:nicknames~{ ~s~})~]~@[~%  (:use~{ ~s~})~]~%  ...)~%```~%~%"
	      defined-package-name nicknames use-package-names))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defparameter-writer (stream source tag)
  (adpsm:with-defparameter-components ((name initial-value documentation) source)
    (format stream "<h4 id=~s>Parameter: ~a</h4>~%~%"
	    (get-symbol-id tag :symbol) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defparameter ~s ~s)~%```~%~%"
	      name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defsetf-writer (stream source)
  (adpsm:with-defsetf-components ((access-fn update-fn documentation) source)
    (format stream "<h4>Defsetf: ~a</h4>~%~%" (escape-characters (princ-to-string access-fn)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defsetf ~s ~s)" access-fn update-fn))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defstruct-writer (stream source tag)
  (adpsm:with-defstruct-components ((structure-name name-and-options slot-descriptions documentation) source)
    (format stream "<h4 id=~s>Struct: ~a</h4>~%~%"
	    (get-symbol-id tag :type) (escape-html-characters (princ-to-string structure-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defstruct ~s~%  ~s)~%```~%~%" name-and-options slot-descriptions))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-deftype-writer (stream source tag)
  (adpsm:with-deftype-components ((name lambda-list documentation) source)
    (format stream "<h4 id=~s>Type: ~a</h4>~%~%"
	    (get-symbol-id tag :type) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(deftype ~s ~s~%  ...)~%```~%~%" name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defun-writer (stream source tag)
  (adpsm:with-defun-components ((function-name lambda-list documentation) source)
    (format stream "<h4 id=~s>Function: ~a</h4>~%~%"
	    (get-symbol-id tag :function) (escape-html-characters (princ-to-string function-name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defun ~s ~s~%  ...)~%```~%~%" function-name lambda-list))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))

(adpsm:define-defvar-writer (stream source tag)
  (adpsm:with-defvar-components ((name initial-value documentation) source)
    (format stream "<h4 id=~s>Variable: ~a</h4>~%~%"
	    (get-symbol-id tag :symbol) (escape-html-characters (princ-to-string name)))
    (let ((*print-pprint-dispatch* adpsm:*adp-pprint-dispatch*))
      (format stream "```Lisp~%(defvar ~s~@[ ~s~])~%```~%~%" name initial-value))
    (when documentation
      (format stream "````~%~a~%````~%~%" documentation))))


;; ----- file functions -----

(adpsm:define-file-extension ()
  "md")
