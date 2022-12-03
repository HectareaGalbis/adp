
(in-package :adp)



(defmacro special-let (let-bindings &body body)
  (let ((vars (mapcar (lambda (binding)
			(if (symbolp binding)
			    binding
			    (car binding)))
		      let-bindings)))
    `(let ,let-bindings
       ,@(when vars
	   `((declare (special ,@vars))))
       ,@body)))

(defmacro with-special-vars ((var &rest vars) &body body)
  `(locally (declare (special ,var ,@vars))
     ,@body))



(cl:defmacro in-file (path)
  (with-special-vars (*adp*)
    (when *adp*
      (check-type path pathname)
      (let* ((true-path (truename path))
	     (fixed-true-path (make-pathname :directory (cons :relative (cdr (pathname-directory true-path)))
					     :name (pathname-name path)
					     :type (pathname-type path))))
	`(with-special-vars (*project*)
	   (project-select-file *project* fixed-true-path))))))


(cl:defmacro define-header-macro (name)
  (let ((macro-doc (format nil "Add a ~a with name str. Also, if tag is not nil but a symbol, a new header-tag is created."
			   (string-downcase (symbol-name name)))))
    (with-gensyms (str tag fixed-tag current-file)
      `(cl:defmacro ,name (,str &option ,tag)
	 ,macro-doc
	 (with-special-vars (*adp*)
	   (when *adp*
	     (check-type ,str string "a string")
	     (check-type ,tag (or null symbol) "a symbol or nil")
	     (let ((,fixed-tag (or ,tag (gensym))))
	       `(with-special-vars (*project*)
		  (adppvt:project-add-element *project* (make-instance ',',name :tag ,,fixed-tag))
		  (values)))))))))

(define-header-macro header)
(define-header-macro subheader)
(define-header-macro subsubheader)


(cl:defmacro text (&rest objects)
  "Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, function-ref and type-ref."
  (with-special-vars (*adp*)
    (when *adp*
      `(with-special-vars (*project*)
	 (adppvt:project-add-element *project* (make-instance 'text :text-elements (list ,@objects)))
	 (values)))))


(cl:defmacro table (&rest rows)
  "Add a table. Each argument must be a list of text macro calls."
  (with-special-vars (*adp*)
    (when *adp*
      (loop with row-length = (length (car rows))
	    for row in rows
	    do (check-type row list "a list")
	       (assert (= row-length (length row)) () "Each row must have the same length.")
	       (loop for elem in row
		     do (assert (eq (car elem) 'text) () "Each cell of a table must be a text macro call. Found: ~s" elem)))
      `(with-special-vars (*project*)
	 (adppvt:project-add-element *project* (make-instance 'table :rows ,@(loop for row in rows
										   collect (cons 'list row))))
	 (values)))))



(eval-when (:compile-toplevel :eval-toplevel :execute)
  
  (cl:defun check-itemize (itemize-form)
    (assert (not (null (cdr itemize-form))) () "Expected at least one expression in a itemize/enumerate form.")
    (assert (and (listp (cadr itemize-form)) (not (member (caadr itemize-form) '(itemize enumerate)))) ()
	    "The first element of itemize/enumerate must be a text macro call.")
    (loop for item in (cddr itemize-form)
	  if (not (and (listp item)
		       (member (car item) '(text itemize enumerate))))
	    do (error "Each item of itemize/enumerate must be a text, itemize or enumerate macro call.")
	  if (member (car item) '(itemize enumerate))
	    do (check-itemize item)))

  (cl:defun process-itemize (itemize-form)
    (case (car itemize-form)
      (text      `(make-instance 'item :text (make-instance 'text :text-elements (list ,@(cdr itemize-form)))))
      (itemize   `(make-instance 'itemize :type :itemize
					  :elements (list ,@(mapcar #'process-itemize (cdr itemize-form)))))
      (enumerate `(make-instance 'itemize :type :enumerate
					  :elements (list ,@(mapcar #'process-itemize (cdr itemize-form))))))))


(cl:defmacro itemize (&whole itemize-form &rest items)
  "Add a list of items. Each argument must be a list. Each list must start with the symbol item, itemize or enumerate. If 
item is used, the rest of the elements in that list will be treated as if using the macro text. If itemize or enumerate is used the rest 
of elements must be lists that must start with item, itemize or enumerate. In other words, when itemize or enumerate is used 
a nested list is added. A certain symbol will be printed before each element of the list."
  (with-special-vars (*adp*)
    (when *adp*
      (check-itemize-forms items)
      `(with-special-vars (*project*)
	 (adppvt:project-add-element *project* ,(process-itemize itemize-form))
	 (values)))))


(cl:defmacro enumerate (&whole enumerate-form &rest items)
  "Same as itemize, but a number is printed before each element."
  (with-special-vars (*adp*)
    (when *adp*
      (check-itemize-forms items)
      `(with-special-vars (*project*)
	 (adppvt:project-add-element *project* ,(process-itemize enumerate-form))
	 (values)))))


(cl:defmacro image (alt-text path)
  "Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located."
  (with-special-vars (*adp*)
    (when *adp*
      (check-type alt-text string "a string")
      (check-type path pathname "a pathname")
      `(with-special-vars (*project*)
	 (adppvt:project-add-element *project* (make-instance 'image :path ,path))
	 (values)))))


(cl:defmacro define-text-enrichment-macro (name docstring)
  (with-gensyms (args)
    `(cl:defmacro ,name (&rest ,args)
       ,docstring
       (with-special-vars (*adp*)
	 (when *adp*
	   `(with-special-vars (*project*)
	      (make-instance ',',name :text (make-instance 'text :text-elements (list ,@,args)))))))))

(define-text-enrichment-macro bold
  "Add bold style to text when using the text macro. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro italic
  "Add italic style to text when using the text macro. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro bold-italic
  "Add bold and italic style to text when using the text macro. Each argument is princ-ed and concatenated into a string.")
(define-text-enrichment-macro code-inline
  "Add inlined style to text when using the text macro. Each argument is princ-ed and concatenated into a string.")


(cl:defmacro web-link (name link)
  "Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings."
  (with-special-vars (*adp*)
    (when *adp*
      (check-type name string "a string")
      (check-type link string "a string")
      `(with-special-vars (*project*)
	 (make-instance 'web-link :text ,name :address ,link)))))


(defmacro define-reference-macro (name tag-table docstring)
  (with-gensyms (tag header-tags)
    (let ((tag-table-key (intern (symbol-name name) "KEYWORD")))
      `(cl:defmacro ,name (,tag)
	 ,docstring
	 (with-special-vars (*adp*)
	   (when *adp*
	     (check-type ,tag symbol "a symbol")
	     `(with-special-vars (*project*)
		(with-slots ((,header-tags header-tags)) *project*
		  (make-instance ',name :tag ,tag
				 ,tag-table-key ,header-tags)))))))))

(define-reference-macro header-ref header-tags
  "Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.")

(define-reference-macro symbol-ref symbol-tags
  "Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.")

(define-reference-macro function-ref function-tags
  "Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.")

(define-reference-macro type-ref type-tags
  "Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.")


(cl:defmacro code-tag (tags &body exprs)
  "Assign several tags to several forms. The forms are placed into a progn form. The argument tags must be a list
of symbols. If no tags are provided, an error is raised. Each symbol in tags will be a code-tag assigned to code.
The same tag can be used several times in different calls to code-tag.  Inside the code-tag form it is correct to use
the next forms: code-hide, code-remove, code-show and code-comment. 
  - code-hide: It has the syntax (code-hide (&rest tags) &rest forms). code-hide receives a list of tags. If a tag 
               used in code-tag also appears in code-hide, the rest of forms will be hidden when using the macro code-block. 
               If the list of tags in code-hide is empty, the forms will be hidden for every tag used in code-tag.
               Hidding the code means printing \"...\" instead of the forms.
  - code-remove: Same as code-hide, but removes the code instead of printing \"...\"
  - code-quote: It has the same syntax as code-hide and code-remove. The forms in code-quote are never evaluated, but will be
                shown when using code-block if some tag from code-tag is used in code-quote. Otherwise it won't print anything.
  - code-comment: It has the systax (code-comment (&rest tags) comment). The comment will be printed when using code-block as
                  a comment using \";;\" if some tag from code-tag is used in code-comment. Otherwise, it won't print anything."
  (with-special-vars (*adp*)
    `(progn
       ,@(when *adp*
	   (check-type tags list "a list")
	   (assert (not (null tags)) (tags) "The list of tags must have at least one symbol.")
	   (loop for tag in tags
		 do (check-type tag symbol "a symbol"))
	   (with-gensyms (tag expr)
	     (with-special-vars (*project*)
	       `((loop for ,tag in ',tags
		       do (loop for ,expr in ',exprs
				do (apply #'adppvt:project-add-code-tag *project*
					  ,tag (make-instance 'tagged-code :tag tag :expr ,expr))))))))
       ,@(adppvt:expr-remove-own-tag-exprs exprs))))


(cl:defmacro code-block ((&rest tags) &body code)
  "Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is printed instead of the symbol."
  (with-special-vars (*adp*)
    (when *adp*
      (check-type tags list "a list")
      (loop for tag in tags
	    do (check-type tag symbol "a symbol")
	       (assert (member tag code) () "The tag ~s is not present in code-block code." tag))
      (assert (not (null code)) () "Expected at least one expression in a code-block form.")
      (with-gensyms (expr)
	`(with-special-vars (*project*)
	   (adppvt:project-add-element *project* (make-instance 'code-block
								:code-elements (loop for ,expr in ',code
										     if (and (symbolp ,expr)
											     (member ,expr ',tags))
										       collect (make-instance 'code-reference :code-tag ,expr)
										     else
										       collect ,expr)))
	   (values))))))


(cl:defmacro verbatim-code-block (lang-or-text &optional (text nil textp))
  "Add a block of text. It can receive up to two arguments. If only one argument is received it must be a string of text that will be printed inside the block.
If two arguments are received, the text to be printed must be the second one, and the first argument is a string representing the language used for writing
the text."
  (with-special-vars (*adp*)
    (when *adp*
      (check-type lang-or-text string)
      (check-type textp (or null string) "a string or NIL")
      (let ((true-lang (and textp lang-or-text))
	    (true-text (if textp text lang-or-text)))
	`(with-special-vars (*project*)
	   (adppvt:project-add-element  (make-instance 'verbatim-code-element :code-type true-lang
									      :code-text true-text))
	   (values))))))
