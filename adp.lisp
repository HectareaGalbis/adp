

(in-package :adp)


(cl:defmacro adv-header (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :header ,str ',label)
       (values))))


(cl:defmacro adv-subheader (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',label)
       (values))))


(cl:defmacro adv-defmacro (&body defmacro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body))))
     (cl:defmacro ,@defmacro-body)))

(cl:defmacro adv-defun (&body defun-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defun-body))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (car ',defun-body))))
     (cl:defun ,@defun-body)))


(cl:defmacro adv-write-in-file (file-path)
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-directory file-path) (file-path) "The ~s pathname has not a directory part." file-path)
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (cons :relative (cdr (pathname-directory ,file-path)))
						:name (pathname-name ,file-path))))
	     (adppvt:push-adp-file ,let-file-path)
	     (adppvt:push-file-tag ,let-file-path)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		   finally (adppvt:empty-header-tags))
	     (loop for ,symbol-tag across adppvt:*symbol-tags*
		   do (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		   finally (adppvt:empty-symbol-tags))
	     (loop for ,function-tag across adppvt:*function-tags*
		   do (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		   finally (adppvt:empty-function-tags))
	     (loop for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-type-tags)))
	   (values))))))


;; ----- ADP interface -----

(adv-header "ADP User Interface" interface-header)


;; ----- Guide functions -----

(adv-subheader "Literate programming functions" literate-subheader)

(adv-defmacro header (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :header ,str ',label)
       (values))))


(adv-defmacro subheader (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',label)
       (values))))


(adv-defmacro subsubheader (str &optional label)
  (when adppvt:*add-documentation*
    (check-type str string "a string")
    (check-type label (or null symbol) "a symbol")
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subsubheader ,str ',label)
       (values))))


(adv-defmacro text (&rest objects)
  (when adppvt:*add-documentation*
    `(progn
       (adppvt:emplace-adp-element :text ,@objects)
       (values))))


(adv-defmacro table (&rest rows)
  (when adppvt:*add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(progn
       (adppvt:emplace-adp-element :table ,@(loop for row in rows
						  collect (cons 'list (loop for elem in row
									    collect (cons 'list elem)))))
       (values))))


(adv-defmacro itemize (&rest items)
  (when adppvt:*add-documentation*
    (labels ((check-items (item-list)
	       (loop for item in item-list
		     if (not (eq (car item) :item))
		       do (if (eq (car item) :itemize) 
			      (check-items (cdr item))
			      (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" item)))))
      (check-items items))
    (labels ((process-itemize-items (item-list)
	       (loop for item in item-list
		     if (eq (car item) :item)
		       collect (cons 'list item)
		     else
		       collect (list* 'list :itemize (process-itemize-items (cdr item))))))
      `(progn
	 (adppvt:emplace-adp-element :itemize ,@(process-itemize-items items))
	 (values)))))


(adv-defmacro image (alt-text path)
  (when adppvt:*add-documentation*
    (check-type alt-text string "a string")
    (check-type path pathname "a pathname")
    `(progn
       (adppvt:emplace-adp-element :image ,alt-text ,path)
       (values))))


(adv-defmacro bold (&rest args)
  (when adppvt:*add-documentation*
    `(adppvt:create-bold-text ,@args)))


(adv-defmacro italic (&rest args)
  (when adppvt:*add-documentation*
    `(adppvt:create-italic-text ,@args)))


(adv-defmacro bold-italic (&rest args)
  (when adppvt:*add-documentation*
    `(adppvt:create-bold-italic-text ,@args)))


(adv-defmacro code-inline (&rest code)
  (when adppvt:*add-documentation*
    `(adppvt:create-code-inline-text ,@code)))


(adv-defmacro web-link (name link)
  (when adppvt:*add-documentation*
    (check-type name string "a string")
    (check-type link string "a string")
    `(adppvt:create-web-link-text ,name ,link)))


(adv-defmacro file-ref (path)
  (when adppvt:*add-documentation*
    (check-type path pathname "a pathname")
    `(adppvt:create-file-ref-text ,path)))


(adv-defmacro header-ref (label)
  (when adppvt:*add-documentation*
    (check-type label symbol "a symbol")
    `(adppvt:create-header-ref-text ',label)))


(adv-defmacro symbol-ref (label)
  (when adppvt:*add-documentation*
    (check-type label symbol "a symbol")
    `(adppvt:create-symbol-ref-text ',label)))


(adv-defmacro function-ref (label)
  (when adppvt:*add-documentation*
    (check-type label symbol "a symbol")
    `(adppvt:create-function-ref-text ',label)))


(adv-defmacro type-ref (label)
  (when adppvt:*add-documentation*
    (check-type label symbol "a symbol")
    `(adppvt:create-type-ref-text ',label)))


(adv-defmacro code-tag (tags &body code)
  (with-gensyms (tag)
    `(progn
       ,@(when adppvt:*add-documentation*
	   (check-type tags list "a list")
	   (loop for tag in tags
		 do (check-type tag symbol "a symbol"))
	   `((loop for ,tag in ',tags
		   do (apply #'adppvt:add-code-tag ,tag ',code))))
       ,@(adppvt:remove-own-code-hide-exprs code))))


(adv-defmacro code-block (tags &body code)
  (when adppvt:*add-documentation*
    (check-type tags list "a list")
    (loop for tag in tags
	  do (check-type tag symbol "a symbol"))
    (with-gensyms (expr)
      `(progn
	 (adppvt:emplace-adp-element :code-block (loop for ,expr in ',code
						     if (and (symbolp ,expr)
							     (member ,expr ',tags))
						       append (adppvt:process-code-tag ,expr (coerce (adppvt:get-code-tag ,expr) 'list))
						     else
						       collect (adppvt:process-code-tag '#:dummy-tag ,expr)))
	 (values)))))


(adv-defmacro code-example (&body code)
  (when adppvt:*add-documentation*
    (let ((evaluated-code (loop for expr in code
				collect (with-gensyms (output result)
					  `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
										  ,(adppvt:remove-code-tag-exprs expr)))))
					     (list ',(adppvt:process-code-tag '#:dummy-tag expr)
						   ,output
						   ,result))))))
      `(progn
	 (adppvt:emplace-adp-element :code-example (list ,@evaluated-code))
	 (values)))))


;; ----- API functions -----

(adv-subheader "API documentation functions" api-subheader)

(adv-defmacro defclass (&body defclass-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defclass-body))
	   (adppvt:emplace-adp-element :defclass '(cl:defclass ,@defclass-body) (car ',defclass-body))))
     (cl:defclass ,@defclass-body)))


(adv-defmacro defconstant (&body defconstant-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defconstant-body))
	   (adppvt:emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body) (car ',defconstant-body))))
     (cl:defconstant ,@defconstant-body)))


(adv-defmacro defgeneric (&body defgeneric-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defgeneric-body))
	   (adppvt:emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body) (car ',defgeneric-body))))
     (cl:defgeneric ,@defgeneric-body)))


(adv-defmacro define-compiler-macro (&body define-compiler-macro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))))
     (cl:define-compiler-macro ,@define-compiler-macro-body)))


(adv-defmacro define-condition (&body define-condition-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',define-condition-body))
	   (adppvt:emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body) (car ',define-condition-body))))
     (cl:define-condition ,@define-condition-body)))


(adv-defmacro define-method-combination (&body define-method-combination-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))))
     (cl:define-method-combination ,@define-method-combination-body)))


(adv-defmacro define-modify-macro (&body define-modify-macro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',define-modify-macro-body))
	   (adppvt:emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body) (car ',define-modify-macro-body))))
     (cl:define-modify-macro ,@define-modify-macro-body)))


(adv-defmacro define-setf-expander (&body define-setf-expander-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))))
     (cl:define-setf-expander ,@define-setf-expander-body)))


(adv-defmacro define-symbol-macro (&body define-symbol-macro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',define-symbol-macro-body))
	   (adppvt:emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body) (car ',define-symbol-macro-body))))
     (cl:define-symbol-macro ,@define-symbol-macro-body)))


(adv-defmacro defmacro (&body defmacro-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body) (car ',defmacro-body))))
     (cl:defmacro ,@defmacro-body)))


(adv-defmacro defmethod (&body defmethod-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))))
     (cl:defmethod ,@defmethod-body)))


(adv-defmacro defpackage (&body defpackage-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))))
     (cl:defpackage ,@defpackage-body)))


(adv-defmacro defparameter (&body defparameter-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defparameter-body))
	   (adppvt:emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body) (car ',defparameter-body))))
     (cl:defparameter ,@defparameter-body)))


(adv-defmacro defsetf (&body defsetf-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))))
     (cl:defsetf ,@defsetf-body)))


(adv-defmacro defstruct (&body defstruct-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defstruct-body))
	   (adppvt:emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body) (car ',defstruct-body))))
     (cl:defstruct ,@defstruct-body)))


(adv-defmacro deftype (&body deftype-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',deftype-body))
	   (adppvt:emplace-adp-element :deftype '(cl:deftype ,@deftype-body) (car ',deftype-body))))
     (cl:deftype ,@deftype-body)))


(adv-defmacro defun (&body defun-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((when (symbolp (car ',defun-body))
	     (adppvt:push-function-tag (car ',defun-body)))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body) (if (symbolp (car ',defun-body))
									   (car ',defun-body)
									   nil))))
     (cl:defun ,@defun-body)))


(adv-defmacro defvar (&body defvar-body)
  `(progn
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defvar-body))
	   (adppvt:emplace-adp-element :defvar '(cl:defvar ,@defvar-body) (car ',defvar-body))))
     (cl:defvar ,@defvar-body)))


;; ----- Writer functions -----

(adv-subheader "Documentation writer function" writer-subheader)


(adv-defmacro write-in-file (file-path)
  (when adppvt:*add-documentation*
    (check-type file-path pathname "a pathname")
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (if (pathname-directory ,file-path)
							       (cons :relative (cdr (pathname-directory ,file-path)))
							       nil)
						:name (pathname-name ,file-path))))
	     (adppvt:push-adp-file ,let-file-path)
	     (adppvt:push-file-tag ,let-file-path)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		   finally (adppvt:empty-header-tags))
	     (loop for ,symbol-tag across adppvt:*symbol-tags*
		   do (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		   finally (adppvt:empty-symbol-tags))
	     (loop for ,function-tag across adppvt:*function-tags*
		   do (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		   finally (adppvt:empty-function-tags))
	     (loop for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-type-tags)))
	   (values))))))


(declaim (ftype (function (t symbol &rest t) t) load-documentation-system))
(adv-defun load-documentation-system (system style &rest style-args)
  (assert (asdf:find-system system) (system) "The system ~s was not found." system)
  (adppvt:remove-current-procs)
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (assert (asdf:find-system style-system) (style-system) "The style ~s was not found." style-system)
    (asdf:load-system style-system :force t))
  (adppvt:check-current-procs)
  (adppvt:check-style-parameters style-args)
  (adppvt:remove-current-data)
  (let ((adppvt:*add-documentation* t))
    (asdf:load-system system :force t))
  (loop for (name value) in style-args by #'cddr
	do (adppvt:set-parameter-value name value))
  (let* ((root-path (asdf:system-source-directory system))
	 (fixed-root-path (make-pathname :host (pathname-host root-path)
					 :device (pathname-device root-path)
					 :directory (pathname-directory root-path))))
    (adppvt:write-system-files fixed-root-path)))


;; ----- Additional functions -----

(adv-subheader "Additional functions")

(adv-defmacro cl-ref (sym)
  `(web-link ,(prin1-to-string sym) ,(hyperspec:lookup sym)))

(adv-write-in-file #P"docs/user-api")
