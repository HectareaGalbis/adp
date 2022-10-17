

(in-package :adp)


(cl:defmacro adv-header (str &optional label)
  (when adppvt:*add-documentation*
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :header ,str ',label))))


(cl:defmacro adv-subheader (str &optional label)
  (when adppvt:*add-documentation*
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',label))))


(cl:defmacro adv-defmacro (&body defmacro-body)
  `(progn
     (cl:defmacro ,@defmacro-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body))))))

(cl:defmacro adv-defun (&body defun-body)
  `(progn
     (cl:defun ,@defun-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defun-body))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body))))))


(cl:defmacro adv-write-in-file (file-path)
  (when adppvt:*add-documentation*
    (assert (pathname-directory file-path) (file-path) "The ~s pathname has not a directory part." file-path)
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (cons :relative (cdr (pathname-directory ,file-path)))
						:name (pathname-name ,file-path))))
	     (adppvt:emplace-adp-file ,let-file-path (copy-array adppvt:*file-adp-elements*))
	     (adppvt:empty-adp-elements)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   for ,symbol-tag across adppvt:*symbol-tags*
		   for ,function-tag across adppvt:*function-tags*
		   for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		      (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		      (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		      (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-header-tags)
			   (adppvt:empty-symbol-tags)
			   (adppvt:empty-function-tags)
			   (adppvt:empty-type-tags))))))))


;; ----- ADP interface -----

(adv-header "ADP User Interface" interface-header)


;; ----- Guide functions -----

(adv-subheader "Literate programming functions" literate-subheader)

(adv-defmacro header (str &optional label)
  (when adppvt:*add-documentation*
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :header ,str ',label))))


(adv-defmacro subheader (str &optional label)
  (when adppvt:*add-documentation*
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subheader ,str ',label))))


(adv-defmacro subsubheader (str &optional label)
  (when adppvt:*add-documentation*
    `(progn
       ,@(when label
	   `((adppvt:push-header-tag ',label ,str)))
       (adppvt:emplace-adp-element :subsubheader ,str ',label))))


(adv-defmacro text (&rest objects)
  (when adppvt:*add-documentation*
    `(adppvt:emplace-adp-element :text ,@objects)))


(adv-defmacro table (&rest rows)
  (when adppvt:*add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(adppvt:emplace-adp-element :table (list ,(loop for row in rows
						     collect `(list ,(loop for elem in row
									   collect (cons 'list elem))))))))


(adv-defmacro itemize (&rest items)
  (when adppvt:*add-documentation*
    (labels ((check-items (item-list)
	       (loop for item in item-list
		     if (not (eq (car item) :item))
		       do (if (eq (car item) :itemize) 
			      (check-items (cdr item))
			      (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" item)))))
      (check-items items))
    `(adppvt:emplace-adp-element :itemize ,@items)))


(adv-defmacro image (alt-text path)
  (when adppvt:*add-documentation*
    (with-gensyms (let-alt-text let-path)
      `(let ((,let-alt-text ,alt-text)
	     (,let-path ,path))
	 (declare (type string ,let-alt-text)
		  (type pathname ,let-path))
	 (adppvt:emplace-adp-element :image ,let-alt-text ,let-path)))))


(adv-defmacro bold (&rest args)
  (when adppvt:*add-documentation*
    `(create-bold-text ,@args)))


(adv-defmacro italic (&rest args)
  (when adppvt:*add-documentation*
    `(create-italic-text ,@args)))


(adv-defmacro code-inline (code)
  (when adppvt:*add-documentation*
    `(create-code-inline-text ,code)))


(adv-defmacro web-link (name link)
  (when adppvt:*add-documentation*
    (with-gensyms (let-name let-link stream)
      `(let ((,let-name ,name)
	     (,let-link ,link))
	 (declare (type string ,let-name ,let-link))
	 (with-output-to-string (,stream)
	   (funcall *web-link-proc* ,stream ,let-name ,let-link))))))


(adv-defmacro header-ref (label)
  (when adppvt:*add-documentation*
    `(create-header-ref-text ,label)))


(adv-defmacro symbol-ref (label)
  (when adppvt:*add-documentation*
    `(create-symbol-ref-text ,label)))


(adv-defmacro function-ref (label)
  (when adppvt:*add-documentation*
    `(create-function-ref-text ,label)))


(adv-defmacro type-ref (label)
  (when adppvt:*add-documentation*
    `(create-type-ref-text ,label)))


;; (adv-defmacro code-focus (tags &rest code)
;;   `(progn ,@code))


(adv-defmacro code-tag (tags &body code)
  (with-gensyms (tag)
    `(progn
       ,@(loop for expr in code
	       collect (adppvt:remove-own-code-focus-exprs expr))
       ,@(when adppvt:*add-documentation*
	   (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol nor a list" tags)
	   (when (listp tags)
	     (assert (every #'symbolp tags) (tags) "Thare is a non-symbol in ~s" tags))
	   `((loop for ,tag in ',tags
		   do (funcall #'process-code-tag ,tag ',code)))))))


(adv-defmacro code-block (tags &body code)
  (when adppvt:*add-documentation*
    (with-gensyms (expr)
      `(adppvt:emplace-adp-element :code-block (loop for ,expr in ',code
						     if (and (symbolp ,expr)
							     (member ,expr ',tags))
						       append (process-code-tag ,expr (get-code-tag ,expr))
						     else
						       collect (process-code-tag '#:dummy-tag ,expr))))))


(adv-defmacro code-example (tags &body code)
  (when adppvt:*add-documentation*
    (let ((evaluated-code (loop for expr in code
				collect (with-gensyms (output result)
					  `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
										  ,(adppvt:remove-code-tag-exprs (if (and (symbolp expr)
															  (member expr tags))
														     (adppvt:get-code-tag expr)
														     expr))))))
					     (list ',(if (and (symbolp expr)
							      (member expr tags))
							 (adppvt:process-code-tag expr (adppvt:get-code-tag expr))
							 (adppvt:process-code-tag '#:dummy-tag expr))
						   ,output
						   ,result))))))
      `(adppvt:emplace-adp-element :code-example (list ,@evaluated-code)))))


;; ----- API functions -----

(adv-subheader "API documentation functions" api-subheader)

(adv-defmacro defclass (&body defclass-body)
  `(progn
     (cl:defclass ,@defclass-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defclass-body))
	   (adppvt:emplace-adp-element :defclass '(cl:defclass ,@defclass-body))))))


(adv-defmacro defconstant (&body defconstant-body)
  `(progn
     (cl:defconstant ,@defconstant-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defconstant-body))
	   (adppvt:emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body))))))


(adv-defmacro defgeneric (&body defgeneric-body)
  `(progn
     (cl:defgeneric ,@defgeneric-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defgeneric-body))
	   (adppvt:emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body))))))


(adv-defmacro define-compiler-macro (&body define-compiler-macro-body)
  `(progn
     (cl:define-compiler-macro ,@define-compiler-macro-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))))))


(adv-defmacro define-condition (&body define-condition-body)
  `(progn
     (cl:define-condition ,@define-condition-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',define-condition-body))
	   (adppvt:emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body))))))


(adv-defmacro define-method-combination (&body define-method-combination-body)
  `(progn
     (cl:define-method-combination ,@define-method-combination-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))))))


(adv-defmacro define-modify-macro (&body define-modify-macro-body)
  `(progn
     (cl:define-modify-macro ,@define-modify-macro-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',define-modify-macro-body))
	   (adppvt:emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body))))))


(adv-defmacro define-setf-expander (&body define-setf-expander-body)
  `(progn
     (cl:define-setf-expander ,@define-setf-expander-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (list 'cl:setf (car ',define-setf-expander-body)))
	   (adppvt:emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))))))


(adv-defmacro define-symbol-macro (&body define-symbol-macro-body)
  `(progn
     (cl:define-symbol-macro ,@define-symbol-macro-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',define-symbol-macro-body))
	   (adppvt:emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body))))))


(adv-defmacro defmacro (&body defmacro-body)
  `(progn
     (cl:defmacro ,@defmacro-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defmacro-body))
	   (adppvt:emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body))))))


(adv-defmacro defmethod (&body defmethod-body)
  `(progn
     (cl:defmethod ,@defmethod-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))))))


(adv-defmacro defpackage (&body defpackage-body)
  `(progn
     (cl:defpackage ,@defpackage-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))))))


(adv-defmacro defparameter (&body defparameter-body)
  `(progn
     (cl:defparameter ,@defparameter-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defparameter-body))
	   (adppvt:emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body))))))


(adv-defmacro defsetf (&body defsetf-body)
  `(progn
     (cl:defsetf ,@defsetf-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (list 'cl:setf (car ',defsetf-body)))
	   (adppvt:emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))))))


(adv-defmacro defstruct (&body defstruct-body)
  `(progn
     (cl:defstruct ,@defstruct-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',defstruct-body))
	   (adppvt:emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body))))))


(adv-defmacro deftype (&body deftype-body)
  `(progn
     (cl:deftype ,@deftype-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-type-tag (car ',deftype-body))
	   (adppvt:emplace-adp-element :deftype '(cl:deftype ,@deftype-body))))))


(adv-defmacro defun (&body defun-body)
  `(progn
     (cl:defun ,@defun-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-function-tag (car ',defun-body))
	   (adppvt:emplace-adp-element :defun '(cl:defun ,@defun-body))))))


(adv-defmacro defvar (&body defvar-body)
  `(progn
     (cl:defvar ,@defvar-body)
     ,@(when adppvt:*add-documentation*
	 `((adppvt:push-symbol-tag (car ',defvar-body))
	   (adppvt:emplace-adp-element :defvar '(cl:defvar ,@defvar-body))))))


;; ----- Writer functions -----

(adv-subheader "Documentation writer function" writer-subheader)


(adv-defmacro write-in-file (file-path)
  (when adppvt:*add-documentation*
    (assert (pathname-name file-path) (file-path) "The ~s pathname has not a name part." file-path)
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (let ((,let-file-path (make-pathname :directory (if (pathname-directory ,file-path)
							       (cons :relative (cdr (pathname-directory ,file-path)))
							       nil)
						:name (pathname-name ,file-path))))
	     (adppvt:emplace-adp-file ,let-file-path (copy-array adppvt:*file-adp-elements*))
	     (adppvt:empty-adp-elements)
	     (loop for (,header-tag . ,header-str) across adppvt:*header-tags*
		   for ,symbol-tag across adppvt:*symbol-tags*
		   for ,function-tag across adppvt:*function-tags*
		   for ,type-tag across adppvt:*type-tags*
		   do (adppvt:add-header-tag-path ,header-tag ,header-str ,let-file-path)
		      (adppvt:add-symbol-tag-path ,symbol-tag ,let-file-path)
		      (adppvt:add-function-tag-path ,function-tag ,let-file-path)
		      (adppvt:add-type-tag-path ,type-tag ,let-file-path)
		   finally (adppvt:empty-header-tags)
			   (adppvt:empty-symbol-tags)
			   (adppvt:empty-function-tags)
			   (adppvt:empty-type-tags))))))))


(adv-defun load-documentation-system (system style root-path &rest style-args)
  (adppvt:remove-current-procs)
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (asdf:load-system style-system :force t))
  (adppvt:check-current-procs)
  (adppvt:check-style-parameters style-args)
  (adppvt:remove-current-data)
  (let ((adppvt:*add-documentation* t))
    (asdf:load-system system :force t))
  (loop for (name value) in style-args by #'cddr
	do (adppvt:set-parameter-value name value))
  (let ((fixed-root-path (make-pathname :host (pathname-host root-path)
					:device (pathname-device root-path)
					:directory (cons :absolute (cdr (pathname-directory root-path))))))
    (adppvt:write-system-files fixed-root-path)))


(adv-write-in-file #P"docs/user-api")
