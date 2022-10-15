
;; ----- ADP interface -----

(in-package :adp)


(cl:defmacro header (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :header ,str ,label))))


(cl:defmacro subheader (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :subheader ,str ,label))))


(cl:defmacro subsubheader (str &optional label)
  (when *add-documentation*
    `(progn
       ,@(when label
	   `((push-header-tag ,label)))
       (emplace-adp-element :subsubheader ,str ,label))))


(cl:defmacro text (&rest objects)
  (when *add-documentation*
    `(emplace-adp-element :text ,@objects)))


(cl:defmacro table (&rest rows)
  (when *add-documentation*
    (loop for row in rows
	  do (check-type row list "a list")
	     (loop for elem in row
		   do (assert (eq (car elem) :cell) () "Each cell of a table must be a list starting with :cell. Found: ~s" elem)))
    `(emplace-adp-element :table (list ,(loop for row in rows
					      collect `(list ,(loop for elem in row
								    collect (cons 'list elem))))))))


(cl:defmacro itemize (&rest items)
  (when *add-documentation*
    (labels ((check-items (item-list)
	       (loop for item in item-list
		     if (not (eq (car item) :item))
		       do (if (eq (car item) :itemize) 
			      (check-items (cdr item))
			      (error "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s" item)))))
      (check-items items))
    `(emplace-adp-element :itemize ,@items)))


(cl:defmacro image (alt-text path)
  (when *add-documentation*
    (with-gensyms (let-alt-text let-path)
      `(let ((,let-alt-text ,alt-text)
	     (,let-path ,path))
	 (declare (type string ,let-alt-text)
		  (type pathname ,let-path))
	 (emplace-adp-element :image ,let-alt-text ,let-path)))))


(cl:defmacro bold (&rest args)
  (when *add-documentation*
    `(create-bold-text ,@args)))


(cl:defmacro italic (&rest args)
    (when *add-documentation*
      `(create-italic-text ,@args)))


(cl:defmacro code-inline (code)
  (when *add-documentation*
    `(create-code-inline-text ,code)))


(cl:defmacro web-link (name link)
  (when *add-documentation*
    (with-gensyms (let-name let-link stream)
      `(let ((,let-name ,name)
	     (,let-link ,link))
	 (declare (type string ,let-name ,let-link))
	 (with-output-to-string (,stream)
	   (funcall *web-link-proc* ,stream ,let-name ,let-link))))))


(cl:defmacro header-ref (label)
  (when *add-documentation*
    `(create-header-ref-text ,label)))


(cl:defmacro symbol-ref (label)
  (when *add-documentation*
    `(create-symbol-ref-text ,label)))


(cl:defmacro function-ref (label)
  (when *add-documentation*
    `(create-function-ref-text ,label)))


(cl:defmacro type-ref (label)
  (when *add-documentation*
    `(create-type-ref-text ,label)))


;; (cl:defmacro code-focus (tags &rest code)
;;   `(progn ,@code))


(cl:defmacro code-tag (tags &rest code)
  (with-gensyms (tag)
    `(progn
     ,@(loop for expr in code
	     collect (remove-own-code-focus-exprs expr))
     ,@(when *add-documentation*
	 (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol nor a list" tags)
	 (when (listp tags)
	   (assert (every #'symbolp tags) (tags) "Thare is a non-symbol in ~s" tags))
	 `((loop for ,tag in ',tags
		 do (apply #'add-code-tag ,tag ',code)))))))


(cl:defmacro code-block (tags &rest code)
  (when *add-documentation*
    (with-gensyms (expr)
      `(emplace-adp-element :code-block (loop for ,expr in ',code
					      if (and (symbolp ,expr)
						      (member ,expr ',tags))
						append (process-code-tag ,expr (get-code-tag ,expr))
					      else
						collect (process-code-tag '#:dummy-tag ,expr))))))


(cl:defmacro code-example (tags &rest code)
  (when *add-documentation*
    (let ((evaluated-code (loop for expr in code
				collect (with-gensyms (output result)
					  `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						  (,result (multiple-value-list (with-output-to-string (*standard-output* ,output)
										  ,(remove-code-tag-exprs (if (and (symbolp expr)
														   (member expr tags))
													      (get-code-tag expr)
													      expr))))))
					     (list ',(if (and (symbolp expr)
							      (member expr tags))
							 (process-code-tag expr (get-code-tag expr))
							 (process-code-tag '#:dummy-tag expr))
						   ,output
						   ,result))))))
      `(emplace-adp-element :code-example (list ,@evaluated-code)))))


(cl:defmacro defclass (&rest defclass-body)
  `(progn
     (cl:defclass ,@defclass-body)
     (push-type-tag (car ',defclass-body))
     (emplace-adp-element :defclass '(cl:defclass ,@defclass-body))))


(cl:defmacro defconstant (&rest defconstant-body)
  `(progn
     (cl:defconstant ,@defconstant-body)
     (push-symbol-tag (car ',defconstant-body))
     (emplace-adp-element :defconstant '(cl:defconstant ,@defconstant-body))))


(cl:defmacro defgeneric (&rest defgeneric-body)
  `(progn
     (cl:defgeneric ,@defgeneric-body)
     (push-function-tag (car ',defgeneric-body))
     (emplace-adp-element :defgeneric '(cl:defgeneric ,@defgeneric-body))))


(cl:defmacro define-compiler-macro (&rest define-compiler-macro-body)
  `(progn
     (cl:define-compiler-macro ,@define-compiler-macro-body)
     (emplace-adp-element :define-compiler-macro '(cl:define-compiler-macro ,@define-compiler-macro-body))))


(cl:defmacro define-condition (&rest define-condition-body)
  `(progn
     (cl:define-condition ,@define-condition-body)
     (push-type-tag (car ',define-condition-body))
     (emplace-adp-element :define-condition '(cl:define-condition ,@define-condition-body))))


(cl:defmacro define-method-combination (&rest define-method-combination-body)
  `(progn
     (cl:define-method-combination ,@define-method-combination-body)
     (emplace-adp-element :define-method-combination '(cl:define-method-combination ,@define-method-combination-body))))


(cl:defmacro define-modify-macro (&rest define-modify-macro-body)
  `(progn
     (cl:define-modify-macro ,@define-modify-macro-body)
     (push-function-tag (car ',define-modify-macro-body))
     (emplace-adp-element :define-modify-macro '(cl:define-modify-macro ,@define-modify-macro-body))))


(cl:defmacro define-setf-expander (&rest define-setf-expander-body)
  `(progn
     (cl:define-setf-expander ,@define-setf-expander-body)
     (push-function-tag (list 'cl:setf (car ',define-setf-expander-body)))
     (emplace-adp-element :define-setf-expander '(cl:define-setf-expander ,@define-setf-expander-body))))


(cl:defmacro define-symbol-macro (&rest define-symbol-macro-body)
  `(progn
     (cl:define-symbol-macro ,@define-symbol-macro-body)
     (push-symbol-tag (car ',define-symbol-macro-body))
     (emplace-adp-element :define-symbol-macro '(cl:define-symbol-macro ,@define-symbol-macro-body))))


(cl:defmacro defmacro (&rest defmacro-body)
  `(progn
     (cl:defmacro ,@defmacro-body)
     (push-function-tag (car ',defmacro-body))
     (emplace-adp-element :defmacro '(cl:defmacro ,@defmacro-body))))


(cl:defmacro defmethod (&rest defmethod-body)
  `(progn
     (cl:defmethod ,@defmethod-body)
     (emplace-adp-element :defmethod '(cl:defmethod ,@defmethod-body))))


(cl:defmacro defpackage (&rest defpackage-body)
  `(progn
     (cl:defpackage ,@defpackage-body)
     (emplace-adp-element :defpackage '(cl:defpackage ,@defpackage-body))))


(cl:defmacro defparameter (&rest defparameter-body)
  `(progn
     (cl:defparameter ,@defparameter-body)
     (push-symbol-tag (car ',defparameter-body))
     (emplace-adp-element :defparameter '(cl:defparameter ,@defparameter-body))))


(cl:defmacro defsetf (&rest defsetf-body)
  `(progn
     (cl:defsetf ,@defsetf-body)
     (push-function-tag (list 'cl:setf (car ',defsetf-body)))
     (emplace-adp-element :defsetf '(cl:defsetf ,@defsetf-body))))


(cl:defmacro defstruct (&rest defstruct-body)
  `(progn
     (cl:defstruct ,@defstruct-body)
     (push-type-tag (car ',defstruct-body))
     (emplace-adp-element :defstruct '(cl:defstruct ,@defstruct-body))))


(cl:defmacro deftype (&rest deftype-body)
  `(progn
     (cl:deftype ,@deftype-body)
     (push-type-tag (car ',deftype-body))
     (emplace-adp-element :deftype '(cl:deftype ,@deftype-body))))


(cl:defmacro defun (&rest defun-body)
  `(progn
     (cl:defun ,@defun-body)
     (push-function-tag (car ',defun-body))
     (emplace-adp-element :defun '(cl:defun ,@defun-body))))


(cl:defmacro defvar (&rest defvar-body)
  `(progn
     (cl:defvar ,@defvar-body)
     (push-symbol-tag (car ',defvar-body))
     (emplace-adp-element :defvar '(cl:defvar ,@defvar-body))))


(cl:defmacro add-documentation-in-file (file-path)
  (when *add-documentation*
    (with-gensyms (let-file-path header-tag header-str symbol-tag function-tag type-tag)
      (once-only (file-path)
	`(progn
	   (assert (pathname-directory ,file-path) (,file-path) "The ~s pathname has not a directory part." ,file-path)
	   (assert (pathname-name ,file-path) (,file-path) "The ~s pathname has not a name part." ,file-path)
	   (let ((,let-file-path (make-pathname :directory (cons :relative (cdr (pathname-directory ,file-path)))
						:name (pathname-name ,file-path))))
	     (emplace-adp-file ,let-file-path (copy-array *file-adp-elements*))
	     (empty-adp-elements)
	     (loop for (,header-tag . ,header-str) across *header-tags*
		   for ,symbol-tag across *symbol-tags*
		   for ,function-tag across *function-tags*
		   for ,type-tag across *type-tags*
		   do (add-header-tag-path ,header-tag ,header-str ,let-file-path)
		      (add-symbol-tag-path ,symbol-tag ,let-file-path)
		      (add-function-tag-path ,function-tag ,let-file-path)
		      (add-type-tag-path ,type-tag ,let-file-path)
		   finally (empty-header-tags)
			   (empty-symbol-tags)
			   (empty-function-tags)
			   (empty-type-tags))))))))



(cl:defun load-documentation-system (system style root-path &rest style-args)
  (check-style-parameters style-args)
  (let ((style-system (intern (concatenate 'string "ADP/" (symbol-name style)) :keyword)))
    (asdf:load-system style-system))
  (let ((*add-documentation* t))
    (asdf:load-system system :force t))
  (loop for (name value) in style-args by #'cddr
	do (set-parameter-value name value))
  (let ((fixed-root-path (make-pathname :host (pathname-host root-path)
					:device (pathname-device root-path)
					:directory (cons :absolute (cdr (pathname-directory root-path))))))
    (write-system-files fixed-root-path)))






