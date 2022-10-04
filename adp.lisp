
(in-package :adp)


(cl:defparameter *add-documentation* nil)
(cl:defparameter *current-style* nil)


;; ----- toplevel guide functions -----

(cl:defun slice-format (&rest args)
  (with-output-to-string (str)
    (loop for arg in args
	  do (if (stringp arg)
		 (princ arg str)
		 (prin1 arg str)))))

(cl:defmacro def-customizable-writer (writer-name impl-name num-writer-args &body body)
  (let ((writer-args (loop repeat num-writer-args do (gensym "WRITER-ARG"))))
    (with-gensyms (doc-global-proc body-arg priv-writer-args priv-writer-arg)
      `(progn
	 (defparameter ,doc-global-proc nil)
	 (defmacro ,writer-name (,writer-args &body ,body-arg)
	   (let ((,priv-writer-args (list ,@writer-args)))
	     (loop for ,priv-writer-arg in ,priv-writer-args
		   do (assert (symbolp ,priv-writer-arg)))
	     `(setq ,',doc-global-proc (lambda ,,priv-writer-args
					 ,@,body-arg))))
	 (macrolet ((,impl-name ,writer-args
		      `(when ,',doc-global-proc
			 (apply ,',doc-global-proc ,@',writer-args))))
	   ,@body)))))


(with-customizable-writer def-header-writer header-impl 2
  (defmacro header (str &optional label)
    (when *add-documentation*
      (once-only (str label)
	`(progn
	   (assert (stringp ,str) (,str) "~s is not a string." ,str)
	   (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
	   (header-impl ,str ,label))))))

(with-customizable-writer def-subheader-writer subheader-impl 2 
  (defmacro subheader (str &optional label)
    (when *add-documentation*
      (once-only (str label)
	`(progn
	   (assert (stringp ,str) (,str) "~s is not a string." ,str)
	   (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
	   (subheader-impl ,str ,label))))))

(with-customizable-writer def-subsubheader-writer subsubheader-impl 2
  (defmacro subsubheader (str &optional label)
    (when *add-documentation*
      (once-only (str label)
	`(progn
	   (assert (stringp ,str) (,str) "~s is not a string." ,str)
	   (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
	   (subsubheader-impl ,str ,label))))))

(with-customizable-writer def-text-writer text-impl 1
  (defmacro text (&rest objects)
    (when *add-documentation*
      `(text-impl (slice-format ,@objects)))))

(with-customizable-writer def-table-writer table-impl 1 
  (defmacro table (&rest rows)
    (when *add-documentation*
      (loop for row in rows
	    do (assert (listp row) () "Each element must be a list representing a row."))
      `(table-impl (list ,(loop for row in rows
				collect `(list ,(loop for elem in row
						      if (listp elem)
							collect `(slice-format ,@elem)
						      else
							collect `(slice-format ,elem)))))))))

(with-customizable-writer def-itemize-writer itemize-impl 1
  (defmacro itemize (&rest items)
    (when *add-documentation*
      (labels ((process-items (item-list)
		 (loop for item in item-list
		       do (assert (and (listp item)
				       (or (eq (car item) :item)
					   (eq (car item) :itemize)))
				  () "Each item must be a list starting with :item or :itemize.")
		       if (eq (car item) :itemize)
			 collect (process-items item) into final-item-list
		       else
			 collect `(slice-format ,@item) into final-item-list
		       finally return `(list :itemize ,@final-item-list))))
	`(itemize-impl ,(process-items items))))))


(defparameter *code-tags* (make-hash-table))

(defun add-code-tag (tag &rest code-list)
  (when (not (get-hash tag *code-tags*))
      (setf (get-hash tag *code-tags*) (make-array 10 :adjustable t :fill-pointer 0)))
  (loop for code in code-list
	do (vector-push-extend code (get-hash tag *code-tags*))))

(defun code-tagp (tag)
  (get-hash tag *code-tags*))

(defun get-code-tag (tag)
  (get-hash tag *code-tags*))

(defparameter *hide-symbol* '#:hide)

(defun code-hidep (code)
  (eq code *hide-symbol*))

(defun plistp (code)
  (or (null code)
      (and (consp code)
	   (plistp (cdr code)))))

(defun remove-code-tag-exprs (code)
  (if (plistp code)
      (cond
	((member (car code) '(code-tag code-focus))
	 (mapcan #'remove-code-tag-exprs (cddr code)))
	(t
	 (list (mapcan #'remove-code-tag-exprs code))))
      (list code)))

(defun process-code-tag (tag code)
  (labels ((process-aux (tag code)
	     (if (plistp code)
		 (if (member (car code) '(code-tag code-focus))
		     (if (and (eq (car code) 'code-focus)
			      (member tag (cadr code)))
			 (list (remove-code-tag-exprs code) t)
			 (loop for expr in (cddr code)
			       for (processed-expr expr-focus-found) = (process-aux tag expr)
			       for focus-found = (or expr-focus-found focus-found)
			       count (not expr-focus-found) into hide-count
			       if expr-focus-found
				 do (setf hide-count 0)
			       if (<= hide-count 1)
				 append processed-expr into processed-code
			       finally (return (list processed-code focus-found))))
		     (let* ((car-processed-values (process-aux tag (car code)))
			    (car-processed (car car-processed-values))
			    (car-focus-found (cadr car-processed-values)))
		       (loop for expr in (cdr code)
			     for (processed-expr expr-focus-found) = (process-aux tag expr)
			     for focus-found = (or expr-focus-found car-focus-found) then (or expr-focus-found focus-found)
			     count (not expr-focus-found) into hide-count
			     if expr-focus-found
			       do (setf hide-count 0)
			     if (<= hide-count 1)
			       append processed-expr into processed-code
			     finally (return (if focus-found
						 (if car-focus-found
						     (list (list (append car-processed processed-code)) t)
						     (list (list (append (remove-code-tag-exprs (car code)) processed-code)) t))
						 (list (list *hide-symbol*) nil))))))
		 (list (list *hide-symbol*) nil))))
    (destructuring-bind (processed-code focus-found) (process-aux tag code)
      (if focus-found
	  processed-code
	  (remove-code-tag-exprs code)))))

(cl:defamcro code-focus (tags &rest code)
  `(progn ,@code))

(with-customizable-writer def-code-tag-writer code-tag.impl 2
  (defmacro code-tag (tags &rest code)
    `(progn
       ,@code
       ,(when *add-documentation*
	  (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol or a list" tags)
	  (when (listp tags)
	    (assert (every (mapcar #'symbolp tags)) (tags) "Thare is a non-symbol in ~s" tags))
	  (labels ((remove-code-tags (code)
		     (cond
		       ((null code) nil)
		       ((eq (car code) 'code-tag) (remove-code-tags (cddr code)))
		       ((consp code) (cons (remove-code-tags (car code)) (remove-code-tags (cdr code))))
		       (t code))))
	    `(code-tag-impl ',tags ',(remove-code-tags code)))))))

(with-customizable-writer def-code-block-writer code-block-impl 1
  (defmacro code-block (tags &rest code)
    `(code-block-impl ',code)))

(with-customizable-writer def-example-writer example-impl 1 
  (defmacro example (&rest code)
    (when *add-documentation*
      (with-gensyms (expr output result expressions)
	(let ((evaluated-code (loop for expr in code
				    collect `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						    (,result (with-output-to-string (*standard-output* ,output)
							       ,expr)))
					       (list ',expr ,output ,result)))))
	  `(example-impl (list ,@evaluated-code)))))))

(with-customizable-writer def-image-writer image-impl 1
  (defmacro image (path)
    (when *add-documentation*
      (once-only (path)
	`(progn
	   (assert (pathnamep (pathname path)) (,path) "~s is not a pathname designator" path)
	   (image-impl (pathname ,path)))))))

(with-customizable-writer def-bold-writer bold-impl 1 
  (defmacro bold (&rest args)
    (when *add-documentation*
      `(bold-impl (slice-format ,@args)))))

(with-customizable-writer def-italic-writer 1
  (defmacro italic (&rest args)
    (when *add-documentation*
      `(italic-impl (slice-format ,@args)))))

(with-customizable-writer def-code-inline-writer code-inline-impl 1 
  (defmacro code-inline (code)
    (when *add-documentation*
      `(code-inline-impl ,code))))

(with-customizable-writer def-web-link-writer web-link-impl 2 
  (defmacro web-link (name link)
    (when *add-documentation*
      (once-only (name link)
	`(progn
	   (assert (stringp ,name) (,name) "~s is not a string" ,name)
	   (assert (stringp ,link) (,link) "~s is not a string" ,link)
	   (web-link-impl ,name ,link))))))

(with-customizable-writer def-symbol-ref-writer symbol-ref-impl 1
  (defmacro symbol-ref (label)
    (when *add-documentation*
      (once-only (label)
	`(progn
	   (assert (symbolp ,label) (,label) "~s is not a symbol" ,label)
	   (symbol-ref-impl ,label))))))

(with-customizable-writer def-function-ref-writer function-ref-impl 1
  (defmacro function-ref (label)
    (when *add-documentation*
      (once-only (label)
	`(progn
	   (assert (symbolp ,label) (,label) "~s is not a symbol" ,label)
	   (function-ref-impl ,label))))))

(with-customizable-writer def-header-ref-writer header-ref-impl 1
  (defmacro header-ref (label)
    (when *add-documentation*
      (once-only (label)
	`(progn
	   (assert (symbolp ,label) (,label) "~s is not a symbol" ,label)
	   (header-ref-impl ,label))))))


;; ----- api functions -----

(cl:defmacro def-cl-customizable-writer (name)
  (let* ((definer-body (gensym "DEFINER-BODY"))
	 (doc-function-global-proc (gensym "DOC-FUNCTION-NAME-PROC"))
	 (doc-writer-name (intern (concatenate 'string "DEF-" name "-WRITER")))
	 (writer-definer-body (gensym "ARGS"))
	 (writer-body (gensym "BODY")))
    `(progn
       (defparameter ,doc-function-global-proc nil)
       (defmacro ,doc-writer-name ((,writer-definer-body) &body ,writer-body)
	 (unless (symbolp ,writer-definer-body)
	   (error "The argument must be a symbol. Found: ~s" ,writer-defined-body))
	 `(setq ,',doc-function-global-proc
		(lambda (,,writer-definer-body)
		  ,@,writer-body)))
       (defmacro ,name (&rest ,definer-body)
	 `(progn
	    ,(cons ',(find-symbol (symbol-name name) '#:cl) ,definer-body)
	    ,@(when *add-documentation*
		`(when ,',doc-function-global-name
		   (apply ,',doc-function-global-name ,,definer-body))))))))


(def-cl-customizable-writer defconstant)
(def-cl-customizable-writer defparameter)
(def-cl-customizable-writer defvar)
(def-cl-customizable-writer defun)
(def-cl-customizable-writer defmacro)
(def-cl-customizable-writer define-compiler-macro)
(def-cl-customizable-writer defstruct)
(def-cl-customizable-writer defclass)
(def-cl-customizable-writer defgeneric)
(def-cl-customizable-writer defmethod)
(def-cl-customizable-writer defpackage)


;; ----- writer functions -----

(macrolet ((def-add-documentation-in-file-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-add-documentation-in-file-writer ((file-path) &body body)
		    (unless (symbolp file-path)
		      (error "The argument must be a symbol. Found: ~s" file-path))
		    `(setq ,',doc-global-proc (lambda (,file-path)
						,@body)))
		  (defun add-documentation-in-file-impl (file-path)
		    (unless (eq :relative (car (pathname-directory (pathname file-path))))
		      (error "The pathname ~s is not a relative path." file-path))
		    (unless (pathname-name (pathname file-path))
		      (error "The pathname ~s does not designate any file." file-path))
		    (when ,doc-global-proc
		      (apply ,doc-global-proc file-path)))
		  (defmacro add-documentation-in-file (file-path)
		    (when *add-documentation*
		      `(add-documentation-in-file-impl ,file-path)))))))
  (def-add-documentation-in-file-definer))


(cl:defun style-file (style)
  (let* ((style-name (string style))
	 (style-file (asdf:system-relative-pathname #:apd (concatenate 'string "/styles/" style-name ".lisp"))))
    (truename style-file)))


(cl:defun load-style (style)
  (load (style-file style))
  (setf *current-style* style))


(macrolet ((def-add-documentation-please-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-add-documentation-please-writer ((root-dir) &body body)
		    (unless (symbolp root-dir)
		      (error "Expected a symbol. Found: ~s" root-dir))
		    `(setq ,',doc-global-proc (lambda (,root-dir)
						,@body)))
		  (defun add-documentation-please (system root-dir)
		    (unless (eq :absolute (car (pathname-directory root-dir)))
		      (error "The pathname ~s is not absolute." root-dir))
		    (unless (uiop:directory-exists-p root-dir)
		      (error "The directory ~s does no exist." root-dir))
		    (when (not *current-style*)
		      (load-style :markdown))
		    (let ((*add-documentation* t))
		      (asdf:load-system system :force t))
		    (funcall ,doc-global-proc root-dir))))))
  (def-add-documentation-please-definer))



