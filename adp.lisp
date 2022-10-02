
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

(cl:defmacro def-customizable-writer (name num-writer-args user-args &body body)
  (let ((writer-name (intern (concatenate 'string "DEF-" (symbol-name name) "-WRITER")))
	(writer-args (loop repeat num-writer-args do (gensym "WRITER-ARG"))))
    (with-gensyms (doc-global-proc body-arg priv-writer-args priv-writer-arg impl-name)
      `(progn
	 (defparameter ,doc-global-proc nil)
	 (defmacro ,writer-name (,writer-args &body ,body-arg)
	   (let ((,priv-writer-args (list ,@writer-args)))
	     (loop for ,priv-writer-arg in ,priv-writer-args
		   do (assert (symbolp ,priv-writer-arg)))
	     `(setq ,',doc-global-proc (lambda ,,priv-writer-args
					 ,@,body-arg))))
	 (defun ,impl-name ,writer-args
	   (when ,doc-global-proc
	     (apply ,doc-global-proc ,@writer-args)))
	 (defmacro ,name ,user-args
	   (when *add-documentation*
	     ,@body))))))


(def-customizable-writer header 1 (str &optional label)
  (once-only (str label)
    `(progn
       (assert (stringp ,str) (,str) "~s is not a string." ,str)
       (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
       (header-impl ,str ,label))))

(def-customizable-writer subheader 1 (str &optional label)
  (once-only (str label)
    `(progn
       (assert (stringp ,str) (,str) "~s is not a string." ,str)
       (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
       (subheader-impl ,str ,label))))

(def-customizable-writer subsubheader 1 (str label)
  (once-only (str label)
    `(progn
       (assert (stringp ,str) (,str) "~s is not a string." ,str)
       (assert (or (not label) (symbolp ,label)) (,label) "~s is not a symbol" ,label)
       (subsubheader-impl ,str ,label))))

(def-customizable-writer text 1 (&rest objects)
  `(text-impl (slice-format ,@objects)))

(def-customizable-writer table 1 (&rest rows)
  (loop for row in rows
	do (assert (listp row) () "Each element must be a list representing a row."))
  `(table-impl (list ,(loop for row in rows
			    collect `(list ,(loop for elem in row
						  if (listp elem)
						    collect `(slice-format ,@elem)
						  else
						    collect `(slice-format ,elem)))))))

(def-customizable-writer itemize (&rest items)
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
		   finally return `(list ,@final-item-list))))
    `(itemize-impl ,(process-items items))))

(def-customizable-writer code-block 1 (&rest code)
  `(code-block-impl ',code))

(def-customizable-writer example 1 (&rest code)
  (with-gensyms (expr output result expressions)
    (let ((evaluated-code (loop for expr in code
				collect `(let* ((,output (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
						(,result (with-output-to-string (*standard-output* ,output)
							   ,expr)))
					   (list ',expr ,output ,result)))))
      `(example-impl (list ,@evaluated-code)))))

(def-customizable-writer image 1 (path)
  (once-only (path)
    `(progn
       (assert (pathnamep (pathname path)) (,path) "~s is not a pathname designator" path)
       (image-impl (pathname ,path)))))

(def-customizable-writer bold 1 (&rest args)
  `(bold-impl (slice-format ,@args)))

(def-customizable-writer italic 1 (&rest args)
  `(italic-impl (slice-format ,@args)))

(def-customizable-writer web-link 2 (name link)
  (once-only (name link)
    `(progn
       (assert (stringp ,name) (,name) "~s is not a string" ,name)
       (assert (stringp ,link) (,link) "~s is not a string" ,link)
       (web-link-impl ,name ,link))))

(def-customizable-writer label-ref 1 (label)
  (once-only (label)
    `(progn
       (assert (symbolp ,label) (,label) "~s is not a symbol" ,label)
       (label-ref-impl ,label))))

(cl:defamcro code-focus (tags &rest code)
  `(progn ,@code))

(def-customizable-writer code-tag 2 (tags &rest code)
  (assert (or (symbolp tags) (listp tags)) (tags) "~s is not a symbol or a list" tags)
  (when (listp tags)
    (assert (every (mapcar #'symbolp tags)) (tags) "Thare is a non-symbol in ~s" tags))
  `(code-tag-impl ',tags ',code))


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



