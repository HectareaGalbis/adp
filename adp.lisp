
(in-package :adp)


(cl:defparameter *add-documentation* nil)

(cl:defparameter *style* #:markdown)


(cl:defmacro defdefiner (name)
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


(defdefiner defconstant)
(defdefiner defparameter)
(defdefiner defvar)
(defdefiner defun)
(defdefiner defmacro)
(defdefiner defstruct)


(macrolet ((def-write-documentation-in-file-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-write-documentation-in-file-writer ((file-path) &body body)
		    (unless (symbolp file-path)
		      (error "The argument must be a symbol. Found: ~s" file-path))
		    `(setq ,',doc-global-proc (lambda (,file-path)
						,@body)))
		  (defun write-documentation-in-file (file-path)
		    (when ,doc-global-proc
		      (apply ,doc-global-proc file-path)))))))
  (def-documentation-in-file-definer))


(macrolet ((def-load-with-documentation-definer ()
	     (let ((doc-global-proc (gensym "GLOBAL-PROC")))
	       `(progn
		  (defparameter ,doc-global-proc nil)
		  (defmacro def-load-with-documentation-writer (args &body body)
		    `(setq ,',doc-global-proc (lambda ,args
					      ,@body)))
		  (defun load-with-documentation (system &rest args &key (style #:markdown) &allow-other-keys)
		    (let* ((style-name (string style))
			   (style-file (asdf:system-relative-pathname #:apd (concatenate 'string "/styles/" style-name ".lisp"))))
		      (load style-file))
		    (asdf:load-system system :force t)
		    (apply ,doc-global-proc args))))))
  (def-load-with-documentation-definer))

