
(in-package :adp)


(cl:defparameter *add-documentation* nil)
(cl:defparameter *current-style* nil)


;; ----- guide functions ----- ;; ARREGLAR TODO ESTO 

(cl:defmacro def-guide-definer (name user-args priv-args &body body)
  (let ((doc-global-proc (gensym "GLOBAL-PROC"))
	(writer-name (intern (concatenate 'string "DEF-" (symbol-name name) "-WRITER")))
	(body-arg (gensym "BODY"))
	(arg-sym (gensym "ARG"))
	(impl-name (gensym "IMPL"))
	(impl-args (gensym "IMPL-ARGS"))
	(whole-arg (gensym "WHOLE-ARG")))
    `(progn
       (defparameter ,doc-global-proc nil)
       (defmacro ,writer-name (,priv-args &body ,body-arg)
	 `(setq ,',doc-global-proc (lambda ,',priv-args
				     ,@,body-arg)))
       (defun ,impl-name ,user-args
	 (when ,doc-global-proc
	   (multiple-value-call ,doc-global-proc (progn ,@body))))
       (defmacro name (&whole ,whole-arg ,@user-args)
	 (when *add-documentation*
	   `(,impl-name ,@(cdr ,whole-arg)))))))

(def-guide-definer header (str) (str)
  (unless (stringp str)
    (error "Expected a string. Found: ~s" header))
  str)

(def-guide-definer subheader (str) (str)
  (unless (stringp str)
    (error "Expected a string. Found: ~s" header))
  str)

(def-guide-definer code-block (code) (code)
  code)

(def-guide-definer example (code) (code output result)
  (macrolet ((eval-code ()
	       code))
    (let ((output (with-output-to-string (*standard-output*)
		    (let ((result (eval-code)))
		      (values code output result))))))))


;; ----- api functions -----

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
(defdefiner define-compiler-macro)
(defdefiner defstruct)
(defdefiner defclass)
(defdefiner defgeneric)
(defdefiner defmethod)
(defdefiner defpackage)


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



