
(in-package :adppvt)


(defstruct parameter
  keyword
  symbol
  optional
  value)

(defvar *style-parameters* nil)

(defun check-style-parameters (keys)
  "Check if all the required keywords arguments are present. Also check if every keyword
is supported."
  (loop for param in *style-parameters*
	do (when (and (not (parameter-optional param))
		      (not (member (parameter-keyword param) keys)))
	     (error "ADP error: The current style must receive the key argument ~s."
		    (parameter-keyword param))))
  (loop for key in keys
	do (when (not (member key *style-parameters* :key #'parameter-keyword))
	     (error "ADP error: The keyword parameter ~s is not supported by this style."
		    key))))

(defun set-style-parameter-values (args)
  (loop for parameter in *style-parameters*
	do (setf (symbol-value (parameter-symbol parameter))
		 (getf args (parameter-keyword parameter) (parameter-value parameter)))))

(defun unset-style-parameter-values ()
  (loop for parameter in *style-parameters*
	do (setf (symbol-value (parameter-symbol parameter)) (parameter-value parameter))))

(defmacro with-new-style-parameter-list (&body body)
  `(let ((*style-parameters* nil))
     ,@body))

(defmacro with-style-parameters (args &body body)
  (with-gensyms (keys key)
    `(let ((,keys (loop for ,key in ,args by #'cddr
			collect ,key)))
       (unwind-protect
	    (progn
	      (check-style-parameters ,keys)
	      (set-style-parameter-values ,args)
	      ,@body)
	 (unset-style-parameter-values)))))

(defmacro define-style-parameter (name &key (value nil) (key nil) (required nil))
  (check-type name symbol "a symbol")
  (check-type key (or null keyword) "a keyword")
  (check-type required boolean)
  
  `(progn
     (push (make-parameter :keyword (or key (intern (symbol-name name) "KEYWORD"))
			   :symbol name
			   :value value
			   :optional (not required))
	   *style-parameters*)
     (defparameter ,name ,value)))
