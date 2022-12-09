
(in-package :adppvt)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct parameter
    keyword
    symbol
    optional
    value)

  (defvar *style-parameters* nil))

(defun check-style-parameters (args)
  (let* ((not-found-sym '#:not-found))
    (loop for param in *style-parameters*
	  do (when (and (not (parameter-optional param))
			(eq (getf args (parameter-keyword param) not-found-sym) not-found-sym))
	       (error "ADP error: The current style must receive the key argument ~s."
		      (parameter-keyword param))))))

(defmacro with-new-style-parameter-list (&body body)
  `(let ((*style-parameters* nil))
     ,@body))

(defmacro with-style-parameters (args &body body)
  (let ((let-parameter-bindings (mapcar (lambda (param)
					  `(,(parameter-symbol param) (getf ,args ,(paramter-keyword param) ,(parameter-value param))))
					*style-parameters*))
	(param-symbols (mapcar #'parameter-symbol *style-parameters*)))
    `(let ,let-parameter-bindings
       (check-style-parameters ',param-symbols)
       ,@body)))

(defmacro define-style-parameter (name &key (value nil) (key nil) (required nil))
  (check-type name symbol "a symbol")
  (check-type key (or null keyword) "a keyword")
  (check-type required boolean)
  (push (make-parameter :keyword (or key (intern (symbol-name name) "KEYWORD"))
			:symbol name
			:value value
			:optional (not required))
	*style-parameters*)
  `(defparameter ,name ,value))
