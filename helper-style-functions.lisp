

(in-package :adp)

;; ----- helper-helper-functions -----

(defun documentationp (expr)
  (stringp expr))

(defun declarationp (expr)
  (and (listp expr)
       (eq (car expr 'declare))))

(defun check-component-symbols (symbols components)
  (loop for sym in symbols
	do (unless (symbolp sym)
	     (error "Expected a symbol. Found: ~s" sym))
	   (unless (member sym components :test #'string=)
	     (error "Expected one of the following symbols: ~s~%Found: ~s" components sym))))

(defmacro def-with-components (name &rest components)
  (apply #'check-symbols name components)
  (let ((name-arg (intern (concatenate 'string "WITH-" (symbol-name name) "-COMPONENTS")))
	(component-rest-args (gensym "COMPONENTS-ARGS"))
	(function-body-arg (gensym (concatenate 'string name "-BODY")))
	(body-arg (gensym "BODY"))
	(function-body-value-var (gensym "FUNCTION-BODY-VALUE"))
	(let-bindings-var (gensym))
	(lambda-var (gensym)))
    `(defmacro ,name-arg (((&rest ,component-rest-args) ,function-body-arg) &body ,body-arg)
       (check-component-symbols ,component-rest-args)
       (let* ((,function-body-value-var (gensym))
	      (,let-bindings-var (mapcar (lambda (,lambda-var)
					   (list ,lambda-var (list (find-symbol (concatenate 'string ,(symbol-name name) "-" (symbol-name ,lambda-var))) ,function-body-value-var))
					   ,component-rest-args))))
	 `(let ((,,function-body-value-var ,,function-body-arg))
	    (let (,,let-bindings-var)
	      ,@,body-arg))))))


;; ----- defconstant components -----

(defun defconstant-name (defconstant-body)
  (car defconstant-body))

(defun defconstant-initial-value (defconstant-body)
  (cadr defconstant-body))

(defun defconstant-documentation (defconstant-body)
  (caddr defconstant-body))

(def-with-components defconstant name initial-value documentation)


;; ----- defparameter components -----

(defun defparameter-name (defparameter-body)
  (car defparameter-body))

(defun defparameter-initial-value (defparameter-body)
  (cadr defparameter-body))

(defun defparameter-documentation (defparameter-body)
  (caddr defparameter-body))

(def-with-components defparameter name initial-value documentation)


;; ----- defvar components -----

(defun defvar-name (defvar-body)
  (car defvar-body))

(defun defvar-initial-value (defvar-body)
  (cadr defvar-body))

(defun defvar-documentation (defvar-body)
  (caddr defvar-body))

(def-with-components defvar name initial-value documentation)


;; ----- defun components -----

(defun defun-function-name (defun-body)
  (car defun-body))

(defun defun-lambda-list (defun-body)
  (cadr defun-body))

(defun defun-declarations (defun-body)
  (let ((post-lambda-list (cddr defun-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun defun-documentation (defun-body)
  (let ((post-lambda-list (cddr defun-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun defun-forms (defun-body)
  (let ((post-lambda-list (cddr defun-body)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally return expr)))

(def-with-components defun function-name lambda-list declarations documentation forms)


;; ----- defmacro components -----

(defun defmacro-name (defmacro-body)
  (car defmacro-body))

(defun defmacro-lambda-list (defmacro-body)
  (cadr defmacro-body))

(defun defmacro-declarations (defmacro-body)
  (let ((post-lambda-list (cddr defmacro-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun defmacro-documentation (defmacro-body)
  (let ((post-lambda-list (cddr defmacro-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun defmacro-forms (defmacro-body)
  (let ((post-lambda-list (cddr defmacro-body)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally return expr)))

(def-with-components defmacro name lambda-list declarations documentation forms)


;; ----- define-compiler-macro components -----

(defun define-compiler-macro-name (define-compiler-macro-body)
  (car define-compiler-macro-body))

(defun define-compiler-macro-lambda-list (define-compiler-macro-body)
  (cadr define-compiler-macro-body))

(defun define-compiler-macro-declarations (define-compiler-macro-body)
  (let ((post-lambda-list (cddr define-compiler-macro-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(defun define-compiler-macro-documentation (define-compiler-macro-body)
  (let ((post-lambda-list (cddr define-compiler-macro-body)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(defun define-compiler-macro-forms (define-compiler-macro-body)
  (let ((post-lambda-list (cddr define-compiler-macro-body)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally return expr)))

(def-with-components define-compiler-macro name lambda-list declarations documentation forms)


;; ----- defstruct -----

(defun defstruct-name-and-options (defstruct-body)
  (car defstruct-body))

(defun defstruct-options (defstruct-body)
  (let ((name-and-options (defstruct-name-and-options defstruct-body)))
    (if (listp name-and-options)
	(cdr name-and-options)
	nil)))

(defun defstruct-conc-name-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(or (car (member :conc-name options))
	    (car (member :conc-name options :key #'car)))
	nil)))

(defun defstruct-constructor-options (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (remove-if-not (lambda (x)
		     (if (listp x)
			 (eq (car x) :constructor)
			 (eq x :constructor)))
		   options)))

(defun defstruct-copier-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(or (car (member :copier options))
	    (car (member :copier options :key #'car)))
	nil)))

(defun defstruct-predicate-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(or (car (member :predicate options))
	    (car (member :predicate options :key #'car)))
	nil)))

(defun defstruct-include-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(or (car (member :include options))
	    (car (member :include options :key #'car)))
	nil)))

(defun defstruct-printer-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(or (car (member :print-object options :key #'car))
	    (car (member :print-function options :key #'car)))
	nil)))

(defun defstruct-print-object-option (defstruct-body)
  (let ((printer-option (defstruct-printer-option defstruct-body)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-object))
	printer-option
	nil)))

(defun defstruct-print-function-option (defstruct-body)
  (let ((printer-option (defstruct-printer-option defstruct-body)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-function))
	printer-option
	nil)))

(defun defstruct-type-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(car (member :type options :key #'car))
	nil)))

(defun defstruct-named-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(car (member :named options))
	nil)))

(defun defstruct-initial-offset-option (defstruct-body)
  (let ((options (defstruct-options defstruct-body)))
    (if (not (null options))
	(car (member :initial-offset options :key #'car))
	nil)))

(defun defstruct-slot-descriptions (defstruct-body)
  (let ((possible-descriptions (cdr defstruct-body)))
    (if (documentationp (car possible-descriptions))
	(cdr possible-descriptions)
	possible-descriptions)))

(defun defstruct-slot-options (defstruct-body)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-body)))
    (loop for slot-description in slot-descriptions
	  collect (if (listp slot-description)
		      (cddr slot-description)
		      nil))))

(defun defstruct-conc-name (defstruct-body)
  (let ((conc-name-option (defstruct-conc-name-option defstruct-body)))
    (and conc-name-option
	 (if (listp conc-name-option)
	     (cadr conc-name-option)
	     nil))))

(defun defstruct-constructor-arglists (defstruct-body)
  (let ((constructor-options (defstruct-constructor-options defstruct-body)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (caddr constructor-option)
			   nil)))))

(defun defstruct-constructor-names (defstruct-body)
  (let ((constructor-options (defstruct-constructor-option)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (cadr constructor-option)
			   nil)))))

(defun defstruct-copier-name (defstruct-body)
  (let ((copier-option (defstruct-copier-option defstruct-body)))
    (and copier-option
	 (if (listp copier-option)
	     (cadr copier-option)
	     nil))))

(defun defstruct-included-structure-name (defstruct-body)
  (let ((include-option (defstruct-include-option defstruct-body)))
    (and include-option
	 (cadr include-option))))

(defun defstruct-include-option-slot-descriptors (defstruct-body)
  (let ((include-option (defstruct-include-option defstruct-body)))
    (and include-option
	 (cddr include-option))))

(defun defstruct-initial-offset (defstruct-body)
  (let ((initial-offset-option (defstruct-initial-offset-option defstruct-body)))
    (and initial-offset-option
	 (cadr initial-offset-option))))

(defun defstruct-predicate-name (defstruct-body)
  (let ((predicate-option (defstruct-predicate-option defstruct-body)))
    (and predicate-option
	 (cadr predicate-option))))

(defun defstruct-printer-name (defstruct-body)
  (let ((printer-option (defstruct-printer-option)))
    (and printer-option
	 (cadr printer-option))))

(defun defstruct-slot-names (defstruct-body)
  (let ((slot-descriptions (defstruct-slot-descriptions)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (car slot-description)
			   slot-description)))))

(defun defstruct-slot-initforms (defstruct-body)
  (let ((slot-descriptions (defstruct-slot-descriptions)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (cadr slot-description)
			   nil)))))

(defun defstruct-slot-types (defstruct-body)
  (let ((slot-options (defstruct-slot-options)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :type slot-option))))))

(defun defstruct-slot-read-only-ps (defstruct-body)
  (let ((slot-options (defstruct-slot-options)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :read-only slot-option))))))

(defun defstruct-type (defstruct-body)
  (let ((type-option (defstruct-type-option defstruct-body)))
    (and type-option
	 (cadr type-option))))

(defun defstruct-documentation (defstruct-body)
  (let ((possible-documentation (cadr defstruct-body)))
    (when (documentationp possible-documentation)
      possible-documentation)))

(def-with-components defstruct name-and-options options conc-name-option constructor-option
  copier-option predicate-option include-option printer-option print-object-option
  print-function-option type-option named-option initial-offset-option slot-descriptions
  slot-options conc-name constructor-arglists constructor-names copier-name
  included-structure-name include-option-slot-descriptions initial-offset predicate-name
  printer-name slot-names slot-initforms slot-read-only-ps slot-types structure-name type
  documentation)


;; ----- defpackage components -----

(defun defpackage-options (defpackage-body)
  (cdr defpackage-body))

(defun defpackage-defined-package-name (defpackage-body)
  (car defpackage-body))

(defun defpackage-use-package-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :use))
				 options))))

(defun defpackage-shadow-symbol-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :shadow))
				 options))))

(defun defpackage-shadowing-import-from-package-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				  options))))

(defun defpackage-shadowing-import-from-symbol-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				 options))))

(defun defpackage-import-from-package-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				  options))))

(defun defpackage-import-from-symbol-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				 options))))
