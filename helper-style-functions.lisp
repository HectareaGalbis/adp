

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


;; ----- defclass components -----

(defun defclass-class-name (defclass-body)
  (car defclass-body))

(defun defclass-superclass-names (defclass-body)
  (cadr defclass-body))

(defun defclass-slot-specifiers (defclass-body)
  (caddr defclass-body))

(defun defclass-slot-names (defclass-body)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-body)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (car slot-specifier)
			   slot-specifier)))))

(defun defclass-slot-options (defclass-body)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-body)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (cdr slot-specifier)
			   nil)))))

(defun defclass-reader-function-names (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(defun defclass-writer-function-names (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(defun defclass-accessor-function-names (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(defun defclass-allocation-types (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(defun defclass-initarg-names (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(defun defclass-initforms (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(defun defclass-type-specifiers (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(defun defclass-slot-documentations (defclass-body)
  (let ((slot-options (defclass-slot-options defclass-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :documentation slot-option)))))))

(defun defclass-class-options (defclass-body)
  (cdddr defclass-body))

(defun defclass-default-initargs (defclass-body)
  (let ((class-options (defclass-class-options defclass-body)))
    (and class-options
	 (cdr (member :default-initargs class-options :key #'car)))))

(defun defclass-documentation (defclass-body)
  (let ((class-options (defclass-class-options defclass-body)))
    (and class-options
	 (cadr (member :documentation class-options :key #'car)))))

(defun defclass-metaclass (defclass-body)
  (let ((class-options (defclass-class-options defclass-body)))
    (and class-options
	 (cadr (member :metaclass class-options :key #'car)))))

(def-with-components defclass class-name superclass-names slot-specifiers slot-names slot-options
  reader-function-names writer-function-names accessor-function-names allocation-types
  initarg-names initforms type-specifiers slot-documentations class-options default-initargs
  documentation metaclass)


;; ----- defconstant components -----

(defun defconstant-name (defconstant-body)
  (car defconstant-body))

(defun defconstant-initial-value (defconstant-body)
  (cadr defconstant-body))

(defun defconstant-documentation (defconstant-body)
  (caddr defconstant-body))

(def-with-components defconstant name initial-value documentation)


;; ----- defgeneric components -----

(defun defgeneric-function-name (defgeneric-body)
  (car defgeneric-body))

(defun defgeneric-gf-lambda-list (defgeneric-body)
  (cadr defgeneric-body))

(defun defgeneric-options (defgeneric-body)
  (remove-if (lambda (x)
	       (eq (car x) :method))
	     (cddr defgeneric-body)))

(defun defgeneric-argument-precedence-order (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cdr (member :argument-precedence-order :key #'car)))))

(defun defgeneric-gf-declarations (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cdr (member 'cl:declare :key #'car)))))

(defun defgeneric-documentation (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cadr (member :documentation :key #'car)))))

(defun defgeneric-method-combination (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cdr (member :method-combination :key #'car)))))

(defun defgeneric-generic-function-class (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cadr (member :generic-function-class :key #'car)))))

(defun defgeneric-method-class (defgeneric-body)
  (let ((options (defgeneric-options defgeneric-body)))
    (and options
	 (cadr (member :method-class :key #'car)))))

(defun defgeneric-method-descriptions (defgeneric-body)
  (remove-if-not (lambda (x)
		   (eq (car x) :method))
		 (cddr defgeneric-body)))

(def-with-components function-name gf-lambda-list options argument-precedence-order gf-declarations
  documentation method-combination generic-function-class method-class method-descriptions)


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


;; ----- define-condition components -----

(defun define-condition-name (define-condition-body)
  (car define-condition-body))

(defun define-condition-parent-types (define-condition-body)
  (cadr define-condition-body))

(defun define-condition-slot-specs (define-condition-body)
  (caddr defince-condition-body))

(defun define-condition-slot-names (define-condition-body)
  (let ((slot-specs (define-condition-slot-specs define-condition-body)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect slot-spec
	       else
		 collect (car slot-spec)))))

(defun define-condition-slot-options (define-condition-body)
  (let ((slot-specs (define-condition-slot-specs defince-condition-body)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect nil
	       else
		 collect (cdr slot-spec)))))

(defun define-condition-slot-readers (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-writers (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-accessors (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-allocations (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(defun define-condition-slot-initargs (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(defun define-condition-slot-initforms (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(defun define-condition-slot-types (define-condition-body)
  (let ((slot-options (define-condition-slot-options define-condition-body)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(defun define-condition-options (define-condition-body)
  (cdddr define-condition-body))

(defun define-condition-default-initargs (define-condition-body)
  (let ((options (define-condition-options define-condition-body)))
    (and options
	 (cdr (member :default-initargs options :key #'car)))))

(defun define-condition-documentation (define-condition-body)
  (let ((options (define-condition-options define-condition-body)))
    (and options
	 (cadr (member :documentation options :key #'car)))))

(defun define-condition-report-name (define-condition-body)
  (let ((options (define-condition-options define-condition-body)))
    (and options
	 (cadr (member :report options :key #'car)))))

(def-with-components define-condition name parent-types slot-specs slot-names slot-options slot-readers
  slot-writers slot-accessors slot-allocations slot-initargs slot-initforms slot-types options
  default-initargs documentation report-name)


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








;; ----- defmethod components -----

(defun defmethod-function-name (defmethod-body)
  (car defmethod-body))

(defun defmethod-method-qualifiers (defmethod-body)
  (loop for qualifier in (cdr defmethod-body)
	while (not (listp qualifier))
	collect qualifier))

(defun defmethod-specialized-lambda-list (defmethod-body)
  (loop for possible-lambda-list in (cdr defmethod-body)
	when (listp possible-lambda-list)
	  return possible-lambda-list))

(defun defmethod-declarations (defmethod-body)
  (loop for rest-defmethod-body on (cdr defmethod-body)
	when (listp (car rest-defmathod-body))
	  return (loop for possible-declaration in (cdr rest-defmethod-body)
		       while (or (declarationp possible-declaration)
				 (documentationp possible-declaration))
		       if (declarationp possible-declaration)
			 collect possible-declaration)))

(defun defmethod-documentation (defmethod-body)
  (loop for rest-defmethod-body on (cdr defmethod-body)
	when (listp (car rest-defmathod-body))
	  return (loop for possible-documentation in (cdr rest-defmethod-body)
		       while (or (declarationp possible-documentation)
				 (documentationp possible-documentation))
		       if (documentationp possible-documentation)
			 return possible-documentation)))

(defun defmethod-forms (defmethod-body)
  (loop for rest-defmethod-body on (cdr defmethod-body)
	when (listp (car rest-defmathod-body))
	  return (loop for rest-possible-forms on (cdr rest-defmethod-body)
		       while (or (declarationp (car rest-possible-forms))
				 (documentationp (car rest-possible-forms)))
		       finally return rest-possible-forms)))

(def-with-components defmethod function-name method-qualifiers specialized-lambda-list declarations
  documentation forms)


;; ----- defpackage components -----

(defun defpackage-options (defpackage-body)
  (cdr defpackage-body))

(defun defpackage-defined-package-name (defpackage-body)
  (car defpackage-body))

(defun defpackage-nicknames (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :nicknames))
				 options))))

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

(defun defpackage-export-symbol-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(defun defpackage-import-symbol-names (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(defun defpackage-size (defpackage-body)
  (let ((options (defpackage-options defpackage-body)))
    (cadar (member :size options :key #'car))))

(def-with-components defpackage options defined-package-name nicknames use-package-names shadow-symbol-names
  shadowing-import-from-package-names shadowing-import-from-symbol-names import-from-package-names
  import-from-symbol-names export-symbol-names import-symbol-names size)
