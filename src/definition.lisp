
(in-package :adppvt)


(defclass definition (element)
  ((code :initarg :code))
  (:documentation
   "Represent a definition element."))


(defclass tagged-definition (definition)
  ((tag :initarg :tag
	:type symbol))
  (:documentation
   "Represent a tagged definition element."))


(defclass symbol-definition (tagged-definition) ()
  (:documentation
   "Represent a symbol tag definition element."))

(defclass function-definition (tagged-definition) ()
  (:documentation
   "Represent a function tag definition element."))

(defclass type-definition (tagged-definition) ()
  (:documentation
   "Represent a type tag definition element."))


(defmacro define-definition-type (name super-type)
  "Define a child class of definition with a customizable writer and a method to print the element."
  (check-type name symbol)
  (let ((proc-name (intern (concatenate 'string "*" (symbol-name name) "-WRITER*")))
	(class-documentation (concatenate 'string "Represents a " (string-downcase (symbol-name name)) " element."))
	(writer-name (intern (concatenate "DEFINE-" (symbol-name name) "-WRITER"))))
    (with-gensyms (code element stream)
      `(progn

	 (defclass ,name (definition)
	   (:documentation
	    ,class-documentation))

	 (define-customizable-writer ,proc-name ,writer-name 2)

	 (defmethod element-print ((,element ,name) ,stream)
	   (with-slots ((,code code)) ,element
	     (funcall ,writer-name ,stream ,code)))))))

(define-definition-type defclass                  type-definition)
(define-definition-type defconstant               symbol-definition)
(define-definition-type defgeneric                function-definition)
(define-definition-type define-compiler-macro     defintion)
(define-definition-type define-condition          type-definition)
(define-definition-type define-method-combination definition)
(define-definition-type define-modify-macro       function-definition)
(define-definition-type define-setf-expander      definition)
(define-definition-type define-symbol-macro       symbol-definition)
(define-definition-type defmacro                  function-definition)
(define-definition-type defmethod                 definition)
(define-definition-type defpackage                definition)
(define-definition-type defparameter              symbol-definition)
(define-definition-type defsetf                   definition)
(define-definition-type defstruct                 type-definition)
(define-definition-type deftype                   type-definition)
(define-definition-type defun                     function-definition)
(define-definition-type defvar                    symbol-definition)
