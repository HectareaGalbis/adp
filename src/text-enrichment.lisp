
(in-package :adppvt)


(defmacro define-text-enrichment-element (name)
  (let ((class-doc (concatenate 'string "Represents a " (string-downcase (symbol-name name)) " element."))
	(make-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
	(make-doc (concatenate 'string (symbol-name name) " element constructor."))
	(writer-proc (intern (concatenate 'string "*" (symbol-name name) "-WRITER*")))
	(writer-definer (intern (concatenate 'string "DEFINE-" (symbol-name name) "-WRITER"))))
    (with-gensyms (text stream)
      `(progn

	 (defclass ,name (element)
	   ((text :initarg :text
		  :type text)
	    (top-level :initform nil))
	   (:documentation
	    ,class-doc))

	 (define-customizable-writer ,writer-proc ,writer-definer 2)

	 (defmethod element-print ((,element ,name) ,stream)
	   (funcall ,writer-proc ,stream (text-to-string (slot-value ,element 'text))))))))


(define-text-enrichment-element bold)
(define-text-enrichment-element italic)
(define-text-enrichment-element bold-italic)
(define-text-enrichment-element code-inline)
