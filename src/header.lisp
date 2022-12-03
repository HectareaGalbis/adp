
(in-package :adppvt)


(defclass header-type (tagged-element)
  ((title :initarg :title
	  :type string))
  (:documentation
   "Represents a header type element."))


(defmacro define-header-type (name)
  "Define a child class of header-type with a customizable writer and a method to print the
element."
  (check-type name symbol)
  (let ((proc-name (intern (concatenate 'string "*" (symbol-name name) "-WRITER*")))
	(class-documentation (concatenate 'string "Represents a " (string-downcase (symbol-name name)) " element."))
	(writer-name (intern (concatenate "DEFINE-" (symbol-name name) "-WRITER"))))
    (with-gensyms (title tag body element stream)
      `(progn

	 (defclass ,name (header-type) ()
	   (:documentation
	    ,class-documentation))

	 (define-customizable-writer ,proc-name ,writer-name 3)

	 (defmethod element-print ((,element ,name) ,stream)
	   (with-slots ((,tittle tittle) (,tag tag)) ,element
	     (funcall ,writer-name ,stream ,tittle ,tag)))))))

(define-header-type header)
(define-header-type subheader)
(define-header-type subsubheader)
