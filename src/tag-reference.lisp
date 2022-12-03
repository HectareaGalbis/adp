
(in-package :adppvt)


(defclass tag-reference (element)
  ((tag :initarg :tag
	:type symbol)))


(defclass header-ref (tag-reference)
  ((header-tags :initarg :header-tags
		:type tag-table))
  (:documentation
   "Represent a header reference element."))

(define-customizable-writer *header-ref-writer* define-header-ref-writer 4)

(defmethod element-print ((element header-ref) stream)
  (with-slots (tag header-tags) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements header-tags tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *header-ref-writer* stream tag file-location))))))


(defmacro define-tag-reference-type (name tag-table writer-proc writer-definer num-args docstring)
  (with-gensyms (element stream tag associated-elements element-found-p file-location)
    `(progn

       (defclass ,name (tag-reference)
	 ((,tag-table :initarg ,(intern (symbol-name tag-table) "KEYWORD")
		      :allocation :class
		      :type tag-table))
	 (:documentation
	  ,docstring))

       (define-customizable-writer ,writer-proc ,writer-definer ,num-args)

       (defmethod element-print ((,element ,name) ,stream)
	 (with-slots (,tag ,tag-table) ,element
	   (multiple-value-bind (,associated-elements ,element-found-p) (tag-table-find-elements ,tag-table ,tag)
	     (if ,element-found-p
		 (with-slots ((,file-location file-location)) (aref associated-elements 0)
		   (funcall ,writer-proc ,stream ,tag ,file-location)))))))))

(define-tag-reference-type symbol-ref symbol-tags *symbol-ref-writer* define-symbol-ref-writer 3
  "Represent a symbol reference element.")
(define-tag-reference-type function-ref function-tags *function-ref-writer* define-function-ref-writer 3
  "Represent a function reference element.")
(define-tag-reference-type type-ref type-tags *type-ref-writer* define-type-ref-writer 3
  "Represent a type reference element.")


(defclass code-reference (element)
  ((code-tag :initarg :code-tag
	     :type symbol))
  (:documentation
   "Represent a code reference element."))



