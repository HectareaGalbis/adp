
(in-package :adppvt)


(define-condition text-subelement-error (error)
  ((text :initarg :text
	 :type text-type)
   (subelement :initarg :subelement
	       :type element))
  (:report (lambda (condition stream)
	     (with-slots (text subelement) condition
	       (format stream "ADP error at '~a': A ~a cannot be used inside a ~a.~%"
		       (slot-value text 'source-location) (slot-value subelement 'name) (slot-value text 'name))))))


(define-condition table-subelement-error (error)
  ((table :initarg :table
	    :type itemize-type)
   (subelement :initarg :subelement))
  (:report (lambda (condition stream)
	     (with-slots (table subelement) condition
	       (format stream "ADP error at '~a': ~a only accepts cell forms, but ~s was found.~%"
		       (slot-value table 'source-location) (slot-value table 'name) subelement)))))


(define-condition null-itemize-error (error)
  ((itemize :initarg :itemize
	    :type itemize-type))
  (:report (lambda (condition stream)
	     (with-slots (itemize) condition
	       (format stream "ADP error at '~a': ~a must receive at least one argument.~%"
		       (slot-value itemize 'source-location) (slot-value itemize 'name))))))


(define-condition itemize-first-element-not-item-error (error)
  ((itemize :initarg :itemize
	    :type itemize-type)
   (subelement :initarg :subelement))
  (:report (lambda (condition stream)
	     (with-slots (itemize subelement) condition
	       (format stream "ADP error at '~a': The first element of ~a must be an item form, but ~s was found.~%"
		       (slot-value itemize 'source-location) (slot-value itemize 'name) subelement)))))


(define-condition itemize-subelement-error (error)
  ((itemize :initarg :itemize
	    :type itemize-type)
   (subelement :initarg :subelement))
  (:report (lambda (condition stream)
	     (with-slots (itemize subelement) condition
	       (format stream "ADP error at '~a': ~a only accepts item, itemize or enumerate, but ~s was found..~%"
		       (slot-value itemize 'source-location) (slot-value itemize 'name) subelement)))))


(define-condition file-not-selected-error (error)
  ((first-element :initarg :first-element
		  :type element))
  (:report (lambda (condition stream)
	     (with-slots (first-element) condition
	       (format stream "ADP error at '~a': ~a is used before selecting a file. Please, use adp:in-file."
		       (slot-value first-element 'source-location) (slot-value first-element 'name))))))


(define-condition already-defined-tag-error (error)
  ((source-element :initarg :source-element
		   :type element)
   (previous-source-element :initarg :previous-source-element
			    :type :element)
   (tag :initarg :tag
	:type symbol))
  (:report (lambda (condition stream)
	     (with-slots (source-element previous-source-element tag) condition
	       (format stream "ADP error at '~a': The tag ~s defined using ~a was already defined at '~a'.~%"
		       (slot-value source-element 'source-location) tag (slot-value source-element 'name)
		       (slot-value previous-source-element 'source-location))))))


(define-condition tag-not-defined-error (error)
  ((source-element :initarg :source-element
		   :type element)
   (tag :initarg :tag
	:type symbol))
  (:report (lambda (condition stream)
	     (with-slots (source-element tag) condition
	       (format stream "ADP error at '~a': The tag ~s used in ~a is not defined."
		       (slot-value source-element 'source-location) tag (slot-value source-element 'name))))))



