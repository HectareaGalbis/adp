

(defpackage #:adp
  (:use #:cl #:alexandria)
  (:shadow #:defclass #:defconstant #:defgeneric #:define-compiler-macro #:define-condition
	   #:define-method-combination #:define-modify-macro #:define-setf-expander #:define-symbol-macro
	   #:defmacro #:defmethod #:defpackage #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar)
  (:export #:header #:subheader #:subsubheader #:text #:table #:itemize #:image #:bold #:italic #:code-inline
	   #:web-link #:header-ref #:symbol-ref #:function-ref #:type-ref #:code-block #:code-example
	   #:defclass #:defconstant #:defgeneric #:define-compiler-macro #:define-condition
	   #:define-method-combination #:define-modify-macro #:define-setf-expander #:define-symbol-macro
	   #:defmacro #:defmethod #:defpackage #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar))
