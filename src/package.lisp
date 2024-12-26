
(defpackage #:adp
  (:use #:cl)
  (:shadow #:defun #:defmacro)
  (:export #:file-component
           #:file-elements
           #:element-value
           #:element-form
           #:define-adp-system
           #:define-adp-file
           #:export-content
           #:defun
           #:defmacro
           #:function-lambda-list))
