
(defpackage #:adp
  (:use #:cl)
  (:shadow #:defun #:defmacro)
  
  (:export #:define-adp-system
           #:define-adp-file
           
           #:export-content
           #:file-component
           #:file-elements
           #:element-value
           #:element-form
           
           #:defun
           #:defmacro
           #:function-lambda-list))
