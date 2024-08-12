
(in-package #:adp)


;; ------ files ------
(defclass file ()
  ((component :initarg :component
              :reader file-component
              :type asdf:source-file)
   (elements :initarg :elements
             :initform (make-array 100 :fill-pointer 0)
             :reader file-elements
	     :type vector))
  (:documentation
   "Represents a unit of documentation that groups several elements."))


(defvar *files* nil
  "A container with all the content files. The keys are the pathnames of the file relative to the asdf root file.")

(defun make-files-container ()
  "Initializes the files container."
  (setf *files* (make-array 16 :fill-pointer 0)))

(defun destroy-files-container ()
  "Destroys the files container."
  (setf *files* nil))

(defun add-file (file)
  "Adds a file to the files container."
  (vector-push-extend file *files*))


;; ------ adp-op ------
(defclass scribble-source-file (asdf:source-file)
  ((type :initform "scrbl"))
  (:documentation "Component class for a Scribble source file (using type \"scrbl\")"))

(defmethod asdf:perform ((o asdf:compile-op) (c scribble-source-file))
  (values))

(defmethod asdf:perform ((o asdf:load-op) (c scribble-source-file))
  (values))


(defclass prepare-adp-op (asdf:upward-operation asdf:sideway-operation)
  ((asdf:sideway-operation :initform 'asdf:load-op :allocation :class))
  (:documentation "Operation for loading and generating the documentation of dependencies."))

(defmethod asdf:perform ((o prepare-adp-op) (c asdf:component))
  (values))

(defmethod asdf:perform ((o prepare-adp-op) (s asdf:system))
  (pre-process-system (get-load-operation o) s)
  (make-files-container))

(defmethod asdf:operation-done-p ((o prepare-adp-op) (c asdf:component))
  "We can't know if output files has been modified or removed. So, operation must always be done."
  nil)


(defclass adp-op (asdf:downward-operation asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'prepare-adp-op :allocation :class))
  (:documentation "Operation for loading and generating documentation."))

(defmethod asdf:perform ((o adp-op) (c asdf:cl-source-file))
  (asdf:perform 'asdf:load-source-op c))

(defun get-package-and-stream (file)
  (let* ((file-strm (open file))
         (in-package-expr (let ((expr (ignore-errors (read file-strm nil))))
                            (and expr
                                 (listp expr)
                                 (eq (car expr) 'in-package)
                                 expr))))
    (if in-package-expr
        (values in-package-expr file-strm)
        (progn
          (close file-strm)
          (values nil (open file))))))

(defmacro with-package-and-stream ((package-name stream-name file) &body body)
  `(multiple-value-bind (,package-name ,stream-name) (get-package-and-stream ,file)
     (unwind-protect
          (progn ,@body)
       (close ,stream-name))))

(define-condition eval-error (error)
  ((error-condition :initarg :error-condition
                    :initform nil
                    :accessor error-condition)
   (expression :initarg :expression
               :initform nil
               :accessor expression)
   (file :initarg :file
         :initform nil
         :accessor file))
  (:report (lambda (c s)
             (format s "Error evaluating: ~s~%" (expression c))
             (format s "Error description: ~a~%" (error-condition c))
             (format s "File: '~a'~%" (file c)))))

(defun eval-expression (expr file)
  (handler-case (eval expr)
    (error (c)
      (error 'eval-error :error-condition c :expression expr :file file))))

(defmethod asdf:perform ((o adp-op) (c scribble-source-file))
  (pre-process-file o c)
  (let ((file (first (asdf:input-files o c))))
    (with-package-and-stream (package-expr strm file)
      (let ((*package* *package*)
            (*readtable* (named-readtables:find-readtable :scribble)))
        (when package-expr
          (eval-expression package-expr file))
        (let* ((file-content (alexandria:read-stream-content-into-string strm))
               (expressions (cdr (read-from-string (format nil "@list{~a}" file-content))))
               (elements (loop for expr in expressions
                               collect (eval-expression expr file)))
               (content-file (make-instance 'file :component c :elements (coerce elements 'vector))))
          (add-file content-file)
          (post-process-file o c))))))

(defmethod asdf:perform ((o adp-op) (c asdf:static-file))
  (values))

(defmethod asdf:perform ((o adp-op) (c asdf:system))
  (post-process-system o c)
  (export-content o *files* c)
  (destroy-files-container))

(defmethod asdf:operation-done-p ((o adp-op) (c asdf:component))
  "We can't know if output files has been modified or removed. So, operation must always be done."
  nil)


;; ------ exporter ------
(defun reexport (symbol package)
  (import symbol package)
  (export symbol package))

(defmacro define-adp-operation (name)
  "Defines an adp operation."
  (check-type name symbol)
  (let ((prepare-name (make-symbol (format nil "~a-PREPARE-OP" (symbol-name name)))))
    `(progn
       (defclass ,prepare-name (prepare-adp-op) ())
       (defclass ,name (adp-op)
         ((asdf:selfward-operation :initform ',prepare-name :allocation :class)))
       (defmethod get-load-operation ((o ,prepare-name))
         (asdf:make-operation (string-downcase (symbol-name ',name))))
       (reexport ',name "ASDF")
       (reexport ',name "ASDF/BUNDLE"))))

(defmacro define-adp-file (name &optional type)
  "Defines an adp file."
  (check-type name symbol)
  (check-type type (or string null))
  `(progn
     (defclass ,name (scribble-source-file)
       ,(and type `((type :initform ,type))))
     (reexport ',name "ASDF")
     (reexport ',name "ASDF/BUNDLE")))

(defgeneric get-load-operation (o)
  (:method ((o prepare-adp-op))
    (asdf:make-operation "adp-op")))

(defgeneric pre-process-system (op s)
  (:method ((op adp-op) s)
    (values)))

(defgeneric post-process-system (op s)
  (:method ((op adp-op) s)
    (values)))

(defgeneric pre-process-file (op f)
  (:method ((op adp-op) f)
    (values)))

(defgeneric post-process-file (op f)
  (:method ((op adp-op) f)
    (values)))

(defgeneric export-content (op files system)
  (:documentation
   "Exports the files gathered by ADP."))
