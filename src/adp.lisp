
(in-package #:adp)


;; ------ elements ------
(defclass scribble-element ()
  ((value :initarg :value
          :reader element-value)
   (form :initarg :form
         :reader element-form))
  (:documentation
   "Represents an element in a scribble file."))

;; ------ files ------
(defclass scribble-file ()
  ((component :initarg :component
              :reader file-component
              :type asdf:source-file)
   (elements :initarg :elements
             :reader file-elements
	     :type list))
  (:documentation
   "Represents a unit of documentation that groups several elements."))


(defvar *files* nil
  "A container with all the content files. The keys are the pathnames of the file relative to the asdf root file.")

(cl:defun make-files-container ()
  "Initializes the files container."
  (setf *files* (make-array 16 :fill-pointer 0)))

(cl:defun destroy-files-container ()
  "Destroys the files container."
  (setf *files* nil))

(cl:defun add-file (file)
  "Adds a file to the files container."
  (vector-push-extend file *files*))


;; ------ adp-system ------
(defclass adp-system (asdf:system) ())

(defmethod asdf:operation-done-p ((o asdf:prepare-op) (c adp-system))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:operation-done-p ((o asdf:load-op) (c adp-system))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:perform :after ((o asdf:prepare-op) (s adp-system))
  (make-files-container))

(defmethod asdf:perform :after ((o asdf:load-op) (s adp-system))
  (export-content s (coerce *files* 'list))
  (destroy-files-container))


;; ------ scribble-source-file ------
(defclass scribble-source-file (asdf:source-file)
  ((type :initform "scrbl"))
  (:documentation "Component class for a Scribble source file"))

(defmethod asdf:operation-done-p ((o asdf:load-op) (c scribble-source-file))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:perform ((o asdf:prepare-op) (c scribble-source-file))
  (values))

(defmethod asdf:perform ((o asdf:compile-op) (c scribble-source-file))
  (values))

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

(cl:defun eval-expression (expr file)
  (handler-case (eval expr)
    (error (c)
      (error 'eval-error :error-condition c :expression expr :file file))))

(cl:defun in-package-expr-p (expr)
  (and (listp expr)
       (eq (car expr) 'in-package)))

(cl:defun in-readtable-expr-p (expr)
  (and (listp expr)
       (eq (car expr) 'named-readtables:in-readtable)))

(cl:defun get-lisp-expressions-and-stream (file)
  (let (package-expr readtable-expr (expr-count 0))
    (with-open-file (stream file)
      (loop for expr = (ignore-errors (read stream nil))
            while expr
            do (cond
                 ((in-package-expr-p expr)
                  (setf package-expr expr))
                 ((in-readtable-expr-p expr)
                  (setf readtable-expr expr))
                 (t
                  (return)))
               (incf expr-count)))
    (let ((stream (open file)))
      (loop for i from 0 below expr-count
            do (read stream nil))
      (values package-expr readtable-expr stream))))

(cl:defmacro with-lisp-expressions-and-stream ((in-package-expr in-readtable-expr stream) file &body body)
  (alexandria:with-gensyms (stream-sym)
    `(multiple-value-bind (,in-package-expr ,in-readtable-expr ,stream)
         (get-lisp-expressions-and-stream ,file)
       (let ((,stream-sym ,stream))
         (unwind-protect
              (progn ,@body)
           (close ,stream-sym))))))

(defmethod asdf:perform ((o asdf:load-op) (c scribble-source-file))
  (let ((file (first (asdf:input-files o c))))
    (with-lisp-expressions-and-stream (in-package-expr in-readtable-expr stream) file
      (let ((*package* *package*)
            (*readtable* (named-readtables:find-readtable :scribble)))
        (when in-package-expr
          (eval-expression in-package-expr file))
        (when in-readtable-expr
          (eval-expression in-readtable-expr file))
        (let* ((file-content (alexandria:read-stream-content-into-string stream))
               (expressions (cdr (read-from-string (format nil "@list{~a}" file-content))))
               (elements (loop for expr in expressions
                               collect (make-instance 'scribble-element
                                                      :value (eval-expression expr file)
                                                      :form expr)))
               (content-file (make-instance 'scribble-file :component c :elements elements)))
          (add-file content-file))))))


;; ------ exporter ------
(cl:defun reexport (symbol package)
  (import symbol package)
  (export symbol package))

(cl:defmacro define-adp-system (name)
  "Defines an adp system."
  (check-type name symbol)
  `(progn
     (defclass ,name (adp-system) ())
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (reexport ',name "ASDF")
       (reexport ',name "ASDF/BUNDLE"))))

(cl:defmacro define-adp-file (name &optional type)
  "Defines an adp file."
  (check-type name symbol)
  (check-type type (or string null))
  `(progn
     (defclass ,name (scribble-source-file)
       ,(and type `((type :initform ,type))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (reexport ',name "ASDF")
       (reexport ',name "ASDF/BUNDLE"))))

(defgeneric export-content (system files)
  (:documentation
   "Exports the files gathered by ADP.")
  (:method (system files)
    (declare (ignore files))
    (error "The system of class ~s is not an ADP system." (class-of system))))
