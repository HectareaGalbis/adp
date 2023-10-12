
(in-package #:adp)


;; ------ adp ------
(defvar *adp* nil
  "Indicates whether adp is generating documentation.")


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

(defun file-add-element (file element)
  "Adds an element to a file."
  (with-slots (elements) file
    (vector-push-extend element elements)))


(defvar *current-content-file* nil
  "The current content file where the elements must be added.
Used while loading a common lisp source file.")

(defun current-file-add-element (element)
  (file-add-element *current-content-file* element))

(defmacro add-element (element)
  "Adds an element to the current selected content file."
  (when *adp*
    `(progn
       (current-file-add-element ,element)
       (current-file-add-element (format nil "~%"))
       (current-file-add-element (format nil "~%")))))


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

(defclass scribble (scribble-source-file) ())

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
  (let ((*adp* t)
        (*current-content-file* (make-instance 'file :component c)))
    (pre-process-file o c)
    (load (first (asdf:input-files o c))
          :external-format (asdf:component-external-format c))
    (when (> (length (file-elements *current-content-file*)) 0)
      (add-file *current-content-file*))
    (post-process-file o c)))

(defun get-package-and-stream (file)
  (let* ((file-strm (open file))
         (in-package-expr (let ((expr (read file-strm nil)))
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

(defmethod asdf:perform ((o adp-op) (c scribble-source-file))
  (pre-process-file o c)
  (let ((file (first (asdf:input-files o c))))
    (with-package-and-stream (package-expr strm file)
      (let* ((file-content (alexandria:read-stream-content-into-string strm))
             (scribble-form (format nil "@vector{~a}" file-content))
             (elements (eval (let ((*package* *package*))
                               (when package-expr
                                 (eval package-expr))
                               (read-from-string scribble-form))))
             (content-file (make-instance 'file :component c :elements elements)))
        (add-file content-file)
        (post-process-file o c)))))

(defmethod asdf:perform ((o adp-op) (c asdf:static-file))
  (values))

(defmethod asdf:perform ((o adp-op) (c asdf:system))
  (post-process-system o c)
  (export-content o *files* c)
  (destroy-files-container))

(defmethod asdf:operation-done-p ((o adp-op) (c asdf:component))
  "We can't know if output files has been modified or removed. So, operation must always be done."
  nil)

(defun reexport (symbol package)
  (import symbol package)
  (export symbol package))

(reexport 'scribble "ASDF")
(reexport 'scribble "ASDF/BUNDLE")


;; ------ exporter ------
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

(defgeneric scribble-package (op f)
  (:documentation
   "Returns the package to be used while loading scribble files.")
  (:method ((op adp-op) f)
    (find-package "CL-USER")))


;; ------ scribble ------
(defun read-sheat-syntax (s c n)
  (declare (ignore n))
  `(add-element ,(scribble::read-at-syntax s c)))

(scribble:enable-scribble-at-syntax)
(set-dispatch-macro-character #\# #\@ #'read-sheat-syntax)
