
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
    `(current-file-add-element ,element)))


(defparameter *files* (make-hash-table :test 'equal)
  "A hash table with all the content files. The keys are the pathnames of the file relative to the asdf root file.")

(defun make-files-container ()
  "Initializes the files container."
  (setf *files* (make-hash-table :test 'equal)))

(defun destroy-files-container ()
  "Destroys the files container."
  (setf *files* nil))

(defun add-file (file)
  "Adds a file to the files container."
  (let ((path (asdf:component-relative-pathname (file-component file))))
    (setf (gethash path *files*) file)))


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

;; (defmethod action-description ((o prepare-adp-op) (c asdf:component))
;;   (format nil (compatfmt "~@<generating documentation for dependencies of ~3i~_~A~@:>") c))

;; (defmethod input-files ((o prepare-adp-op) (s asdf:system))
;;   (asdf:if-let (it (system-source-file s)) (list it)))

(defmethod asdf:perform ((o prepare-adp-op) (c asdf:component))
  (values))

(defmethod asdf:perform ((o prepare-adp-op) (s asdf:system))
  (make-files-container))


(defclass adp-op (;; asdf::basic-load-op
                  asdf:downward-operation asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'prepare-adp-op :allocation :class))
  (:documentation "Operation for loading and generating documentation."))

;; (defmethod action-description ((o adp-op) (c asdf:component))
;;   (format nil (asdf:compatfmt "~@<Generating documentation of ~3i~_~A~@:>") c))

;; (defmethod action-description ((o adp-op) (c asdf:parent-component))
;;   (format nil (asdf:compatfmt "~@<Generated documentation of ~3i~_~A~@:>") c))

(defmethod asdf:perform ((o adp-op) (c asdf:cl-source-file))
  (let ((*adp* t)
        (*current-content-file* (make-instance 'file :component c)))
    (load (first (asdf:input-files o c))
          :external-format (asdf:component-external-format c))
    (when (> (length (file-elements *current-content-file*)) 0)
      (add-file *current-content-file*)))
  ;; (asdf:call-with-around-compile-hook
  ;;  c #'(lambda ()
  ;;        (let ((*adp* t)
  ;;              (*current-content-file* (make-instance 'file :component c)))
  ;;          (load* (first (input-files o c))
  ;;                 :external-format (component-external-format c))
  ;;          (when (> (length (file-elements *current-content-file*)) 0)
  ;;            (add-file *current-content-file*)))))
  )

(defmethod asdf:perform ((o adp-op) (c scribble-source-file))
  (let* ((file (first (asdf:input-files o c)))
         (file-content (alexandria:read-file-into-string file))
         (scribble-form (format nil "@vector{~a}" file-content))
         (elements (eval (let ((*package* (find-package "ADP-USER")))
                           (read-from-string scribble-form))))
         (content-file (make-instance 'file :component c :elements elements)))
    (add-file content-file)))

(defmethod asdf:perform ((o adp-op) (c asdf:static-file))
  (values))

(defmethod asdf:perform ((o adp-op) (c asdf:system))
  (export-content o *files* c)
  (destroy-files-container))


(defun reexport (symbol package)
  (import symbol package)
  (export symbol package))

(reexport 'scribble "ASDF")
(reexport 'scribble "ASDF/BUNDLE")


;; ------ exporter ------
(defmacro define-adp-operation (name)
  "Defines an adp operation."
  (check-type name symbol)
  `(progn
     (defclass ,name (adp-op) ())
     (reexport ',name "CL-USER")
     (reexport ',name "ASDF")
     (reexport ',name "ASDF/BUNDLE")))

(defmacro define-adp-file (name &optional type)
  "Defines an adp file."
  (check-type name symbol)
  (check-type type (or string null))
  `(progn
     (defclass ,name (scribble-source-file)
       ,(and type `((type :initform ,type))))
     (reexport ',name "ASDF")
     (reexport ',name "ASDF/BUNDLE")))

(defgeneric export-content (op files system)
  (:documentation
   "Exports the files gathered by ADP."))

(defun export-adp-symbol (sym)
  "Imports a symbol into the adp-user package and then exports it."
  (check-type sym symbol)
  (import sym "ADP-USER"))


;; ------ scribble ------
(scribble:enable-scribble-at-syntax)
