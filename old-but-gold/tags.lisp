
(in-package #:adp-core)


;; (defclass tag ()
;;   ((symbol :initarg :symbol
;;            :type symbol)
;;    (path :initarg :path
;;          :type pathname)))


(defvar *tags* (make-hash-table))

(defun ensure-tag-table (type)
  "Returns the hash table of tags of a given type."
  (or (gethash type *tags*)
      (setf (gethash type *tags*) (make-hash-table))))

(defun add-tag (symbol type)
  "Adds or updates a tag."
  (let ((tag-table (ensure-tag-table type)))
    (setf (gethash symbol tag-table) *current-target-pathname*))
  (values))

(defun tag-pathname (symbol type)
  "Retreives the pathname of a tag or nil if tag does not exist."
  (let* ((tag-table (ensure-tag-table type)))
    (gethash symbol tag-table)))
