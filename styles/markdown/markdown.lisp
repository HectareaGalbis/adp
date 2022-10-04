
(in-package :adp)


;; ----- parameters -----

(defparameter *file-documentation* (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'string))

(defparameter *system-documentation* (make-array 10 :fill-pointer 0 :adjustable t :element-type 'string))

(defparameter *header-labels* nil)
(defparameter *name-labels* nil)
(defparameter *function-labels* nil)

(defparameter *code-tags* (make-hash-table))


;; ----- help macros -----

(defmacro with-stream-string (stream str &body body)
  `(let ((,str (make-array 100 :adjustable t :fill-pointer 0 :element-type 'character)))
     (with-output-to-string (,stream ,str)
       ,@body)))

;; ----- guide functions -----

(def-header-writer (text label)
    (when label
      (if (member label *header-labels*)
	  (warn "The label ~s is already used" label)
	  (setf *header-labels* (cons label *header-labels*))))
  (vector-push-extend (format nil "# ~a~%~%" text) *file-documentation*))


(def-subheader-writer (text label)
    (when label
      (if (member label *header-labels*)
	  (warn "The label ~s is already used" label)
	  (setf *header-labels* (cons label *header-labels*))))
  (vector-push-extend (format nil "## ~a~%~%" text) *file-documentation*))


(def-subsubheader-writer (text label)
    (when label
      (if (member label *header-labels*)
	  (warn "The label ~s is already used" label)
	  (setf *header-labels* (cons label *header-labels*))))
  (vector-push-extend (format nil "### ~a~%~%" text) *file-documentation*))


(def-text-writer (text)
    (vector-push-extend (format nil "~a~%~%" text) *file-documentation*))


(def-table-writer (table)
    (let ((table-head (car table))
	  (table-tail (cdr table)))
      (with-stream-string (stream table-str)
	(format stream "~{| ~a ~}|~%" table-head)
	(format stream "~v@{| --- ~}|~%" (length table-head) nil)
        (format stream "~{~{| ~a ~}|~%~}~%" table-tail)
	(vector-push-extend table-str *file-documentation*))))


(def-itemize-writer (items)
    (labels ((itemize-aux (item-list stream level)
	       (if (eq (car item-list) :itemize)
		   (loop for item in item-list
			 do (itemize-aux item))
		   (format stream "~v@{  ~}* ~a~%" level item-list))))
      (with-stream-string (stream str)
	(itemize-aux items stream 0)
	(vector-push-extend str *file-documentation*))))


(def-code-tag-writer (tags code-list)
    (loop for tag in tags
	  if (get-hash tag *code-tags*)
	    do (warn "The tag ~s is already used" tag)
	  else
	    do (setf (get-hash tag *code-tags*) code-list)))


;; ----- api functions -----

(def-defconstant-writer (defconstant-body)
    (with-defconstant-components ((name initial-value documentation) defconstant-body)
      (vector-push-extend
       (format nil "```lisp~%(defconstant ~s ~s)~%```~%~%~@[~a~%~%~]"
	       name initial-value documentation)
       *file-documentation*)))


(def-defparameter-writer (defparameter-body)
    (with-defparameter-components ((name initial-value documentation) defparameter-body)
      (vector-push-extend
       (format nil "```lisp~%(defparameter ~s ~s)~%```~%~%~@[~a~%~%~]"
	       name initial-value documentation)
       *file-documentation*)))


(def-defvar-writer (defvar-body)
    (with-defvar-components ((name initial-value documentation) defvar-body)
      (vector-push-extend
       (format nil "```lisp~%(defvar ~s ~s)~%```~%~%~@[~a~%~%~]"
	       name initial-value documentation)
       *file-documentation*)))


(def-defun-writer (defun-body)
    (with-defun-components ((function-name lambda-list documentation) defun-body)
      (vector-push-extend
       (format nil "```lisp~%(defun ~s ~s)~%```~%~%~@[~a~%~%~]"
	       function-name lambda-list documentation)
       *file-documentation*)))


(def-defmacro-writer (defun-body)
    (with-defun-components ((name lambda-list documentation) defmacro-body)
      (vector-push-extend
       (format nil "```lisp~%(defmacro ~s ~s)~%```~%~%~@[~a~%~%~]"
	       name lambda-list documentation)
       *file-documentation*)))



;; ----- structural functions -----

(def-documentation-in-file-writer (file)
    (vector-push-extend (cons file (with-output-to-string (s)
				     (loop for doc across *file-documentation*
					   do (princ doc s))))
			*system-documentation*)
      (setf (fill-pointer *file-documentation*) 0))


(def-load-with-documentation-writer (root-directory)
    (loop for (file . doc) in *system-documentation*
	  for complete-file = (merge-pathnames file root-directory)
	  for actual-file = (setf (pathname-type complete-file) "md")
	  do (ensure-directories-exist actual-file)
	     (with-open-file (stream actual-file :direction :output :if-does-not-exist :create :if-exists :supersede)
	       (princ doc actual-file))))
