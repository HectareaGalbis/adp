
(in-package :adp)


;; ----- guide functions -----

(def-header-writer (stream text tag)
    (format stream "# ~a~%~%" text))

(def-subheader-writer (stream text tag)
    (format stream "## ~a~%~%" text))

(def-subsubheader-writer (stream text tag)
  (format stream "### ~a~%~%" text))

(def-text-writer (stream text)
    (format stream "~a~%~%" text))

(def-table-writer (stream table)
    (format stream "~{| ~a ~}|~%" (car table))
  (format stream "~v@{| --- ~}|~%" (length (car table)) nil)
  (format stream "~{~{| ~a ~}|~%~}~%~%" (cdr table)))

(def-itemize-writer (stream items)
    (labels ((itemize-aux (item-list level)
	       (loop for item in item-list
		     if (eq (car item) :item)
		       do (format stream "~v@{  ~}* ~a~%" level item)
		     else if (eq (car item) :itemize)
			    do (itemize-aux item (1+ level))
		     finally (when (zerop level)
			       (format stream "~%~%")))))
      (itemize-aux items 0)))

(def-image-writer (stream alt-text root-path rel-image-path)
  (format stream "![~a](~a)~%~%" alt-text (merge-pathnames root-path rel-image-path)))

(def-bold-writer (stream text)
  (format stream "**~a**" text))

(def-italic-writer (stream text)
    (format stream "_~a_" text))

(def-code-inline-writer (stream code)
    (let ((*print-pretty* nil))
      (format stream "`~s`" code)))

(def-web-link-writer (stream name link)
    (format stream "[~a](~a)" name link))

(def-header-ref-writer (stream tag header-text root-path file-path)
    (format stream "***~a***" header-text))

(def-symbol-ref-writer (stream tag root-path file-path)
    (format stream "***~a***" tag))

(def-function-ref-writer (stream tag root-path file-path)
    (format stream "***~a***" tag))

(def-type-ref-writer (stream tag root-path file-path)
  (format stream "***~a***" tag))

(def-code-block-writer (stream code-list)
    (format stream "```~%~{~s~%~^~%~}~%```~%~%" code-list))

(def-code-example-writer (stream code-list)
    (format stream "```~%")
    (loop for (code output result) in code-list
	  do (format stream "~s~%Output:~%~a~%Result:~%~{~s~%~}~%" code output result))
  (format stream "```~%~%"))


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
