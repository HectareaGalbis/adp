
(in-package :adppvt)


;; ----- adp parameters -----

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *add-documentation* nil))


;; ----- adp files -----

(deftype adp-file ()
  '(cons pathname vector))

(declaim (ftype (function (pathname vector) adp-file) create-adp-file))
(defun create-adp-file (path contents)
  (cons path contents))

(declaim (ftype (function (adp-file) pathname) adp-file-path))
(defun adp-file-path (file)
  (car file))

(declaim (ftype (function (adp-file) vector) ado-file-contents))
(defun adp-file-contents (file)
  (cdr file))

(declaim (type (vector adp-file) *project-adp-files*))
(defvar *project-adp-files* (make-array 10 :adjustable t :fill-pointer 0))
(defvar *current-adp-file* nil)
(defvar *current-adp-file-contents* nil)

(declaim (ftype (function (pathname) boolean) adp-file-presentp))
(defun get-project-adp-file-contents (path)
  (loop for file across *project-adp-files*
	  thereis (and (equal path (adp-file-path file))
		       (adp-file-contents file))))

(declaim (ftype (function (pathname) t) push-adp-file))
(defun push-adp-file (path)
  (let ((file-contents (get-project-adp-file-contents path)))
    (when (not file-contents)
      (setf file-contents (make-array 100 :adjustable t :fill-pointer 0))
      (vector-push-extend (create-adp-file path file-contents) *project-adp-files*))
    (setf *current-adp-file* path)
    (setf *current-adp-file-contents* file-contents)))

(declaim (ftype (function () t) empty-adp-files))
(defun empty-adp-files ()
  (setf (fill-pointer *project-adp-files*) 0))


;; ----- adp elements -----

(deftype adp-element ()
  '(cons keyword list))

(declaim (ftype (function (keyword &rest t) adp-element) create-adp-element))
(defun create-adp-element (key-type &rest contents)
  (cons key-type contents))

(declaim (ftype (function (adp-element) keyword) adp-element-key-type))
(defun adp-element-key-type (elem)
  (car elem))

(declaim (ftype (function (adp-element) list) adp-element-contents))
(defun adp-element-contents (elem)
  (cdr elem))


(declaim (type (vector adp-element) *file-adp-elements*))
(defvar *file-adp-elements* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'adp-element))

(declaim (ftype (function (adp-element) t) push-adp-element))
(defun push-adp-element (elem)
  (when (not *current-adp-file-contents*)
    (error "No documentation file assigned. Use write-in-file."))
  (vector-push-extend elem *current-adp-file-contents*))

(declaim (ftype (function (keyword &rest t) t) emplace-adp-element))
(defun emplace-adp-element (key-type &rest contents)
  (push-adp-element (apply #'create-adp-element key-type contents)))


;; ----- adp table of contents -----

(declaim (ftype (function (pathname) vector) adp-files-file-headers))
(defun adp-files-file-headers (path)
  (let ((headers (make-array 10 :adjustable t :fill-pointer 0)))
    (loop for file across *project-adp-files*
	  for file-path = (adp-file-path file)
	  until (equal path file-path)
	  finally (let ((file-contents (adp-file-contents file)))
		    (loop for element across file-contents
			  when (member (adp-element-key-type element) '(:header :subheader :subsubheader))
			    do (vector-push-extend element headers))))
    (values headers)))

(declaim (ftype (function () vector) adp-files-headers))
(defun adp-files-headers ()
  (let ((headers (make-array 100 :adjustable t :fill-pointer 0)))
    (loop for file across *project-adp-files*
	  for file-contents = (adp-file-contents file)
	  do (loop for element across file-contents
		   when (member (adp-element-key-type element) '(:header :subheader))
		     do (vector-push-extend element headers)))
    (values headers)))


(declaim (ftype (function (adp-element) symbol) header-contents-tag))
(defun header-contents-tag (header)
  (let* ((header-contents (adp-element-contents header)))
    (cadr header-contents)))


(declaim (ftype (function (keyword) fixnum) header-deep-level))
(defun header-deep-level (header-type)
  (case header-type
    (:header 0)
    (:subheader 1)
    (:subsubheader 2)
    (t (error "Header type ~s not recognized." header-type))))


(declaim (ftype (function ((vector adp-element)) (vector fixnum)) create-toc-deep-level))
(defun create-toc-deep-levels (headers)
  (let ((deep-levels (make-array 100 :adjustable t :fill-pointer 0 :element-type 'fixnum)))
    (loop for header across headers
	  for prev-min-deep-level = (header-deep-level :subsubheader) then next-min-deep-level
	  for prev-deep-level = (header-deep-level :subsubheader) then next-deep-level
	  for (next-min-deep-level next-deep-level) = (let ((header-deep-level (header-deep-level (adp-element-key-type header))))
							(cond
							  ((> header-deep-level prev-deep-level)
							   (let ((next-deep-level (1+ prev-deep-level)))
							     (list prev-min-deep-level next-deep-level)))
							  ((< header-deep-level prev-deep-level)
							   (if (>= header-deep-level prev-min-deep-level)
							       (list prev-min-deep-level (- header-deep-level prev-min-deep-level))
							       (list header-deep-level 0)))
							  (t
							   (list prev-min-deep-level header-deep-level))))
	  do (vector-push-extend next-deep-level deep-levels))
    (values deep-levels)))

(declaim (ftype (function ((vector adp-element)) list) create-headers-toc-list))
(defun create-headers-toc-list (headers)
  (let* ((deep-levels (create-toc-deep-levels headers))
	 (total-deep-levels (length deep-levels))
	 (index 0))
    (labels ((create-headers-toc-list-aux (current-level)
	       (loop while (< index total-deep-levels)
		     for header = (aref headers index)
		     for deep-level = (aref deep-levels index)
		     until (< deep-level current-level)
		     if (> deep-level current-level)
		       collect (cons :itemize (create-headers-toc-list-aux (1+ current-level)))
			 into toc-list
		     else
		       collect `(:item ,(create-header-ref-text (header-contents-tag header)))
			 into toc-list
		       and do (incf index)
		     finally (return toc-list))))
      (create-headers-toc-list-aux 0))))

(declaim (ftype (function () list) create-toc-list))
(defun create-toc-list ()
  (let ((headers (adp-files-headers)))
    (create-headers-toc-list headers)))

(declaim (ftype (function (pathname) list) create-mini-toc-list))
(defun create-mini-toc-list (path)
  (let ((headers (adp-files-file-headers path)))
    (create-headers-toc-list headers)))


;; ----- adp ref tags -----

(declaim (type (vector symbol) *never-used-header-tags*))
(defvar *never-used-header-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (ftype (function (symbol) t) add-never-used-header-tag))
(defun add-never-used-header-tag (tag)
  (vector-push-extend tag *never-used-header-tags*))

(declaim (ftype (function (symbol) t) remove-never-used-header-tag))
(defun remove-never-used-header-tag (tag)
  (let ((vector-length (length *never-used-header-tags*)))
    (loop for i from 0 below vector-length
	  if (eq tag (aref *never-used-header-tags* i))
	    do (setf (aref *never-used-header-tags* i)
		     (aref *never-used-header-tags* (1- vector-length)))
	       (decf (fill-pointer *never-used-header-tags*))
	    and return nil)))

(declaim (ftype (function () t) empty-never-used-header-tags))
(defun empty-never-used-header-tags ()
  (setf (fill-pointer *never-used-header-tags*) 0))

(declaim (type hash-table *header-tags-table* *symbol-tags-table* *function-tags-table* *type-tags-table*))
(defvar *header-tags* (make-hash-table))
(defvar *symbol-tags* (make-hash-table))
(defvar *function-tags* (make-hash-table))
(defvar *type-tags* (make-hash-table))

(declaim (ftype (function (symbol string pathname) t) add-header-tag-path))
(defun add-header-tag (tag str)
  (when (gethash tag *header-tags*)
    (error "Header tag ~s already used." tag))
  (when (find-symbol (symbol-name tag))
    (add-never-used-header-tag tag))
  (setf (gethash tag *header-tags*) (cons str *current-adp-file*)))


(declaim (ftype (function (symbol pathname) t) add-symbol-tag-path add-function-tag-path add-type-tag-path))
(defun add-symbol-tag (tag)
  (setf (gethash tag *symbol-tags*) *current-adp-file*))

(defun add-function-tag (tag)
  (setf (gethash tag *function-tags*) *current-adp-file*))

(defun add-type-tag (tag)
  (setf (gethash tag *type-tags*) *current-adp-file*))


(declaim (ftype (function (symbol) (or (cons string pathname) null)) get-header-tag-path))
(defun get-header-tag-info (tag)
  (values (gethash tag *header-tags*)))


(declaim (ftype (function (symbol) (or pathname null)) get-symbol-tag-path get-function-tag-path
		get-type-tag-path))
(defun get-symbol-tag-info (tag)
  (values (gethash tag *symbol-tags*)))

(defun get-function-tag-info (tag)
  (values (gethash tag *function-tags*)))

(defun get-type-tag-info (tag)
  (values (gethash tag *type-tags*)))


(declaim (ftype (function () t) empty-header-tags-table empty-symbol-tags-table empty-function-tags-table
		empty-type-tags-table))
(defun empty-header-tags ()
  (setf *header-tags* (make-hash-table)))

(defun empty-symbol-tags ()
  (setf *symbol-tags* (make-hash-table)))

(defun empty-function-tags ()
  (setf *function-tags* (make-hash-table)))

(defun empty-type-tags ()
  (setf *type-tags* (make-hash-table)))


;; ----- table of functions/symbols/types -----

(declaim (ftype (function () list) create-table-of-functions create-table-of-symbols create-table-of-types))
(defun create-table-of-functions ()
  (let ((functions-list (sort (hash-table-keys *function-tags*) #'string>=))
	(temp-list nil)
	(items-list nil))
    (loop for function-tag in functions-list
	  for prev-letter = (aref (symbol-name function-tag) 0) then current-letter
	  for current-letter = (aref (symbol-name function-tag) 0)
	  if (equal prev-letter current-letter)
	    do (push `(:item ,(create-function-ref-text function-tag)) temp-list)
	  else
	    do (push `(:itemize ,@temp-list) items-list)
	    and do (push `(:item ,prev-letter) items-list)
	    and do (setf temp-list nil)
	    and do (push `(:item ,(create-function-ref-text function-tag)) temp-list)
	  finally (when temp-list
		    (push `(:itemize ,@temp-list) items-list)
		    (push `(:item ,current-letter) items-list))
		  (return items-list))))

(defun create-table-of-symbols ()
  (let ((symbols-list (sort (hash-table-keys *symbol-tags*) #'string>=))
	(temp-list nil)
	(items-list nil))
    (loop for symbol-tag in symbols-list
	  for prev-letter = (aref (symbol-name symbol-tag) 0) then current-letter
	  for current-letter = (aref (symbol-name symbol-tag) 0)
	  if (equal prev-letter current-letter)
	    do (push `(:item ,(create-symbol-ref-text symbol-tag)) temp-list)
	  else
	    do (push `(:itemize ,@temp-list) items-list)
	    and do (push `(:item ,prev-letter) items-list)
	    and do (setf temp-list nil)
	    and do (push `(:item ,(create-symbol-ref-text symbol-tag)) temp-list)
	  finally (when temp-list
		    (push `(:itemize ,@temp-list) items-list)
		    (push `(:item ,current-letter) items-list))
		  (return items-list))))

(defun create-table-of-types ()
  (let ((types-list (sort (hash-table-keys *type-tags*) #'string>=))
	(temp-list nil)
	(items-list nil))
    (loop for type-tag in types-list
	  for prev-letter = (aref (symbol-name type-tag) 0) then current-letter
	  for current-letter = (aref (symbol-name type-tag) 0)
	  if (equal prev-letter current-letter)
	    do (push `(:item ,(create-type-ref-text type-tag)) temp-list)
	  else
	    do (push `(:itemize ,@temp-list) items-list)
	    and do (push `(:item ,prev-letter) items-list)
	    and do (setf temp-list nil)
	    and do (push `(:item ,(create-type-ref-text type-tag)) temp-list)
	  finally (when temp-list
		    (push `(:itemize ,@temp-list) items-list)
		    (push `(:item ,current-letter) items-list))
		  (return items-list))))


;; ----- adp code tags -----

(declaim (type (vector symbol) *never-used-code-tags*))
(defvar *never-used-code-tags* (make-array 100 :adjustable t :fill-pointer 0 :element-type 'symbol))

(declaim (ftype (function (symbol) t) add-never-used-code-tag))
(defun add-never-used-code-tag (tag)
  (vector-push-extend tag *never-used-code-tags*))

(declaim (ftype (function (symbol) t) remove-never-used-code-tag))
(defun remove-never-used-code-tag (tag)
  (let ((vector-length (length *never-used-code-tags*)))
    (loop for i from 0 below vector-length
	  if (eq tag (aref *never-used-code-tags* i))
	    do (setf (aref *never-used-code-tags* i)
		     (aref *never-used-code-tags* (1- vector-length)))
	       (decf (fill-pointer *never-used-code-tags*))
	    and return nil)))

(declaim (ftype (function () t) empty-never-used-code-tags))
(defun empty-never-used-code-tags ()
  (setf (fill-pointer *never-used-code-tags*) 0))


(declaim (type hash-table *code-tags*))
(defvar *code-tags* (make-hash-table))

(declaim (ftype (function (symbol &rest t) t) add-code-tag))
(defun add-code-tag (tag &rest list-code)
  (when (not (gethash tag *code-tags*))
    (setf (gethash tag *code-tags*) (make-array 10 :adjustable t :fill-pointer 0)))
  (add-never-used-code-tag tag)
  (loop for code in list-code
	do (vector-push-extend code (gethash tag *code-tags*))))

(declaim (ftype (function (symbol) boolean) code-tagp))
(defun code-tagp (tag)
  (values (gethash tag *code-tags*)))

(declaim (ftype (function (symbol) (or vector null)) get-code-tag))
(defun get-code-tag (tag)
  (values (gethash tag *code-tags*)))

(declaim (ftype (function () t) empty-code-tags))
(defun empty-code-tags ()
  (setf *code-tags* (make-hash-table)))

(declaim (type symbol *hide-symbol*))
(defparameter *hide-symbol* '#:hide)

(declaim (ftype (function (t) boolean) hide-symbolp))
(defun hide-symbolp (code)
  (eq code *hide-symbol*))

(declaim (ftype (function (t) boolean) plistp))
(defun plistp (code)
  (or (null code)
      (and (consp code)
	   (plistp (cdr code)))))

(intern "CODE-HIDE"   :adp) ; Advance intern
(intern "CODE-TAG"    :adp) ; Advance intern
(intern "CODE-REMOVE" :adp) ; Advance intern
(intern "CODE-SHOW"   :adp) ; Advance intern

(declaim (ftype (function (t) t) remove-code-tag-exprs))
(defun remove-code-tag-exprs (code)
  (labels ((remove-code-tag-exprs-aux (code)
	     (if (plistp code)
		 (cond
		   ((member (car code) '(adp::code-tag adp::code-hide adp::code-remove))
		    (mapcan #'remove-code-tag-exprs-aux (cddr code)))
		   ((eq (car code) 'adp::code-show)
		    nil)
		   (t
		    (list (mapcan #'remove-code-tag-exprs-aux code))))
		 (list code))))
    (car (remove-code-tag-exprs-aux code))))

(declaim (ftype (function (t) t) remove-own-code-focus-exprs))
(defun remove-own-code-tag-exprs (code)
  (labels ((remove-own-code-tag-exprs-aux (code)
	     (if (plistp code)
		 (cond
		   ((member (car code) '(adp::code-hide adp::code-remove))
		    (mapcan #'remove-own-code-tag-exprs-aux (cddr code)))
		   ((eq (car code) 'adp::code-tag)
		    (list code))
		   ((eq (car code) 'adp::code-show)
		    nil)
		   (t
		    (list (mapcan #'remove-own-code-tag-exprs-aux code))))
		 (list code))))
    (car (remove-own-code-tag-exprs-aux code))))

(declaim (ftype (function (symbol list) boolean) valid-tag-p))
(defun valid-tag-p (tag tags)
  (or (null tags)
      (member tag tags)))

(declaim (ftype (function (symbol t) t) process-code-tag))
(defun process-code-tag (tag code)
  (labels ((process-aux (tag code)
	     (if (plistp code)
		 (cond
		   ((eq (car code) 'adp::code-hide)
		    (if (valid-tag-p tag (cadr code))
			(list *hide-symbol*)
			(loop for expr in (cddr code)
			      append (process-aux tag expr))))
		   ((eq (car code) 'adp::code-remove)
		    (if (valid-tag-p tag (cadr code))
			nil
			(loop for expr in (cddr code)
			      append (process-aux tag expr))))
		   ((eq (car code) 'adp::code-show)
		    (if (valid-tag-p tag (cadr code))
			(loop for expr in (cddr code)
			      append (process-aux tag expr))
			nil))
		   ((eq (car code) 'adp::code-tag)
		    (loop for expr in (cddr code)
			  append (process-aux tag expr)))
		   (t
		    (list (loop for expr in code
				append (process-aux tag expr)))))
		 (list code))))
    (car (process-aux tag code))))


;; ----- text variations -----

(declaim (type symbol *bold-symbol* *italic-symbol* *bold-italic-symbol* *code-inline-symbol*
	       *header-ref-symbol* *symbol-ref-symbol* *function-ref-symbol* *type-ref-symbol*))
(defparameter *bold-symbol* '#:bold)
(defparameter *italic-symbol* '#:italic)
(defparameter *bold-italic-symbol* '#:bolditalic)
(defparameter *code-inline-symbol* '#:code-inline)
(defparameter *web-link-symbol* '#:web-link)
(defparameter *header-ref-symbol* '#:header)
(defparameter *symbol-ref-symbol* '#:symbol)
(defparameter *function-ref-symbol* '#:function)
(defparameter *type-ref-symbol* '#:type)

(declaim (ftype (function (&rest t) (cons symbol list)) create-bold-text create-italic-text
		create-code-inline-text))
(defun create-bold-text (&rest args)
  (cons *bold-symbol* args))

(defun create-italic-text (&rest args)
  (cons *italic-symbol* args))

(defun create-bold-italic-text (&rest args)
  (cons *bold-italic-symbol* args))

(defun create-code-inline-text (&rest args)
  (cons *code-inline-symbol* args))

(declaim (ftype (function (string string) list) create-web-link-text))
(defun create-web-link-text (text link)
  (list *web-link-symbol* text link))

(declaim (ftype (function (symbol) (cons symbol list)) create-header-ref-text create-symbol-ref-text
		create-function-ref-text create-type-ref-text))
(defun create-header-ref-text (label)
  (list *header-ref-symbol* label))

(defun create-symbol-ref-text (label)
  (list *symbol-ref-symbol* label))

(defun create-function-ref-text (label)
  (list *function-ref-symbol* label))

(defun create-type-ref-text (label)
  (list *type-ref-symbol* label))

(declaim (ftype (function (t) boolean) bold-textp italic-textp code-inline-textp web-link-textp
		header-ref-textp symbol-ref-textp function-ref-textp type-ref-textp))
(defun bold-textp (arg)
  (and (listp arg)
       (eq (car arg) *bold-symbol*)))

(defun italic-textp (arg)
  (and (listp arg)
       (eq (car arg) *italic-symbol*)))

(defun bold-italic-textp (arg)
  (and (listp arg)
       (eq (car arg) *bold-italic-symbol*)))

(defun code-inline-textp (arg)
  (and (listp arg)
       (eq (car arg) *code-inline-symbol*)))

(defun web-link-textp (arg)
  (and (listp arg)
       (eq (car arg) *web-link-symbol*)))

(defun header-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *header-ref-symbol*)))

(defun symbol-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *symbol-ref-symbol*)))

(defun function-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *function-ref-symbol*)))

(defun type-ref-textp (arg)
  (and (listp arg)
       (eq (car arg) *type-ref-symbol*)))


;; ----- tag variation -----

(declaim (type symbol *code-block-tag-symbol*))
(defparameter *code-block-tag-symbol* '#:tag)

(declaim (ftype (function (symbol) t) create-code-block-tag))
(defun create-code-block-tag (tag)
  (list *code-block-tag-symbol* tag))

(defun code-block-tagp (arg)
  (and (listp arg)
       (eq (car arg) *code-block-tag-symbol*)))


;; ----- style parameters -----

(declaim (type list *style-parameters*))
(defvar *style-parameters* nil)

(declaim (ftype (function (symbol keyword boolean) t) add-style-parameter))
(defun add-style-parameter (name key-name required)
  (push (list name key-name required) *style-parameters*))

(declaim (ftype (function () t) empty-style-parameter))
(defun empty-style-parameters ()
  (setf *style-parameters* nil))

(declaim (ftype (function (keyword) boolean) style-parameterp style-parameter-requiredp))
(defun style-parameterp (key-name)
  (member key-name *style-parameters* :key #'cadr))

(defun style-parameter-requiredp (key-name)
  (cadar (member key-name *style-parameters* :key #'cadr)))

(declaim (ftype (function () list) style-required-parameters))
(defun style-required-parameters ()
  (loop for (name key-name requiredp) in *style-parameters*
	if requiredp
	  collect key-name))

(declaim (ftype (function (keyword t) t) set-parameter-value))
(defun set-parameter-value (key-name value)
  (loop for (name key requiredp) in *style-parameters*
	until (eq key key-name)
	finally (setf (symbol-value name) value)))

(declaim (ftype (function (list) t) check-style-parameters))
(defun check-style-parameters (style-params)
  (let ((required-parameters (style-required-parameters))
	(style-parameter-key-names (loop for key-name in style-params by #'cddr
					 collect key-name)))
    (loop for style-parameter-key-name in style-parameter-key-names
	  if (not (style-parameterp style-parameter-key-name))
	    do (error "The parameter ~s is not allowed." style-parameter-key-name))
    (loop for required-param in required-parameters
	  if (not (member required-param style-parameter-key-names))
	    do (error "The required param ~s is not used." required-param))))


;; ----- current data -----

(declaim (ftype (function () t) remove-current-data))
(defun remove-current-data ()
  (empty-adp-files)
  (empty-header-tags)
  (empty-symbol-tags)
  (empty-function-tags)
  (empty-type-tags)
  (empty-never-used-header-tags)
  (empty-never-used-code-tags)
  (empty-code-tags)
  (empty-style-parameters))


;; ----- writing functions -----

(declaim (type (or null (function (stream string symbol) t)) *header-proc* *subheader-proc* *subsubheader-proc*))
(defvar *header-proc* nil)
(defvar *subheader-proc* nil)
(defvar *subsubheader-proc* nil)

(declaim (type (or null (function (stream string) t)) *text-proc*))
(defvar *text-proc* nil)

(declaim (type (or null (function (stream list) t)) *table-proc*))
(defvar *table-proc* nil)

(declaim (type (or null (function (stream list) t)) *itemize-proc*))
(defvar *itemize-proc* nil)

(declaim (type (or null (function (stream string pathname pathname) t)) *image-proc*))
(defvar *image-proc* nil)

(declaim (type (or null (function (stream string) t)) *bold-proc* *italic-proc* *bold-italic-proc*))
(defvar *bold-proc* nil)
(defvar *italic-proc* nil)
(defvar *bold-italic-proc* nil)

(declaim (type (or null (function (stream t) t)) *code-inline-proc*))
(defvar *code-inline-proc* nil)

(declaim (type (or null (function (stream string string) t)) *web-link-proc*))
(defvar *web-link-proc* nil)

(declaim (type (or null (function (stream symbol string pathname pathname) t)) *header-ref-proc*))
(defvar *header-ref-proc* nil)

(declaim (type (or null (function (stream symbol pathname pathname) t)) *symbol-ref-proc* *function-ref-proc*
	       *type-ref-proc*))
(defvar *symbol-ref-proc* nil)
(defvar *function-ref-proc* nil)
(defvar *type-ref-proc* nil)

(declaim (type (or null (function (stream list) t)) *code-block-proc*))
(defvar *code-block-proc* nil)

(declaim (type (or null (function (stream list string list) t)) *code-example-proc*))
(defvar *code-example-proc* nil)


(declaim (type (or null (function (stream t) t)) *defclass-proc* *defconstant-proc* *defgeneric-proc*
	       *define-condition-proc* *define-modify-macro-proc* *define-symbol-macro-proc* *defmacro-proc*
	       *defparameter-proc* *defstruct-proc* *deftype-proc* *defun-proc* *defvar-proc*))
(declaim (type (or null (function (stream t symbol) t)) *define-compiler-macro-proc*
	       *define-method-combination-proc* *define-setf-expander-proc* *defmethod-proc* *defpackage-proc*
	       *defsetf-proc*))
(defvar *defclass-proc* nil)
(defvar *defconstant-proc* nil)
(defvar *defgeneric-proc* nil)
(defvar *define-compiler-macro-proc* nil)
(defvar *define-condition-proc* nil)
(defvar *define-method-combination-proc* nil)
(defvar *define-modify-macro-proc* nil)
(defvar *define-setf-expander-proc* nil)
(defvar *define-symbol-macro-proc* nil)
(defvar *defmacro-proc* nil)
(defvar *defmethod-proc* nil)
(defvar *defpackage-proc* nil)
(defvar *defparameter-proc* nil)
(defvar *defsetf-proc* nil)
(defvar *defstruct-proc* nil)
(defvar *deftype-proc* nil)
(defvar *defun-proc* nil)
(defvar *defvar-proc* nil)

(declaim (type (or null (function () string)) *get-file-extension-proc*))
(defvar *get-file-extension-proc* nil)

(declaim (type (or null (function (stream) t)) *file-header-proc* *file-foot-proc*))
(defvar *file-header-proc* nil)
(defvar *file-foot-proc* nil)

(declaim (type (or null (function (pathname) t)) *system-files-proc*))
(defvar *system-files-proc* nil)


(declaim (ftype (function () t) remove-current-procs))
(defun remove-current-procs ()
  (setf *header-proc* nil
	*subheader-proc* nil
	*subsubheader-proc* nil
	*text-proc* nil
	*table-proc* nil
	*itemize-proc* nil
	*image-proc* nil
	*bold-proc* nil
	*italic-proc* nil
	*bold-italic-proc* nil
	*code-inline-proc* nil
	*web-link-proc* nil
	*header-ref-proc* nil
	*symbol-ref-proc* nil
	*function-ref-proc* nil
	*type-ref-proc* nil
	*code-block-proc* nil
	*code-example-proc* nil
	*defclass-proc* nil
	*defconstant-proc* nil
	*defgeneric-proc* nil
	*define-compiler-macro-proc* nil
	*define-condition-proc* nil
	*define-method-combination-proc* nil
	*define-modify-macro-proc* nil
	*define-setf-expander-proc* nil
	*define-symbol-macro-proc* nil
	*defmacro-proc* nil
	*defmethod-proc* nil
	*defpackage-proc* nil
	*defparameter-proc* nil
	*defsetf-proc* nil
	*defstruct-proc* nil
	*deftype-proc* nil
	*defun-proc* nil
	*defvar-proc* nil
	*get-file-extension-proc* nil
	*file-header-proc* nil
	*file-foot-proc* nil
	*system-files-proc* nil))


(declaim (ftype (function () t) check-current-procs))
(defun check-current-procs ()
  (unless *header-proc*
    (error "The function header is not defined in the current style."))
  (unless *subheader-proc*
    (error "The function subheader is not defined in the current style."))
  (unless *subsubheader-proc*
    (error "The function subsubheader is not defined in the current style."))
  (unless *text-proc*
    (error "The function text is not defined in the current style."))
  (unless *table-proc*
    (error "The function table is not defined in the current style."))
  (unless *itemize-proc*
    (error "The function itemize is not defined in the current style."))
  (unless *image-proc*
    (error "The function image is not defined in the current style."))
  (unless *bold-proc*
    (error "The function bold is not defined in the current style."))
  (unless *italic-proc*
    (error "The function italic is not defined in the current style."))
  (unless *bold-italic-proc*
    (error "The function bold-italic is not defined in the current style."))
  (unless *code-inline-proc*
    (error "The function code-inline is not defined in the current style."))
  (unless *web-link-proc*
    (error "The function web-link is not defined in the current style."))
  (unless *header-ref-proc*
    (error "The function header-ref is not defined in the current style."))
  (unless *symbol-ref-proc*
    (error "The function symbol-ref is not defined in the current style."))
  (unless *function-ref-proc*
    (error "The function function-ref is not defined in the current style."))
  (unless *type-ref-proc*
    (error "The function type-ref is not defined in the current style."))
  (unless *code-block-proc*
    (error "The function code-block is not defined in the current style."))
  (unless *code-example-proc*
    (error "The function code-example is not defined in the current style."))
  (unless *defclass-proc*
    (error "The function defclass is not defined in the current style."))
  (unless *defconstant-proc*
    (error "The function defconstant is not defined in the current style."))
  (unless *defgeneric-proc*
    (error "The function defgeneric is not defined in the current style."))
  (unless *define-compiler-macro-proc*
    (error "The function define-compiler-macro is not defined in the current style."))
  (unless *define-condition-proc*
    (error "The function define-condition is not defined in the current style."))
  (unless *define-method-combination-proc*
    (error "The function define-method-combination is not defined in the current style."))
  (unless *define-modify-macro-proc*
    (error "The function define-modify-macro is not defined in the current style."))
  (unless *define-setf-expander-proc*
    (error "The function define-setf-expander is not defined in the current style."))
  (unless *define-symbol-macro-proc*
    (error "The function define-symbol-macro is not defined in the current style."))
  (unless *defmacro-proc*
    (error "The function defmacro is not defined in the current style."))
  (unless *defmethod-proc*
    (error "The function defmethod is not defined in the current style."))
  (unless *defpackage-proc*
    (error "The function defpackage is not defined in the current style."))
  (unless *defparameter-proc*
    (error "The function defparameter is not defined in the current style."))
  (unless *defsetf-proc*
    (error "The function defsetf is not defined in the current style."))
  (unless *defstruct-proc*
    (error "The function defstruct is not defined in the current style."))
  (unless *deftype-proc*
    (error "The function deftype is not defined in the current style."))
  (unless *defun-proc*
    (error "The function defun is not defined in the current style."))
  (unless *defvar-proc*
    (error "The function defvar is not defined in the current style."))
  (unless *get-file-extension-proc*
    (error "The function get-file-extension is not defined in the current style.")))


(declaim (ftype (function (string string) t) check-space-at-boundaries))
(defun check-space-at-boundaries (from text)
  (when (or (member (aref text 0) '(#\Space #\Newline) :test #'equal)
	      (member (aref text (1- (length text))) '(#\Space #\Newline) :test #'equal))
      (warn "A space/newline character is found at boundaries from ~a: ~s." from text)))


(declaim (ftype (function (&rest t) string) simple-slice-format))
(defun simple-slice-format (from &rest args)
  (let ((sliced-text (with-output-to-string (stream)
		       (loop for arg in args
			     do (when (or (bold-textp arg)
					  (italic-textp arg)
					  (bold-italic-textp arg)
					  (code-inline-textp arg)
					  (web-link-textp arg)
					  (header-ref-textp arg)
					  (symbol-ref-textp arg)
					  (function-ref-textp arg)
					  (type-ref-textp arg))
				  (error "Non-toplevel functions cannot be nested."))
				(princ arg stream)))))
    (check-space-at-boundaries (symbol-name from) sliced-text)
    (values sliced-text)))


(declaim (ftype (function (&rest t) string) slice-format))
(defun slice-format (&rest args)
  (with-output-to-string (stream)
    (loop for arg in args
	  do (cond
	       ((bold-textp arg)
		(let ((bold-args (cdr arg)))
		  (funcall *bold-proc* stream (apply #'simple-slice-format (car arg) bold-args))))
	       ((italic-textp arg)
		(let ((italic-args (cdr arg)))
		  (funcall *italic-proc* stream (apply #'simple-slice-format (car arg) italic-args))))
	       ((bold-italic-textp arg)
		(let ((bold-italic-args (cdr arg)))
		  (funcall *bold-italic-proc* stream (apply #'simple-slice-format (car arg) bold-italic-args))))
	       ((code-inline-textp arg)
		(let ((code-inline-args (cdr arg)))
		  (funcall *code-inline-proc* stream (apply #'simple-slice-format (car arg) code-inline-args))))
	       ((web-link-textp arg)
		(destructuring-bind (name link) (cdr arg)
		  (check-space-at-boundaries (symbol-name (car arg)) name)
		  (funcall *web-link-proc* stream name link)))
	       ((header-ref-textp arg)
		(assert (get-header-tag-info (cadr arg)) ((cadr arg)) "~s is not a header tag." (cadr arg)) 
		(let* ((header-tag (cadr arg))
		       (header-str-path (get-header-tag-info header-tag))
		       (header-str (car header-str-path))
		       (header-rel-path (cdr header-str-path)))
		  (remove-never-used-header-tag header-tag)
		  (funcall *header-ref-proc* stream header-tag header-str header-rel-path)))
	       ((symbol-ref-textp arg)
		(assert (get-symbol-tag-info (cadr arg)) ((cadr arg)) "~s is not a symbol tag." (cadr arg))
		(let* ((symbol-tag (cadr arg))
		       (symbol-path (get-symbol-tag-info symbol-tag)))
		  (funcall *symbol-ref-proc* stream symbol-tag symbol-path)))
	       ((function-ref-textp arg)
		(assert (get-function-tag-info (cadr arg)) ((cadr arg)) "~s is not a function tag." (cadr arg))
		(let* ((function-tag (cadr arg))
		       (function-path (get-function-tag-info function-tag)))
		  (funcall *function-ref-proc* stream function-tag function-path)))
	       ((type-ref-textp arg)
		(assert (get-type-tag-info (cadr arg)) ((cadr arg)) "~s is not a type tag." (cadr arg))
		(let* ((type-tag (cadr arg))
		       (type-path (get-type-tag-info type-tag)))
		  (funcall *type-ref-proc* stream type-tag type-path)))
	       (t (princ arg stream))))))


(declaim (ftype (function (stream pathname (vector adp-element)) t) write-file-contents))
(defun write-file-contents (stream rel-path elements)
  (loop for element across elements
	do (case (adp-element-key-type element)
	     (:header (apply *header-proc* stream (adp-element-contents element)))
	     (:subheader (apply *subheader-proc* stream (adp-element-contents element)))
	     (:subsubheader (apply *subsubheader-proc* stream (adp-element-contents element)))
	     (:text (funcall *text-proc* stream (apply #'slice-format (adp-element-contents element))))
	     (:table (funcall *table-proc* stream (loop for row in (adp-element-contents element)
							collect (loop for elem in row
								      collect (apply #'slice-format (cdr elem))))))
	     ((:itemize :table-of-contents :mini-table-of-contents :table-of-functions :table-of-symbols :table-of-types)
	      (labels ((write-itemize (item-list)
			 (loop for item in item-list
			       if (eq (car item) :item)
				 collect (list :item (apply #'slice-format (cdr item)))
			       else
				 collect (list* :itemize (write-itemize (cdr item))))))
		(let ((item-list (case (adp-element-key-type element)
				   (:table-of-functions (create-table-of-functions))
				   (:table-of-symbols (create-table-of-symbols))
				   (:table-of-types (create-table-of-types))
				   (:table-of-contents (create-toc-list))
				   (:mini-table-of-contents (create-mini-toc-list rel-path))
				   (:itemize (adp-element-contents element)))))
		  (funcall *itemize-proc* stream (write-itemize item-list)))))
	     (:image (destructuring-bind (alt-text image-path) (adp-element-contents element)
		       (funcall *image-proc* stream alt-text image-path)))
	     (:code-block (let* ((contents (car (adp-element-contents element)))
				 (processed-contents (mapcan (lambda (code)
							       (if (code-block-tagp code)
								   (let ((associated-code (coerce (get-code-tag (cadr code)) 'list)))
								     (assert associated-code () "~s is not a code-tag." (cadr code))
								     (remove-never-used-code-tag (cadr code))
								     associated-code)
								   (list code)))
							     contents)))
			    (funcall *code-block-proc* stream processed-contents)))
	     (:code-example (apply *code-example-proc* stream (adp-element-contents element)))
	     (:defclass (apply *defclass-proc* stream (adp-element-contents element)))
	     (:defconstant (apply *defconstant-proc* stream (adp-element-contents element)))
	     (:defgeneric (apply *defgeneric-proc* stream (adp-element-contents element)))
	     (:define-compiler-macro (apply *define-compiler-macro-proc* stream (adp-element-contents element)))
	     (:define-condition (apply *define-condition-proc* stream (adp-element-contents element)))
	     (:define-method-combination (apply *define-method-combination-proc* stream (adp-element-contents element)))
	     (:define-modify-macro (apply *define-modify-macro-proc* stream (adp-element-contents element)))
	     (:define-setf-expander (apply *define-setf-expander-proc* stream (adp-element-contents element)))
	     (:define-symbol-macro (apply *define-symbol-macro-proc* stream (adp-element-contents element)))
	     (:defmacro (apply *defmacro-proc* stream (adp-element-contents element)))
	     (:defmethod (apply *defmethod-proc* stream (adp-element-contents element)))
	     (:defpackage (apply *defpackage-proc* stream (adp-element-contents element)))
	     (:defparameter (apply *defparameter-proc* stream (adp-element-contents element)))
	     (:defsetf (apply *defsetf-proc* stream (adp-element-contents element)))
	     (:defstruct (apply *defstruct-proc* stream (adp-element-contents element)))
	     (:deftype (apply *deftype-proc* stream (adp-element-contents element)))
	     (:defun (apply *defun-proc* stream (adp-element-contents element)))
	     (:defvar (apply *defvar-proc* stream (adp-element-contents element)))
	     (t (error "Element not recognized: ~s" (adp-element-key-type element))))))


(declaim (ftype (function (pathname pathname (vector adp-element)) t) write-file))
(defun write-file (root-path rel-path elements)
  (let* ((complete-path (make-pathname :host (pathname-host root-path)
				       :device (pathname-device root-path)
				       :directory (append (pathname-directory root-path) (cdr (pathname-directory rel-path)))
				       :name (pathname-name rel-path)
				       :type (funcall *get-file-extension-proc*))))
    (format t "~%Writing file: ~a" complete-path)
    (ensure-directories-exist complete-path :verbose nil)
    (with-open-file (stream complete-path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (when *file-header-proc*
	(funcall *file-header-proc* stream))
      (write-file-contents stream rel-path elements)
      (when *file-foot-proc*
	(funcall *file-foot-proc* stream)))))


(defun write-system-files (root-path)
  (when *system-files-proc*
    (funcall *system-files-proc* root-path))
  (loop for file across *project-adp-files*
	for rel-path = (adp-file-path file)
	for file-elements = (adp-file-contents file)
	do (write-file root-path rel-path file-elements))
  (loop for never-used-header-tag across *never-used-header-tags*
	do (warn "The header tag ~s is defined but never used." never-used-header-tag))
  (loop for never-used-code-tag across *never-used-code-tags*
	do (warn "The code tag ~s is defined but never used." never-used-code-tag)))
