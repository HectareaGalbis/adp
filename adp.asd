

(asdf:defsystem #:adp
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A Common Lisp literate programming tool."
  :license "The Unlicense"
  :depends-on (:alexandria :hyperspec)
  :components ((:file "package")
	       (:module "src"
		:components ((:file "error")
			     (:file "element")
			     (:file "file")
			     (:file "project")
			     (:file "tag-table")
			     (:file "writer")
			     (:file "parameter")
			     (:file "core")
			     (:file "adp")
			     (:file "helper-style-functions")))))


(asdf:defsystem #:adp/doc
  :author "Héctor Galbis Sanchis"
  :description "ADP, add documentation, please."
  :license "The Unlicense"
  :depends-on (:alexandria :hyperspec)
  :components ((:file "package")
	       (:file "adp-core")
	       (:file "adp")
	       (:file "adp-writers")
	       (:file "helper-style-functions")
	       (:file "readme")
	       (:module "guides"
		:components ((:file "user-guide")
			     (:file "style-maker-guide")))))


;; ----- ADP styles -----

(asdf:defsystem #:adp/github-md
  :author "Héctor Galbis Sanchis"
  :description "Markdown style for adp"
  :license "The Unlicense"
  :depends-on (:adp)
  :components ((:module "styles/github-md"
		:components ((:file "package")
			     (:file "github-md")))))

;; (asdf:defsystem #:adp/html
;;   :author ""
;;   :description "HTML style for adp"
;;   :license "The Unlicense"
;;   :depends-on (:adp)
;;   :components ((:module "styles/html"
;; 		:components ((:file "package")
;; 			     (:file "html")))))
