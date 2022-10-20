

(asdf:defsystem #:adp
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A documentation generator."
  :license "The Unlicense"
  :depends-on (:alexandria)
  :components ((:file "package")
	       (:file "adp-core")
	       (:file "adp")
	       (:file "adp-writers")
	       (:file "helper-style-functions")))


(asdf:defsystem #:adp/doc
  :author "Héctor Galbis Sanchis"
  :description "ADP, add documentation, please."
  :license "The Unlicense"
  :depends-on (:uiop :alexandria)
  :components ((:file "package")
	       (:file "adp-core")
	       (:file "adp")
	       (:file "adp-writers")
	       (:file "helper-style-functions")
	       (:file "readme")))


;; ----- ADP styles -----

(asdf:defsystem #:adp/markdown
  :author "Héctor Galbis Sanchis"
  :description "Markdown style for adp"
  :license "The Unlicense"
  :depends-on (:alexandria :adp)
  :components ((:module "styles/markdown"
		:components ((:file "package")
			     (:file "markdown")))))

(asdf:defsystem #:adp/html
  :author ""
  :description "HTML style for adp"
  :license "The Unlicense"
  :depends-on (:alexandria :adp)
  :components ((:module "styles/html"
		:components ((:file "package")
			     (:file "html")))))
