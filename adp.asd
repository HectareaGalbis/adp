

(asdf:defsystem "adp"
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A documentation generator."
  :license "The Unlicense"
  :depends-on (:alexandria :hyperspec)
  :components ((:file "package")
	       (:file "adp-core")
	       (:file "adp")
	       (:file "adp-writers")
	       (:file "helper-style-functions")))


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

(asdf:defsystem #:adp/html
  :author ""
  :description "HTML style for adp"
  :license "The Unlicense"
  :depends-on (:adp)
  :components ((:module "styles/html"
		:components ((:file "package")
			     (:file "html")))))
