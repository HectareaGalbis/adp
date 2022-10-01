

(asdf:defsystem #:adp
  :author "Héctor Galbis Sanchis"
  :description "Documentation generator facility"
  :license "The Unlicense"
  :depends-on (:uiop)
  :components ((:file "package")
	       (:file "adp")))
