
(defsystem "adp-docs"
  :author "Héctor Galbis Sanchis"
  :description "Documentation of ADP."
  :license "MIT"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  :depends-on ("adp")
  :components ((:module "scribble"
                :components ((:file "package")
                             (:scribble "README")
                             (:scribble "reference")))))
