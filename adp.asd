
(defsystem "adp"
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A Common Lisp documentation generator with Scribble files and @-syntax support."
  :license "MIT"
  :depends-on ("named-readtables" "scribble" "alexandria")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "adp")
                             (:file "racket")))))
