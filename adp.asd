
(defsystem "adp"
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A Common Lisp documentation generator and literate programming tool with Scribble files and @-syntax support."
  :license "MIT"
  :depends-on ("named-readtables" "scribble" "alexandria")
  :components ((:file "package")
               (:file "adp" :depends-on ("package"))))
