
(defsystem "adp"
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please. A Common Lisp semi-automatic documentation generator and literate programming tool with Scribble files and @-syntax support."
  :license "MIT"
  :depends-on ("scribble" "alexandria")
  :components ((:file "package")
               (:file "adp")))
