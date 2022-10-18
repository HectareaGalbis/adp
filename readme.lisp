
(in-package :adp)


(header "Add Documentation, Please")

(text (italic "Add Documentation, Please") " is a library for literate programming and semi-automatic API generation.")

(code-tag (prueba)
  (defun prueba (x y z)
    (let ((code-hide (prueba) (h x) (u y) (l z)))
      (+ h z l u)
      (code-hide (prueba)))))

(code-block (prueba)
  prueba)


(write-in-file #P"README")
