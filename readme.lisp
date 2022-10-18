
(in-package :adp)


(header "Add Documentation, Please")

(text (italic "Add Documentation, Please") " is a library for literate programming and semi-automatic API generation.")

(text "Let's try to use some links. For example: " (web-link "Hyperspec" "http://www.lispworks.com/documentation/HyperSpec/Front/"))

(code-tag (prueba)
  (defun prueba (x y z)
    (let ((code-hide (prueba) (h x) (u y) (l z)))
      (+ h z l u)
      (code-hide (prueba)))))

(code-block (prueba)
  prueba
  (defun otra-prueba (x)
    (print "Hola")))


(itemize (:item "Esto es una lista:")
	 (:itemize (:item "Esto es una sublista:")
		   (:itemize (:item "Otra sublista.")
			     (:item "3+4 = " (+ 3 4)))
		   (:item "Final de la sublista"))
	 (:item "Final de la lista."))

(code-example
  (let ((bottom 2) (ceil 8))
    (loop for i from bottom below ceil
	  do (format t "Iteración ~s~%" i))
    (values 'hola 'adios)))

(table ((:cell "Header1") (:cell "Header 2") (:cell "Header 3"))
       ((:cell 3) (:cell 4) (:cell 5))
       ((:cell "Hola " (+ 3 4)) (:cell "Adios") (:cell "Casa")))

(write-in-file #P"README")
