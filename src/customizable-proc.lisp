
(in-package :adppvt)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct writer
    proc
    definer)
  
  (let ((writers nil))

    (defmacro with-special-writers (&body body)
      (let ((let-proc-binds (mapcar (lambda (writer)
				      (list (writer-proc writer) nil))
				    writers)))
	`(let ,let-proc-binds
	   (declare (special ,@writers))
	   ,@body)))

    (defmacro check-special-writers ()
      (let ((unless-exprs (mapcar (lambda (writer)
				    `(unless ,(writer-proc writer)
				       (error ,(concatenate 'string "The function " (symbol-name (writer-definer writer)) " is not used in the current style."))))
				  writers)))
	`(progn
	   ,@unless-exprs)))
    
    (defmacro define-customizable-writer (writer-proc writer-definer num-args)
      (check-type name symbol)
      (check-type args unsigned-byte)
      (let ((writer-args (loop repeat num-args do (gensym))))
	(push (make-writer :proc writer-proc :definer writer-definer) writers)
	(with-gensyms (body proc-args)
	  `(defmacro ,writer-definer (,writer-args &body ,body)
	     (declare (special ,writer-proc))
	     (let ((,proc-args (list ,@writer-args)))
	       `(setf ,',writer-proc (lambda ,@,proc-args
				       ,@,body)))))))))
