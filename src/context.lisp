
(in-package :adppvt)



(defmacro with-adp (&body body)
  `(let ((*adp* t)
	 (*context* (make-instance 'constext)))
     (declare (special *adp* *context*))
     ,@body))


(defmacro if-adp (then else)
  `(locally (declare (special *adp*))
     (if *adp*
	 ,then
	 ,else)))


(defmacro when-adp (&body body)
  `(if-adp
    (progn
      ,@body)
    nil))



