;图层全开、解锁、解冻
(defun c:ca ()				
  (foreach x (setq *laystatus
		    (mapcar
		      '(lambda (x) (entget (TBLOBJNAME "layer" x)))
		      (getty "layer");获得所有图层名列表
		    )
	     )
    (entmod (subst (cons 62 (abs (cdr (assoc 62 x))))
		   (assoc 62 x)
		   (subst '(70 . 0) (assoc 70 x) x)
	    )
    )
  )
  (princ)
)
 ;;恢复图层至上一个状态
(defun c:cf ()
  (foreach x *laystatus
    (entmod x)
  )
  (setq *laystatus nil)
  (princ)
)