;;;===================================================================================;;
;;;名称：标注对齐                                                                     ;;
;;;命令：dqd;;
;;;功能：动态对齐一排标注，支持水平、垂直、对齐，鼠标左键或按空格键结束程序           ;;
;;;引用：ss->lst
;;;-----------------------------------------------------------------------------------;;
(defun c:dqd (/ ss sslst gr pt entlst typlst pt10 pt11 pt13 pt14 ang50 pt1 pt2)
  (setq ss (ssget '((0 . "DIMENSION"))))
  (setq sslst (ss->lst ss))
  (while (not (or (eq 3 (car (setq gr (grread T 15 0))));鼠标左键或按空格键结束
		  (eq '(2 13) (car gr))
	      )
	 )
    (foreach x sslst
      (setq entlst (entget x)
	    typlst (mapcar
		     'cdr
		     (vl-remove-if-not '(lambda (x) (= 100 (car x))) entlst)
		   )
	    pt	   (last gr)
      )
      (member "AcDbAlignedDimension" typlst)
      (setq pt10 (cdr (assoc 10 entlst))
	    pt11 (cdr (assoc 11 entlst))
	    pt14 (cdr (assoc 14 entlst))
      )
      (if (member "AcDbRotatedDimension" typlst)
	(setq ang50 (cdr (assoc 50 entlst)))
	(setq pt13  (cdr (assoc 13 entlst))
	      ang50 (angle pt14 pt13)
	)
      )
      (setq pt1	   (inters pt (polar pt ang50 1.0) pt14 pt10 nil)
	    pt2	   (mapcar '(lambda (x1 x2 x3) (+ x1 (- x3 x2)))
			   pt11
			   pt10
			   pt1
		   )
	    entlst (subst (cons 10 pt1) (assoc 10 entlst) entlst)
	    entlst (subst (cons 11 pt2) (assoc 11 entlst) entlst)
      )
      (entmod entlst)
    )
  )
  (princ)
)

;;;-----------------------------------------------------------------------------------;;