;;;-----------------------------------------------------------------------------------;;
;;;名称:  尺寸替代  命令:dtd                                                          ;;
;;;说明:  达到自动更换标注值800==》8×100                                             ;;
;;;       将除数和商中小的放置在前面 如 输入100 或者8 都 =》8×100                    ;;
;;;       如果除数 > 被除数 ,则颠倒除数和被除数， 例 输入1600 =》 1600/2
;;;       如果以上均不存在，则替换标注值为输入的 被除数 ，如输入30,rem800÷30≠0，800==》30
;;;最后更新作者及时间：xyccf 20070819
(defun c:dtd (/ ss a b n i ent dxf data)
  (setq ss (ssget (list '(0 . "DIMENSION,ARC_DIMENSION"))))
  (setq a (getstring "\n请输入除数：")
        b (atof a))
  (if (<= b 0) (progn (princ "\n除数不正确")(exit)))
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq dxf (entget ent))
    (setq data (cdr (assoc 42 dxf)))
    (if (< data b)
      (progn
	(setq data b)
	(setq b (cdr (assoc 42 dxf)))
      )
    )
    (if	(or (< (rem data b) 10e-3)
	    (< (abs (- (rem data b) b)) 0.01)
	)
      (progn
	(setq n (/ data b))
	(if (not (equal n 1 10e-3))
	  (progn
	    (if (> (atof a) (cdr (assoc 42 dxf)))
	      (setq data (strcat a "/" (rtos n 2 0)))
	      (if (> (atof a) n)
	        (setq data (strcat (rtos n 2 0) "×" a))
	        (setq data (strcat a "×" (rtos n 2 0)))
	      )
	    )
	    (if	(/= (assoc 1 dxf) nil)
	      (setq dxf (subst (cons 1 data) (assoc 1 dxf) dxf))
	      (setq dxf (append (list (cons 1 data)) dxf))
	    )
	    (entmod dxf)
	  )
	)
      )
      (progn
	(setq dxf (subst (cons 1 a) (assoc 1 dxf) dxf))
	(entmod dxf)
;;;        (princ "\n无法整除")
      )
    )
    (setq i (1+ i))
  )
  (princ)
)
;;;-----------------------------------------------------------------------------------;;