;;;-----------------------------------------------------------------------------------;;
;;;名称:  智能标注  命令:dzz                                                          ;;
;;;说明:  ①共线图块、donut视为点钢筋标注引线及对齐标注；                             ;;×
;;;       ②平行齐头直线视为线钢筋标注；                                              ;;×
;;;       ③闭合多段线视为标注面积及板件尺寸；                                        ;;×
;;;       ④非闭合多段线混合直线视为预应力钢筋标注 ；                                 ;;√
;;;       ⑤单圆弧则视为动态弧长标注，多个圆弧则批量标注弧长;                         ;;×
;;;       ⑥圆则则标注d，r                                                            ;;×
;;;引用： ss->lst-type / chc
(defun c:dzz (/ ss ss_typ_lst major_ent maj_ent_lst)
  (setq ss nil)
  (while (not ss)
    (setq ss (ssget (car (list nil (print "请选择图元")))))
  )
  (setq ss_typ_lst (ss->lst-type ss)
	major_ent (caar ss_typ_lst)
	maj_ent_lst (cdar ss_typ_lst)
	)
  (cond ((member major_ent '("BLOCK" "DONUT" "POINT" "INSERT"))
	 (princ "\n功能完善中》》》")
	 )
	((member major_ent '("LINE" "POLYLINE" "LWPOLYLINE" "SPLINE" "ELLIPSE"))
	 (princ "\n功能完善中》》》")
	 )
	((equal major_ent "CIRCLE")
	 (foreach x maj_ent_lst
	   (setq dxf (entget x))
	   (setq c (cdr (assoc 10 dxf));圆心
		 r (cdr (assoc 40 dxf));半径
		 pt1 (polar c (- (* (/ pi 6) 5)) r) 
		 pt2 (polar c (/ pi 6) (* 2 r))
		 pt3 (polar c (/ pi 6) r)
		 )
	     (entmake (list '(0 . "DIMENSION")
			    '(100 . "AcDbEntity")
			    '(100 . "AcDbDimension")
			    (cons 10 pt1)
			    (cons 11 pt2)
			    '(70 . 163)
			    '(100 . "AcDbDiametricDimension")
			    (cons 15 pt3)
                      )
             )
	  )
	)
	((equal major_ent "ARC")
	 (if (= (length (car ss_typ_lst)) 2)
	   (if (setq sel (nth 1 maj_ent_lst))
	    (chc sel)
	   )
	 )
        )
	(t
	 (exit)
	)
  )
)
;;;-----------------------------------------------------------------------------------;;