(defun qt()
    (setq a nil)
    (while lst
      (setq b nil)
      (repeat 2                            ;一层表嵌套2层
        (setq b (cons (car lst) b) lst (cdr lst)))
      (setq a (cons (reverse b) a))
    )
)
(defun px()
     (vl-sort a                   ;以下根据y坐标对表排序(从大到小)
	 '(lambda (e1 e2)
	    (> (nth 1 e1) (nth 1 e2)
	    )
	 )
     )
)
;获得图元颜色（可以随层）
(defun ys (en1)
  (if (= (assoc 62 en1) nil)
      (setq color (cdr (assoc 62 (entget (tblobjname "layer" (cdr (assoc 8 en1)))))))
      (setq color (cdr (assoc 62 en1)))
  )
)
;标注半径dimradius
(defun dimrad(pt3 pt4 pt5 dr)
     (entmake (list '(0 . "DIMENSION")
		    '(2 . "*D1102")
	            '(100 . "AcDbEntity")
	            '(100 . "AcDbDimension")
	             (cons 10 pt3)          ;箭尾
	             (cons 11 pt5)          ;文字中心
		     (cons 1 dr)
	            '(70 . 164)             ;文字在外164 ，附着36
	            '(71 . 5)               ;文字附着样式
	            '(100 . "AcDbRadialDimension")
	             (cons 15 pt4)          ;箭头
	      )
     )
)
;水平标注
(defun entdimlin(pt pt1 pt2)
     (entmake (list '(0 . "DIMENSION")
	            '(100 . "AcDbEntity")
	            '(100 . "AcDbDimension")
	             (cons 10 pt)          ;尺寸线位置
	            '(70 . 32)
	            '(8 . "标注")          ;此处修改图层，可不设置
	            '(1 . "")
	            '(100 . "AcDbAlignedDimension")
	             (cons 13 pt1)         ;起点
	             (cons 14 pt2)         ;终点
	            '(100 . "AcDbRotatedDimension")
	      )
     )
)

;标注角度和弧度的子函数
(defun bulge (sel bl / elst obj vex i tu pt1 pt2 dis h h1 ang txt txtpt)
       (setq elst (entget sel))
       (setq obj (vlax-ename->vla-object sel));将AutoLISP 类型的对象名转换为 VLA 对象
       (setq vex (cdr (assoc 90 elst)));返回顶点数
       (setq i 0)
      (repeat (- vex 1)
	(setq tu (vla-getbulge obj i));得到曲线第一段的凸度 !(负数代表向下凸起，正数向上)
	(setq pt1 (vlax-curve-getpointatparam sel i))
	(setq pt2 (vlax-curve-getpointatparam sel (1+ i)))
	(if (< (car pt1) (car pt2))
	  (setq j t) (setq j nil)
	  )
	(setq dis (distance pt1 pt2))
	(if (/= tu 0)
	  (progn
	    (setq radius (/ (* (1+ (* tu tu)) dis 0.25) (abs tu)))
	    (setq dr (strcat "R"(rtos (* radius blyz) 2 0)))
	    (setq h (* dis (abs tu) 0.5) ;弦高
		  h1 (- radius h)
            )
	    (if (= j t)
	      (setq tem (angle pt1 pt2))
	      (setq tem (angle pt2 pt1))
	      )
	    (setq cen (midp pt1 pt2)) ;弦中心点坐标
	    (setq pt3 (polar cen (+ (angle pt1 pt2)
			           (cond ( (and (= s t) (< tu 0)) (* pi -0.5))
					 ( (and (= s t) (> tu 0)) (* pi 0.5))
					 ( (and (= s nil) (< tu 0)) (* pi -0.5))
			                 ( (and (= s nil) (> tu 0)) (* pi 0.5))
			           )
			         )
		        (* bl 12)
		      )
            )                        ;标注箭尾
	    (setq pt4 (polar cen (+ tem
			    (cond ( (and (< tu 0) (= j t)) (* pi 0.5) )
			          ( (and (< tu 0) (= j nil)) (* pi -0.5) )
				  ( (and (> tu 0) (= j nil)) (* pi 0.5) )
				  ( (and (> tu 0) (= j t)) (* pi -0.5) )
			    )
			         )
			 h
		      )              ;（标注箭头）圆弧中点
            )  
	    (cond
	      ((and (< (car pt4) (car pt3)) (< (nth 1 pt4) (nth 1 pt3))) 
	       (setq pt5 (list (- (car pt3) (* 1.5 bl)) (- (cadr pt3) (* 3 bl)) 0))
	       )
              ((and (< (car pt4) (car pt3)) (> (nth 1 pt4) (nth 1 pt3))) 
	       (setq pt5 (list (+ (car pt3) (* 1.5 bl)) (+ (cadr pt3) (* 3 bl)) 0))
	       )
	      ((and (> (car pt4) (car pt3)) (< (nth 1 pt4) (nth 1 pt3))) 
	       (setq pt5 (list (+ (car pt3) (* 1.5 bl)) (- (cadr pt3) (* 3 bl)) 0))
	       )
	      ((and (> (car pt4) (car pt3)) (> (nth 1 pt4) (nth 1 pt3))) 
	       (setq pt5 (list (- (car pt3) (* 1.5 bl)) (+ (cadr pt3) (* 3 bl)) 0))
	       )
	    ) ;end cond
          (dimrad pt3 pt4 pt5 dr)
	  );end progn
	  (progn
	    (if (= j t)
	      (setq ang (* (/ (angle pt1 pt2) pi) 180))
	      (setq ang (* (/ (angle pt2 pt1) pi) 180))
	      )
	    (setq a (rtos (abs ang) 2 0))
	    (if (and (/= a "0") (/= a "180") (/= a "360"))
	      (progn
	        (cond
		   ((> ang 270)
		     (setq txt (strcat (rtos (abs (- 360 ang)) 2 1)"°"))
		    )
		   ((< ang 90)
		     (setq txt (strcat (rtos ang 2 1)"°"))
		    )
		 )
	        (setq txtpt (list (/ (+ (car pt1) (car pt2)) 2) (+ (/ (+ (nth 1 pt1) (nth 1 pt2)) 2) (* 2 bl)) 0))
		(command "text" "m" txtpt (* 2 bl) ang txt)
	      )
	    )
	  );end progn
       ) ;end if
	(setq i (1+ i))
      )
)

;;;===================================================================================;;
;说明：1dsc用于预应力束的批量标注，请保持预应力束与边界线的颜色区别，函数中默认(1-红色)为预应力束
;      2使用当前的图层、标注样式、文字样式，请在命令开始前设置好
;      3成对框选目标，不得夹杂其他非边界线和预应力束的线段，文字图块可以
;      4请将标注样式中的文字位置设为（尺寸线旁边）
;本程序作者xyccf，水平有限，欢迎指正.邮箱1176483465@qq.com      prestressed reinforcements
(defun c:dsc ()
  (setq ss (ssget (list '(0 . "CIRCLE,ARC,ELLIPSE,LINE,*POLYLINE,SPLINE"))))
  (dim_pr ss)
)
(defun dim_pr (ss / os ent yy temp lst n i ptlst entlst ent e e1 s bl blyz r ddlst minpty minpty gxlst pt1 pt2 fzx jd1 jd2 interpts s sel)
    (setvar "cmdecho" 0)
    (setq os (getvar "osmode"))
    (setvar "osmode" 0)
    (setq n 0
          lst nil)
    (repeat (sslength ss)
      (setq ent (ssname ss n))
      (setq yy (eval (append (list +) (mapcar '(lambda (x) (nth 1 x)) (xl-3p ent) ))));这句妙吧^_^
      (setq temp (list ent yy))
      (setq lst (append lst temp))
      (setq n (1+ n))
    )                                  ;获得图元名及y坐标的表以便排序
    (qt)
    (setq ptlst (px))
    (setq entlst nil)
    (repeat (length ptlst)                    ;以下从偶数表中剔除偶数位元素,去掉y坐标只留图名
      (setq temp (list (caar ptlst)))
      (setq entlst (append entlst temp))
      (setq ptlst (cdr ptlst))
    )                                         ;边界线和预应力束成对元素组成的表
    ;(setvar "dimtmove" 2)
    ;(setq dxf (entget (tblobjname "dimstyle" (getvar "dimstyle"))))
    ;(entmod (if (assoc 287 dxf)
	      ; (subst (cons 287 5) (assoc 287 dxf) dxf)
	     ;  (append dxf (list '(287 . 5)))
	     ; )
	  ;  )
 (repeat (/ (length entlst) 2)
    (setq en1 (entget(car entlst)))
    (if (= (ys en1) 1)                        ; 1代表判断条件，预应力束是e
           (setq e (car entlst) e1 (cadr entlst) s t)
           (setq e (cadr entlst) e1 (car entlst) s nil) 
      )                                       ;红色为预应力线，其他为边界线
    (pldlst)                                  ;此处得到预应力束节点坐标表
    (setq bl (cdr (assoc 40 (tblsearch "dimstyle" (getvar "dimstyle")))))
    (setq blyz (cdr (assoc 144 (tblsearch "dimstyle" (getvar "dimstyle")))));设置测量比例因子
    (setq r (* bl 0.5))                       ;圆半径
    (setq ent e1)
    (setq ddlst (xl-3p))                        ;此处得到边界线端点坐标
    (if (< (cadr (car ddlst)) (cadr (cadr ddlst)))
       (setq minpty (cadr (car ddlst))
	     maxpty (cadr (cadr ddlst)))
       (setq maxpty (cadr (car ddlst))
	     minpty (cadr (cadr ddlst)))
      )                                     ;此处得到边界线坐标范围
    (setq gxlst nil)
   (repeat (length lst)
     (setq pt1 (car lst))
     (if (= s t) (setq pt2 (list (car pt1) (- minpty 150)))
                 (setq pt2 (list (car pt1) (+ maxpty 150)))
       )                                      ;生成直线与边界线相交，以取得交点坐标
     (entmake (list '(0 . "CIRCLE") (cons 10 pt1) (cons 40 r)))
     (entmake (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt2)))
     (setq fzx (entlast))
     (setq jd1 (vlax-ename->vla-object fzx) 
           jd2 (vlax-ename->vla-object e1)       ;图元名转VLA对象
           interpts (vla-intersectwith jd1 jd2 0);交点变体
           interpts (vlax-variant-value interpts);数组
           pt2 (vlax-safearray->list interpts)   ;坐标
     )
     (command "._erase" fzx "")
     (entmake (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt2)))
     (setq gx (entlast))
     (setq gxlst (cons gx gxlst))
     (setq txt (rtos (* (distance pt1 pt2) blyz) 2 1))
     (if (= s t)
        (setq txtpt (list (+ (car pt1) bl) (+ (cadr pt1) (* bl 2)))) 
        (setq txtpt (list (+ (car pt1) bl) (- (cadr pt1) (* bl (+ (strlen txt) 2))))) ;(strlen txt)字符长度
      )
     (if (/= (distance pt1 pt2) 0)
      (entmake (list '(0 . "TEXT")
		    '(100 . "AcDbEntity")
		    '(100 . "AcDbText")
		     (cons 1 txt)
		     (cons 10 txtpt)
		    '(8 . "文本")
		     (cons 40 (* 2 bl))
		    '(41 . 0.75)
		    '(50 . 1.5708)
                    '(72 . 0) ;4-文字对正样式中间，0是左(? 4不能生成)
		    )
	      )
       )
     ;(command "justifytext" (entlast) "" "m");对生成的文字对正
     (setq lst (cdr lst))
  )
     (if (= s t)
       (setq pt (list (car (cdr (assoc 11 (entget (car gxlst))))) (- (cadr (cdr(assoc 11 (entget (car gxlst))))) (* bl 8)) 0))
       (setq pt (list (car (cdr (assoc 11 (entget (car gxlst))))) (+ (cadr (cdr(assoc 11 (entget (car gxlst))))) (* bl 8)) 0));开始标注
     )
     (repeat (- (length gxlst) 1)
          (setq pt1 (cdr (assoc 11 (entget (car gxlst)))))
          (setq pt2 (cdr (assoc 11 (entget (cadr gxlst)))))
          (entdimlin pt pt1 pt2)
     (setq gxlst (cdr gxlst))
     )
    (if (= s t)
      (progn
        (setq sel (car entlst))
        (bulge sel bl)
	)
      (progn
        (setq sel (cadr entlst))
        (bulge sel bl)
	)
    )
    (setq entlst (cddr entlst))
 );标注结束，退出循环
  (setvar "osmode" os)
  (princ)
)
;;;===================================================================================;;