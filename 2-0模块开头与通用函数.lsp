;;;-----------------------------------------------------------------------------------;;
;;;模块：  				    标注                                      ;;
;;;功能列表：                 命令 +   名称   +   修改日期   + 作者                   ;;
;;;     	     	       dzz   智能标注    2016-04-06    xyccf                  ;;×缺损组件
;;;			       dtd   尺寸替代    2015-12-24    xyccf                  ;;√
;;;                            dsd   尺寸文字锁定 2016-08-30   xyccf                  ;;√
;;;			       dcc   尺寸检查    2015-11-13    langjs                 ;;√
;;;			       dbr   尺寸避让    2015-08-25    waterchen              ;;√仍有BUG
;;;	           (不对应按钮)dgw   尺寸归位    2016-08-24    xyccf                  ;;√
;;;                            ddq   标注对齐    2016-02-04    xyccf                  ;;√
;;;			       dhf   焊缝标注       未知       无法考证               ;;√
;;;                (不对应按钮)dsc   预应力束    2015-09-29    xyccf                  ;;√
;;;                            dee   动态引线    2013-06-xx    langjs                 ;;√
;;;                            dqq   图框比例       未知       无法考证               ;;√
;;;-----------------------------------------------------------------------------------;;



;;;弧长标注子程序
(defun chc (sel / r ang angs ange larc pnt multi pt pt1 pt2 ename ent
	      a_last ssa)
  (setvar "CMDECHO" 0)
  (setq multi (getvar 'dimlfac))
  (setq blyz (cdr (assoc 144 (tblsearch "dimstyle" (getvar "dimstyle")))));设置测量比例因子
  (setq pt1 (getpoint "\n指定起点:"))
  (setq pt2 (getpoint "\n指定终点:"))
  (setq r (cdr (assoc 40 (entget sel)))) ;获得半径
  (setq pt (cdr (assoc 10 (entget sel)))) ;获得
  (setq ang (abs (- (angle pt pt1) (angle pt pt2))))
  (setq larc (* r ang))
  (setq larc (* larc multi))
  (setq larc (rtos larc 2 2))		;精度
  (vl-cmdf ".dimangular" "" pt pt1 pt2 "t" larc)
  (vl-cmdf pause)
  (setq entname (entlast))
  (setq ent (entget entname))
  (setq ang (cdr (assoc 42 ent)))
  (setq larc (* r ang))			;弧长
  (setq larc (* larc multi))
  (entmod (subst 
          (cons 1 (strcat "⌒" (rtos larc 2 1)))
		 (assoc 1 ent)
		 ent
	  )
  )					;
  (vl-cmdf "dimcontinue")
  (while (> (getvar "CMDACTIVE") 0)	;命令期间如果按下右健时结束命令
    (setq g (grread t 2 0))
    (cond
      ((and (= (car g) 2) (member (cadr g) '(13 32)))
       (vl-cmdf "" "" "")
      )
      ((member (car g) '(11 25)) (vl-cmdf "" "" ""))
      ((= (car g) 5)
       (vl-cmdf PAUSE)
       (setq dxf (entget (setq a_last (entlast))))
       (setq val (assoc 42 dxf))
       (setq new (* (cdr val) r blyz))
       (setq new (strcat "⌒" (rtos new 2 1))) ;精度
       (setq dxf (subst (cons 1 new) (assoc 1 dxf) dxf))
       (entmod dxf)
      )
    )
  )
  (setvar "CMDECHO" 1)
  (princ)
)
;;;-----------------------------------------------------------------------------------;;



(defun centsel (msg f / el)
 (while (if (setq el (car (entsel msg))) (if (= (cdr (assoc 0 (entget el))) f) nil t) nil)) el
)




	
