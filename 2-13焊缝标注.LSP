;;;===================================================================================;;
;;;名称:  焊缝标注  命令:dhf                                                          ;;
;;;说明:  本程序用于焊缝符号的标注                                                    ;;
;;;作者:  Rimford 2015-4-17
(defun c:dhf()			
;**************************本程序为错误处理程序*****************************************
  (defun sys_var ()
    (setq e_lst
	   (mapcar
	     (function
	       (lambda (n) (list 'setvar n (getvar n)))
	     ) ;_ 结束function
	     '("cecolor"     "clayer"	   "LUPrec"	 "autosnap"
	       "osmode"	     "aperture"	   "hpspace"	 "hpassoc"
	       "mirrtext"    "auprec"	   "luprec"	 "dimzin"
	      )
	   ) ;_ 结束mapcar
    ) ;_ 结束setq
    (defun *error* (msg) (mapcar 'eval e_lst))
  ) ;_ 结束defun

  (defun RD&leader (/ RDlst RDct)	;绘制带箭头的引线
    (setq RDlst '())
    (initget 1 "N")
    (setq RDpt1 (getpoint "\n起点: "))
    (if	(= RDpt1 "N")
      (setq RDpt1 nil)
      (progn (setq RDlst (append RDlst (list RDpt1)))
	     (initget 1)
	     (while (setq RDpt2 (getpoint RDpt1 "\n指向: "))
	       (grdraw RDpt1 RDpt2 7 0)
	       (setq RDlst (append RDlst (list RDpt2)))
	       (setq RDpt3 RDpt1
		     RDpt1 RDpt2
	       ) ;_ 结束setq
	     ) ;_ 结束while
	     (setq RDct 1)
	     (setvar "cmdecho" 0)
	     (setvar "osmode" 0)
	     (command "_.leader" (nth 0 RDlst))
	     (while (nth RDct RDlst)
	       (command (nth RDct RDlst))
	       (setq RDct (1+ RDct))
	     ) ;_ 结束while
	     (command "" "" "_n")
	     (redraw)
	     (setq RDlast (entlast))
      ) ;_ 结束progn
    ) ;_ 结束if
  ) ;_ 结束defun

  (defun show_list (key lista)
    (start_list key)
    (mapcar 'add_list lista)		;mapcar=将dat_list中的元素一一加入add_list中,要加 '
    (end_list)
  )

  (defun point (point0 dx dy)		;获得由point0偏移(dx dy)的点
    (polar (polar point0 0 dx) (dtor 90) dy)
  )

  (defun dtor (dang)			;角度转弧度
    (* dang (/ pi 180))
  )

;****************************************************************************************************************
  (defun wcircle (/ p1 p2 p3)		;绘制全周焊接符号
    (if	(= n_circle "1")
      (progn
	(setq p1 RDpt1
	      p2 (polar p1 0 (* 5 wline))
	      p3 (polar p1 (dtor 90) (* 5 wline))

	) ;_ 结束setq
	(command "pline" p2 "w" wline wline "A" "ce" p1 p3 "cl")
	(setq l1 (* 8 wline)
	      l2 (* 8 wline)
	)
      ) ;_ 结束progn
      (progn
	(setq l1 (* 4 wline)
	      l2 (* 4 wline)
	)
      )
    ) ;_ 结束if
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun wflag (/ p1 p2 p3 p4 p5)	;绘制现场焊接符号
    (if	(= n_local "1")
      (progn
	(setq p1 RDpt1
	      p2 (polar p1 (dtor 90) (* 12 wline))
	      p3 (polar p2 (dtor 90) (* 10 wline))
	      p4 (polar p2 0 (* 15 wline))
	      p5 (polar p2 (dtor 45) (* 2 wline))
	) ;_ 结束setq
	(command "pline" p1 "w" wline wline p2 p3 p4 p2 "")
	(setvar "hpname" "Solid")	;设置填充图案
	(command "bhatch" p5 "")	;填充
      ) ;_ 结束progn
    ) ;_ 结束if
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun wtri (/ p0 p1 p2 p3 p4)	;绘制三面焊缝符号
    (if	(= n_tri_top "1")
      (progn
	(setq p1 (polar RDpt1 0 l1)
	      p2 (polar p1 (dtor 90) (* 10 wline))
	      p3 (polar p2 0 (* 12 wline))
	      p4 (polar p1 0 (* 12 wline))
	) ;_ 结束setq
	(command "pline" p3 "w" wline wline p2 p1 p4 "")
	(ssadd (entlast) sstext)
	(setq l1 (+ l1 (* 14 wline)))
      ) ;_ 结束progn
    ) ;_ 结束if

    (if	(= n_tri_bottom "1")
      (progn
	(if (= n_symbol_top n_symbol_bottom)
	  (setq p1 (point RDpt1 l2 0))
	  (setq p1 (point RDpt1 l2 (- 0 (* 3 wline))))
	)
	(setq p2 (polar p1 (dtor 270) (* 10 wline))
	      p3 (polar p2 0 (* 12 wline))
	      p4 (polar p1 0 (* 12 wline))
	) ;_ 结束setq
	(command "pline" p3 "w" wline wline p2 p1 p4 "")
	(ssadd (entlast) sstext)
	(setq l2 (+ l2 (* 14 wline)))
      )
    ) ;_ 结束if
    (setq l1 (max l1 l2)
	  l2 (max l1 l2)
    )
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun wtextfront (/ a an ptx1)	;绘制焊缝符号前的文字
    (if	(/= front_top "")
      (progn
	(setq ptx1 (polar RDpt1 0 l1))
	(command "_.TEXT" "R" ptx1 htext "0" front_top)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l1 (+ l1 l0 wline))
      ) ;_ 结束progn
    )

    (if	(/= front_bottom "")
      (progn
	(if (= n_symbol_top n_symbol_bottom)
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 10 wline))))
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 13 wline))))
	)
	(command "_.TEXT" "R" ptx1 htext "0" front_bottom)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l2 (+ l2 l0 wline))
      ) ;_ 结束progn
    )
    (setq l1 (max l1 l2)
	  l2 (max l1 l2)
    )
  )

;****************************************************************************************************************
  (defun wtextback (/ a an ptx1)	;绘制焊缝符号前的文字
    (if	(/= back_top "")
      (progn
	(setq ptx1 (polar RDpt1 0 l1))
	(command "_.TEXT" "R" ptx1 htext "0" back_top)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l1 (+ l1 l0 wline))
      ) ;_ 结束progn
    )

    (if	(/= back_bottom "")
      (progn
	(if (= n_symbol_top n_symbol_bottom)
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 10 wline))))
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 13 wline))))
	)
	(command "_.TEXT" "R" ptx1 htext "0" back_bottom)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l2 (+ l2 l0 wline))
      ) ;_ 结束progn
    )

    (setq l1 (max l1 l2)
	  l2 (max l1 l2)
    )
  )

;****************************************************************************************************************
  (defun wtextalter (/ a an ptx1)	;绘制交错符号后的文字
    (if	(/= distance_top "")
      (progn
	(setq ptx1 (polar RDpt1 0 l1))
	(command "_.TEXT"
		 "R"
		 ptx1
		 htext
		 "0"
		 (strcat "(" distance_top ")")
	)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l1 (+ l1 l0 wline))
      ) ;_ 结束progn
    )

    (if	(/= distance_bottom "")
      (progn
	(if (= n_symbol_top n_symbol_bottom)
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 10 wline))))
	  (setq ptx1 (point RDpt1 l2 (- 0 (* 13 wline))))
	)
	(command "_.TEXT"
		 "R"
		 ptx1
		 htext
		 "0"
		 (strcat "(" distance_bottom ")")
	)
	(setq b (entget (entlast)))
	(setq l0 (distance (cdr (assoc 10 b)) (cdr (assoc 11 b))))
	(setq ptx2 (polar ptx1 0 l0))
	(command "move" (entlast) "" ptx1 ptx2)
	(ssadd (entlast) sstext)
	(setq l2 (+ l2 l0 wline))
      ) ;_ 结束progn
    )

    (if	(= n_circle "1")
      (setq l1 (+ (max l1 l2) (* 8 wline)))
      (setq l2 (+ (max l1 l2) (* 4 wline)))
    )

  )

;****************************************************************************************************************
  (defun weldline (/ p1 p2 p3 p4)	;绘制基准线
    (setq p1 RDpt1
	  p2 (polar p1 0 l1)
	  p3 (polar p1 (dtor 270) (* 3 wline))
	  p4 (polar p2 (dtor 270) (* 3 wline))
    ) ;_ 结束setq
    (command "line" p1 p2 "")
    (ssadd (entlast) sstext)
    (if	(/= n_symbol_top n_symbol_bottom)
      (progn
	(setvar "clayer" "dash")
	(setvar "CELTSCALE" 0.2)
	(command "line" p3 p4 "")
	(ssadd (entlast) sstext)
	(setvar "clayer" "dim")
	(setvar "CELTSCALE" 1.0)
      ) ;_ 结束progn
    ) ;_ 结束if
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_T_top ()			;绘制角焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 0 (* 5 wline))
	  p3 (polar p1 (dtor 180) (* 5 wline))
	  p4 (polar p3 (dtor 90) (* 10 wline))
    ) ;_ 结束setq
    (command "pline" p4 "w" wline wline p2 p3 "c")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 16 wline)))
  ) ;_ 结束defun	

;****************************************************************************************************************
  (defun ws_T_bottom ()			;绘制角焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )

    (setq p2 (polar p11 0 (* 5 wline))
	  p3 (polar p11 (dtor 180) (* 5 wline))
	  p4 (polar p3 (dtor 270) (* 10 wline))
    ) ;_ 结束setq
    (command "pline" p4 "w" wline wline p2 p3 "c")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 16 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_V_top ()			;绘制V焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 60) (* 11.547 wline))
	  p3 (polar p1 (dtor 120) (* 11.547 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p1 p3 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun	  

;****************************************************************************************************************
  (defun ws_V_bottom ()			;绘制V焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2 (polar p11 (dtor 240) (* 11.547 wline))
	  p3 (polar p11 (dtor 300) (* 11.547 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p11 p3 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_Y_top ()			;绘制Y焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 90) (* 4 wline))
	  p3 (polar p2 (dtor 60) (* 6.928 wline))
	  p4 (polar p2 (dtor 120) (* 6.928 wline))
    ) ;_ 结束setq
    (command "pline" p1 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p3 "w" wline wline p2 p4 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun	  

;****************************************************************************************************************
  (defun ws_Y_bottom ()			;绘制Y焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2 (polar p11 (dtor 270) (* 4 wline))
	  p3 (polar p2 (dtor 240) (* 6.928 wline))
	  p4 (polar p2 (dtor 300) (* 6.928 wline))
    ) ;_ 结束setq
    (command "pline" p11 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p3 "w" wline wline p2 p4 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_U_top ()			;绘制U焊缝符号
    (setq p1   (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2   (polar p1 (dtor 90) (* 3 wline))
	  pcen (polar p2 (dtor 90) (* 4.5 wline))
	  p4   (point pcen (- 0 (* 4.5 wline)) (* 2.5 wline))
	  p5   (polar pcen (dtor 180) (* 4.5 wline))
	  p6   (polar pcen 0 (* 4.5 wline))
	  p7   (polar p6 (dtor 90) (* 2.5 wline))
    ) ;_ 结束setq
    (command "pline" p1 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p4 "w" wline wline p5 "A" p6 "L" p7 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_U_bottom ()			;绘制U焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2   (polar p11 (dtor 270) (* 3 wline))
	  pcen (polar p2 (dtor 270) (* 4.5 wline))
	  p4   (point pcen (- 0 (* 4.5 wline)) (- 0 (* 2.5 wline)))
	  p5   (polar pcen (dtor 180) (* 4.5 wline))
	  p6   (polar pcen 0 (* 4.5 wline))
	  p7   (polar p6 (dtor 270) (* 2.5 wline))
    ) ;_ 结束setq
    (command "pline" p11 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p4 "w" wline wline p5 "A" p6 "L" p7 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_DV_top ()			;绘制单边V焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 180) (* 5 wline))
	  p3 (polar p2 (dtor 90) (* 10 wline))
	  p4 (polar p2 (dtor 45) (* 14.14 wline))
    ) ;_ 结束setq
    (command "pline" p3 "w" wline wline p2 p4 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun	  

;****************************************************************************************************************
  (defun ws_DV_bottom ()		;绘制单边V焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq pp2 (polar p11 (dtor 180) (* 5 wline))
	  p3  (polar p2 (dtor 270) (* 10 wline))
	  p4  (polar p2 (dtor 315) (* 14.14 wline))
    ) ;_ 结束setq
    (command "pline" p3 "w" wline wline p2 p4 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_DY_top ()			;绘制单边Y焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 180) (* 5 wline))
	  p3 (polar p2 (dtor 90) (* 4 wline))
	  p4 (polar p3 (dtor 90) (* 6 wline))
	  p5 (point p3 (* 10 wline) (* 6 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (command "pline" p3 "w" wline wline p5 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun
  
;****************************************************************************************************************
  (defun ws_DY_bottom ()		;绘制单边Y焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2 (polar p11 (dtor 180) (* 5 wline))
	  p3 (polar p2 (dtor 270) (* 4 wline))
	  p4 (polar p3 (dtor 270) (* 6 wline))
	  p5 (point p3 (* 10 wline) (- 0 (* 6 wline)))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (command "pline" p3 "w" wline wline p5 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_DU_top ()			;绘制单边U焊缝符号
    (setq p1   (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2   (polar p1 (dtor 90) (* 3 wline))
	  pcen (polar p2 (dtor 90) (* 4.5 wline))
	  p3   (polar pcen (dtor 90) (* 2.5 wline))
	  p4   (polar pcen 0 (* 4.5 wline))
	  p5   (polar p4 (dtor 90) (* 2.5 wline))
    ) ;_ 结束setq
    (command "pline" p1 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p3	"w" wline wline	p2 "A" "CE" pcen p4 "L"	p5 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_DU_bottom (/)		;绘制单边U焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2   (polar p11 (dtor 270) (* 3 wline))
	  pcen (polar p2 (dtor 270) (* 4.5 wline))
	  p3   (polar pcen (dtor 270) (* 2.5 wline))
	  p4   (polar pcen 0 (* 4.5 wline))
	  p5   (polar p4 (dtor 270) (* 2.5 wline))
    ) ;_ 结束setq
    (command "pline" p11 "w" wline wline p2 "")
    (ssadd (entlast) sstext)
    (command "pline" p5	"w" wline wline	p4 "A" "CE" pcen p2 "L"	p3 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_I_top ()			;绘制I焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 180) (* 3.5 wline))
	  p3 (polar p2 (dtor 90) (* 10 wline))
	  p4 (polar p2 0 (* 7 wline))
	  p5 (polar p4 (dtor 90) (* 10 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p3 "")
    (ssadd (entlast) sstext)
    (command "pline" p5 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_I_bottom ()			;绘制I焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2 (polar p11 (dtor 180) (* 3.5 wline))
	  p3 (polar p2 (dtor 270) (* 10 wline))
	  p4 (polar p2 0 (* 7 wline))
	  p5 (polar p4 (dtor 270) (* 10 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p3 "")
    (ssadd (entlast) sstext)
    (command "pline" p5 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_FD_top ()			;绘制封底焊缝符号
    (setq p1 (polar RDpt1 0 (+ l1 (* 6 wline)))
	  p2 (polar p1 (dtor 180) (* 7.416 wline))
	  p3 (polar p1 (dtor 270) (* 3 wline))
	  p4 (polar p1 0 (* 7.416 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (command "pline" p4 "w" wline wline "A" "CE" p3 p2 "")
    (ssadd (entlast) sstext)
    (setq l1 (+ l1 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun ws_FD_bottom ()		;绘制封底焊缝符号
    (if	(= n_symbol_top n_symbol_bottom)
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) 0))
      (setq p11 (point RDpt1 (+ l2 (* 6 wline)) (- 0 (* 3 wline))))
    )
    (setq p2 (polar p11 (dtor 180) (* 7.416 wline))
	  p3 (polar p11 (dtor 90) (* 3 wline))
	  p4 (polar p11 0 (* 7.416 wline))
    ) ;_ 结束setq
    (command "pline" p2 "w" wline wline p4 "")
    (ssadd (entlast) sstext)
    (command "pline" p2 "w" wline wline "A" "CE" p3 p4 "")
    (ssadd (entlast) sstext)
    (setq l2 (+ l2 (* 12 wline)))
  ) ;_ 结束defun

;****************************************************************************************************************
  (defun wsymbol ()			;本程序用于安排绘制哪个焊缝符号
    (cond
      ((= n_symbol_top "1") (ws_T_top))	;角焊缝
      ((= n_symbol_top "2") (ws_V_top))	;V形焊缝
      ((= n_symbol_top "3") (ws_Y_top))	;Y形焊缝
      ((= n_symbol_top "4") (ws_U_top))	;U形焊缝
      ((= n_symbol_top "5") (ws_DV_top)) ;单边V形焊缝
      ((= n_symbol_top "6") (ws_DY_top)) ;单边Y形焊缝
      ((= n_symbol_top "7") (ws_DU_top)) ;单边U形焊缝
      ((= n_symbol_top "8") (ws_I_top))	;I形焊缝
      ((= n_symbol_top "9") (ws_FD_top)) ;封底焊缝
    )
    (cond
      ((= n_symbol_bottom "1") (ws_T_bottom)) ;角焊缝
      ((= n_symbol_bottom "2") (ws_V_bottom)) ;V形焊缝
      ((= n_symbol_bottom "3") (ws_Y_bottom)) ;Y形焊缝
      ((= n_symbol_bottom "4") (ws_U_bottom)) ;U形焊缝
      ((= n_symbol_bottom "5") (ws_DV_bottom)) ;单边V形焊缝
      ((= n_symbol_bottom "6") (ws_DY_bottom)) ;单边Y形焊缝
      ((= n_symbol_bottom "7") (ws_DU_bottom)) ;单边U形焊缝
      ((= n_symbol_bottom "8") (ws_I_bottom)) ;I形焊缝
      ((= n_symbol_bottom "9") (ws_FD_bottom)) ;封底焊缝
    )
  )

;****************************************************************************************************************
  (defun wangle	(/)			;本程序用于绘制焊缝角度文字
    (setq angle_top (nth (atoi n_angle_top) listangle))
    (setq angle_bottom (nth (atoi n_angle_bottom) listangle))

    (if	(/= angle_top "")
      (progn
	(setq p2 (polar p1 (dtor 90) (* 10 wline)))
	(command "_.TEXT" "BC" p2 htext 0 angle_top)
	(ssadd (entlast) sstext)
      )
    )

    (if	(/= angle_bottom "")
      (progn
	(setq p2 (polar p11 (dtor 270) (* 10 wline)))
	(command "_.TEXT" "TC" p2 htext 0 angle_bottom)
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_T_top ()			;绘制凸面形状符号
    (if	(= n_symbol_top "1")
      (progn
	(setq p2 (point p1 (- 0 wline) (* 4 wline)))
	(setq p3 (polar p2 (dtor 90) (* 8 wline)))
	(setq p4 (polar p2 0 (* 8 wline)))
	(command "pline"
		 p4
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p3
		 ""
	)
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_top "")
	  (setq p2 (polar p1 (dtor 90) (* 20 wline)))
	  (setq p2 (polar p1 (dtor 90) (* 12 wline)))
	)
	(setq p3 (polar p2 0 (* 6.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 6.5 wline)))
	(command "pline"
		 p3
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p4
		 ""
	)
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_T_bottom	()		;绘制凸面形状符号
    (if	(= n_symbol_bottom "1")
      (progn
	(setq p2 (point p11 (- 0 wline) (- 0 (* 4 wline))))
	(setq p3 (polar p2 (dtor 270) (* 8 wline)))
	(setq p4 (polar p2 0 (* 8 wline)))
	(command "pline"
		 p3
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p4
		 ""
	)
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_bottom "")
	  (setq p2 (polar p11 (dtor 270) (* 20 wline)))
	  (setq p2 (polar p11 (dtor 270) (* 12 wline)))
	)
	(setq p3 (polar p2 0 (* 6.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 6.5 wline)))
	(command "pline"
		 p4
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p3
		 ""
	)
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_P_top ()			;绘制平面形状符号
    (if	(= n_symbol_top "1")
      (progn
	(setq p2 (polar p1 (dtor 90) (* 5 wline)))
	(setq p3 (polar p2 (dtor 45) (* 4 wline)))
	(setq p4 (polar p3 (dtor 135) (* 7.5 wline)))
	(setq p5 (polar p3 (dtor 315) (* 7.5 wline)))
	(command "pline" p5 "w" wline wline p4 "")
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_top "")
	  (setq p2 (polar p1 (dtor 90) (* 20 wline)))
	  (setq p2 (polar p1 (dtor 90) (* 12 wline)))
	)
	(setq p3 (polar p2 0 (* 7.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 7.5 wline)))
	(command "pline" p3 "w" wline wline p4 "")
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_P_bottom	()		;绘制平面形状符号
    (if	(= n_symbol_bottom "1")
      (progn
	(setq p2 (polar p11 (dtor 270) (* 5 wline)))
	(setq p3 (polar p2 (dtor 315) (* 4 wline)))
	(setq p4 (polar p3 (dtor 45) (* 7.5 wline)))
	(setq p5 (polar p3 (dtor 225) (* 7.5 wline)))
	(command "pline" p5 "w" wline wline p4 "")
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_bottom "")
	  (setq p2 (polar p11 (dtor 270) (* 20 wline)))
	  (setq p2 (polar p11 (dtor 270) (* 12 wline)))
	)
	(setq p3 (polar p2 0 (* 7.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 7.5 wline)))
	(command "pline" p3 "w" wline wline p4 "")
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_A_top ()			;绘制凹面形状符号
    (if	(= n_symbol_top "1")
      (progn
	(setq p2 (point p1 (- 0 wline) (* 4 wline)))
	(setq p3 (polar p2 (dtor 90) (* 8 wline)))
	(setq p4 (polar p2 0 (* 8 wline)))
	(command "pline"
		 p3
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p4
		 ""
	)
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_top "")
	  (setq p2 (polar p1 (dtor 90) (* 24 wline)))
	  (setq p2 (polar p1 (dtor 90) (* 14 wline)))
	)
	(setq p3 (polar p2 0 (* 6.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 6.5 wline)))
	(command "pline"
		 p4
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p3
		 ""
	)
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_A_bottom	()		;绘制凹面形状符号
    (if	(= n_symbol_bottom "1")
      (progn
	(setq p2 (point p11 (- 0 wline) (- 0 (* 4 wline))))
	(setq p3 (polar p2 (dtor 270) (* 8 wline)))
	(setq p4 (polar p2 0 (* 8 wline)))
	(command "pline"
		 p4
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p3
		 ""
	)
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_bottom "")
	  (setq p2 (polar p11 (dtor 270) (* 24 wline)))
	  (setq p2 (polar p11 (dtor 270) (* 14 wline)))
	)
	(setq p3 (polar p2 0 (* 6.5 wline)))
	(setq p4 (polar p2 (dtor 180) (* 6.5 wline)))
	(command "pline"
		 p3
		 "w"
		 wline
		 wline
		 "A"
		 "R"
		 (* 7.5 wline)
		 p4
		 ""
	)
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_Y_top ()			;绘制圆滑过渡形状符号
    (if	(= n_symbol_top "1")
      (progn
	(setq p2 (polar p1 (dtor 90) (* 5 wline)))
	(setq p3 (polar p2 (dtor 45) (* 5 wline)))
	(setq p4 (polar p3 (dtor 45) (* 5 wline)))
	(setq p5 (polar p3 (dtor 135) (* 6.5 wline)))
	(setq p6 (polar p3 (dtor 315) (* 6.5 wline)))

	(command "pline" p4 "w" wline wline P3 "A" "A" 180 p6 "")
	(ssadd (entlast) sstext)
	(command "pline" p5 "w" wline wline "A" "A" 180 p3 "")
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_top "")
	  (setq p2 (polar p1 (dtor 90) (* 20 wline)))
	  (setq p2 (polar p1 (dtor 90) (* 12 wline)))
	)
	(setq p3 (polar p2 (dtor 90) (* 5 wline)))
	(setq p4 (polar p3 (dtor 90) (* 5 wline)))
	(setq p5 (polar p3 (dtor 180) (* 6.5 wline)))
	(setq p6 (polar p3 0 (* 6.5 wline)))

	(command "pline" p4 "w" wline wline P3 "A" "A" 180 p6 "")
	(ssadd (entlast) sstext)
	(command "pline" p5 "w" wline wline "A" "A" 180 p3 "")
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun shape_Y_bottom	()		;绘制圆滑过渡形状符号
    (if	(= n_symbol_bottom "1")
      (progn
	(setq p2 (polar p11 (dtor 270) (* 5 wline)))
	(setq p3 (polar p2 (dtor 315) (* 5 wline)))
	(setq p4 (polar p3 (dtor 315) (* 5 wline)))
	(setq p5 (polar p3 (dtor 45) (* 6.5 wline)))
	(setq p6 (polar p3 (dtor 225) (* 6.5 wline)))

	(command "pline" p4 "w" wline wline P3 "A" "A" 180 p6 "")
	(ssadd (entlast) sstext)
	(command "pline" p5 "w" wline wline "A" "A" 180 p3 "")
	(ssadd (entlast) sstext)
      )
      (progn
	(if (/= angle_bottom "")
	  (setq p2 (polar p11 (dtor 270) (* 20 wline)))
	  (setq p2 (polar p11 (dtor 270) (* 12 wline)))
	)
	(setq p3 (polar p2 (dtor 270) (* 5 wline)))
	(setq p4 (polar p3 (dtor 270) (* 5 wline)))
	(setq p5 (polar p3 0 (* 6.5 wline)))
	(setq p6 (polar p3 (dtor 180) (* 6.5 wline)))

	(command "pline" p4 "w" wline wline P3 "A" "A" 180 p6 "")
	(ssadd (entlast) sstext)
	(command "pline" p5 "w" wline wline "A" "A" 180 p3 "")
	(ssadd (entlast) sstext)
      )
    )
  )

;****************************************************************************************************************
  (defun wshape	()			;本程序用于安排绘制哪个焊缝符号
					;(setq listshape '("" "凸面 " "平面 " "凹面 " "圆滑过渡"))
    (cond
      ((= n_shape_top "1") (shape_T_top)) ;凸面
      ((= n_shape_top "2") (shape_P_top)) ;平面
      ((= n_shape_top "3") (shape_A_top)) ;凹面
      ((= n_shape_top "4") (shape_Y_top)) ;圆滑过渡
    )
    (cond
      ((= n_shape_bottom "1") (shape_T_bottom)) ;凸面
      ((= n_shape_bottom "2") (shape_P_bottom)) ;平面
      ((= n_shape_bottom "3") (shape_A_bottom)) ;凹面
      ((= n_shape_bottom "4") (shape_Y_bottom)) ;圆滑过渡
    )
  )

;****************************************************************************************************************
  (defun walter	()			;绘制交错焊缝符号
    (if	(= n_alter "1")
      (progn
	(setq p1 (point RDpt1 l1 0)
	      p2 (point p1 0 (* 12.5 wline))
	      p3 (point p2 (* 10 wline) 0)
	      p4 (point p1 0 (* -12.5 wline))
	      p5 (point p4 (* 10 wline) 0)
	) ;_ 结束setq
	(command "pline" p2 "w" wline wline p3 p4 p5 "")
	(ssadd (entlast) sstext)
	(setq l1 (+ l1 (* 11 wline)))
	(setq l2 (+ l2 (* 11 wline)))
      )
    )
  ) ;_ 结束defun	  

;****************************************************************************************************************
  (defun weldinsert ()			;绘制焊缝标注图形

    (setvar "clayer" "dim")
    (setq wline 0.5)
    (setq htext (* 10 wline))

    (RD&leader)
    (wcircle)
    (wflag)
    (setq sstext (ssadd))
    (wtri)
    (wtextfront)
    (wsymbol)
    (wangle)
    (wshape)
    (wtextback)
    (walter)				;绘制交错焊缝符号
    (wtextalter)			;绘制交错焊缝文字
    (weldline)

    (if	(and (> (angle RDpt3 RDpt1) (dtor 90))
	     (< (angle RDpt3 RDpt1) (dtor 270))
	)
      (progn
	(command "move" sstext "" RDpt1 (polar RDpt1 (dtor 180) l1))
	(if (/= n_mark "0")
	  (progn
	    (setq mark (nth (atoi n_mark) listmark))
	    (setq p1 (polar RDpt1 (dtor 180) l1)
		  p2 (point p1 (* -10 wline) (* 10 wline))
		  p3 (point p1 (* -10 wline) (* -10 wline))
		  p4 (polar p1 (dtor 180) (* 10 wline))
	    )
	    (command "pline" p2 "w" wline wline p1 p3 "")
	    (command "_.TEXT" "MR" p4 htext 0 mark)
	  )
	)
      )
      (progn
	(if (/= n_mark "0")
	  (progn
	    (setq mark (nth (atoi n_mark) listmark))
	    (setq p1 (polar RDpt1 0 l1)
		  p2 (point p1 (* 10 wline) (* 10 wline))
		  p3 (point p1 (* 10 wline) (* -10 wline))
		  p4 (polar p1 0 (* 10 wline))
	    )
	    (command "pline" p2 "w" wline wline p1 p3 "")
	    (command "_.TEXT" "ML" p4 htext 0 mark)
	  )
	)
      )
    )

    (prompt "\n 焊缝符号设计成功！")
  )

;****************************************************************************************************************
  (defun setalter ()
    (if	(= n_alter "0")
      (progn
	(mode_tile "edit_distance_top" 1)
	(mode_tile "edit_distance_bottom" 1)
      )
      (progn
	(mode_tile "edit_distance_top" 0)
	(mode_tile "edit_distance_bottom" 0)
      )
    )
  )

;以下为主程序
  (setvar "cmdecho" 0)			;控制AutoLisp的command函数运行时Autocad是否显示提示和输入
  
;------------------ActiveX准备工作-------------------------------------------
  (vl-load-com)				;加载Visual Lisp扩展功能函数，同时可以控制ActiveX对象
  (setq acadobj (vlax-get-acad-object))	;取得当前AutoCAD应用程序母体对象
  (setq dwgobj (vla-get-ActiveDocument acadobj)) ;取得作用中的图形文件
  (setq mspace (vla-get-ModelSpace dwgobj)) ;取得模型空间集合对象
  (setq layersobj (vla-get-layers dwgobj)) ;取得图层集合对象
  (setq stylesobj (vla-get-textstyles dwgobj)) ;取得字体集合对象
  (setq linetypesobj (vla-get-linetypes dwgobj)) ;取得字体集合对象
  (setq blocksobj (vla-get-blocks dwgobj)) ;取得图形中的块集合对象
  (setq layoutsobj (vla-get-layouts dwgobj))
					;取得图形中的所有布局设置集合对象

  (vl-catch-all-apply
    'vla-load
    (list linetypesobj "DASHED" (findfile "acadiso.lin"))
  )					;加载虚线

  (setq	baselayers
	 (list '("dim" 3 "continuous" 18)
	       '("dash" 2 "dashed" 18)
	 )
  )					;定义图层设置
  (mapcar '(lambda (lays)
	     (setq lay (vla-add layersobj (nth 0 lays)))
	     (vla-put-color lay (nth 1 lays))
	     (vla-put-linetype lay (nth 2 lays))
	     (vla-put-lineweight lay (nth 3 lays))
	   ) ;_ 结束lambda
	  baselayers
  )					;设置图层

  (setvar "osmode" 16383)		;打开所有对象捕捉; 
  (sys_var)

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  (setq	dclname
	 (cond (
		(setq tempname (vl-filename-mktemp "yx.dcl"))
		(setq filen (open tempname "w"))
		(foreach stream	'("\n"
				  "yx1:dialog {\n"
				  "label = \"焊缝标注\" ;\n"
				  ":boxed_row {\n"
				  "label = \"可选项\" ;\n"
				  ":toggle {\n"
				  "key = \"tog_circle\" ;\n"
				  "label = \"周围焊缝\" ;\n"
				  "}\n"
				  ":toggle {\n"
				  "key = \"tog_local\" ;\n"
				  "label = \"现场焊缝\" ;\n"
				  "}\n"
				  ":toggle {\n"
				  "key = \"tog_alter\" ;\n"
				  "label = \"交错断续\" ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "label = \"注解\" ;\n"
				  "key=\"pop_mark\";\n"
				  "}\n"
				  "}\n"
				  ":row {\n"
				  ":column {\n"
				  ":boxed_column {\n"
				  "label = \"箭头侧\" ;\n"
				  ":row {\n"
				  ":toggle {\n"
				  "key = \"tog_tri_top\" ;\n"
				  "label = \"三面焊缝\" ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "alignment = centered ;\n"
				  "fixed_height = true ;\n"
				  "fixed_width = true ;\n"
				  "height = 8 ;\n"
				  "key = \"pop_shape_top\" ;\n"
				  "label = \"焊缝形状\" ;\n"
				  "width = 20 ;\n"
				  "}\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 24.1;\n"
				  "}\n"
				  "}\n"
				  ":row {\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 9 ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "alignment = centered ;\n"
				  "fixed_height = true ;\n"
				  "fixed_width = true ;\n"
				  "height = 8 ;\n"
				  "key = \"pop_angle_top\" ;\n"
				  "label = \"焊缝角度\" ;\n"
				  "width = 20 ;\n"
				  "}\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 20.1;\n"
				  "}\n"
				  "}\n"
				  ":row {\n"
				  ":edit_box {\n"
				  "key = \"edit_front_top\" ;\n"
				  "label = \"符号前\" ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "key = \"pop_symbol_top\" ;\n"
				  "label = \"焊缝符号\" ;\n"
				  "}\n"
				  ":edit_box {\n"
				  "key = \"edit_back_top\" ;\n"
				  "label = \"符号后\" ;\n"
				  "}\n"
				  ":edit_box {\n"
				  "key = \"edit_distance_top\" ;\n"
				  "is_enabled = false ;\n"
				  "}\n"
				  "}\n"
				  "}\n"
				  ":boxed_column {\n"
				  "label = \"非箭头侧\" ;\n"
				  ":row {\n"
				  ":edit_box {\n"
				  "key = \"edit_front_bottom\" ;\n"
				  "label = \"符号前\" ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "key = \"pop_symbol_bottom\" ;\n"
				  "label = \"焊缝符号\" ;\n"
				  "}\n"
				  ":edit_box {\n"
				  "key = \"edit_back_bottom\" ;\n"
				  "label = \"符号后\" ;\n"
				  "}\n"
				  ":edit_box {\n"
				  "key = \"edit_distance_bottom\" ;\n"
				  "is_enabled = false ;\n"
				  "}\n"
				  "}\n"
				  ":row {\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 9 ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "alignment = centered ;\n"
				  "fixed_height = true ;\n"
				  "fixed_width = true ;\n"
				  "height = 8 ;\n"
				  "key = \"pop_angle_bottom\" ;\n"
				  "label = \"焊缝角度\" ;\n"
				  "width = 20 ;\n"
				  "}\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 20.1 ;\n"
				  "}\n"
				  "}\n"
				  ":row {\n"
				  ":toggle {\n"
				  "key = \"tog_tri_bottom\" ;\n"
				  "label = \"三面焊缝\" ;\n"
				  "}\n"
				  ":popup_list {\n"
				  "alignment = centered ;\n"
				  "fixed_height = true ;\n"
				  "fixed_width = true ;\n"
				  "height = 8 ;\n"
				  "key = \"pop_shape_bottom\" ;\n"
				  "label = \"焊缝形状\" ;\n"
				  "width = 20 ;\n"
				  "}\n"
				  ":text{\n"
				  "fixed_width = true ;\n"
				  "width = 24.1 ;\n"
				  "}\n"
				  "}\n"
				  "}\n"
				  "}\n"
				  "}\n"
				  "ok_cancel;\n"
				  "}\n"	;--------------------------------------------------
				 )
		  (princ stream filen)
		)
		(close filen)
		tempname
	       )
	 )
  )
  (setq dcl_re (load_dialog dclname))
  (if (not (new_dialog "yx1" dcl_re))
    (exit)
  )
;*********************************************************************************
					;此处填写dcl动作内容
  (setq listshape '("" "凸面 " "平面 " "凹面 " "圆滑过渡"))
  (setq
    listangle '("" "35°" "37.5°" "45°" "60°" "70°" "75°" "90°")
  )
  (setq	listsymbol
	 '(""		"角焊缝"     "V形焊缝"	  "Y形焊缝"
	   "U形焊缝"	"单边V"	     "单边Y"	  "单边U"
	   "I形焊缝"	"封底焊缝"
	  )
  )
  (setq	listmark '(""	     "TYPE. "  "2处 "	 "3处 "	   "4处 "
		   "5处 "    "6处 "    "7处 "	 "8处 "	   "9处 "
		   "10处"
		  )
  )

  (setq	n_circle "0"
	n_local	"0"
	n_tri_top "0"
	n_tri_bottom "0"
	n_alter	"0"
	n_shape_top "0"
	n_angle_top "0"
	n_symbol_top
	 "0"
	n_shape_bottom "0"
	n_angle_bottom "0"
	n_symbol_bottom
	 "0"
	front_top ""
	back_top ""
	distance_top ""
	front_bottom ""
	back_bottom ""
	distance_bottom
	 ""
	n_mark "0"
  )

  (show_list "pop_shape_top" listshape)
  (show_list "pop_angle_top" listangle)
  (show_list "pop_symbol_top" listsymbol)
  (show_list "pop_shape_bottom" listshape)
  (show_list "pop_angle_bottom" listangle)
  (show_list "pop_symbol_bottom" listsymbol)
  (show_list "pop_mark" listmark)

  (action_tile "pop_shape_top" "(setq n_shape_top $value)")
  (action_tile "pop_angle_top" "(setq n_angle_top $value)")
  (action_tile "pop_symbol_top" "(setq n_symbol_top $value)")
  (action_tile
    "pop_shape_bottom"
    "(setq n_shape_bottom $value)"
  )
  (action_tile
    "pop_angle_bottom"
    "(setq n_angle_bottom $value)"
  )
  (action_tile
    "pop_symbol_bottom"
    "(setq n_symbol_bottom $value)"
  )
  (action_tile "pop_mark" "(setq n_mark $value)")

  (action_tile "edit_front_top" "(setq front_top $value)")
  (action_tile "edit_back_top" "(setq back_top $value)")
  (action_tile
    "edit_distance_top"
    "(setq distance_top $value)"
  )
  (action_tile
    "edit_front_bottom"
    "(setq front_bottom $value)"
  )
  (action_tile "edit_back_bottom" "(setq back_bottom $value)")
  (action_tile
    "edit_distance_bottom"
    "(setq distance_bottom $value)"
  )

  (action_tile "tog_circle" "(setq n_circle $value)")
  (action_tile "tog_local" "(setq n_local $value)")
  (action_tile "tog_tri_top" "(setq n_tri_top $value)")
  (action_tile "tog_tri_bottom" "(setq n_tri_bottom $value)")
  (action_tile "tog_alter" "(setq n_alter $value)(setalter)")

;********************************************************************************
  (setq dd (start_dialog))
  (if (= dd 1)
    (progn
      (weldinsert)
    )
  )
;*********************************************************************************
  (unload_dialog dcl_re)
  (vl-file-delete dclname)
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  (setvar "osmode" 16383)		;打开所有对象捕捉;
  (*error* nil)
  (prin1)
)
;;;===================================================================================;;