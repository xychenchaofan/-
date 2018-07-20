;;;===================================================================================;;
;;;名称：动态引线标注    命令dee                                                      ;;
;;;功能：带对话框，可设置参数                                      ;;
;;;引用：reent / relst / getty / showlst                                             ;;
;;;说明：正交时自动对齐，基线随字长    ;;
;;;-----------------------------------------------------------------------------------;;
(defun c:dee (/ bb bi code data dcl_re dclname ent ent1 ent2 filen gr i lst n1 n2 n3 name1 name2 nent pt pt0
	       ptlst stream tempname tulst tulst1 tulst2 txlst txlst1 ty w x x0 x1 xunh y0 y1
	    )
  ; 按点表顺序更新多段线顶点,无须更换顶点用nil代替。by:langjs
  (defun reent (ent ptlst / i nent x)
    (setq i -1
	  nent '()
    )
    (foreach x ent
      (setq nent (append
		   nent
		   (list (if (and
			       (= (car x) 10)
			       (/= (nth (setq i (1+ i))
					ptlst
				   ) nil
			       )
			     )
			   (cons 10 (nth i ptlst))
			   x
			 )
		   )
		 )
      )
    )
  )
  ; 替换表中第i个元素。 x -- 替换元素  i -- 列表需要替换的元素所在位置 lst -- 需要替换的列表
  (defun relst (x i lst)	       
    (if (= 0 i)
      (cons x (cdr lst))
      (cons (car lst) (relst x (1- i) (cdr lst)))
    )
  )
  (defun getty (ty / lst x)	       ; 图层列表
    (setq x (tblnext ty t))
    (while (/= x nil)
      (setq lst (cons (cdr (assoc 2 x)) lst))
      (setq x (tblnext ty))
    )
    (acad_strlsort lst)
  )
  (defun showlst (i lst)	       ; 显示列表
    (start_list i)
    (mapcar
      'add_list
      lst
    )
    (end_list)
  )
  (defun show ()        
    (setq n1 "5文本"
	  n2 "6标注线"
	  n3 (getvar "TEXTSTYLE")
	  tulst1 (cons n1 (vl-remove n1 tulst))
	  tulst2 (cons n2 (vl-remove n2 tulst))
	  txlst1 (cons n3 (vl-remove n3 txlst))
	  lstsz (relst n1 6 lstsz)
	  lstsz (relst n2 7 lstsz)
	  lstsz (relst n3 3 lstsz)
	  lstsz (relst '("开" "关") 4 lstsz)
    )
    (setvar "orthomode" 1)
    (showlst "e07" '("开" "关"))
    (showlst "e10" tulst1)
    (showlst "e11" tulst2)
    (showlst "e12" txlst1)
    (showlst "e13" (nth 4 lstsz))
  )
  (setvar "cmdecho" 0)
  (command ".UNDO" "BE")
  (while (/= bb 4)
    (setq bi (getvar "DIMSCALE")
	  bb 3
	  xunh t
    )
    (if (null txt)
      (setq txt "")
    )
    (if (null ptlast)
      (setq ptlast '(0.0 0.0))
    )
    (if (null lstsz)
      (setq lstsz (list 2.5
			(getvar "DIMASZ")
			(getvar "DIMEXO")
			(getvar "TEXTSTYLE")
			'("开" "关")
			0
			"5文本"
			"6标注线"
		  )
      )
    )
    (setq tulst (getty "LAYER")
	  txlst (getty "style")
	  n1 "5文本"
	  n2 "6标注线"
	  n3 (nth 3 lstsz)
	  tulst1 (if (= (member n1 tulst) nil) (progn (mak:layer "5文本" 4 "Continuous" 18)(cons n1 tulst)) (cons (getvar "clayer") (vl-remove (getvar "clayer") tulst)) )
	  tulst2 (if (= (member n2 tulst) nil) (progn (mak:layer "6标注线" 3 "Continuous" 18)(cons n2 tulst)) (cons (getvar "clayer") (vl-remove (getvar "clayer") tulst)) )
	  txlst1 (cons n3 (vl-remove n3 txlst))
    )
    (while (= bb 3)
      (setq dclname (cond
		      ((setq tempname (vl-filename-mktemp "yx.dcl")
			     filen (open tempname "w")
		       )
			(foreach stream '("\n" "yx1:dialog {\n"
			   "    label = \"引线标柱 2.0\" ;\n"
			   "    :row { :edit_box { key = \"e01\" ; width = 30 ;   height = 1.2 ;  }  }\n"
			   "    :row { :button { key = \"e02\" ; label = \"确认\" ;  is_default = true ;   }\n"
			   "           :button { key = \"e04\" ; label = \"设置\" ; }  \n"
			   "           :button { key = \"e03\" ; label = \"取消\" ; is_cancel = true ; } } }\n"
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
      (set_tile "e01" txt)
      (action_tile "e02" "(setq txt (get_tile \"e01\"))(done_dialog 1)")
      (action_tile "e04" "(setq txt (get_tile \"e01\"))(done_dialog 2)")
      (action_tile "e03" "(setq txt (get_tile \"e01\"))(done_dialog 4)")
      (setq bb (start_dialog))
      (unload_dialog dcl_re)
      (vl-file-delete dclname)
      (if (= bb 2)
	(progn
	  (setq dclname (cond
			  ((setq tempname (vl-filename-mktemp "yx.dcl")
				 filen (open tempname "w")
			   )
			    (foreach stream '("\n" "yx1:dialog {\n"
			       "    label = \"引线标柱设置\" ;\n"
				   "    :edit_box { label = \"文字高度\" ; key = \"e00\" ; }\n"
;;;			       "    :edit_box { label = \"宽度比例\" ; key = \"e08\" ; }\n"
			       "    :edit_box { label = \"文字偏移\" ; key = \"e04\" ; }\n"
;;;			       "    :edit_box { label = \"文字颜色\" ; key = \"e01\" ; }\n"
			       "    :edit_box { label = \"基线宽度\" ; key = \"e14\" ; }\n"
			       "    :edit_box { label = \"箭头大小\" ; key = \"e03\" ; }\n"
;;;			       "    :edit_box { label = \"引线颜色\" ; key = \"e02\" ; }\n"
			       "    :popup_list { label = \"数字递增\" ; key = \"e13\" ; }\n"
			       "    :popup_list { label = \"正交对齐\" ; key = \"e07\" ; }\n"
			       "    :popup_list { label = \"文字样式\" ; key = \"e12\" ; }\n"
			       "    :popup_list { label = \"文字图层\" ; key = \"e10\" ; }\n"
			       "    :popup_list{ label = \"引线图层\" ; key = \"e11\" ; }\n"
			       "    :row { :button { key = \"e05\" ; label = \"确认\" ;  is_default = true ;   }\n"
			       "           :button { key = \"e09\" ; label = \"默认\" ; }  \n"
			       "           :button { key = \"e06\" ; label = \"取消\" ; is_cancel = true ; } } }\n"
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
	  (set_tile "e00" (rtos (nth 0 lstsz) 2 2))
;;;	  (set_tile "e01" (itoa (nth 1 lstsz)))
;;;	  (set_tile "e02" (itoa (nth 2 lstsz)))
	  (set_tile "e03" (rtos (nth 1 lstsz) 2 2))
	  (set_tile "e04" (rtos (nth 2 lstsz) 2 2))
;;;	  (set_tile "e08" (rtos (nth 5 lstsz) 2 2))
	  (set_tile "e14" (rtos (nth 5 lstsz) 2 2))
	  (showlst "e10" tulst1)
	  (showlst "e11" tulst2)
	  (showlst "e12" txlst1)
	  (showlst "e13" (nth 4 lstsz))
	  (if (= (getvar "ORTHOMODE") 0)
	    (showlst "e07" '("关" "开"))
	    (showlst "e07" '("开" "关"))
	  )
	  (action_tile "e05" "(setq  txlst1 (cons n3 (vl-remove n3 txlst))  tulst1 (cons n1 (vl-remove n1 tulst))  tulst2 (cons n2 (vl-remove n2 tulst))  lstsz (relst n1 6 lstsz)  lstsz (relst n2 7 lstsz)  lstsz (relst n3 3 lstsz)   lstsz (relst (atof(get_tile \"e14\")) 5 lstsz)  lstsz (relst(atof(get_tile \"e00\")) 0 lstsz)  lstsz (relst(atof(get_tile \"e03\")) 1 lstsz) lstsz (relst(atof(get_tile \"e04\")) 2 lstsz) )(done_dialog 3)")
	  (action_tile "e09" "(show) (set_tile \"e14\" (rtos 0 2 2)) (set_tile \"e00\" (rtos 2.5 2 2)) (set_tile \"e03\" (rtos 1.5 2 2)) (set_tile \"e04\" (rtos 1 2 2))" )
	  (action_tile "e06" "(done_dialog 3)")
	  (action_tile "e10" "(setq n1 (nth (atoi  $value ) tulst1)) ")
	  (action_tile "e11" "(setq n2 (nth (atoi  $value ) tulst2)) ")
	  (action_tile "e12" "(setq n3 (nth (atoi  $value ) txlst1)) ")
	  (action_tile "e07" "(setvar \"orthomode\" (rem (+ (getvar \"ORTHOMODE\") (atoi  $value ) ) 2 ) ) (if (= (getvar \"ORTHOMODE\") 0) (showlst \"e07\" '(\"关\" \"开\")) (showlst \"e07\" '(\"开\" \"关\")))")
	  (action_tile "e13" "(if (= (atoi $value) 1) (setq lstsz (relst (reverse (nth 4 lstsz)) 4 lstsz))) (showlst \"e13\" (nth 4 lstsz)) ")
	  (setq bb (start_dialog))
	  (unload_dialog dcl_re)
	  (vl-file-delete dclname)
	)
      )
    )
    (if (= bb 1)   
      (if (setq pt0 (getpoint "\n命令:_yb 指定第一点:"))
	(progn
	  (princ (strcat "\n指定下一点:"))
	  (entmake (list '(0 . "LWPOLYLINE")
			 '(100 . "AcDbEntity")
			 (cons 8 (nth 7 lstsz))
;;;			 (cons 62 (nth 2 lstsz))
			 '(100 . "AcDbPolyline")
			 (cons 90 4)
			 (cons 10 pt0)
			 '(40 . 0.0)
			 (cons 41 (* 0.3 bi (nth 1 lstsz)))
			 (cons 10 pt0)
			 (cons 10 pt0)
			 (cons 40 (* bi (nth 5 lstsz)))
			 (cons 41 (* bi (nth 5 lstsz)))
			 (cons 10 pt0)
		   )
	  )
	  (setq ent1 (entget (setq name1 (entlast))))
	  (entmake (list '(0 . "TEXT")
			 (cons 8 (nth 6 lstsz))
;;;			 (cons 62 (nth 1 lstsz))
			 (cons 1 txt)
			 (cons 10 pt0)
			 (cons 40 (* bi (nth 0 lstsz)))
  			 (cons 41 0.7)
			 (cons 7 (nth 3 lstsz))
		   )
	  )
	  (setq ent2 (entget (setq name2 (entlast)))
		w (caadr (textbox ent2))
	  )
	  (while (progn
		   (setq gr (grread t 15 0)
			 code (car gr)
			 data (cadr gr)
		   )
		   (cond
		     ((= code 2)       ; 键盘区域
		       (redraw)
		       (if (= data 15)
			 (if (= (getvar "ORTHOMODE") 0)
			   (progn
			     (prompt "\n命令: <正交 开>")
			     (setvar "orthomode" 1)
			   )
			   (progn
			     (prompt "\n命令: <正交 关>")
			     (setvar "orthomode" 0)
			   )
			 )
		       )
		     )
		     ((= code 3)       ; 鼠标左击
		       (setq ptlast pt
			     xunh nil
		       )
		     )
		     ((= code 5)       ; 鼠标移动
		       (if (= (getvar "ORTHOMODE") 1)
			 (progn
			   (setq x0 (car ptlast)
				 y0 (cadr ptlast)
				 x1 (car data)
				 y1 (cadr data)
			   )
			   (if (< (abs (- x0 x1)) (* 1.5 bi (nth 0 lstsz)))
			     (setq pt (list x0 y1))
			     (if (< (abs (- y0 y1)) (* 2 bi (nth 0 lstsz)))
			       (setq pt (list x1 y0))
			       (setq pt data)
			     )
			   )
			 )
			 (setq pt data)
		       )
		       (entmod (reent ent1 (list nil (polar pt0 (angle pt0 pt) (* bi (nth 1 lstsz))) pt (polar pt (if
														    (>
														       (car pt)
														       (car pt0)
														    )
														    0
														    pi
														  ) w
													)
					   )
			       )
		       )
		       (entmod (subst
				 (cons 10 (list (- (car pt) (if (> (car pt) (car pt0))
							      0
							      w
							    )
						) (+ (cadr pt) (* bi (nth 2 lstsz)))
					  )
				 )
				 (assoc 10 ent2)
				 ent2
			       )
		       )
		       (redraw)
		     )
		     ((or
			(= code 11)
			(= code 25)
		      )		       ; 鼠标右击
		       (if (and
			     (wcmatch txt "~*[~.0-9]*")
			     (= (car (nth 4 lstsz)) "开")
			   )
			 (setq txt (itoa (1- (atoi txt))))
		       )
		       (entdel name1)
		       (entdel name2)
		       (setq xunh nil)
		       (redraw)
		     )
		     (t
		     )
		   )
		   xunh
		 )
	  )
	  (if (and
		(wcmatch txt "*[0-9]*")
		(= (car (nth 4 lstsz)) "开")
	      )
	    (progn
(setq txt (vl-string-translate "-" (chr 1) txt))
(setq txt_n (last (str_to_num txt)))
(setq txt (strcat (vl-string-right-trim  (rtos txt_n 2 0) txt) (rtos (1+ txt_n) 2 0)))
(setq txt (vl-string-translate (chr 1) "-" txt))
)
	  )
	)
      )
    )
  )
  (command ".UNDO" "E")
  (princ)
)
;;;===================================================================================;;