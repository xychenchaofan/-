;;;-----------------------------------------------------------------------------------;;
;;;名称:  尺寸检查  命令:dcc                                                          ;;
;;;功能:  假尺寸(手动修改过的尺寸)数值显示红色                                        ;;
;;;       运行dcc显示假尺寸,再次运行恢复不显示                                        ;;
;;;       连续不断运行闪烁。支持修改假尺寸数值                                        ;;
(defun c:dcc (/	     #errjcc	   $orr	  ak	 bb	bl     col
	      ctr    dcl_pt dcl_re dclname	 ent	er     filen
	      i	     jj	    k	   lb	  len1	 len2	len3   n
	      name   name1  np1	   np2	  obj	 pt	px     py
	      scale  screen ss	   ss0	  ss1	 ss3	stream tempname
	      u	     wzgd   wzh	   xsize  xx	 ysize	yy
	     )
  (defun getcursorpos (pt / ctr ysize screen scale xsize lb xx yy); 点坐标近似转像素坐标供对话框定位
    (setq ctr	 (getvar "VIEWCTR")
	  ysize	 (getvar "VIEWSIZE")
	  screen (getvar "SCREENSIZE")
	  scale	 (/ (car screen) (cadr screen))
	  xsize	 (* scale ysize)
	  lb	 (list (- (car ctr) (/ xsize 2)) (- (cadr ctr) (/ ysize 2)))
	  xx	 (- (car pt) (car lb))
	  yy	 (- (cadr pt) (cadr lb))
    )
    (list (fix (+ 50 (* (/ xx xsize) (car screen))))
	  (fix
	    (+ 120 (- (cadr screen) (* (/ yy ysize) (cadr screen))))
	  )
    )
  )
  (setvar "cmdecho" 0)
  (command ".UNDO" "BE")
  (setq u t)
  (while u
    (if
      (progn
	(setq ss  (ssadd)
	      ss1 (ssadd)
	)
	(if (setq ss0 (ssget "X" '((0 . "DIMENSION"))))
	  (progn
	    (repeat (setq i (sslength ss0))
	      (setq ent (ssname ss0 (setq i (1- i))))
	      (if
		(= (vla-get-textcolor (vlax-ename->vla-object ent)) 1)
					; 查找红色的假尺寸
		 (setq ss (ssadd ent ss))
		 (setq ss1 (ssadd ent ss1))
	      )
	    )
	    (if	(> (sslength ss) 0)
	      (if (> (sslength ss1) 0)
		(setq col (vla-get-textcolor
			    (vlax-ename->vla-object (ssname ss1 0))
			  )
		)
		(progn
		  (setq	col
			 (getint
			   "\n  输入尺寸恢复颜色[1红,2黄,3绿,4青,5蓝,6洋红,7白]:<3>"
			 )
		  )
		  (if (null col)
		    (setq col 3)
		  )
		)
	      )
	    )
	  )
	)
	(> (sslength ss) 0)
      )
       (progn
	 (setq u nil)
	 (repeat (setq i (sslength ss))
	   (vlax-put-property
	     (vlax-ename->vla-object (ssname ss (setq i (1- i))))
	     "textcolor"
	     col
	   )				; 颜色恢复
	 )
       )
       (if (setq ss (ssget "X"
			   '((0 . "DIMENSION")
			     (-4 . "<AND")
			     (-4 . "<NOT")
			     (1 . "")
			     (-4 . "NOT>")
			     (-4 . "<NOT")
			     (1 . "*<>*")
			     (-4 . "NOT>")
			     (-4 . "AND>")
			    )
		    )
	   )
	 (progn
	   (princ (strcat "\n  找到"
			  (itoa (sslength ss))
			  "个假尺寸并显示红色。"
		  )
	   )
	   (repeat (setq i (sslength ss))
	     (vlax-put-property
	       (vlax-ename->vla-object (ssname ss (setq i (1- i))))
	       "textcolor"
	       1
	     )				; 显示红色
	   )
	   (setq obj (vlax-ename->vla-object (ssname ss 0)))
	   (setq wzgd (vla-get-textheight obj))
					; 得到标注样式的文字高度
	   (setq bl (vla-get-scalefactor obj)) ; 得到标注的调整比例
	   (setq wzh (* wzgd bl))	; 得到真正的文字高度
	   (setq er 888
		 jj 0
	   )
	   (while (or
		    (= er 7)
		    (= er 888)
		  )
	     (while
	       (and
		 (setq name (entsel "点空白缩放,点尺寸查看:<右键退出>"))
		 (setq dcl_pt (getcursorpos (cadr name)))
		 (setq name (car name))
		 (= (cdr (assoc 0 (entget name))) "DIMENSION")
	       )
		(progn
		  (command "delay" "50")
		  (setq	ent  (vlax-ename->vla-object name)
			len3 (vla-get-textoverride ent)
			len2 (vla-get-measurement ent)
		  )
		  (setq	n 0
			len1 ""
		  )
		  (repeat (strlen len3)
		    (setq k  (substr len3
				     (setq n (+ 1 n))
				     1
			     )
			  ak (ascii k)
		    )
		    (if	(/= ak 123 ak 125)
		      (setq len1 (strcat len1 k))
		    )
		  )
		  (setq	dclname
			 (cond
			   ((setq tempname (vl-filename-mktemp "jcc.dcl")
				  filen	   (open tempname "w")
			    )
			    (foreach stream
				     '("\n"
				       "jcc:dialog {    label = \"检查尺寸\" ;\n"
				       "    :edit_box { label = \"假值\" ; key = \"e01\" ;　width = 10 ;  }\n"
				       "    :edit_box { label = \"真值\" ; key = \"e02\" ;　width = 10 ;  }\n"
				       "    :row {\n"
				       "      :button { label = \"修改\" ; key = \"e05\" ; }\n"
				       "      :button { label = \"恢复\" ; key = \"e03\" ; is_default = true ;  }\n"
				       "      :button { label = \"取消\" ; key = \"e04\" ;  is_cancel = true ;  }\n"
				       "    }\n"
				       "}\n"
				      )
			      (princ stream filen)
			    )
			    (close filen)
			    tempname
			   )
			 )
		  )
		  (setq dcl_re (load_dialog dclname))
		  (if (not (new_dialog "jcc" dcl_re "" dcl_pt))
		    (exit)
		  )
		  (set_tile "e01" len1)
		  (set_tile "e02" (rtos len2))
		  (action_tile "e03" "(setq dcl_pt (done_dialog 1)) ")
		  (action_tile
		    "e05"
		    "(setq k (get_tile \"e01\"  ) )(setq dcl_pt (done_dialog 2)) "
		  )
		  (setq bb (start_dialog))
		  (if (= bb 2)
		    (vla-put-textoverride ent k)
		  )
		  (if (= bb 1)
		    (progn
		      (setq ss3	(ssget "x"
				       '((0 . "dimension")
					 (-4 . "<or")
					 (1 . "")
					 (1 . "*<>*")
					 (-4 . "or>")
					)
				)
		      )
		      (vla-put-textoverride ent "")
		      (if ss3
			(setq
			  col (vla-get-textcolor
				(vlax-ename->vla-object (ssname ss3 0))
			      )
			)
			(progn
			  (setq	col
				 (getint
				   "\n  输入尺寸恢复颜色[1红,2黄,3绿,4青,5蓝,6洋红,7白]:<3>"
				 )
			  )
			  (if (null col)
			    (setq col 3)
			  )
			)
		      )
		      (vlax-put-property ent "textcolor" col)
		    )
		  )
		  (unload_dialog dcl_re)
		  (vl-file-delete dclname)
		  (princ "\n  ")
		)
	     )
	     (setq er (getvar "errno"))
	     (if name1
	       (redraw name1 4)
	     )
	     (if (= er 7)
	       (progn
		 (if (>= jj (sslength ss))
		   (progn
		     (setq jj 0)
		     (alert (strcat "\n检查了"
				    (itoa (sslength ss))
				    "个假尺寸,重新检查"
			    )
		     )
		   )
		 )
		 (setq name1 (ssname ss jj))
		 (redraw name1 3)
		 (setq np1 (cdr (assoc 11 (entget name1))))
		 (setq px  (car np1)
		       py  (cadr np1)
		       np1 (list (- px (* wzh 40)) (- py (* wzh 20)) 0.0)
					; 缩放窗口计算与字体高度关联
		       np2 (list (+ px (* wzh 40)) (+ py (* wzh 20)) 0.0)
		 )
		 (command "zoom" "W" np1 np2)
		 (setq jj (1+ jj))
		 (princ "\n  ")
	       )
	     )
	   )
	 )
	 (progn
	   (setq u nil)
	   (alert "\n本图未找到假尺寸!")
	 )
       )
    )
  )
  (command ".UNDO" "E")
  (princ)
)
;;;-----------------------------------------------------------------------------------;;