;;;===================================================================================;;
;;;名称:  尺寸避让  命令:dbr                                                          ;;
;;;说明:  尺寸文字自动避让                                                            ;;
(defun c:dbr ( / dir dxf_3 j n olderr ss)
;;;子函数，定义出错处理
  (defun *error* (msg)
    (princ (strcat "\nError: " msg))
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      )
    (setvar "CMDECHO" 1)
    (princ)
  )
  ;;;dim_txt_move_h for move dimenstion text along horizontal direction
  ;;;ent:dimenstion list
  ;;;ang:direction angle
  
  (defun dim_txt_move_h  (ent ang / data dim  dxf en mtext pt10 pt11)
    (setq
      dim    (cdr (assoc 0 ent))
      data   (entget dim)
      dxf    (+ 128 (rem (cdr (assoc 70 data)) 128))
      pt11   (polar (cdr (assoc 11 ent)) ang (cdr (assoc 21 ent)))
      )
    (entmod (setq data (subst (cons 70 dxf) (assoc 70 data) data)))
    (entmod (setq data (subst (cons 11 pt11) (assoc 11 data) data)))
    
    (setq en (cdr (assoc -2 (tblsearch "block" (cdr (assoc 2 data))))))
    (while (/= (cdr (assoc 0 (setq mtext (entget (setq en (entnext en)))))) "MTEXT"))
    (entmod (setq mtext (subst (cons 10 pt11) (assoc 10 mtext) mtext)))
    (if (not (equal dir (cdr (assoc 50 mtext)) 1e-8))
      (progn
	(entmod (setq mtext (subst (cons 50 (cdr (assoc 32 ent))) (assoc 50 mtext) mtext)))
	(entupd en)
	)
      )
    (entupd dim)
    )
  
  ;;;dim_txt_move_h for move dimenstion text along vertical direction
  ;;;ent :dimenstion list
  ;;;pt11:dimenstion text insert point
  (defun dim_txt_move_v  (ent pt11 / data dim  dxf en mtext)
    (setq
      dim    (cdr (assoc 0 ent))
      data   (entget dim)
      dxf    (+ 128 (rem (cdr (assoc 70 data)) 128))
      )
    (entmod (setq data (subst (cons 70 dxf) (assoc 70 data) data)))
    (entmod (setq data (subst (cons 11 pt11) (assoc 11 data) data)))
    
    (setq en (cdr (assoc -2 (tblsearch "block" (cdr (assoc 2 data))))))
    (while (/= (cdr (assoc 0 (setq mtext (entget (setq en (entnext en)))))) "MTEXT"))
    (entmod (setq mtext (subst (cons 10 pt11) (assoc 10 mtext) mtext)))
    (if (not (equal dir (cdr (assoc 50 mtext)) 1e-8))
      (progn
	(entmod (setq mtext (subst (cons 50 (cdr (assoc 32 ent))) (assoc 50 mtext) mtext)))
	(entupd en)
	)
      )
    (entupd dim)
    )
  
  ;;;dim_ang_direction for calculation all dimenstions direction angle
  ;;;ent1:dimenstion list ((reference point) (text point) text_width text_height dim_measure dimension)
  ;;;ent2:dimenstion entity
  ;;;return:direction angle
  (defun dim_ang_direction (ent1 ent2 / ang p1 p2 p3 p4)
    (setq
      p1  (cdr (assoc 12 ent1))
      p2  (cdr (assoc 10 ent1))
      p3  (cdr (assoc 10 ent2))
      p4  (cdr (assoc 14 ent2))
      ang (angle p1 (inters p1 p2 p3 p4 nil))
      )
    (if (equal ang (* 2.0 pi) 1e-8)
      0.0
      ang
      )
    )
  
  ;;;dim_divide_into_groups for Dimensions are grouped according to whether Continuous
  ;;;s:Dimensions before grouping
  ;;;return:Dimensions after grouping
  (defun dim_divide_into_groups ( s /
				 data dimlst dimst pt10 pt11 pt13 pt14
				 )
    ;;;get_point_inside Whether the point is inside the line
    ;;;d: dinstance of the line
    ;;;lst: (x-distance,y-distance,distance of refer point to start point of the line)
    ;;;return: t or nil
    (defun get_point_inside (d lst)
      (and
	(equal (car lst) 0.0 1e-8)
	(equal (cadr lst) 0.0 1e-8)
	(<= 0.0 (caddr lst) d)
	)
      )
    
    (while s
      (setq
	dim   (car s)
	data  (entget dim)
	pt10  (cdr (assoc 10 data))
	pt13  (cdr (assoc 13 data))
	pt14  (cdr (assoc 14 data))
	pt11  (polar pt13
		     (angle pt14 pt10)
		     (sqrt (- (expt (distance pt10 pt13) 2) (expt (cdr (assoc 42 data)) 2)))
		     )
	
	s    (vl-remove dim s)
	dimst nil
	)
      (mapcar
	'(lambda (dimn /
		  datan dist1 dist2
		  pt11n pt13n pt15n
		  pt10n pt14n pt16n
		  )
	   (setq
	     datan  (entget dimn)
	     pt10n  (cdr (assoc 10 datan))
	     pt13n  (cdr (assoc 13 datan))
	     pt14n  (cdr (assoc 14 datan))
	     pt11n  (polar pt13n
			   (angle pt14n pt10n)
			   (sqrt (- (expt (distance pt10n pt13n) 2) (expt (cdr (assoc 42 datan)) 2)))
			   )
	     
	     pt15n  (polar pt11n (angle pt11n pt13n) (* 0.25 (distance pt11n pt13n)))
	     pt16n  (polar pt10n (angle pt10n pt14n) (* 0.25 (distance pt10n pt14n)))
	     
	     dist1  (distance pt11 pt13)
	     dist2  (distance pt10 pt14)
	     )
	   
	   (if (or
		 (get_point_inside dist1 (trans (mapcar '- pt15n pt11) 0 (mapcar '- pt13 pt11)))
		 (get_point_inside dist1 (trans (mapcar '- pt16n pt11) 0 (mapcar '- pt13 pt11)))
		 (get_point_inside dist2 (trans (mapcar '- pt15n pt10) 0 (mapcar '- pt14 pt10)))
		 (get_point_inside dist2 (trans (mapcar '- pt16n pt10) 0 (mapcar '- pt14 pt10)))
		 )
	     (progn
	       (setq
		 dimst (cons dimn dimst)
		 s     (vl-remove dimn s)
		 pt10  pt10n
		 pt11  pt11n
		 pt13  pt13n
		 pt14  pt14n
		 )
	       )
	     )
	   )
	s
	)
      (if dimst (setq dimlst (cons (cons dim (reverse dimst)) dimlst)))
      )
    (reverse dimlst)
    )
  
  ;;;**************************main program**************************;;;
  (princ "\n请选择水平竖直或对其标注，单层")
  (setq ss (ssget ":L" '(
			 (0 . "DIMENSION")
			 (-4 . "<OR")
			 (70 . 33)
			 (70 . 32)
			 (70 . 128)
			 (70 . 160)
			 (70 . 161)
			 (-4 . "OR>")
			 (-4 . "<OR")
			 (100 . "AcDbAlignedDimension")
			 (100 . "AcDbRotatedDimension")
			 (-4 . "OR>")
			 )
		  )
	)
  (if (and ss (>= (sslength ss) 2))
    (progn
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setvar "CMDECHO" 0)
      (vl-cmdf "_.dimedit" "_h" ss "")
      (setvar "CMDECHO" 1)
      (setq dxf_3
	     '(-3
	       (
		"ACAD"
		(1000 . "DSTYLE")
		(1002 . "{")
		(1070 . 73)
		(1070 . 0)   ;;;"DIMTIH" is off:Aligns text with the dimension line
		(1070 . 77)
		(1070 . 1)   ;;;"DIMTAD" is 1:Places the dimension text above the dimension line except
		;;;when the dimension line is not horizontal and text inside the extension lines is forced horizontal (DIMTIH = 1).
		;;;The distance from the dimension line to the baseline of the lowest line of text is the current DIMGAP value.
		(1070 . 172)
		(1070 . 1)   ;;;"DIMTOFL" is on: Draws dimension lines between the measured points even when arrowheads are placed outside the measured points
		(1070 . 174)
		(1070 . 1)   ;;;"DIMTIX" is on: Draws dimension text between the extension lines even if it would ordinarily be placed outside those lines
		(1070 . 279)
		(1070 . 2)   ;;;"DIMTMOVE" is 2: Allows text to be moved freely without a leader
		(1070 . 280)
		(1070 . 0)   ;;;"DIMJUST" is 0: Positions the text above the dimension line and center-justifies it between the extension lines
		(1070 . 288)
		(1070 . 0)   ;;;"DIMUPT"  is off: Cursor controls only the dimension line location
		(1002 . "}")
		)
	       )
	    )
      (setq
	ss (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
	ss (dim_divide_into_groups ss)
	)
      (mapcar
	'(lambda (s)
	   
	   (mapcar
	     '(lambda (dim / data)
		(setq data (vl-remove-if '(lambda (x) (= (car x) -3)) (entget dim '("*"))))
		(entmod (setq data (reverse (cons dxf_3 (reverse data)))))
		(entupd dim)
		)
	     s
	     )
	   
	   (setq s
		  (mapcar
		    '(lambda (dim /
			      ang
			      data dimang dimoff dimms dimsty
			      en
			      pt10 pt11 pt111 pt112 pt12 pt13 pt131 pt14
			      mtext
			      txtang txth txtw)
		       (setq
			 data   (entget dim '("*"))
			 pt10   (cdr (assoc 10 data))
			 pt11   (cdr (assoc 11 data))
			 pt13   (cdr (assoc 13 data))
			 pt14   (cdr (assoc 14 data))
			 dimsty (tblsearch "Dimstyle" (cdr (assoc 3 data)))
			 dimms  (/ (cdr (assoc 42 data)) (cdr (assoc 144 dimsty)))
			 dimoff (* (cdr (assoc 40 dimsty)) (cdr (assoc 147 dimsty)))
			 en     (cdr (assoc -2 (tblsearch "block" (cdr (assoc 2 data)))))
			 )
		       (while   (/= (cdr (assoc 0 (setq mtext (entget (setq en (entnext en)))))) "MTEXT"))
		       (setq
			 ;txtang (cdr (assoc 50 mtext))
			 txth   (max (cdr (assoc 40 mtext)) (cdr (assoc 43 mtext)))
			 txtw   (cdr (assoc 42 mtext))
			 pt131  (polar pt13
				       (angle pt14 pt10)
				       (sqrt (- (expt (distance pt10 pt13) 2) (expt dimms 2)))
				       )
			 dimang (angle pt131 pt10)
			 pt12   (polar pt10 (angle pt10 pt131) (* dimms 0.5))
			 ang    (angle pt12 pt11)
			 pt111  (polar pt12 ang        (+ (* txth 2.0) dimoff))
			 pt112  (polar pt12 (+ ang pi) (+ (* txth 1.0) dimoff))
			 )
		       (list
			 (cons  0   dim)   ;;;dimension entity name
			 (assoc 10  data)  ;;;definition point for dimension line
			 (assoc 11  data)  ;;;original middle point of dimension text
			 (cons  111 pt111) ;;;distance 2 times of text height middle point of dimension text
			 (cons  112 pt112) ;;;middle point of dimension text mirror from dimension line
			 (cons  12  pt12)  ;;;middle point of dimension line
			 (assoc 13  data)  ;;;definition point for dimension first extension line
			 (assoc 14  data)  ;;;definition point for dimension second extension line
			 (cons  20  txth)  ;;;dimension text height
			 (cons  21  txtw)  ;;;dimension text width
			 ;(cons  22  txtang)
			 (cons  30  dimms) ;;;dimension measure value
			 ;(cons  31  dimoff)
			 (cons  32  dimang);;;dimension angle from start point to end point
			 )
		       )
		    s
		    )
		 )
	   
	   (setq
	     s   (vl-sort
		   (vl-sort
		     s
		     (function (lambda (e1 e2) (< (car (cdr (assoc 12 e1))) (car (cdr (assoc 12 e2)))))
			       )
		     )
		   (function
		     (lambda (e1 e2) (< (cadr (cdr (assoc 12 e1))) (cadr (cdr (assoc 12 e2)))))
		     )
		   )
	     dir (dim_ang_direction (car s) (last s))
	     n   (length s)
	     )
	   
	   (cond
	     ((>= n 2)
	      (setq j 0)
	      (mapcar
		'(lambda (dim /
			  ang angp1 angp2
			  dimms
			  dimp1 dimp2
			  dist dist1  dist2
			  pt11 pt11p1 pt11p2
			  pt12 pt12p1 pt12p2
			  pt10 pt14
			  txtw txtwp1 txtwp2
			  txth
			  )
		   (setq
		     dimms (cdr (assoc 30 dim))
		     txth  (cdr (assoc 20 dim))
		     txtw  (cdr (assoc 21 dim))
		     )
		   
		   (cond
		     ;;;开始的那个
		     ((= j 0)
		      (if (> txtw dimms)
			(dim_txt_move_h dim (+ dir pi))
			)
		      )

		     ;;;最后的那个
		     ((= j (- n 1))
		      (if (> txtw dimms)
			(dim_txt_move_h dim dir)
			)
		      )

		     ;;;除头,除尾之外的
		     (t
		      (setq
			dimp1   (nth (- j 1) s)
			pt11p1  (cdr (assoc 11 (entget (cdr (assoc 0 dimp1)))))
			pt12p1  (cdr (assoc 12 dimp1))
			angp1   (angle pt12p1 pt11p1)
			dist1   (distance pt12p1 pt11p1)
			txtwp1  (cdr (assoc 21 dimp1))
			
			pt10    (cdr (assoc 10 dim))
			pt11    (cdr (assoc 11 dim))
			pt12    (cdr (assoc 12 dim))
			pt14    (cdr (assoc 14 dim))
			ang     (angle pt12 pt11)
			dist    (distance pt12 pt11)
			)
		      (if (and
			    (equal ang angp1 1e-8)        ;;;角度相等,说明文字同在尺寸线某一边
			    (< (abs (- dist dist1)) txth) ;;;如果与尺寸线距离小于1倍字高,说明前1文字位置没有移动过
			    (or
			      (> txtw dimms)              ;;;当前的尺寸文字宽度已大于其尺寸界线宽度
			      (> (* (+ txtwp1 txtw) 0.5) (distance pt11p1 pt11))
			      )                           ;;;前1的尺寸文字宽度已大于其尺寸界线宽度
			    )
			
			;;;如果上面3个条件成立,则要再次检查当前与前2的尺寸文字位置关系
			(progn
			  (if (>= j 3)
			    
			    ;;;相当于j-2=1,为什么不是0?因为第0个尺寸文字不是沿垂直尺寸线方向移动的
			    (progn
			      (setq
				dimp2   (nth (- j 2) s)
				pt11p2  (cdr (assoc 11 (entget (cdr (assoc 0 dimp2)))))
				pt12p2  (cdr (assoc 12 dimp2))
				angp2   (angle pt12p2 pt11p2)
				dist2   (distance pt12p2 pt11p2)
				txtwp2  (cdr (assoc 21 dimp2))
				
				pt11    (cdr (assoc 111 dim))
				dist    (distance pt12 pt11)
				)
			      (if (and
				    (equal ang angp2 1e-8)                  ;;;角度相等,说明文字同在尺寸线某一边
				    (< (abs (- dist dist2)) txth)           ;;;如果与尺寸线距离小于1倍字高,说明前2文字位置没有移动过
				    (or
				      (> txtw dimms)                        ;;;当前的尺寸文字宽度已大于其尺寸界线宽度
				      (> (* (+ txtwp2 txtw) 0.5) (distance pt11p2 pt11))
				      )                                     ;;;前2的尺寸文字宽度已大于其尺寸界线宽度
				    )
				(dim_txt_move_v dim (cdr (assoc 112 dim)))  ;;;如果上面条件成立,  则要移动尺寸文字到尺寸线下方1倍字高的位置
				(dim_txt_move_v dim (cdr (assoc 111 dim)))  ;;;如果上面条件不成立,则要移动尺寸文字到尺寸线上方2倍字高的位置
				)
			      )
			    
			    ;;;即j=2,第0个已可能沿尺寸线正或反方向移动,第1个不用移动,此时第2个有可能沿垂直尺寸线方向移动
			    (if (< (car (trans (mapcar '- pt14 pt10) 0 (mapcar '- pt12 pt10))) 0)
			      ;;;第2尺寸界线定义点在尺寸线的右手边(<0)
			      (if (< (car (trans (mapcar '- pt11 pt10) 0 (mapcar '- pt12 pt10))) 0)  ;;;如果文字中心点在尺寸线的右手边
				(dim_txt_move_v dim (cdr (assoc 112 dim)))                           ;;;移动尺寸文字到尺寸线下方1倍字高的位置
				(dim_txt_move_v dim (cdr (assoc 111 dim)))                           ;;;移动尺寸文字到尺寸线上方2倍字高的位置
				)
			      ;;;第2尺寸界线定义点在尺寸线的左手边(>0)
			      (if (< (car (trans (mapcar '- pt11 pt10) 0 (mapcar '- pt12 pt10))) 0)  ;;;如果文字中心点在尺寸线的右手边
				(dim_txt_move_v dim (cdr (assoc 111 dim)))                           ;;;移动尺寸文字到尺寸线上方2倍字高的位置
				(dim_txt_move_v dim (cdr (assoc 112 dim)))                           ;;;移动尺寸文字到尺寸线下方1倍字高的位置
				)
			      )
			    )
			  )
			;;;如果上面3个条件有不成立的,
			;;;角度不等则,说明前1文字在尺寸线的另一边,
			;;;与尺寸线距离天于1倍字高,则说明前1文字位置在2倍字高处
			(progn
			  (if (>= j 3)
			    (progn
			      (setq
				dimp2   (nth (- j 2) s)
				pt11p2  (cdr (assoc 11 (entget (cdr (assoc 0 dimp2)))))
				pt12p2  (cdr (assoc 12 dimp2))
				angp2   (angle pt12p2 pt11p2)
				dist2   (distance pt12p2 pt11p2)
				txtwp2  (cdr (assoc 21 dimp2))
				
				dist    (distance pt12 pt11)
				)
			      (if (and
				    (equal ang angp2 1e-8)                  ;;;角度相等,说明文字同在尺寸线某一边
				    (< (abs (- dist dist2)) txth)           ;;;如果与尺寸线距离小于1倍字高,说明前2文字位置没有移动过
				    (or
				      (> txtw dimms)                        ;;;当前的尺寸文字宽度已大于其尺寸界线宽度
				      (> (* (+ txtwp2 txtw) 0.5) (distance pt11p2 pt11))
				      )                                     ;;;前2的尺寸文字宽度已大于其尺寸界线宽度
				    )
				;;;如果上面3个条件成立,则要移动当前文字到新位置
				(if (equal (+ ang pi) angp1 1e-8)           ;;;如果前1文字已在尺寸线的另一边
				  (dim_txt_move_v dim (cdr (assoc 111 dim)));;;移动尺寸文字到尺寸线上方2倍字高的位置
				  (dim_txt_move_v dim (cdr (assoc 112 dim)));;;移动尺寸文字到尺寸线下方1倍字高的位置
				  )
				;;;如果上面3个条件成立,则当前文字保留在原位置
				(dim_txt_move_v dim (cdr (assoc 11 dim)))
				)
			      )
			    )
			  )
			)
		      )
		     )
		   (setq j (1+ j))
		   )
		s
		)
	      )
	     )
	   )
	ss
	)
      
      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      
      )
    
    (progn
      (if (null ss)
	(princ "未选择图元")
	(princ "\n请选择两个以上的尺寸标注")
	)
      )
    )
  (princ)
  )
;;;-----------------------------------------------------------------------------------;;