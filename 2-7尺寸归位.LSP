
;;;标注文字恢复原位 
;;;利用dimedit实现 尺寸文字避让的逆运算；
(defun c:dgw (/ ss i ename pt pt1 pt2 dxf)
;;;(command "_.undo" "_begin")
  (princ "\n标注文字恢复原位：")
  (setq	ss (ssget '((0 . "dimension,arc_dimension")))
	i  -1
  )
  (while (setq ename (ssname ss (setq i (1+ i))))
    (setq dxf (entget ename))
    (setq pt1 (cdr (assoc 13 dxf))
	  pt2 (cdr (assoc 14 dxf))
	  pt (midp pt1 pt2)
	  )
;;;    (command "_dimtedit" "h" "" (list ename pt));似乎在2016上可行
    (vl-cmdf "_dimtedit" ename "h" pt "");在2007、2010上正常
  )
;;;(command "_.undo" "_end")
  (princ)
)
;;;-----------------------------------------------------------------------------------;;