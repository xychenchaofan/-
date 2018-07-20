;;;=====================================================================================;;
;;;名称:  解锁某层  命令:cjs                                                         <3>;;
;;;功能： 将选定图元所在的图层列表中的图层都锁定（可显示不可修改）                      ;;
;;;引用： ss->lst / delsame / layer?lock                                                ;;
(defun c:cjs (/ ss entlst Laylist *LAYS*)
  (princ "\n请选择需要解锁的图层上的图元")
  (setq ss (ssget))
  (setq entlst (ss->lst ss))
  (setq Laylist
    (delsame (mapcar '(lambda(x) (cdr (assoc 8 (entget x))) ) entlst))
	;返回entlst中的图元所在的图层列表，去重
  )
  (layer?lock nil Laylist)
)
;;;                                   End of File                                    <3>;;
;;;-------------------------------------------------------------------------------------;;