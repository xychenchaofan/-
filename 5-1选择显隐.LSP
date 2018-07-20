;;;=====================================================================================;;
;;;名称:  显示隐藏  命令:cxx                                                         <7>;;
;;;功能： ①  选择显示 —— 只显示 选中的图元                                           ;;
;;;       ②  选择隐藏 —— 隐藏   选中的图元                                           ;;
;;;       ③  全部显示 —— 显示   全部  图元                                           ;;
;;; 改自 Gu_xl
(defun c:xy() (c:cxx))
(defun c:cxx (/ flag sel mode)
  (setq flag nil)
  (initget "s d f")
  (setq flag (getkword "\n[选择显示(s)/选择隐藏(d)/全部显示(f)]"))
  (cond ((= flag "s")
	 (prompt "\n选择只显示的对象，未选中的都隐藏")
	 (c:gldx)
	 )
	((= flag "d")
	 (prompt "\n选择需要隐藏的对象")
	 (c:ycdx)
	 )
	((or (= flag "f") (= flag nil)  )
	 (c:hfdx)
	)
  );end cond
)
;隐藏选择对象 by Gu_xl
(defun c:ycdx (/ obj)
  (if (ssget)
    (progn
      (vlax-for  obj
                    (vla-get-activeselectionset
                      (vla-get-activedocument (vlax-get-acad-object))
                    )
        (vla-put-Visible obj :vlax-false)
      )
    )
  )
  (princ)
)
;;隔离选择对象 By Gu_xl
(defun c:gldx (/ ss ss1 obj)
  (if (setq ss (ssget))
    (progn
      (setq ss1 (ssget "x"))
      (command "_select" ss1 "r" ss "")
      (vlax-for        obj
                    (vla-get-activeselectionset
                      (vla-get-activedocument (vlax-get-acad-object))
                    )
        (vla-put-Visible obj :vlax-false)
      )
    )
  )
  (princ)
)
;;恢复显示 By Gu_xl
(defun c:hfdx (/ ss obj)
  (if (setq ss (ssget "x" '((60 . 1))))
    (vlax-for obj
                  (vla-get-activeselectionset
                    (vla-get-activedocument (vlax-get-acad-object))
                  )
      (vla-put-Visible obj :vlax-true)
    )
  )
  (princ)
)
;;;                                   End of File                                     <7>;;
;;;--------------------------------------------------------------------------------------;;