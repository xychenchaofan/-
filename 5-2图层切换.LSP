;;;=====================================================================================;;
;;;名称:  切换当前  命令:chh / cdq                                                   <6>;;
;;;功能： chh---将选中图元的图层切换到当前图层                                          ;;
;;;       cqh---将选中图元的图层切换到选中图层 （不对应按钮）(带对话框)                 ;;
;;;引用： ss->lst / delsame                                                             ;;
(defun c:chh (/ ss entlst Laylist )
  (princ "\n请选择需要切换图层的图元")
  (setq ss (ssget))
  (setq entlst (ss->lst ss))
  (setq Laylist  ;返回entlst中的图元所在的图层列表（已去重）
    (delsame (mapcar '(lambda(x) (cdr (assoc 8 (entget x))) ) entlst))
  )
  (vl-cmdf "Chprop" ss "" "layer" (getvar 'clayer) "")
)

(defun c:cqh (/ dclname tempname filen stream dcl_re lst key sdt)
  (princ "\n请选择需要切换图层的图元")
  (setq ss (ssget))
     (setq dclname (cond ((setq tempname (vl-filename-mktemp "tt.dcl")
				 filen (open tempname "w")
			 )
			 (foreach stream '("\n" "ttqh:dialog {\n"
			       "    label = \"图层修改\" ;\n"
			       "    :popup_list{ label = \"切换的图层列表\" ; key = \"e03\" ; }\n"
			       "    :row { :button { key = \"e01\" ; label = \"打开\" ;  is_default = true ;   }\n"
			       "           :button { key = \"e02\" ; label = \"取消\" ; is_cancel = true ; } } }\n"
			      )
			      (princ stream filen)
			    )
			    (close filen)
			    tempname
			  )
			)
	  )
	  (setq dcl_re (load_dialog dclname))
	  (if (not (new_dialog "ttqh" dcl_re))
	    (exit)
	  )
          (setq lst (getty "layer")
		key "e03")
          (showlst key lst)
          (action_tile "e01" " (done_dialog 1) " )
          (action_tile "e03"  "(setq pc (atoi $value ) )" )
          (action_tile "e02" " (done_dialog 0) " )
        (setq sdt (start_dialog))
        (cond ((= sdt 1)
	       (setq chosen (nth pc lst))
;;;	       (setvar 'clayer chosen)
	       (vl-cmdf "Chprop" ss "" "layer" chosen "")
	      )
	      ((= sdt 0)
	       (unload_dialog dcl_re)
	      )
         )
  	 (unload_dialog dcl_re)
	 (vl-file-delete dclname)
)


;;;                                   End of File                                    <6>;;
;;;-------------------------------------------------------------------------------------;;