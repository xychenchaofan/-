;;;=====================================================================================;;
;;;名称:  打开某层  命令:cqz                                                         <5>;;
;;;功能： 将选中的已关闭的图层都打开,双击修改图层列表                                   ;;
;;;引用： sjsclst  /  MJ:ListLayers-Off  /  MJ:LayerOn                                               ;;
(defun c:cqc(/ dclname tempname filen stream dcl_re lst key sdt)
     (setq dclname (cond ((setq tempname (vl-filename-mktemp "tc.dcl")
				 filen (open tempname "w")
			 )
			 (foreach stream '("\n" "tcxg:dialog {\n"
			       "    label = \"图层修改\" ;\n"
			       "    :list_box{ label = \"关闭的图层列表\" ; key = \"e03\" ; }\n"
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
	  (if (not (new_dialog "tcxg" dcl_re))
	    (exit)
	  )
          (setq lst (MJ:ListLayers-Off)
                key "e03")
          (showlst key lst)
          (action_tile "e01" " (done_dialog 1) " )
          (action_tile "e03"  "(sjsclst key)" )
          (action_tile "e02" " (done_dialog 0) " )
        (setq sdt (start_dialog))
        (cond ((= sdt 1)
	       (MJ:LayerOn lst)
	      )
	      ((= sdt 0)
	       (unload_dialog dcl_re)
	      )
         )
  	 (unload_dialog dcl_re)
	 (vl-file-delete dclname)
)
;;;                                   End of File                                    <5>;;
;;;-------------------------------------------------------------------------------------;;