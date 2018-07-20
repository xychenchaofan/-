;;;-------------------------------------------------------------------------------------;;
;;;模块：  			   图层、填充、比例、线型                               ;;
;;;功能列表：                 命令 +   名称   +   修改日期   + 作者（修改时间倒序排列） ;;
;;;        <1>  (无按钮)       chb   图层合并    2012-06-19    yjr111                   ;;√
;;;        <2>		       cs    锁定某层    2016-06-20    xyccf mjtd 自贡黄明儒    ;;√
;;;        <3>                 cjs   解锁某层    2016-06-20    xyccf mjtd 自贡黄明儒    ;;√
;;;        <4>                 cg    关闭某层    2016-06-20    xyccf mjtd 自贡黄明儒    ;;√
;;;        <5>                 cqc   打开某层    2015-12-22    xyccf mjtd 自贡黄明儒    ;;√
;;;        <6>           cqh / chh   切换图层    2016-06-21    xyccf mjtd 自贡黄明儒    ;;√
;;;        <7>                 cx    显示隐藏    2016-07-25    xyccf Gu_cl              ;;√
;;;        <8>                 cd    图层清理    2016-06-16    xyccf mjtd 自贡黄明儒    ;;√
;;;        <9>                 cw    图层漫游    2016-12-23    Autodesk                 ;;


(defun c:cs() (c:csd))
(defun c:cj() (c:cjs))
(defun c:cg() (c:cgb))
(defun c:cq() (c:cqc))







;;;-------------------------------==[功能  中的通用函数]==----------------------------;;

; 列表操作函数，双击位置删除处列表
(defun sjsclst (key / sjwz) 
  (cond((= $reason 4)
	(setq sjwz (atoi $value))     ;双击处在列表中的位置，返回0 1 2 3
	(setq lst (MJ:removeNth sjwz lst))
	(start_list key)
        (mapcar 'add_list lst)
        (end_list)
	)
       (= $reason 1())
       )
)

;;131.2 [功能] 从列表中删除指定的第n个元素
;;index 要删除的第几个元素,从0开始
;;lst 列表   (MJ:removeNth 1 lst)
(defun MJ:removeNth (index lst / c)
  (setq c -1)
  (apply 'append
   (mapcar '(lambda (x)
        (if  (/= (setq c (1+ c)) index)
          (list x)
        )
      )
     lst
   )
  )
)



;;;[功能] 表中去除重复元素
;;;[说明] 重复过的元素取第一次出现
(defun delsame (LST1 / LST2) 
  (while (setq LST2 (cons (car LST1) LST2)
	       LST1 (vl-remove (car LST1) (cdr LST1))
	 )
  )
  (reverse LST2)
)
;;;[功能] 图层列表 解锁 或锁定
;;;[说明] flag为T 锁定， nil代表解锁，Laylist 图层列表
(defun layer?lock (flag Laylist)
  (vlax-for each (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if (member (strcase (vla-get-name each)) LayList)
      (if (vlax-write-enabled-p each)
	(if flag
	  (vla-put-Lock each :vlax-True) ;锁定
	  (vla-put-Lock each :vlax-False);解锁
	)
      )
    )
    (vlax-release-object each)
  )
)





;;;[功能] 图层列表 打开 或 关闭 改自 mjtd 自贡黄明儒 通用函数
;;;[说明] flag为T 打开， nil代表关闭，Laylist 图层列表
(defun layer?open (flag Laylist)
  (vlax-for each (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if (member (strcase (vla-get-name each)) LayList)
      (if (vlax-write-enabled-p each)
	(if flag
          (vla-put-LayerOn each :vlax-True)
	  (vla-put-LayerOn each :vlax-False)
	)
      )
    )
    (vlax-release-object each)
  )
)




;;71.1图层列表 开
(defun MJ:LayerOn (LayList)
  (vlax-for each (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if	(member (strcase (vla-get-name each)) LayList)
      (if (vlax-write-enabled-p each)
	(vla-put-LayerOn each :vlax-True)
      )
    )
    (vlax-release-object each)
  )
)
;关闭图层列表
(defun MJ:ListLayers-Off (/ each out) 
  (vlax-for each (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if (= (vlax-get-property each "LayerOn") :vlax-false)
      (setq out (cons (vla-get-name each) out))
    )
  )
  out
)

(defun showlst (i lst)	       ; 显示列表
    (start_list i)
    (mapcar
      'add_list
      lst
    )
    (end_list)
)




;;;符号表状态检查
;;;tab输入类型  "LAYER"、"LTYPE"、"VIEW"、"STYLE"、"BLOCK"、"UCS"、"APPID"、"DIMSTYLE" 、 "VPORT"
;;;(STB "LTYPE")
(Defun STB (TAB / snl sll)
  (SetQ	snl '()
	sll (TblNext TAB T)
  )
  (While (setq snl (Cons (Cdr (Assoc 2 sll)) snl)
	       sll (TblNext TAB nil)
	 )
  )
  (vl-sort snl '<)
)
;;;(getty "LTYPE")
(defun getty (ty / lst x)	       
    (setq x (tblnext ty t))
    (while (/= x nil)
      (setq lst (cons (cdr (assoc 2 x)) lst))
      (setq x (tblnext ty))
    )
    (acad_strlsort lst)
)


  


;;;------------------------------------[子程序] by mjtd ---------------------------------;;
;;;功能说明： 删除无用图层                                                               ;;
;;;引用函数：                                                                            ;;
;;;引用参数：                                                                            ;;
;;;--------------------------------------------------------------------------------------;;
(defun MJ:LayerDelete ()
  (vl-Catch-All-Apply
    '(lambda ()
       (vla-Remove
  (vla-GetExtensionDictionary
    (vla-Get-Layers
      *DOC*
    )
  )
  "ACAD_LAYERFILTERS"
       )
     )
  )  
  (princ)
)

;;;------------------------------------[子程序] by xyccf---------------------------------;;
;;;功能说明： 创建图层                                                                   ;;
;;;引用函数： name--图层名 ，color--颜色 ， Ltype--线型 ， Lwith线宽                     ;;
;;;--------------------------------------------------------------------------------------;;
(defun mak:layer (Name Color Ltype Lwith)
  (entmake (list '(0 . "LAYER")
		 '(100 . "AcDbSymbolTableRecord")
		 '(100 . "AcDbLayerTableRecord")
		 '(70 . 0)
		 (cons 6 Ltype)		;如"Continuous"
		 (cons 2 Name)
		 (cons 62 Color)
		 (cons 370 Lwith)
	   )
  )
)
;;;                                   End of File                                     <8>;;
;;;--------------------------------------------------------------------------------------;;






;;;------------------------------------[子程序] by xyccf---------------------------------;;
;;;功能说明： 导入线型                                                                   ;;
;;;引用函数： lintype_lst--需要导入的线型名列表 exp. '("CENTER" "DASHED" "DIVIDE")       ;;
;;;--------------------------------------------------------------------------------------;;
(defun lintype_initia (lintype_lst / found doc entry)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (mapcar '(lambda (x)
    (setq found :vlax-false)
    (vlax-for entry (vla-get-Linetypes doc)
        (if (= (vla-get-Name entry) x)
          (setq found :vlax-true)
        )  
    )
    (if (= found :vlax-false)
        (vla-Load (vla-get-Linetypes doc) x "acad.lin")  
    )
  )
  lintype_lst
  )
)













