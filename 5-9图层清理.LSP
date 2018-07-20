;;;待解决:  对图块里的图层没有作用！



;;;=====================================================================================;;
;;;名称:  图层清理  命令:cad                                                          <8>;;
;;;功能： ①删除未使用的图层（该层上没有图元且未被参照）,比purge彻底                    ;;
;;;       ②所有尺寸标注归入  "6标注线"  层   只适用于大桥局绘图习惯                    ;;
;;;         所有文字归入  "5文本"  层       只适用于大桥局绘图习惯                      ;;
;;;       ③如果图中没有"6标注线""5文本"图层，则创建图层后实现②中的功能                ;;
;;;       ④根据企标绘图规范，将图层归并、重命名                                        ;;
(defun c:cad( / *DOC*)
  (setvar 'cmdecho 0)
  (layermerger)   ;功能②③④
  (if (= nil *DOC*)
     (setq *DOC* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (MJ:LayerDelete);功能①
  (setvar 'cmdecho 1)
)
;;;------------------------------------[子程序] by xyccf---------------------------------;;
;;;功能说明： 创建"5文本"，"6标注线"，把标注和文字分别归入这两层                           ;;
;;;引用函数： mak:layer                                                                  ;;
;;;引用参数：                                                                            ;;
;;;--------------------------------------------------------------------------------------;;
(defun layermerger (/ ss)
;-=1=-  检查图纸中是否有符合企标的图层，若不存在则建立图层，若存在则修改图层特性使其符合规范
  (if (or (= nil (tblobjname "layer" "6标注线"))
	  (= nil (tblobjname "layer" "5文本"))
	  )
    (progn
      (mak:layer "6标注线" 3 "Continuous" 18)
      (mak:layer "5文本" 4 "Continuous" 18)
    )
  )
;-=2=-  根据企标将不符合规范的图层归并、重命名
  (setq lst1 (list "*粗实线*,*粗线*" "*细实线,细线*" "*中心线*,*点划线*,*点化线*" "*虚线*" "*文本*,*text*" "*标注*,*dim*" "*边界*,*轮廓*,*构造*" "*图框*")
	lst2 (list "1粗实线" "2细实线" "3中心线" "4虚线" "5文本" "6标注线" "7边界线" "8边框线")
	)
  (foreach x lst1
    (setq ss (ssget "x" (list (cons 8 x))))
;;;    (vl-cmdf "Chprop" ss "" "layer" (nth (vl-position x lst1) lst2) "")在2016版本中不能用。。。尴尬
    (setq layname (nth (vl-position x lst1) lst2))
    (mapcar '(lambda (y) (vla-put-Layer y layname))  (ss->vla ss) )
  )
;-=3=-  将标注与文本分别置入各层
  (setq ss (ssget "X" '((0 . "DIMENSION,LEADER,ARC_DIMENSION"))))
  (vl-cmdf "Chprop" ss "" "layer" "6标注线" "")
  (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
  (vl-cmdf "Chprop" ss "" "layer" "5文本" "")
)
;;;(setq ss (ssget "x" '((8 . "*标注*,*dim*"))))
;;;(setq ssv (vla-get-ActiveSelectionSet (vla-get-ActiveDocument (vlax-get-acad-object))))
;;;(foreach x (ss->lst ss) (vla-put-Layer (vlax-ename->vla-object x) "2细实线"))
;;;(vl-cmdf "Chprop" ss "" "layer" "5标注" "")
;;;  (nth (vl-position "2" '("1" "2" "3" "4")) '("1" "2" "3" "4"))

;选择图层表("图层1" "图层2" "图层3" "图层4" ---------------有很多个图层)内的线（这个要感谢网友eachy，感谢学院派xyp1964进行了优化）
(defun c:test18()
  (setq la '("1" "2" "3" "4")
     ss (ssget (list '(0 . "line") (cons 8 (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) la)))))
  ))
;(cons 8 (substr (apply 'strcat (mapcar '(lambda (x) (strcat "," x)) la)) 2))