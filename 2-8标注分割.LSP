;;;===================================================================================;;
;;;名称:  标注分割  命令:dfg                                                          ;;
;;;功能: 标注断开+连续标注
;;;引用:  centsel / get:dxf / ptper / Lsort
;; by  明经通道  QQ: 9034598 2009年8月15日
;;========================================
(defun c:dfg (/ n x ent entL p2 p3 px1 px2 py1 py2 ptdd xl sa pt0 ppt ptdd)
;;排序 0 水平 1 垂直 2 倾斜
(defun Lsort (LT i)
(cond
((or (= i 0)(= i 2))(setq Lt (vl-sort LT (function (lambda (e1 e2)(< (car e1) (car e2)))))))
((or (= i 1)(= i 2))(setq Lt (vl-sort LT (function (lambda (e1 e2)(< (cadr e1) (cadr e2)))))))
))
 (setvar "cmdecho" 0)
 (command "undo" "be")
 (setq entL '())
 (if (setq ent (centsel "\n选择标注 或 <退出>:" "DIMENSION"))
   (progn
     (setq x (entget ent)
        entL (cons ent entL)
          p2 (get:dxf 13 x)
          p3 (get:dxf 14 x)
          px1 (list (car p2) (/ (+ (cadr p2)(cadr p3)) 2.0))
          px2 (list (car p3) (/ (+ (cadr p2)(cadr p3)) 2.0))
          py1 (list (/ (+ (car p2)(car p3)) 2.0) (cadr p2))
          py2 (list (/ (+ (car p2)(car p3)) 2.0) (cadr p3))
          ptdd (list p2 p3)
          XL (entget (get:dxf -2 (tblsearch "block" (get:dxf 2 x))))
          SA (abs (sin (angle (get:dxf 10 xl) (get:dxf 11 xl)))))
   (while (setq pt0 (getpoint "\n取点 或 <退出>:"))
       (vl-cmdf ".copy" ent "" "0,0" "@")
       (setq entL (cons (entlast) entL))
    (cond
      ((equal SA 1 1e-6) ;;水平
         (setq ptdd (cons (ptper pt0 px1 px2) ptdd)
                 ppt (Lsort ptdd 0))
        )
      ((equal SA 0 1e-6) ;;垂直
         (setq ptdd (cons (ptper pt0 py1 py2) ptdd)
                ppt (Lsort ptdd 1))
       )
       (t (setq ptdd (cons (ptper pt0 p2 p3) ptdd)
            ppt (Lsort ptdd 2)))
      )
       (setq ppL (mapcar 'list ppt (cdr ppt))
              n 0)
         (repeat (length ppL)
           (setq xf (entget (nth n entL))
                nxf (subst (cons 13 (car  (nth n ppL)))(assoc 13 xf) xf)
                wxf (subst (cons 14 (cadr (nth n ppL)))(assoc 14 nxf) nxf)
                n (1+ n))
           (entmod wxf)
         )
     ))
  (princ "\n退出")
 )
  (command "undo" "e") (setvar "cmdecho" 1)
  (princ)
)
;;;                                          End of File                                               ;;
;;;----------------------------------------------------------------------------------------------------;;