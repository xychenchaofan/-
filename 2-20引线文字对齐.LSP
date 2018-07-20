(defun c:T2L (/ ss ssyx sswz obj i ent ss jd jd1x jd1y jd2x jd2y yxfx en num ch)

(setvar "cmdecho" 0)
(command "_undo" "_be")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun mydxf (ent i)
      (if (= (type ent) 'ename)
          (setq ent (entget ent))
      )
      (cdr (assoc i ent))
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun ch_dxf(en num ch / old_num new_num ent)
   (if (setq ent (entget en)
             new_num (cons num ch)
             old_num (assoc num ent)
       )
       (entmod(subst new_num old_num ent))
       (entmod(reverse(cons new_num (reverse  ent))))
   )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun pdyxfx (ent)
      (setq jds (mydxf ent 76))
      (setq bili (vla-get-ScaleFactor (vlax-ename->vla-object ent)))
      (setq jd (vla-get-Coordinates (vlax-ename->vla-object ent)))
      (setq jd (vlax-safearray->list (vlax-variant-value jd)))
      (setq jd1x (nth (- (* jds 3) 6) jd)
            jd1y (nth (- (* jds 3) 5) jd)
            jd2x (nth (- (* jds 3) 3) jd)
            jd2y (nth (- (* jds 3) 2) jd)
      )
      (if (> (abs (- jd1x jd2x)) (abs (- jd1y jd2y)));true为横向
          (if (> jd1x jd2x)
              (setq yxfx "HR")
              (setq yxfx "HL")
          )
          (if (> jd1y jd2y)
              (setq yxfx "VU")
              (setq yxfx "VD")
          )
     )
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ssyx (ssget '((0 . "LEADER"))))
(setq i 0)
(setq n 0)
(repeat (sslength ssyx)
        (setq ent (ssname ssyx i))
        (pdyxfx ent)
        (setq jd1 (list jd1x jd1y))
        (cond ((= yxfx "HR");;条件1：引线方向水平向右
               (setq jd2L (list (- jd2x (* bili 5)) (+ jd2y (* bili 5))))	       
	       (setq sswzL (ssget "_C" jd1 jd2L '((0 . "*TEXT"))))
	       (if (null sswzL)
		   (setq n (+ n 1))
		   (progn
                   (if (> (sslength sswzL) 1)
	               (progn (setq j 0)
	                  (while (> (sslength sswzL) 1)
		                 (setq ent1 (ssname sswzL j))
		                 (setq ent2 (ssname sswzL (+ j 1)))
		                 (if (> (abs (- (cadr (mydxf ent1 10)) jd2y)) (abs (- (cadr (mydxf ent2 10)) jd2y)))
			             (ssdel ent1 sswzL)
			             (ssdel ent2 sswzL)
			         );end if
	                  );end while
	               );end progn
	           );end if
	           (command "justifytext" sswzL "" "L")
	           (setq en (ssname sswzL 0)
		         num 40
		         ch (* bili 3.5)
	           )
	           (ch_dxf en num ch)
	           (setq en (ssname sswzL 0)
	                 num 10
		         ch (list jd2x (+ jd2y (* bili 1)) 0)
	           )
	           (ch_dxf en num ch)
	          );end progn
	       );end if
	      );end 条件1
	      ((= yxfx "HL");;条件2：引线方向水平向左
               (setq jd2R (list (+ jd2x (* bili 5)) (+ jd2y (* bili 5))))	       
	       (setq sswzR (ssget "_C" jd1 jd2R '((0 . "*TEXT"))))
	       (if (null sswzR)
	           (setq n (+ n 1))
                   (progn
	           (if (> (sslength sswzR) 1)
	               (progn (setq j 0)
	                  (while (> (sslength sswzR) 1)
		                 (setq ent1 (ssname sswzR j))
		                 (setq ent2 (ssname sswzR (+ j 1)))
		                 (if (> (abs (- (cadr (mydxf ent1 10)) jd2y)) (abs (- (cadr (mydxf ent2 10)) jd2y)))
			             (ssdel ent1 sswzR)
			             (ssdel ent2 sswzR)
			         );end if
	                  );end while
	               );end progn
	          );end if
	          (command "justifytext" sswzR "" "R")
	          (setq en (ssname sswzR 0)
		        num 40
	                ch (* bili 3.5)
	          )
	          (ch_dxf en num ch)
	          (setq en (ssname sswzR 0)
	                num 11
		        ch (list jd2x (+ jd2y (* bili 1)) 0)
	          )
	          (ch_dxf en num ch)
	          );end progn
	       );end if
	      );end 条件2
	);end cond
        (setq i (+ i 1))
);end repeat
(command "_undo" "_e")
(princ (strcat "\n 共有" (rtos n 2) "条引线未能匹配文字。"))
(princ)
)