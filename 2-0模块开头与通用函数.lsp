;;;-----------------------------------------------------------------------------------;;
;;;ģ�飺  				    ��ע                                      ;;
;;;�����б�                 ���� +   ����   +   �޸�����   + ����                   ;;
;;;     	     	       dzz   ���ܱ�ע    2016-04-06    xyccf                  ;;��ȱ�����
;;;			       dtd   �ߴ����    2015-12-24    xyccf                  ;;��
;;;                            dsd   �ߴ��������� 2016-08-30   xyccf                  ;;��
;;;			       dcc   �ߴ���    2015-11-13    langjs                 ;;��
;;;			       dbr   �ߴ����    2015-08-25    waterchen              ;;������BUG
;;;	           (����Ӧ��ť)dgw   �ߴ��λ    2016-08-24    xyccf                  ;;��
;;;                            ddq   ��ע����    2016-02-04    xyccf                  ;;��
;;;			       dhf   �����ע       δ֪       �޷���֤               ;;��
;;;                (����Ӧ��ť)dsc   ԤӦ����    2015-09-29    xyccf                  ;;��
;;;                            dee   ��̬����    2013-06-xx    langjs                 ;;��
;;;                            dqq   ͼ�����       δ֪       �޷���֤               ;;��
;;;-----------------------------------------------------------------------------------;;



;;;������ע�ӳ���
(defun chc (sel / r ang angs ange larc pnt multi pt pt1 pt2 ename ent
	      a_last ssa)
  (setvar "CMDECHO" 0)
  (setq multi (getvar 'dimlfac))
  (setq blyz (cdr (assoc 144 (tblsearch "dimstyle" (getvar "dimstyle")))));���ò�����������
  (setq pt1 (getpoint "\nָ�����:"))
  (setq pt2 (getpoint "\nָ���յ�:"))
  (setq r (cdr (assoc 40 (entget sel)))) ;��ð뾶
  (setq pt (cdr (assoc 10 (entget sel)))) ;���
  (setq ang (abs (- (angle pt pt1) (angle pt pt2))))
  (setq larc (* r ang))
  (setq larc (* larc multi))
  (setq larc (rtos larc 2 2))		;����
  (vl-cmdf ".dimangular" "" pt pt1 pt2 "t" larc)
  (vl-cmdf pause)
  (setq entname (entlast))
  (setq ent (entget entname))
  (setq ang (cdr (assoc 42 ent)))
  (setq larc (* r ang))			;����
  (setq larc (* larc multi))
  (entmod (subst 
          (cons 1 (strcat "��" (rtos larc 2 1)))
		 (assoc 1 ent)
		 ent
	  )
  )					;
  (vl-cmdf "dimcontinue")
  (while (> (getvar "CMDACTIVE") 0)	;�����ڼ���������ҽ�ʱ��������
    (setq g (grread t 2 0))
    (cond
      ((and (= (car g) 2) (member (cadr g) '(13 32)))
       (vl-cmdf "" "" "")
      )
      ((member (car g) '(11 25)) (vl-cmdf "" "" ""))
      ((= (car g) 5)
       (vl-cmdf PAUSE)
       (setq dxf (entget (setq a_last (entlast))))
       (setq val (assoc 42 dxf))
       (setq new (* (cdr val) r blyz))
       (setq new (strcat "��" (rtos new 2 1))) ;����
       (setq dxf (subst (cons 1 new) (assoc 1 dxf) dxf))
       (entmod dxf)
      )
    )
  )
  (setvar "CMDECHO" 1)
  (princ)
)
;;;-----------------------------------------------------------------------------------;;



(defun centsel (msg f / el)
 (while (if (setq el (car (entsel msg))) (if (= (cdr (assoc 0 (entget el))) f) nil t) nil)) el
)




	
