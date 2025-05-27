(vl-load-com)
(or *acad*   (setq *acad* (vlax-get-acad-object)))
(or *acdoc*  (setq *acdoc* (vla-get-ActiveDocument *acad*)))
(or *blocks* (setq *blocks* (vla-get-Blocks *acdoc*)))
(or *layers* (setq *layers* (vla-get-Layers *acdoc*)))

; InsEdit (gile)
;; Redéfinit le bloc sélectionné (déplacement du point de base sur le point
;; spécifié) et déplace ou non en conséquence toutes les références insérées.
;; Modifié par Patrick_35 pour éviter une synchro avec les attributs

(defun c:InsEdit (/ *error* ent elst ins pos bName lst disp ss n xform posat)

  (defun *error* (msg)
    (or (member (strcase msg) '("FUNCTION CANCELLED" ""QUIT / EXIT ABORT"" "FONCTION ANNULEE" "QUITTER / SORTIR ABANDON"))
      (princ (strcat "\nErreur : " msg))
    )
    (and lst (mapcar '(lambda(x)(vla-put-Lock x :vlax-true)) lst))
    (vla-EndUndoMark *acdoc*)
    (princ)
  )

  (vla-StartUndoMark *acdoc*)
  (if (and (setq ent (car (entsel "\nSélectionnez un bloc: ")))
	   (setq elst (entget ent))
	   (= (cdr (assoc 0 elst)) "INSERT")
	   (setq ins (getpoint "\nSpécifiez le nouveau point d'insertion: "))
      )
    (progn
      (initget "Oui Non")
      (or (setq pos (getkword "\nConserver la position ? [Oui/Non] <Oui> : "))
	(setq pos "Oui")
      )
      (vlax-for l *layers*
	(and (= (vla-get-Lock l) :vlax-true)
	  (setq lst (cons l lst))
	  (vla-put-Lock l :vlax-false)
	)
      )
      (setq ang   (- (cdr (assoc 50 elst)))
	    norm  (cdr (assoc 210 elst))
	    disp  (mxv (mxm (list (list (/ 1 (cdr (assoc 41 elst))) 0.0 0.0)
				  (list 0.0 (/ 1 (cdr (assoc 42 elst))) 0.0)
				  (list 0.0 0.0 (/ 1 (cdr (assoc 43 elst))))
			    )
			    (mxm (list (list (cos ang) (- (sin ang)) 0.0)
				       (list (sin ang) (cos ang) 0.0)
				       '(0.0 0.0 1.0)
				 )
				 (mapcar (function (lambda (v) (trans v norm 0 T)))
					 '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
				 )
			    )
		       )
		       (mapcar '-
			       (trans ins 1 0)
			       (trans (cdr (assoc 10 elst)) norm 0)
		       )
                  )
	    bName (cdr (assoc 2 elst))
      )
      (vlax-for obj (vla-item *blocks* bName)
	(vla-Move obj
		  (vlax-3d-point disp)
		  (vlax-3d-point '(0. 0. 0.))
	)
      )
      (if (= "Oui" pos)
	(progn
	  (ssget "_X" (list '(0 . "INSERT") (cons 2 bName)))
	  (vlax-for obj (setq ss (vla-get-ActiveSelectionSet *acdoc*))
	    (setq elst (entget (vlax-vla-object->ename obj))
		  ang  (cdr (assoc 50 elst))
		  norm (cdr (assoc 210 elst))
		  mat  (mxm (mapcar (function (lambda (v) (trans v 0 norm T)))
				    '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
			    )
			    (mxm (list (list (cos ang) (- (sin ang)) 0.0)
				       (list (sin ang) (cos ang) 0.0)
				      '(0.0 0.0 1.0)
				 )
				 (list (list (cdr (assoc 41 elst)) 0.0 0.0)
				       (list 0.0 (cdr (assoc 42 elst)) 0.0)
				       (list 0.0 0.0 (cdr (assoc 43 elst)))
				 )
			    )
		       )
	    )
	    (setq posat (mapcar '(lambda(x)(list x (vlax-get x 'InsertionPoint)))(vlax-invoke obj 'getattributes)))
	    (vla-Move obj
	      (vlax-3d-Point '(0. 0. 0.))
	      (vlax-3d-Point (mxv mat disp))
	    )
	    (and posat (mapcar '(lambda(x)(vlax-put (car x) 'insertionpoint (cadr x))) posat))
	  )
	  (vla-Delete ss)
	)
      )
    )
  )
  (setq *error* nil)
  (princ)
)

; TRP
;; transpose une matrice -Doug Wilson-
;;
;; Argument : une matrice
(defun trp (m) (apply 'mapcar (cons 'list m)))

; MXV
;; Applique une matrice de transformation à un vecteur -Vladimir Nesterovsky-
;;
;; Arguments : une matrice et un vecteur
(defun mxv (m v)
  (mapcar (function (lambda (r) (apply '+ (mapcar '* r v))))
	  m
  )
)
;; MXM
;; Multiplie (combine) deux matrices -Vladimir Nesterovsky-
;;
;; Arguments : deux matrices
(defun mxm (m q)
  (mapcar (function (lambda (r) (mxv (trp q) r))) m)
)