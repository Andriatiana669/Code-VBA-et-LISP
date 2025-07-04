(defun c:imshow ( / ent dxf_ent flag_display new_flag)
	(while (not (setq ent (entsel "\nSélectionner une image: "))))
	(setq dxf_ent (entget (car ent)))
	(cond
		((eq (cdr (assoc 0 dxf_ent)) "IMAGE")
			(setq flag_display (cdr (assoc 70 dxf_ent)))
			(if (zerop (boole 1 flag_display 1))
				(setq new_flag (1+ flag_display))
				(setq new_flag (1- flag_display))
			)
			(entmod (subst (cons 70 new_flag) (assoc 70 dxf_ent) dxf_ent))
		)
		(T
			(princ "\nN'est pas une image!")
		)
	)
	(prin1)
)
