(defun c:InsertBlockIntoPoly (/ obj plist ss blkName)
 (vl-load-com)
 (setq mdspace (vla-get-modelspace
		(vla-get-activedocument
		 (vlax-get-acad-object)
		)
	       )
 )

 (setvar "cmdecho" 0)

 (setq blkName (getstring T "Entrer bloc name : <1>"))
 (if (or (= nil blkName) (= "" blkName))
  (setq blkName "1")
 )

 (setq ss (ssget))
 (setq i 0)
 (while	(setq obj (ssname ss i))
  (setq i (+ i 1))
  (if (wcmatch (cdr (assoc 0 (entget obj))) "POLYLINE")
   (progn
    (setq obj (entnext obj))
    (while (/= "SEQEND" (cdr (assoc 0 (entget obj))))
     (progn
      (setq point (cdr (assoc 10 (entget obj))))
      (setq block (vla-InsertBlock mdspace
				   (vlax-3d-point point)
				   blkName
				   1
				   1
				   1
				   0
		  )
      )
     )
     (setq obj (entnext obj))
    )
   )
  )
 )
 (princ)
)
 ;|«Visual LISP© Format Options»
(155 1 1 2 nil "Fin de " 60 20 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;