;CFC_ATTR

; ------------------------------------------
; Changer le facteur d'extension d'attribut
; ------------------------------------------

; Fichier info: CFC_ATTR.TXT

(defun c:VVS ()
  ;;; On sélectionne un attribut pour retrouver le nom du bloc
  (setq attr (car (entsel "\nCliquez sur un attribut")))
                
  (if (and 
        (= (cdr (assoc 0 (entget attr))) "INSERT")
        (= (cdr (assoc 66 (entget attr))) 1)
        )
    (progn
        (setq b (cdr (assoc 2 (entget attr))))

        ;;; Nous forçons une seule modification pour ce bloc
        (setq jeu (ssadd))  ; Crée une sélection vide, puis ajoute l'attribut sélectionné
        (ssadd attr jeu)

        (setq nb (sslength jeu))

        (princ "\nUn seul bloc trouvé.") 

        ;;; On définit le facteur d'extension ou de compression avec la valeur par défaut de 0.7
        (setq nht 0.7) ; Valeur par défaut

        ;;; Valeurs fixes pour rotation et angle oblique en termes de PI/2
        (setq rotation (* (/ 5 9) pi)) ; Rotation = 100 degrés = 5π/9 radians (sens des aiguilles d'une montre)
        (setq angle_oblique (* (/ 5 9) pi)) ; Angle oblique = 5π/9 radians

        (princ "\nPatience...travail en cours.")

        ;;; On modifie uniquement l'attribut du bloc sélectionné
        (setq bl (ssname jeu 0)) ; Prend le premier (et seul) élément de la sélection

        (extat) ;;; module de récupération des infos des attributs

        ;;; On provoque une régénération du dessin
        (command "regen")
        (princ "\nFIN DU TRAVAIL...")
        (princ)
                
    ) ; fin du progn

    (princ "\nL'entité sélectionnée n'est pas un attribut !!")
        
  ) ;fin du if

  (princ)

) ; fin du defun du programme principal CHATTR

;;; ---------------------------------------------------
;;; MODULE DE RECUPERATION DES INFOS DANS LES ATTRIBUTS
;;; ---------------------------------------------------

(defun extat ()
  (setq att (entnext bl))

  (setq cle_0 (cdr (assoc 0 (entget att))))
        
  ;;; On fait tant que la valeur de la clé 0 est différente de SEQEND
  (while (/= cle_0 "SEQEND")

    ;;; On récupère le facteur contenu dans la clé 41
    (setq cle_41  (assoc 41 (entget att)))

    ;;; On crée une nouvelle clé 41 contenant le nouveau facteur
    (setq ncle_41 (cons 41 nht)) 

    ;;; On modifie la base de donnée de l'attribut
    (setq natt (subst ncle_41 cle_41 (entget att)))

    ;;; Modifications supplémentaires pour la hauteur, rotation et angle oblique
    (setq cle_40 (assoc 40 (entget att))) ; Hauteur
    (setq cle_50 (assoc 50 (entget att))) ; Rotation
    (setq cle_51 (assoc 51 (entget att))) ; Angle oblique

    ;;; Modification des valeurs
    (setq natt (subst (cons 40 0.5) cle_40 natt))  ; Hauteur = 0.5
    (setq natt (subst (cons 50 rotation) cle_50 natt))  ; Rotation = 5π/9 radians
    (setq natt (subst (cons 51 angle_oblique) cle_51 natt))  ; Angle oblique = 5π/9 radians

    ;;; On met à jour la base de données
    (entmod natt)                              

    ;;; On récupère le code entité du prochain attribut
    (setq att (entnext att))

    ;;; On récupère la valeur de sa clé 0
    (setq cle_0 (cdr (assoc 0 (entget att)))) 
        
  ) ; fin du while                               

) ; fin du defun module EXTAT                                       

;;; Message affiché lors du chargement du programme CFC_ATTR.LSP
(princ "\n==> CFC_ATTR")
(princ)
