;CFC_ATTR

; ------------------------------------------
; Changer le facteur d'extension d'attribut
; ------------------------------------------


; Fichier info: CFC_ATTR.TXT



(defun c:cfc_attr ()

;;; On sélectionne un attribut pour retrouver le nom du bloc

(setq attr (car (entsel "\nCliquez sur un attribut")))
                
(if (and 
        (= (cdr (assoc 0 (entget attr))) "INSERT")
        (= (cdr (assoc 66 (entget attr))) 1)
        )
(progn                

        (setq b (cdr (assoc 2 (entget attr))))

;;; On crée un jeu de sélection contenant tous les blocs avec attributs
;;; ou on ne modifie que celui sélectionné

(setq jeu nil)

(initget 7 "T U")
(setq choix (getkword "\nModification Totale ou Unique T/U: "))

(if (= choix "T")
(setq jeu (ssget "x" (list (cons 0 "insert")(cons 2 b))))
(progn
(setq jeu (ssadd))
(ssadd attr jeu)
)
)

(setq nb (sslength jeu))

(princ "\nIl y a ") (princ nb) (princ " blocs trouvés.") 

;;; On définit le nouveau facteur d'extension ou de compression
(initget (+ 1 2 4))
(setq nht (getreal "\nNouveau facteur d'extension ou de compression du texte: "))

(princ "\nPatience...travail en cours.")

(setq compte 0)

;;; On crée une boucle tant qu'il y a de blocs 

(repeat nb
    (setq bl (ssname jeu compte))
    (extat) ;;; module de récupération des infos des attributs
    (setq compte (+ 1 compte))
) ;fin de boucle


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

;;; On récupère le facteur  contenu dans la clé 41

(setq cle_41  (assoc 41 (entget att)))

;;; On crée une nouvelle clé 41 contenant le nouveau facteur
(setq ncle_41 (cons 41 nht)) 

;;; On modifie la base de donnée de l'attribut

(setq natt (subst ncle_41 cle_41 (entget att)))

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
