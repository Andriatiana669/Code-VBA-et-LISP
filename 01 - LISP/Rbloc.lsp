;;;=================================================================
;;;
;;; RBLOC V2.22
;;;
;;; Remplacer un bloc par un autre
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:rbloc(/ bl conserver_attr conserver_dyna dcl_id echo echu echx echy echz
		 js liste_bl obj_liste old_error redef resultat selectiono selectionr
		 *errrbloc* MsgBox affiche_choix idem_lst ech_u affiche_dial
		 liste_choix liste_sel selection verif_valeur parcourir
		 selection_ecran changer_blocs)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion des erreurs
  ;;;
  ;;;---------------------------------------------------------------

  (defun *errrbloc* (msg)
    (if (/= msg "Function cancelled")
      (if (= msg "quit / exit abort")
	(princ)
	(princ (strcat "\nErreur : " msg))
      )
      (princ)
    )
    (setq *error* old_error)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Message
  ;;;
  ;;;---------------------------------------------------------------

  (defun MsgBox (Titre Bouttons Message / Reponse WshShell)
    (vl-load-com)  
    (setq WshShell (vlax-create-object "WScript.Shell"))
    (setq Reponse  (vlax-invoke WshShell 'Popup Message 0 Titre (itoa Bouttons)))
    (vlax-release-object WshShell)
    Reponse
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Affichage tu type de sélection du bloc d'origine
  ;;;
  ;;;---------------------------------------------------------------

  (defun affiche_choix()
    (if (eq selectiono "0")
      (if js
	(set_tile "texte1" (strcat "Sélection Multiple de " (itoa (sslength js)) " bloc(s)"))
	(set_tile "texte1" "Sélection Multiple dans tout le dessin")
      )
      (if js
	(set_tile "texte1" (strcat "Sélection défini de " (itoa (sslength js)) " bloc(s)"))
	(set_tile "texte1" "Sélection défini dans tout le dessin")
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Comparaison des deux listes
  ;;;
  ;;;---------------------------------------------------------------

  (defun idem_lst()
    (if (and (eq (1- (atoi selectiono))(atoi selectionr)) (not redef))
      (progn
	(set_tile "texte2" "L'origine et le remplaçant ne peuvent pas être identique")
	(mode_tile "accept" 1)
	(mode_tile "cancel" 2)
      )
      (progn
	(if redef
	  (set_tile "texte2" (strcat "Bloc : " redef))
	  (set_tile "texte2" "")
	)
	(mode_tile "accept" 0)
	(mode_tile "accept" 2)
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Gestion de l'afficjage des facteurs d'échelles
  ;;;
  ;;;---------------------------------------------------------------

  (defun ech_u()
    (if (eq echo "1")
      (progn
	(mode_tile "fact_x" 1)
	(mode_tile "fact_y" 1)
	(mode_tile "fact_z" 1)
	(mode_tile "uniforme" 1)
      )
      (progn
	(mode_tile "fact_x" 0)
	(mode_tile "uniforme" 0)
	(if (eq echu "1")
	  (progn
	    (mode_tile "fact_y" 1)
	    (mode_tile "fact_z" 1)
	  )
	  (progn
	    (mode_tile "fact_y" 0)
	    (mode_tile "fact_z" 0)
	  )
	)
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Afficher la boite de dialogue
  ;;;
  ;;;---------------------------------------------------------------

  (defun affiche_dial()
    (new_dialog "rbloc" dcl_id)
    (set_tile "titre" "Rbloc V2.22")
    (start_list "listeo")
    (add_list "** Bloc(s) Multiple(s) **")
    (mapcar 'add_list liste_bl)
    (end_list)
    (set_tile "listeo" selectiono)
    (set_tile "attr" conserver_attr)
    (set_tile "dyna" conserver_dyna)
    (affiche_choix)
    (start_list "lister")
    (mapcar 'add_list liste_bl)
    (end_list)
    (set_tile "lister" selectionr)
    (if redef
      (mode_tile "lister" 1)
      (mode_tile "lister" 0)
    )
    (set_tile "echori" echo)
    (set_tile "fact_x" echx)
    (set_tile "fact_y" echy)
    (set_tile "fact_z" echz)
    (set_tile "uniforme" echu)
    (ech_u)
    (idem_lst)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Comparaison avec la liste du bloc remplaçant
  ;;;
  ;;;---------------------------------------------------------------

  (defun liste_choix(val)
    (setq selectiono val js nil)
    (affiche_choix)
    (idem_lst)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Comparaison avec la liste du bloc d'origine
  ;;;
  ;;;---------------------------------------------------------------

  (defun liste_sel(val)
    (setq selectionr val)
    (idem_lst)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Sélection dans le dessin suivant un filtre
  ;;;
  ;;;---------------------------------------------------------------

  (defun selection(/ js1)
    (if (eq selectiono "0")
      (setq js1 (ssget (list (cons 0 "INSERT"))))
      (setq js1 (ssget (list (cons 0 "INSERT") (cons 2 (strcat (nth (1- (atoi selectiono)) liste_bl) ",`*U*")))))
    )
    (and js1
      (setq js js1)
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Vérification que la valeur zéro n'est pas entrée
  ;;;
  ;;;---------------------------------------------------------------

  (defun verif_valeur(var val)
    (if (zerop (read val))
      (cond
	((= var "x")
	  (set_tile "texte3" "Le facteur d'échelle X ne peut être nul")
	  (mode_tile "fact_x" 2)
	)
	((= var "y")
	  (set_tile "texte3" "Le facteur d'échelle Y ne peut être nul")
	  (mode_tile "fact_y" 2)
	)
	((= var "z")
	  (set_tile "texte3" "Le facteur d'échelle Z ne peut être nul")
	  (mode_tile "fact_z" 2)
	)
      )
      (cond
	((= var "x")
	  (set_tile "texte3" "")
	  (setq echx val)
	)
	((= var "y")
	  (set_tile "texte3" "")
	  (setq echy val)
	)
	((= var "z")
	  (set_tile "texte3" "")
	  (setq echz val)
	)
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Rechercher un bloc en tant que fichier
  ;;;
  ;;;---------------------------------------------------------------

  (defun parcourir(/ fic n result trouve)
    (if (setq fic (getfiled "Sélectionnez votre bloc" "" "dwg" 16))
      (progn
	(setq n 0 redef nil)
	(while (nth n liste_bl)
	  (if (eq (strcase (nth n liste_bl)) (strcase (vl-filename-base fic)))
	    (progn
	      (setq trouve T)
	      (setq result (msgbox "Rbloc - Bloc existant" (+ 4 48 256) (strcat "Le bloc " (strcase (nth n liste_bl)) " est déjà dans le dessin.\nDésirez-vous le remplacer ?")))
	      (if (eq result 6)
		(setq redef fic selectionr (itoa n))
		(setq redef nil selectionr (itoa n))
	      )
	    )
	  )
	  (setq n (1+ n))
	)
	(or trouve redef (setq redef fic))
      )
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Selection d'un bloc remplacant sur l'écran
  ;;;
  ;;;---------------------------------------------------------------

  (defun selection_ecran(/ bl n no sel)
    (while (not (setq sel (ssget "_:E:S" (list (cons 0 "INSERT")))))
      (princ "\nVeuillez sélectionner un bloc.")
    )
    (setq no (vlax-ename->vla-object (ssname sel 0))
	  no (if (vlax-property-available-p no 'effectivename)
		(vla-get-effectivename no)
		(vla-get-name no)
	     )
    )
    (setq sel (tblsearch "block" no) n 0)
    (if (and (not (eq (logand (cdr (assoc 70 sel)) 1) 1))
	     (not (eq (logand (cdr (assoc 70 sel)) 4) 4))
	     (not (eq (logand (cdr (assoc 70 sel)) 16) 16))
	)
      (while (setq bl (nth n liste_bl))
	(if (eq (cdr (assoc 2 sel)) bl)
	  (setq selectionr (itoa n))
	)
	(setq n (1+ n))
      )
      (Msgbox "Rbloc" 48 "Ce bloc est un xref.")
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Remplacer un bloc par un autre
  ;;;
  ;;;---------------------------------------------------------------

  (defun changer_blocs(/ bl n nbl nn no nw imod nom cont result sav_dyna tot)
    (if (and (not js) (eq selectiono "0"))
      (progn
	(setq result (msgbox "Rbloc - ATTENTION !!!" (+ 4 16 256) "Vous allez remplacer tous les blocs du dessin par un SEUL TYPE !!!\nDésirez-vous continuer ?"))
	(if (eq result 7)
	  (setq cont T)
	)
      )
    )
    (if (not cont)
      (progn
	(setq imod (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
	(if redef
	  (progn
	    (vla-delete (vla-insertblock imod (vlax-3d-point '(0.0 0.0 0.0)) redef 1 1 1 0))
	    (setq nom (vl-filename-base redef))
	  )
	  (setq nom (nth (atoi selectionr) liste_bl))
	)
	(if (not js)
	  (if (eq selectiono "0")
	    (setq js (ssget "x" (list (cons 0 "INSERT"))))
	    (setq js (ssget "x" (list (cons 0 "INSERT") (cons 2 (strcat (nth (1- (atoi selectiono)) liste_bl) ",`*U*")))))
	  )
	)
	(setq n 0 tot 0)
	(while (ssname js n)
	  (setq bl (entget (ssname js n))
		nw (vlax-ename->vla-object (ssname js n))
		no (if (vlax-property-available-p nw 'effectivename)
		      (vla-get-effectivename nw)
		      (vla-get-name nw)
		   )
	  )
	  (if (or (eq selectiono "0")
		  (and (not (eq selectiono "0"))
			    (eq no (nth (1- (atoi selectiono)) liste_bl))
		  )
	      )
	    (progn
	      (and (eq conserver_dyna "1")
		   (eq (vla-get-isdynamicblock nw) :vlax-true)
		(setq sav_dyna (mapcar '(lambda(x)(cons (vla-get-propertyname x) (vla-get-value x))) (vlax-invoke nw 'getdynamicblockproperties)))
	      )
	      (if (eq conserver_attr "1")
		(if (not (eq (strcase no) (strcase nom)))
		  (setq bl (subst (cons 2 nom) (assoc 2 bl) bl))
		)
		(progn
		  (setq nbl (entget (vlax-vla-object->ename (vla-insertblock imod (vlax-3d-point (cdr (assoc 10 bl))) nom (cdr (assoc 41 bl)) (cdr (assoc 42 bl)) (cdr (assoc 43 bl)) (cdr (assoc 50 bl))))))
		  (entdel (cdr (assoc -1 bl)))
		  (foreach n '(6 8 44 45 67 70 71 210 410)
		    (setq nbl (subst (assoc n bl) (assoc n nbl) nbl))
		  )
		  (setq bl nbl)
		)
	      )
	      (if (eq echo "0")
		(if (eq echu "0")
		  (setq bl (subst (cons 41 (atof echx)) (assoc 41 bl) bl)
			bl (subst (cons 42 (atof echy)) (assoc 42 bl) bl)
			bl (subst (cons 43 (atof echz)) (assoc 43 bl) bl)
		  )
		  (setq bl (subst (cons 41 (atof echx)) (assoc 41 bl) bl)
			bl (subst (cons 42 (atof echx)) (assoc 42 bl) bl)
			bl (subst (cons 43 (atof echx)) (assoc 43 bl) bl)
		  )
		)
	      )
	      (entmod bl)
	      (entupd (cdr (assoc -1 bl)))
	      (setq nw (vlax-ename->vla-object (cdr (assoc -1 bl))))
	      (and (eq conserver_dyna "1")
		   (eq (vla-get-isdynamicblock nw) :vlax-true)
		(progn
		  (foreach no (vlax-invoke nw 'getdynamicblockproperties)
		    (and (setq nn (assoc (vla-get-propertyname no) sav_dyna))
			 (/= (car nn) "Origin")
			 (/= (vlax-get no 'value) (vlax-variant-value (cdr nn)))
		      (vl-catch-all-apply 'vla-put-value (list no (cdr nn)))
		    )
		  )
		)
	      )
	      (setq tot (1+ tot))
	    )
	  )
	  (setq n (1+ n))
	)
	(princ (strcat "\n\tRemplacement de "  (itoa tot) " bloc(s)."))
      )
      (princ "\n\tAbandon.")
    )
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale.
  ;;;
  ;;;---------------------------------------------------------------

  (vl-load-com)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq Old_Error *error* *error* *errrbloc*)
  (if (findfile "rbloc.dcl")
    (progn
      (setq bl (tblnext "block" t))
      (while bl
	(if (and (not (eq (logand (cdr (assoc 70 bl)) 1) 1))
		 (not (eq (logand (cdr (assoc 70 bl)) 4) 4))
		 (not (eq (logand (cdr (assoc 70 bl)) 16) 16)))
	  (setq liste_bl (append liste_bl (list (cdr (assoc 2 bl)))))
	)
	(setq bl (tblnext "block"))
      )
      (if liste_bl
	(progn
	  (setq dcl_id (load_dialog (findfile "rbloc.dcl"))
		liste_bl (acad_strlsort liste_bl)
		conserver_attr "1"
		conserver_dyna "1"
		selectiono "0"
		selectionr "0"
		echo "1" echu "0" echx "1" echy "1" echz "1")
	  (while (and (not (eq resultat 0))(not (eq resultat 1)))
	    (affiche_dial)
	    (mode_tile "accept" 2)
	    (action_tile "listeo"   "(liste_choix $value)")
	    (action_tile "lister"   "(liste_sel $value)")
	    (action_tile "sel"      "(done_dialog 2)")
	    (action_tile "rech"     "(done_dialog 3)")
	    (action_tile "pick"     "(done_dialog 4)")
	    (action_tile "attr"     "(setq conserver_attr $value)")
	    (action_tile "dyna"     "(setq conserver_dyna $value)")
	    (action_tile "echori"   "(setq echo $value)(ech_u)")
	    (action_tile "fact_x"   "(verif_valeur \"x\" $value)")
	    (action_tile "fact_y"   "(verif_valeur \"y\" $value)")
	    (action_tile "fact_z"   "(verif_valeur \"z\" $value)")
	    (action_tile "uniforme" "(setq echu $value)(ech_u)")
	    (action_tile "cancel"   "(done_dialog 0)")
	    (action_tile "accept"   "(done_dialog 1)")
	    (setq resultat (start_dialog))
	    (cond
	      ((= resultat 1)
		(changer_blocs)
	      )
	      ((= resultat 2)
		(selection)
	      )
	      ((= resultat 3)
		(parcourir)
	      )
	      ((= resultat 4)
		(selection_ecran)
	      )
	    )
	  )
	  (unload_dialog dcl_id)
	)
	(msgbox "Rbloc" 48 "Pas de bloc dans le dessin")
      )
    )
    (msgbox "Rbloc" 16 "Le fichier RBLOC.DCL est introuvable.")
  )
  (setq *error* Old_Error)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)

(setq nom_lisp "RBLOC")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)