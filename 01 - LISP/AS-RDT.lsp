(defun c:AS-RDT (/ ss pline obj userText offsetDistance textHeight layerName textLayer n totalLength dist step pt deriv ang side startDeriv count offsetPt)
  ;; Paramètres fixes
  (setq offsetDistance 0.25)   ; Décalage vertical
  (setq textHeight 0.16)       ; Hauteur du texte
  (setvar "LTSCALE" 0.5)       ; Echelle du type de ligne
  
  ;; Saisie utilisateur
  (setq userText (getstring T "\nEntrez le texte à insérer : "))
  
  ;; Sélection des polylignes
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  
  (cond
    (ss
     (repeat (setq n (sslength ss))
       (setq pline (ssname ss (setq n (1- n))))
       (setq obj (vlax-ename->vla-object pline))
       
       (if (eq (vla-get-ObjectName obj) "AcDbPolyline")
         (progn
           ;; Gestion des calques
           (setq layerName (cdr (assoc 8 (entget pline))))
           (setq textLayer
             (cond
	       ;; Cas pour ECA_RSX_AEP
               ((member layerName '("ECA_RSX_AEP-ABD" "ECA_RSX_AEP-Classe A" "ECA_RSX_AEP-Classe B" "ECA_RSX_AEP-Classe C" "ECA_RSX_AEP.AERIEN" "ECA_RSX_AEP.DICT" "ECA_RSX_AEP.EQUI"))
                "ECA_RSX_AEP.TXT")
	       ;; Cas pour ECA_RSX_AEG
               ((member layerName '("ECA_RSX_AEG-ABD" "ECA_RSX_AEG-Classe A" "ECA_RSX_AEG-Classe B" "ECA_RSX_AEG-Classe C" "ECA_RSX_AEG.AERIEN" "ECA_RSX_AEG.DICT" "ECA_RSX_AEG.EQUI"))
               "ECA_RSX_AEG.TXT")
               ;; Cas pour ECA_RSX_ARO
               ((member layerName '("ECA_RSX_ARO-ABD" "ECA_RSX_ARO-Classe A" "ECA_RSX_ARO-Classe B" "ECA_RSX_ARO-Classe C" "ECA_RSX_ARO.AERIEN" "ECA_RSX_ARO.DICT" "ECA_RSX_ARO.EQUI"))
               "ECA_RSX_ARO.TXT")
               ;; Cas pour ECA_RSX_BEL
               ((member layerName '("ECA_RSX_BEL-ABD" "ECA_RSX_BEL-Classe A" "ECA_RSX_BEL-Classe B" "ECA_RSX_BEL-Classe C" "ECA_RSX_BEL.AERIEN" "ECA_RSX_BEL.DICT" "ECA_RSX_BEL.EQUI"))
               "ECA_RSX_BEL.TXT")
               ;; Cas pour ECA_RSX_BTA
               ((member layerName '("ECA_RSX_BTA-ABD" "ECA_RSX_BTA-Classe A" "ECA_RSX_BTA-Classe B" "ECA_RSX_BTA-Classe C" "ECA_RSX_BTA.AERIEN" "ECA_RSX_BTA.DICT" "ECA_RSX_BTA.EQUI"))
               "ECA_RSX_BTA.TXT")
               ;; Cas pour ECA_RSX_CFA
               ((member layerName '("ECA_RSX_CFA-ABD" "ECA_RSX_CFA-Classe A" "ECA_RSX_CFA-Classe B" "ECA_RSX_CFA-Classe C" "ECA_RSX_CFA.AERIEN" "ECA_RSX_CFA.DICT" "ECA_RSX_CFA.EQUI"))
               "ECA_RSX_CFA.TXT")
               ;; Cas pour ECA_RSX_CHI
               ((member layerName '("ECA_RSX_CHI-ABD" "ECA_RSX_CHI-Classe A" "ECA_RSX_CHI-Classe B" "ECA_RSX_CHI-Classe C" "ECA_RSX_CHI.AERIEN" "ECA_RSX_CHI.DICT" "ECA_RSX_CHI.EQUI"))
               "ECA_RSX_CHI.TXT")
               ;; Cas pour ECA_RSX_CHU
               ((member layerName '("ECA_RSX_CHU-ABD" "ECA_RSX_CHU-Classe A" "ECA_RSX_CHU-Classe B" "ECA_RSX_CHU-Classe C" "ECA_RSX_CHU.AERIEN" "ECA_RSX_CHU.DICT" "ECA_RSX_CHU.EQUI"))
               "ECA_RSX_CHU.TXT")
               ;; Cas pour ECA_RSX_ECL
               ((member layerName '("ECA_RSX_ECL-ABD" "ECA_RSX_ECL-Classe A" "ECA_RSX_ECL-Classe B" "ECA_RSX_ECL-Classe C" "ECA_RSX_ECL.AERIEN" "ECA_RSX_ECL.DICT" "ECA_RSX_ECL.EQUI"))
               "ECA_RSX_ECL.TXT")
               ;; Cas pour ECA_RSX_EPL
               ((member layerName '("ECA_RSX_EPL-ABD" "ECA_RSX_EPL-Classe A" "ECA_RSX_EPL-Classe B" "ECA_RSX_EPL-Classe C" "ECA_RSX_EPL.AERIEN" "ECA_RSX_EPL.DICT" "ECA_RSX_EPL.EQUI"))
               "ECA_RSX_EPL.TXT")
               ;; Cas pour ECA_RSX_EUS
               ((member layerName '("ECA_RSX_EUS-ABD" "ECA_RSX_EUS-Classe A" "ECA_RSX_EUS-Classe B" "ECA_RSX_EUS-Classe C" "ECA_RSX_EUS.AERIEN" "ECA_RSX_EUS.DICT" "ECA_RSX_EUS.EQUI"))
               "ECA_RSX_EUS.TXT")
               ;; Cas pour ECA_RSX_FOP
               ((member layerName '("ECA_RSX_FOP-ABD" "ECA_RSX_FOP-Classe A" "ECA_RSX_FOP-Classe B" "ECA_RSX_FOP-Classe C" "ECA_RSX_FOP.AERIEN" "ECA_RSX_FOP.DICT" "ECA_RSX_FOP.EQUI"))
               "ECA_RSX_FOP.TXT")
               ;; Cas pour ECA_RSX_FRV
               ((member layerName '("ECA_RSX_FRV-ABD" "ECA_RSX_FRV-Classe A" "ECA_RSX_FRV-Classe B" "ECA_RSX_FRV-Classe C" "ECA_RSX_FRV.AERIEN" "ECA_RSX_FRV.DICT" "ECA_RSX_FRV.EQUI"))
               "ECA_RSX_FRV.TXT")
               ;; Cas pour ECA_RSX_GAZ
               ((member layerName '("ECA_RSX_GAZ-ABD" "ECA_RSX_GAZ-Classe A" "ECA_RSX_GAZ-Classe B" "ECA_RSX_GAZ-Classe C" "ECA_RSX_GAZ.AERIEN" "ECA_RSX_GAZ.DICT" "ECA_RSX_GAZ.EQUI"))
               "ECA_RSX_GAZ.TXT")
               ;; Cas pour ECA_RSX_HT5.5KV
               ((member layerName '("ECA_RSX_HT5.5KV-ABD" "ECA_RSX_HT5.5KV-Classe A" "ECA_RSX_HT5.5KV-Classe B" "ECA_RSX_HT5.5KV-Classe C" "ECA_RSX_HT5.5KV.AERIEN" "ECA_RSX_HT5.5KV.DICT" "ECA_RSX_HT5.5KV.EQUI"))
               "ECA_RSX_HT5.5KV.TXT")
               ;; Cas pour ECA_RSX_HT20KV
               ((member layerName '("ECA_RSX_HT20KV-ABD" "ECA_RSX_HT20KV-Classe A" "ECA_RSX_HT20KV-Classe B" "ECA_RSX_HT20KV-Classe C" "ECA_RSX_HT20KV.AERIEN" "ECA_RSX_HT20KV.DICT" "ECA_RSX_HT20KV.EQUI"))
               "ECA_RSX_HT20KV.TXT")
               ;; Cas pour ECA_RSX_HTA
               ((member layerName '("ECA_RSX_HTA-ABD" "ECA_RSX_HTA-Classe A" "ECA_RSX_HTA-Classe B" "ECA_RSX_HTA-Classe C" "ECA_RSX_HTA.AERIEN" "ECA_RSX_HTA.DICT" "ECA_RSX_HTA.EQUI"))
               "ECA_RSX_HTA.TXT")
               ;; Cas pour ECA_RSX_HTB
               ((member layerName '("ECA_RSX_HTB-ABD" "ECA_RSX_HTB-Classe A" "ECA_RSX_HTB-Classe B" "ECA_RSX_HTB-Classe C" "ECA_RSX_HTB.AERIEN" "ECA_RSX_HTB.DICT" "ECA_RSX_HTB.EQUI"))
               "ECA_RSX_HTB.TXT")
               ;; Cas pour ECA_RSX_HYD
               ((member layerName '("ECA_RSX_HYD-ABD" "ECA_RSX_HYD-Classe A" "ECA_RSX_HYD-Classe B" "ECA_RSX_HYD-Classe C" "ECA_RSX_HYD.AERIEN" "ECA_RSX_HYD.DICT" "ECA_RSX_HYD.EQUI"))
               "ECA_RSX_HYD.TXT")
               ;; Cas pour ECA_RSX_INC
               ((member layerName '("ECA_RSX_INC-ABD" "ECA_RSX_INC-Classe A" "ECA_RSX_INC-Classe B" "ECA_RSX_INC-Classe C" "ECA_RSX_INC.AERIEN" "ECA_RSX_INC.DICT" "ECA_RSX_INC.EQUI"))
               "ECA_RSX_INC.TXT")
               ;; Cas pour ECA_RSX_MLT
               ((member layerName '("ECA_RSX_MLT-ABD" "ECA_RSX_MLT-Classe A" "ECA_RSX_MLT-Classe B" "ECA_RSX_MLT-Classe C" "ECA_RSX_MLT.AERIEN" "ECA_RSX_MLT.DICT" "ECA_RSX_MLT.EQUI"))
               "ECA_RSX_MLT.TXT")
               ;; Cas pour ECA_RSX_NDE
               ((member layerName '("ECA_RSX_NDE-ABD" "ECA_RSX_NDE-Classe A" "ECA_RSX_NDE-Classe B" "ECA_RSX_NDE-Classe C" "ECA_RSX_NDE.AERIEN" "ECA_RSX_NDE.DICT" "ECA_RSX_NDE.EQUI"))
               "ECA_RSX_NDE.TXT")
               ;; Cas pour ECA_RSX_SGN
               ((member layerName '("ECA_RSX_SGN-ABD" "ECA_RSX_SGN-Classe A" "ECA_RSX_SGN-Classe B" "ECA_RSX_SGN-Classe C" "ECA_RSX_SGN.AERIEN" "ECA_RSX_SGN.DICT" "ECA_RSX_SGN.EQUI"))
               "ECA_RSX_SGN.TXT")
               ;; Cas pour ECA_RSX_TCC
               ((member layerName '("ECA_RSX_TCC-ABD" "ECA_RSX_TCC-Classe A" "ECA_RSX_TCC-Classe B" "ECA_RSX_TCC-Classe C" "ECA_RSX_TCC.AERIEN" "ECA_RSX_TCC.DICT" "ECA_RSX_TCC.EQUI"))
               "ECA_RSX_TCC.TXT")
               ;; Cas pour ECA_RSX_TEL
               ((member layerName '("ECA_RSX_TEL-ABD" "ECA_RSX_TEL-Classe A" "ECA_RSX_TEL-Classe B" "ECA_RSX_TEL-Classe C" "ECA_RSX_TEL.AERIEN" "ECA_RSX_TEL.DICT" "ECA_RSX_TEL.EQUI"))
               "ECA_RSX_TEL.TXT")
               ;; Cas pour ECA_RSX_UNI
               ((member layerName '("ECA_RSX_UNI-ABD" "ECA_RSX_UNI-Classe A" "ECA_RSX_UNI-Classe B" "ECA_RSX_UNI-Classe C" "ECA_RSX_UNI.AERIEN" "ECA_RSX_UNI.DICT" "ECA_RSX_UNI.EQUI"))
               "ECA_RSX_UNI.TXT")
               ;; Cas pour ECA_RSX_VID
               ((member layerName '("ECA_RSX_VID-ABD" "ECA_RSX_VID-Classe A" "ECA_RSX_VID-Classe B" "ECA_RSX_VID-Classe C" "ECA_RSX_VID.AERIEN" "ECA_RSX_VID.DICT" "ECA_RSX_VID.EQUI"))
               "ECA_RSX_VID.TXT")
               ;; Par défaut, utiliser le calque de la polyligne
               (T layerName)
             )
           )
           (setvar "CLAYER" textLayer)
           
           ;; Calcul de la longueur totale
           (setq totalLength (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
           
           ;; Détermination du côté pour le décalage (toujours au-dessus)
           (setq startDeriv (vlax-curve-getFirstDeriv obj (vlax-curve-getStartParam obj)))
           (setq side (if (minusp (car startDeriv)) -1 1))
           
           (cond
             ;; Cas 1: Longueur <= 50 unités - 1 texte au milieu
             ((<= totalLength 50.0)
              (setq midPoint (vlax-curve-getPointAtDist obj (/ totalLength 2.0)))
              (setq deriv (vlax-curve-getFirstDeriv obj (vlax-curve-getParamAtPoint obj midPoint)))
              (setq ang (atan (cadr deriv) (car deriv)))
              (setq pt (polar midPoint (+ ang (/ pi 2)) (* side offsetDistance)))
              
              (entmake (list '(0 . "TEXT")
                            (cons 10 pt)
                            (cons 40 textHeight)
                            (cons 1 userText)
                            (cons 50 ang)
                            (cons 7 (getvar "TEXTSTYLE"))
                            (cons 8 textLayer)
                            (cons 72 1)
                            (cons 11 pt)
                            (cons 73 2)
              ))
              (princ (strcat "\nTexte ajouté au milieu (" (rtos totalLength 2 2) " unités)"))
             )
             
             ;; Cas 2: Longueur > 50 unités - texte tous les 50 unités (sans début et fin)
             ((> totalLength 50.0)
              (setq step 50.0)
              (setq count 0)
              
              ;; Commencer à 25m et finir à (totalLength-25)
              (setq dist 25.0)
              (while (<= dist (- totalLength 25.0))
                (setq pt (vlax-curve-getPointAtDist obj dist))
                (if pt
                  (progn
                    (setq deriv (vlax-curve-getFirstDeriv obj (vlax-curve-getParamAtPoint obj pt)))
                    (setq ang (atan (cadr deriv) (car deriv)))
                    (setq offsetPt (polar pt (+ ang (/ pi 2)) (* side offsetDistance)))
                    
                    (entmake (list '(0 . "TEXT")
                                  (cons 10 offsetPt)
                                  (cons 40 textHeight)
                                  (cons 1 userText)
                                  (cons 50 ang)
                                  (cons 7 (getvar "TEXTSTYLE"))
                                  (cons 8 textLayer)
                                  (cons 72 1)
                                  (cons 11 offsetPt)
                                  (cons 73 2)
                    ))
                    (setq count (1+ count))
                  )
                )
                (setq dist (+ dist step))
              )
              (princ (strcat "\n" (itoa count) " textes ajoutés (" (rtos totalLength 2 2) " unités)"))
             )
           )
         )
       )
     )
    )
    (T (princ "\nAucune polyligne sélectionnée."))
  )
  (princ)
)