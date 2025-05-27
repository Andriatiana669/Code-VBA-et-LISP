(vl-load-com)

;; Select window poly - modified to select only on the polyline, works with 2D and 3D polylines
(defun c:MILS (/)
  (SelPolyOn "_WP")
  (princ)
)

;; Function to select objects on polyline vertices (2D and 3D)
(defun SelPolyOn (WC / pSS ss ss1 n m en ptList pline entData tolerance)
  (setq tolerance 0) ; Définir la tolérance
  (if (and
        (progn
          (prompt "\nSelect polylines to form selection borders: ")
          (setq pSS (ssget '((0 . "LWPOLYLINE,POLYLINE")))) ; Include both 2D and 3D polylines
        )
        (setq n (sslength pSS))
        (setq ss (ssadd))
      )
    (while (> (setq n (1- n)) -1)
      (setq pline (ssname pSS n))
      (setq entData (entget pline))
      (cond
        ((= (cdr (assoc 0 entData)) "LWPOLYLINE") ; 2D Polyline
         (setq ptList (mapcar 'cdr (vl-remove-if-not '(lambda (item) (= (car item) 10)) entData)))
        )
        ((= (cdr (assoc 0 entData)) "POLYLINE") ; 3D Polyline
         (setq ptList (get_3d_polyline_vertices pline))
        )
      )
      (foreach pt ptList
        ;; Sélectionner les objets dans une zone autour du point avec la tolérance
        (setq ss1 (ssget "_C" (mapcar '- pt (list tolerance tolerance)) (mapcar '+ pt (list tolerance tolerance))))
        (if ss1
          (progn
            (setq m (sslength ss1))
            (while (> (setq m (1- m)) -1)
              (setq en (ssname ss1 m))
              ;; Vérifier si l'objet est proche du point avec la tolérance
              (if (and (not (ssmemb en pSS)) (not (ssmemb en ss))
                      (<= (distance pt (cdr (assoc 10 (entget en)))) tolerance))
                (ssadd en ss)
              )
            )
          )
        )
      )
      (setq ss1 nil)
      (gc)
    )
  )
  (if (and ss (> (sslength ss) 0))
    (sssetfirst nil ss)
  )
)

;; Helper function to get vertices from a 3D polyline
(defun get_3d_polyline_vertices (pline / ent vertexList)
  (setq vertexList nil)
  (setq ent (entnext pline)) ; Get the first vertex entity
  (while (and ent (/= (cdr (assoc 0 (entget ent))) "SEQEND"))
    (if (= (cdr (assoc 0 (entget ent))) "VERTEX")
      (setq vertexList (cons (cdr (assoc 10 (entget ent))) vertexList))
    )
    (setq ent (entnext ent)) ; Move to the next entity
  )
  (reverse vertexList)
)