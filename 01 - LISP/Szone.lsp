(vl-load-com)

;; Select window poly
(defun c:SSZONE (/)
 (SelPoly "_WP")
 (princ)
)

;; Select crossing poly
(defun c:SelCP (/)
 (SelPoly "_CP")
 (princ)
)

;; Select objects inside a polyline
(defun SelPoly (WC / pSS ss ss1 n m en)
 (if (and
       (progn
         (prompt "\nSelect polylines to form WPoly borders: ")
         (setq pSS (ssget '((0 . "LWPOLYLINE"))))
       )
       (setq n (sslength pSS))
       (setq ss (ssadd))
     )
   (while (> (setq n (1- n)) -1)
     (if (and
           (setq ss1 (ssget WC (mapcar 'cdr (vl-remove-if-not '(lambda (item) (= (car item) 10)) (entget (ssname pSS n))))))
           (setq m (sslength ss1))
         )
       (while (> (setq m (1- m)) -1)
         (setq en (ssname ss1 m))
         (if (not (ssmemb en pSS)) (ssadd en ss))
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