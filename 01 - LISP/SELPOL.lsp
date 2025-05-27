(defun c:SELPOL (/ add ss i e l d g lst s n)
 ;;------------------------------------;;
 ;;	Tharwat 15.09.2015		;;
 ;; Select blocks that are touching	;;
 ;; the selected LWpolylines		;;
 ;;------------------------------------;;
 (princ "\nSelect LWPolyines :")
 (if (setq add (ssadd)
           ss  (ssget '((0 . "LWPOLYLINE")))
     )
   (repeat (setq i (sslength ss))
     (setq e   (ssname ss (setq i (1- i)))
           l   (vlax-curve-getdistatpoint e (vlax-curve-getendpoint e))
           d   (/ l 300.)
           g   d
           lst nil
     )
     (repeat 300
       (setq lst (cons (vlax-curve-getpointatdist e g) lst)
             g   (+ g d)
       )
     )
     (setq lst (append lst (list (vlax-curve-getstartpoint e))))
     (if (setq s (ssget "_F" lst '((0 . "INSERT"))))
       (repeat (setq n (sslength s))
         (ssadd (ssname s (setq n (1- n))) add)
       )
     )
   )
 )
 (sssetfirst nil add)
 (princ)
)