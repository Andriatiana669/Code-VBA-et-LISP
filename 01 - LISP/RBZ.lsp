;;;Taper RBZ pour utiliser


(defun c:RBZ (/ ss n lst)

 (if (setq ss (ssget '((0 . "INSERT"))))

   (progn

     (setq n -1)

     (while (setq pt (ssname ss (setq n (1+ n))))

       (setq lst (cons (cdr (assoc 10 (entget pt))) lst))

     )

     (command "_.pline")

     (mapcar '(lambda (x) (command "_non" x)) lst)

     (command)

   )

 )

 (princ)

)