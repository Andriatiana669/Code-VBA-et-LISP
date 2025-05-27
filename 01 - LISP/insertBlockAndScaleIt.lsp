; --------------------------------------------------------------------------------------------------------------------
(defun print_pt(pt) (princ (strcat "\npt[ " (rtos (car pt)) ";" (rtos (cadr pt)) ";" (rtos (caddr pt)) "]")))
; --------------------------------------------------------------------------------------------------------------------
(defun vecTo(p1 p2)
    (setq dx (- (car p1) (car p2)))
    (setq dy (- (cadr p1) (cadr p2)))
    (list dx dy)
)

(defun rotateVector(v rotation)
    (setq x (car v))
    (setq y (cadr v))

    (setq newX (- (* x (cos rotation)) (* y (sin rotation))))
    (setq newY (+ (* x (sin rotation)) (* y (cos rotation))))

    (list newX newY)
)

; --------------------------------------------------------------------------------------------------------------------
(defun calculateScaleFactor (rotation ptOrigin ptRef ptDest)
    (setq vecRef (vecTo ptRef ptOrigin))
    (setq vecDest (vecTo ptDest ptOrigin))

    (setq vecRef (rotateVector vecRef (* -1 rotation)))
    (setq vecDest (rotateVector vecDest (* -1 rotation)))

    (setq scaleX (/ (car vecDest ) (car vecRef))
          scaleY (/ (cadr vecDest ) (cadr vecRef)))

    (if ( < scaleX 0) (setq scaleX (* -1 scaleX)))
    (if ( < scaleY 0) (setq scaleY (* -1 scaleY)))

    (list scaleX scaleY)
)

; --------------------------------------------------------------------------------------------------------------------
(defun c:InsertBlockWithScaleAndRotationInput ( / 
    doc *error* blkName data rotation 
    ptInsertion ptScaleRef ptScaleDest 
    scaleFactor oldX oldY oldZ)
    ; load vla function
    (vl-load-com)

    ; set the undo mark and an error if needed
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))  
    (vla-startundomark doc)  
    (defun *error* ( msg )
        (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
        (vla-endundomark doc)
        (exit)
    )
    
    ; the core program
    (setq blkName (getstring "\nEntrer le nom du bloc: "))
    (command "_.insert" blkName "E" 1 pause pause )

    (setq data (entget (entlast)))
    (setq rotation (cdr (assoc 50 data)))
    (setq ptInsertion (cdr (assoc 10 data)))
    (setq ptScaleRef (getpoint "\nIndiquez la taille réference: "))
    (setq ptScaleDest (getpoint "\nIndiquez la taille finale: "))
    (setq scaleFactor (calculateScaleFactor rotation ptInsertion ptScaleRef ptScaleDest))

    (setq oldX (cdr (assoc 41 data))
          oldY (cdr (assoc 42 data))
          oldZ (cdr (assoc 43 data)))

    (setq data (subst (cons 41 (* oldX (car scaleFactor))) (assoc 41 data) data))
    (setq data (subst (cons 42 (* oldX (cadr scaleFactor))) (assoc 42 data) data))
    (entmod data)
    (entupd ent)

    (vla-endundomark doc)
    (princ)
)
