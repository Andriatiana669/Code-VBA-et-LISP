; Written by Brian Strandberg, c3dk.com
; email: hello@c3dk.com
; Transforms coordinates from the X & Y in your drawing
; to Lattitude & Longitude
; then opens google street view at that point.
; code is not fully debugged
; requires a coordinate zone is assigned to your drawing.

(defun c:gsv (/)
(command "UCS" "World")
(if (not (getvar "CGOECS"))()(alert "fail - set coordinate zone first"))
(setq pkpt (getpoint "Select Point to see in Street View"))
(ade_projsetsrc (getvar "CGEOCS"))
(ade_projsetdest "LL84")
(setq cvp (list (car pkpt) (cadr pkpt)))
(setq result (ade_projptforward CVP))
(if (null result)
(alert "\nError in Transformation "))

(setq gsv (strcat "http://maps.google.com/maps?q=&layer=c&cbll=" (rtos (cadr result) 2 8) "," (rtos (car result) 2 8)))

(command "UCS" "P")
(command "browser" gsv)


); close defun