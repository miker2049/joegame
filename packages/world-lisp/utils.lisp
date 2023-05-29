(defpackage utils (:use :cl)
    (:export e-distance))
(in-package utils)
(defun e-distance (x1 y1 x2 y2)
    (sqrt
        (+
            (expt (- x2 x1) 2)
            (expt (- y2 y1) 2))))

