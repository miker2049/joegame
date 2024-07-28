(defpackage worldtiles
  (:use :cl)
  (:export world-tile))

(in-package worldtiles)

(defun world-tile (x y z)
  (declare (type integer x y z))
  (let ((tz (expt 2 z)))
    (worldconf:make-world-image-scaled worldconf:*worldconf* 256 256 (* 1/256 tz)
                                       (mod x tz) (mod y tz))))
