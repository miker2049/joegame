(defpackage worldtiles
  (:use :cl)
  (:export world-tile))

(in-package worldtiles)

;; (declaim (optimize (speed 0) (space 0) (debug 3)))
(defun world-tile (x y z)
  (declare (type integer x y z))
  (let ((tz (expt 2 z)))
    (print z)
    (worldconf:make-world-image-scaled worldconf:*worldconf* 256 256 (* 1/256 tz)
                                       (* 256 (mod x tz))
                                       (* 256 (mod y tz)))))
