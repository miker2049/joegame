(defpackage world
  (:use :cl)
  (:import-from worldconf
    dump-csv
    render-big-img
    finalconf
    dbsync
    run)
  (:export main))

(in-package world)

(setf cl-progress-bar:*progress-bar-enabled* t)
(defparameter *map-n* 2000)
(defun main ()
  ;; (dump-csv 0 0 8000 8000 :threads 8)
  ;; (dbsync)
  (render-big-img finalconf 2000 2000 "outi.png" :threads 16 :xoff 250 :yoff 250)

  (sb-ext:exit)
  )

;; (render-big-img finalconf 2000 2000 "outi.png" :threads 16 :xoff 250 :yoff 250)



(defun debug-big-img ()
  (progn
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 250)
    (print "one")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 375)
    (print "two")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 500)
    (print "3")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 625)
    (print "4")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 750)
    (print "5")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 875)
    (print "6")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1000)
    (print "7")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1125)
    (print "8")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1250)
    (print "9")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1375)
    (print "10")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1500)
    (print "11")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1625)
    (print "12")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1750)
    (print "13")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 1875)
    (print "14")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 2000)
    (print "15")
    (worldconf::make-world-image-scaled finalconf 2000 125 1 250 2125)
    (print "16")))
