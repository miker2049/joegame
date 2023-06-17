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
(defun main ()
  (dump-csv 0 0 4000 4000 :threads 8)
  ;; (dbsync)
  ;; (render-big-img finalconf 4000 4000 "outi.png")
  )
