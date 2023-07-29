(defpackage data
  (:use :cl :jonathan)
  (:export *world-data*))
(in-package data)
(defvar *world-data* nil
  "A data structure containing mapobject..")

;; (with-open-file (s "_data.lisp" :if-does-not-exist :create :direction :output :if-exists :overwrite)
;;   (format s "~W"
;;     (jojo:parse  (uiop:read-file-string #P"/home/mik/joegame/assets/data.json"))))

(with-open-file (s "_data.lisp" :direction :input)
  (let ((d (read s)))
    (setf *world-data* d)))
