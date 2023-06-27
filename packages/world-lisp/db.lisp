(defpackage db
  (:use :cl)
  (:import-from render
    parse-rgb-color
    rgb-to-integer)
  (:import-from config
    *db-path*)
  (:export list-terrs
    list-tiles
    init-db
    add-terr
    sync-tiles
    get-terr-by-name))

(in-package db)

(defvar *db-lock* (bt:make-lock))

(defun drop-tables (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-non-query db "DROP TABLE area_def")
    (sqlite:execute-non-query db "DROP TABLE area")))
(defun init-tables (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-non-query db config:*area-def-table*)
    (sqlite:execute-non-query db config:*area-table*)))


(defun populate-terrs (dbpath)
  (sqlite:with-open-database (db dbpath)
    (let ((statement (sqlite:prepare-statement db
                       "INSERT INTO area_def (id, name, color) VALUES (?,?,?)")))
      (dolist (terr (mapcar #'cdr config:*area-set*))
        (sqlite:reset-statement statement)
        (sqlite:bind-parameter statement 1 (getf terr :id))
        (sqlite:bind-parameter statement 2 (getf terr :name))
        (sqlite:bind-parameter statement 3 (getf terr :color))
        (bt:acquire-lock *db-lock*)
        (sqlite:step-statement statement)
        (bt:release-lock *db-lock*))
      (sqlite:finalize-statement statement))))


(defun quad-insert-statement (db)
  (sqlite:prepare-statement db
    "INSERT INTO area (area_def_id, x, y) VALUES (?, ?, ?)"))
(defun step-quad-stmt (stmt terr x y)
  (sqlite:reset-statement stmt)
  (sqlite:bind-parameter stmt 1 terr)
  (sqlite:bind-parameter stmt 2 x)
  (sqlite:bind-parameter stmt 3 y)
  (bt:acquire-lock *db-lock*)
  (sqlite:step-statement stmt)
  (bt:release-lock *db-lock*))

(defun reset-areas (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-non-query db "DROP TABLE area")
    (sqlite:execute-non-query db config:*area-table*)))

;; (defmacro sync-tiles (dbpath iterator &rest iter-args)
;;   `(sqlite:with-open-database (db ,dbpath)
;;      (let ((stmt (quad-insert-statement db)))
;;        (,iterator ,@iter-args
;;          #'(lambda (terr x y alt)
;;              (step-quad-stmt stmt terr x y alt)))
;;        (sqlite:finalize-statement stmt))
;;      (print "howdy")))


(defun area-count (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-single db "SELECT count(*) from area;")))


(ignore-errors
  (drop-tables config:*db-path*)
  (reset-tiles config:*db-path*))
(init-tables config:*db-path*)
(populate-terrs config:*db-path*)
