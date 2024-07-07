(defpackage db
  (:use :cl)
  (:import-from render
                parse-rgb-color
                rgb-to-integer)
  (:export list-terrs
           list-tiles
           init-db
           add-terr
           sync-tiles
           get-terr-by-name))

(in-package db)

(defparameter *db-path* "world.db")
(defparameter *area-def-table*
  "CREATE TABLE IF NOT EXISTS area_def (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  color INTEGER NOT NULL)")

(defparameter *area-table*
  "CREATE TABLE IF NOT EXISTS area (
  area_def_id INTEGER NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  FOREIGN KEY (area_def_id) REFERENCES area_def (id)
  UNIQUE(x, y)
)")
(defvar *db-lock* (bt:make-lock))

(defun drop-tables (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-non-query db "DROP TABLE area_def")
    (sqlite:execute-non-query db "DROP TABLE area")))
(defun init-tables (dbpath)
  (sqlite:with-open-database (db dbpath)
    (sqlite:execute-non-query db *area-def-table*)
    (sqlite:execute-non-query db *area-table*)))


(defun populate-terrs (dbpath as)
  (sqlite:with-open-database (db dbpath)
    (let ((statement (sqlite:prepare-statement db
                                               "INSERT INTO area_def (id, name, color) VALUES (?,?,?)")))
      (dolist (terr (mapcar #'cdr as))
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
    (sqlite:execute-non-query db *area-table*)))

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
 (drop-tables *db-path*)
 (reset-tiles *db-path*))
(init-tables *db-path*)
;;(populate-terrs *db-path*)
