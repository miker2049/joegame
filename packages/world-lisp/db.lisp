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
    get-terr-by-name))

(in-package db)

(mito:connect-toplevel :sqlite3 :database-name *db-path*)

(mito:deftable terrain ()
  ((name :col-type :text)
    (color :col-type :integer))
  (:unique-keys name))

(mito:deftable terrain-tile ()
  ((x :col-type :integer)
    (y :col-type :integer)
    (alt :col-type :integer)
    (terrain :col-type terrain))
  (:unique-keys (x y alt)))


(defun ensure-tables ()
  (mapcar #'mito:ensure-table-exists '(terrain terrain-tile)))


(defun insert-terrain (name color)
  (mito:insert-dao
    (make-instance 'terrain :name name :color color )))

(defun insert-terrain-tile (x y alt ))


(defun make-terrain (name color)
  (make-instance 'terrain :name name :color color))

(defun name-from-sym (n)
  (remove-if
    #'(lambda (c) (eq c #\_))
    (string-downcase(symbol-name n))))


(defmacro defterrain (name color)
  `(let ((instance (make-terrain (name-from-sym ,name) ,color)))
     (progn
       (defconstant ,name instance)
       (mito:save-dao instance))))


(defun add-terr (terrain x y alt)
  (let ((curr (mito:find-dao 'terrain-tile :x x :y y :alt alt)))
    (if curr
      (progn
        (setf (slot-value curr 'terrain) terrain)
        (mito:save-dao curr))
      (mito:create-dao
        'terrain-tile :x x :y y :alt alt :terrain terrain))))

(defun list-terrs ()
  (mito:select-dao 'terrain))

(defun get-terr-by-name (name)
  (mito:find-dao 'terrain :name name))

(defun list-tiles ()
  (mito:select-dao 'terrain-tile))

(ensure-tables)

;; terrains
(defun init-db ()
  "Clear terrain and tiles, and repopulate
terrain with config terrains."
  (mito:delete-by-values 'terrain-tile)
  (mito:delete-by-values 'terrain)
  (dolist (item config:*terrain-set*)
    (mito:insert-dao (make-terrain (car item) (cadr item)))))
