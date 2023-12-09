(in-package :cl-user)
(defpackage server.asset-db
  (:use :cl :server.db :sxql :datafly)
  (:export :images :table-count :image-data :image-name))
(in-package :server.asset-db)

(defun images ()
  (with-connection (db)
    (retrieve-all
     (select (:name :hash)
       (from :images)))))

(defun random-image ()
  (with-connection (db)
    (retrieve-one
     (select (:name :hash :data)
       (from :images)
       (limit 1)
       (order-by (:random))))))


(defun image-data (hash)
  (with-connection (db)
    (retrieve-one-value
     (select :data
       (from :images)
       (where (:= hash :hash))))))

(defun image-name (hash)
  (with-connection (db)
    (retrieve-one-value
     (select :name
       (from :images)
       (where (:= hash :hash))))))

(defun table-count (tb)
  (with-connection (db)
    (retrieve-one-value
     (select ((:count :*))
       (from tb)))))

