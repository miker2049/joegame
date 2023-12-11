(in-package :cl-user)
(defpackage server.asset-db
  (:use :cl :server.db :sxql :datafly)

  (:import-from server.config
                config)
  (:export :random-image :images :insert-images-from-dir
   :insert-images :table-count :image-data :image-name))
(in-package :server.asset-db)


(defun get-db-path ()
  (cadddr (assoc :maindb (config :databases))))

(defun images ()
  (with-connection (db)
    (retrieve-all
     (select (:name :hash)
       (from :images)
       (limit 100)))))

;; (defun add-image-from-file (file)
;;   (let ((data
;;           (alexandria:read-file-into-byte-vector
;;            file)))
;;     (with-connection (db)
;;       (execute
;;        (insert-into :images
;;          (:data :name :hash)
;;          (list
;;           data
;;           (file-namestring file)
;;           (utils:sha256 data)))))))





(defun random-image ()
  (with-connection (db)
    (retrieve-one
     (select (:name :hash :data)
       (from :images)
       (limit 1)
       (order-by (:random))))))

;; (getf
;;  (random-image) :data)

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

(defun -image-id (hash)
  (retrieve-one-value
   (select :id
     (from :images)
     (where (:= hash :hash)))))

(defun image-id (hash)
  (with-connection (db)
    (-image-id hash)))

(defun -image-meta (id)
  (retrieve-one-value
   (select :*
     (from :imagesmeta)
     (where (:= id :id)))))

(defun image-meta (hash)
  (with-connection (db)
    (retrieve-one-value
     (-image-meta (-image-id hash)))))



(defun table-count (tb)
  (with-connection (db)
    (retrieve-one-value
     (select ((:count :*))
       (from tb)))))

(defun add-image-from-file (file data hash)
  (sqlite:with-open-database (db (get-db-path))
    (let ((statement
            (sqlite:prepare-statement
             db
             "INSERT INTO images (data, name, hash) VALUES (?,?,?) ON CONFLICT DO NOTHING RETURNING id")))
      (sqlite:reset-statement statement)
      (sqlite:bind-parameter statement 1 data)
      (sqlite:bind-parameter statement 2 (file-namestring file))
      (sqlite:bind-parameter statement 3 hash)
      (sqlite:step-statement statement)
      (sqlite:finalize-statement statement))
    (sqlite:last-insert-rowid db)))


()
(defun -add-image-from-files (files)
  (sqlite:with-open-database (db (get-db-path) :busy-timeout 4000)
    (let (out
          (statement
            (sqlite:prepare-statement
             db
             "INSERT INTO images (data, name, hash) VALUES (?,?,?) ON CONFLICT DO NOTHING RETURNING id")))
      (setf out
            (loop for  file in files
                  :collect (let* ((data
                                    (alexandria:read-file-into-byte-vector
                                     file))
                                  (hash (utils:sha256 data))
                                  (id (image-id hash)))
                             (if id id
                                 (progn
                                   (sqlite:reset-statement statement)
                                   (sqlite:bind-parameter statement 1 data)
                                   (sqlite:bind-parameter statement 2 (file-namestring file))
                                   (sqlite:bind-parameter statement 3 hash)
                                   (sqlite:step-statement statement)
                                   (sqlite:last-insert-rowid db))))))
      (sqlite:finalize-statement statement)
      (mapcar #'cons out files))))

(defun --add-image-from-files (files)
  (loop for  file in files
        :collect (let* ((data
                          (alexandria:read-file-into-byte-vector
                           file))
                        (hash (utils:sha256 data))
                        (id (image-id hash)))
                   (cons (if id id
                             (add-image-from-file file data hash))
                         file))))

(defun -insert-image-meta-default-statement (id w h)
  "Needs to be within with-connection.  Makes framesizes same as image size"
  (insert-into :imagesmeta
    (set=
     :id id
     :width w
     :height h
     :framewidth w
     :frameheight h
     :columns 1
     :tilecount 1
     :spacing 0
     :margin 0)
    (on-conflict-do-update '(:id)
                           (set=
                            :width w
                            :height h))))

(defun -init-image-meta-from-file (id file)
  (let ((dimensions (magicklib:image-dimensions file)))
    (with-connection (db)
      (execute
       (-insert-image-meta-default-statement
        id (first dimensions) (second dimensions))))))


(defun insert-images (files)
  (loop for (id . filename)
          in
          (--add-image-from-files files)
        :collect (progn
                   (-init-image-meta-from-file  id filename)
                   id)))

(defun insert-images-from-dir (dir)
  (insert-images
   (utils:find-files dir "*.png")))

(insert-images-from-dir "/home/mik/joegame/assets/images/")
