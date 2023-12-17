(in-package :cl-user)
(defpackage server.asset-db
  (:use :cl :server.db :sxql :datafly)

  (:import-from server.config
                config)
  (:export :random-image :images :insert-images-from-dir
   :new-source :sources
   :insert-images :table-count :image-data :image-name
   :set-meta :image-id
   :image-meta))
(in-package :server.asset-db)


(defun get-db-path ()
  (cadddr (assoc :maindb (config :databases))))

(defun images (&optional q)
  (with-connection (db)
    (retrieve-all
     (select (:name :hash)
       (from :images)
       (where (:like :name (if q
                               (utils:fmt "%~a%" q)
                               "%")))
       (limit 1000)))))

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

(defun -image-name (hash)
  (retrieve-one-value
   (select :name
     (from :images)
     (where (:= hash :hash)))))

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
  (retrieve-one
   (select :*
     (from :imagesmeta)
     (where (:= id :id)))))

(defun image-meta (hash)
  (with-connection (db)
    (let* ((id (-image-id hash))
           (name (-image-name hash))
           (out (-image-meta id)))
      (setf (getf out :hash) hash)
      (setf (getf out :name) name)
      (setf (getf out :id) id)
      out)))

;; (image-meta (getf (random-image) :hash))

(defun table-count (tb)
  (with-connection (db)
    (retrieve-one-value
     (select ((:count :*))
       (from tb)))))

(defun add-image-from-file (file data hash)
  (with-connection (db)
    (let* ((query (dbi:prepare *connection*
                               "INSERT INTO images (data, name, hash) VALUES (?,?,?) ON CONFLICT DO NOTHING RETURNING id"))
           (query (dbi:execute query (list data file hash))))

      (getf (dbi:fetch query) :|id|))))


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
                             (add-image-from-file (file-namestring file) data hash))
                         file))))



(defun -insert-image-meta-default-statement (id w h &key source)
  "Needs to be within with-connection.  Makes framesizes same as image size"
  (insert-into :imagesmeta
    (set=
     :id id
     :width w
     :height h
     :source source
     :framewidth w
     :frameheight h
     :columns 1
     :tilecount 1
     :spacing 0
     :margin 0)
    (on-conflict-do-nothing)))

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


(defmacro -set-meta (id opts)
  `(update :imagesmeta
     (set= ,@opts)
     (where (:= :id ,id))))


(defmacro set-meta (id opts)
  `(with-connection (db)
     (execute
      (-set-meta ,id ,opts))))


;; (insert-images-from-dir "/home/mik/joegame/assets/images/")


(defun -new-source (name website)
  "Needs to be within with-connection.  Makes framesizes same as image size"
  (insert-into :sources
    (set=
     :name name
     :website website)
    (returning :id)))

(defun new-source (name website)
  (with-connection (db)
    (retrieve-one-value
     (-new-source name website))))

(defun sources ()
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :sources)))))
