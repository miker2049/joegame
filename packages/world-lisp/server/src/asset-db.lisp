(in-package :cl-user)
(defpackage server.asset-db
  (:use :cl :server.db :sxql :datafly)

  (:import-from config
                config)
  (:export
   random-image images insert-images-from-dir
   new-source sources
   insert-images table-count image-data image-name
   insert-frameanim update-frameanim
   set-meta image-id
   image-meta
   image-meta-from-id
   image-info
   get-tileset-tilemap
   insert-object update-object image-objects objects delete-object))
(in-package :server.asset-db)


(defun get-db-path ()
  (cadddr (assoc :maindb (config :databases))))

(defun table-count (tb)
  (with-connection (db)
    (retrieve-one-value
     (select ((:count :*))
       (from tb)))))

(defun table-column (table col)
  (intern
   (string-upcase
    (format nil "~a.~a"
            (symbol-name table)
            col))
   'keyword))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Images               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-connection (db)
  (execute
   (create-table (:image :if-not-exists t)
       ((id :type 'integer
            :primary-key t)
        (name :type 'text
              :not-null t)
        (hash :type 'text :unique t)
        (data :type 'blob))))
  (execute
   (create-table (:imagemeta :if-not-exists t)
       ((imageid :type 'integer :primary-key t)
        (source :type 'integer)
        (width :type 'integer)
        (height :type 'integer)
        (framewidth :type 'integer)
        (frameheight :type 'integer)
        (columns :type 'integer)
        (tilecount :type 'integer)
        (margin :type 'integer)
        (spacing :type 'integer))
     (foreign-key '(:source) :references '(:source :id))
     (foreign-key '(:imageid) :references '(:image :id))))
  (execute
   (create-table (:source :if-not-exists t)
       ((id :type 'integer
            :primary-key t)
        (name :type 'text
              :not-null t)
        (website :type 'text))))
  (execute
   (create-table (:object :if-not-exists t)
       ((id :type 'integer :primary-key t)
        (imageid :type 'integer)
        (name :type 'text :not-null t)
        (tiles :type 'text)
        (tileswidth :type 'integer))
     (foreign-key '(:imageid) :references '(:image :id))))
  (execute
   (create-table (:frameanim :if-not-exists t)
       ((id :type 'integer :primary-key t)
        (imageid :type 'integer)
        (frames :type 'text)
        (name :type 'text))
     (foreign-key '(:imageid) :references '(:image :id)))))


(defun images (&optional q)
  (with-connection (db)
    (retrieve-all
     (select (:name :hash)
       (from :image)
       (where (:like :name (if q
                               (utils:fmt "%~a%" q)
                               "%")))
       (limit 100)))))


(defun random-image ()
  (with-connection (db)
    (retrieve-one
     (select (:name :hash :data)
       (from :image)
       (limit 1)
       (order-by (:random))))))


(defun image-data (hash)
  (with-connection (db)
    (retrieve-one-value
     (select :data
       (from :image)
       (where (:= hash :hash))))))


(defun -image-name (hash)
  (select :name
    (from :image)
    (where (:= hash :hash))))

(defun -image-name-from-id (id)
  (retrieve-one-value
   (select :name
     (from :image)
     (where (:= id :id)))))

(defun image-name (hash)
  "With hash, retrieve an image name."
  (with-connection (db)
    (-image-name hash)))

(defun -image-id (hash)
  (retrieve-one-value
   (select :id
     (from :image)
     (where (:= hash :hash)))))

(defun -image-hash (id)
  (retrieve-one-value
   (select :hash
     (from :image)
     (where (:= id :id)))))

(defun image-id (hash)
  (with-connection (db)
    (-image-id hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              imagemeta              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (with-connection (db)
;;   (execute
;;    (eval
;;     `(create-table (,*image-meta-table* :if-not-exists t)
;;          ((imageid :type 'integer :primary-key t)
;;           (source :type 'integer)
;;           (width :type 'integer)
;;           (height :type 'integer)
;;           (framewidth :type 'integer)
;;           (frameheight :type 'integer)
;;           (columns :type 'integer)
;;           (tilecount :type 'integer)
;;           (margin :type 'integer)
;;           (spacing :type 'integer))
;;        (foreign-key '(:source) :references '(:source :id))
;;        (foreign-key '(:imageid) :references '(:image :id))))))

(defun -image-meta (id)
  (retrieve-one
   (select :*
     (from :imagemeta)
     (where (:= id :id)))))


(defun -image-full (id)
  (retrieve-one
   (select (:image.name :hash
            (:as (table-column :image "id") :imageid)
            :width :height
            :framewidth :frameheight
            :columns :tilecount
            :spacing :margin
            (:as :source.name :source-name)
            (:as :source.website :source-website))
     (from :imagemeta)
     (where (:= id :image.id))
     (inner-join :image :on (:= :image.id id))
     (inner-join :source :on (:= :imagemeta.source :source.id)))))

(with-connection (db)
  (-image-full 123))

;; (set-meta 123 (:spacing 10))


(defun image-meta (inp)
  "If inp is string, assume hash; integer, id"
  (with-connection (db)
    (let* ((id (if (stringp inp)
                   (-image-id inp)
                   inp)))
      (-image-full id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          images from files          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun -add-image-from-file (file data hash)
  (with-connection (db)
    (let* ((query (dbi:prepare *connection*
                               "INSERT INTO image (data, name, hash) VALUES (?,?,?) ON CONFLICT DO NOTHING RETURNING id"))
           (query (dbi:execute query (list data file hash))))

      (getf (dbi:fetch query) :|id|))))

(defun add-image-from-file (file)
  (let* ((data
           (alexandria:read-file-into-byte-vector
            file))
         (hash (utils:sha256 data))
         (id (image-id hash)))
    (cons (if id id
              (-add-image-from-file (file-namestring file) data hash))
          file)))

(vectorp
 (alexandria:read-file-into-byte-vector "/home/mik/Sync/squirrelicon.png"))

(defun add-image-from-files (files)
  (loop for file in files
        :collect (add-image-from-file file)))


(defun -insert-image-meta-default-statement (id w h &key (source ""))
  "Needs to be within with-connection.  Makes framesizes same as image size"
  (insert-into :imagemeta
    (set=
     :imageid id
     :width w
     :height h
     :source (get-source source)
     :framewidth w
     :frameheight h
     :columns 1
     :tilecount 1
     :spacing 0
     :margin 0)
    (on-conflict-do-nothing)))

(defun -init-image-meta-from-file (id file &key source)
  (let ((dimensions (magicklib:image-dimensions file)))
    (with-connection (db)
      (execute
       (-insert-image-meta-default-statement
        id (first dimensions) (second dimensions) :source source)))))


(defun insert-images (files &key source)
  (loop for (id . filename)
          in
          (add-image-from-files files)
        :collect (progn
                   (-init-image-meta-from-file  id filename :source source)
                   id)))

(defun insert-images-from-dir (dir)
  (insert-images
   (utils:find-files dir "*.png")))

(defun insert-images-from-dir-source (dir source)
  (insert-images
   (utils:find-files dir "*.png")
   :source source))

;; ;; (defun insert-zip (zippath source)
;;   (zip:with-zipfile (zf zippath)
;;     (zip:do-zipfile-entries (zname zentry zf)
;;       (print ))))


(defmacro -set-meta (id opts)
  `(update :imagemeta
     (set= ,@opts)
     (where (:= :imageid ,id))))


(defmacro set-meta (id opts)
  `(with-connection (db)
     (execute
      (-set-meta ,id ,opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               sources               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (execute
;;  (create-table (:source :if-not-exists t)
;;      ((id :type 'integer
;;           :primary-key t)
;;       (name :type 'text
;;             :not-null t)
;;       (website :type 'text))))


(defun -new-source (name website)
  "Needs to be within with-connection.  Makes framesizes same as image size"
  (insert-into :source
    (set=
     :name name
     :website website)
    (returning :id)))

(defun new-source (name website)
  (with-connection (db)
    (retrieve-one-value
     (-new-source name website))))

(defun get-source (name &key (website ""))
  (with-connection (db)
    (alexandria:if-let ((id
                         (retrieve-one-value
                          (select :id
                            (from :source)
                            (where (:= :name name))))))
      id
      (retrieve-one-value
       (-new-source name website)))))

(defun sources ()
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :source)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           image utilities           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro -image-info (test)
  `(with-connection (db)
     (retrieve-one
      (select (:image.name
               :image.id
               :image.hash
               :imagemeta.framewidth
               :imagemeta.frameheight
               :imagemeta.width
               :imagemeta.height
               :imagemeta.margin
               :imagemeta.spacing
               :source.name
               :source.website)
        (from :imagemeta)
        (join :image
              :on (:= :image.id :imagemeta.imageid))
        (join :source
              :on (:= :source.id :imagemeta.source))
        (where ,test)))))

(defun image-info-id (id)
  (-image-info (:= :image.id id)))

(defun image-info-hash (hash)
  (-image-info (:= :image.hash hash)))

(defun image-info (it)
  "If it is a string, assume hash; if number, id."
  (if (stringp it)
      (image-info-hash it)
      (image-info-id it)))

(defun get-tileset (hash)
  (let* ((ii (image-info hash))
         (name (getf ii :name))
         (margin (getf ii :margin))
         (spacing (getf ii :spacing))
         (iwidth (getf ii :width))
         (iheight (getf ii :height))
         (tw (getf ii :framewidth))
         (th (getf ii :frameheight))
         (cols (tiledmap:tiles-in-dimension iwidth tw margin spacing))
         (rows (tiledmap:tiles-in-dimension iheight th margin spacing)))
    (make-instance 'tiledmap:tileset
                   :tilewidth tw
                   :tileheight th
                   :imagewidth iwidth
                   :imageheight iheight
                   :properties '()
                   :image (utils:fmt "/db/image/~a" (getf ii :hash))
                   :name name
                   :margin margin
                   :spacing spacing
                   :columns cols
                   :tilecount (* cols rows))))

(defun get-tileset-tilemap (hash)
  (tiledmap:map-to-json
   (tiledmap:make-tileset-tilemap
    (get-tileset hash))))

(defun -get-tileset-tilemap (ts)
  (tiledmap:map-to-json
   (tiledmap:make-tileset-tilemap ts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               objects               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (create-table (:object :if-not-exists t)
;;     ((id :type 'integer :primary-key t)
;;      (imageid :type 'integer)
;;      (name :type 'text :not-null t)
;;      (tiles :type 'json)
;;      (tileswidth :type 'integer))
;;   (foreign-key '(:imageid) :references '(:image :id)))

(defun -insert-object  (name image-id tiles tiles-width)
  (insert-into :object
    (set=
     :name name
     :imageid image-id
     :tiles tiles
     :tilesWidth tiles-width)
    (returning :id)))

(defun insert-object (name image-id tiles tiles-width)
  (with-connection (db)
    (retrieve-one-value
     (-insert-object name image-id tiles tiles-width))))

(defun update-object (id name image-id tiles tiles-width)
  (with-connection (db)
    (retrieve-one-value
     (update :object
       (set=
        :name name
        :imageid image-id
        :tiles tiles
        :tileswidth tiles-width)
       (where
        (:= :id id))))))

(defun image-objects (image-id)
  (with-connection (db)
    (retrieve-all
     (select (:*)
       (from :object)
       (where
        (:= :imageid image-id))))))

(defun objects (&optional lmt)
  (with-connection (db)
    (retrieve-all
     (select (:*)
       (from :object)
       (limit (or lmt 200))))))

(defun delete-object (id)
  (with-connection (db)
    (retrieve-one
     (delete-from :object
       (where (:= :id id))))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;                                   frameanim                                ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (create-table (:frameanim :if-not-exists t)
;;     ((id :type 'integer :primary-key t)
;;      (imageid :type 'integer)
;;      (frames :type 'json)
;;      (name :type 'text))
;;   (foreign-key '(:imageid) :references '(:image :id)))

(defun -insert-frameanim (name image-id frames)
  (insert-into :frameanim
    (set=
     :name name
     :imageid image-id
     :frames frames)
    (returning :id)))

(defun insert-frameanim (name image-id frames)
  (with-connection (db)
    (retrieve-one-value
     (-insert-frameanim name image-id frames))))

(defun update-frameanim (id name image-id frames)
  (with-connection (db)
    (retrieve-one-value
     (update :frameanim
       (set=
        :name name
        :imageid image-id
        :frames frames)
       (where
        (:= :id id))))))

(defun image-frameanims (image-id)
  (with-connection (db)
    (retrieve-all
     (select (:*)
       (from :frameanim)
       (where
        (:= :imageid image-id))))))

(defun frameanims (&optional lmt)
  (with-connection (db)
    (retrieve-all
     (select (:*)
       (from :frameanim)
       (limit (or lmt 200))))))

(defun delete-frameanim (id)
  (with-connection (db)
    (retrieve-one
     (delete-from :frameanim
       (where (:= :id id))))))
