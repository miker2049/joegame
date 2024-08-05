(in-package :cl-user)
(defpackage server.web
  (:import-from :cl-who
                with-html-output-to-string
                html-mode)
  (:use cl
        caveman2
        config
        ;;parenscript
        server.view
        server.db
        server.asset-db
        datafly
        sxql)
  (:export :*web*))
(in-package :server.web)

(setf (cl-who:html-mode) :HTML5)
;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defun range (n)
  (loop for i from 0 to n collect i))

(defroute "/" ()
  (let ((mapcoords (loop for n below 16 :collect (* n 100))))
    (render #P"index.djhtml" `(:full-world-image-url "/tiles/world.png"
                               :full-world-image-alt "The entire joegame map."
                               :tile-urls "/zone/${x}/${y}"
                               :x-map-coords ,mapcoords
                               :y-map-coords ,mapcoords))))

(defroute "/zone/:x/:y" (&key x y)
  (let ((mapcoords (loop for n below 160 :collect (cons (* n 10)  n))))
    (render #P"zone.djhtml" `(:image-url ,(format nil "/tiles/zone_~a_~a.png" x y)
                              :image-alt ,(format nil "The ~a,~a zone" x y)
                              :map-urls-prefix ,(format nil "/map/~a/~a/" x y)
                              :x-map-coords ,mapcoords
                              :y-map-coords ,mapcoords))))

(defroute "/map/:zx/:zy/:x/:y" (&key zx zy x y)
  (render #P"layouts/game-view.djhtml" `(:zoneX ,zx :zoneY ,zy :x ,x :y ,y)))

(defroute "/mapjson/:zx/:zy/:x/:y" (&key zx zy x y)
  (let ((map (worldconf:get-tiled-map-from-conf
              worldconf:*worldconf*
              (+ (* (parse-integer zx) 1600 1)
                 (* 10 (parse-integer x)))
              (+ (* (parse-integer zy) 1600 1)
                 (* 10 (parse-integer y)))
              10 10)))
    (worldconf:generate-asset-pack map "/images/")
    (tiledmap:fix-map-tilesets-path map "/images/")
    (tiledmap:assure-unique-layer-names map)
    (setf (getf (response-headers *response*) :content-type) "application/json")
    (tiledmap:map-to-json map)))

(defroute "/terrains" ()
  (render #P"terrains.djhtml"
          (list :terrains (mapcar #'(lambda (it)
                                      (let* ((pl (cdr it))
                                             (new-pl (copy-list pl)))
                                        (setf (getf new-pl :color) (utils:fmt "#~6,'0x" (getf pl :color)))
                                        new-pl))
                                  worldconf:*terrain-set*))))

(defroute "/db/upload" ()
  (render #P"upload.djhtml"))

(defun get-terr-tileset (tstr)
  (getf
   (cdr
    (assoc (intern
            (string-upcase
             tstr)
            :keyword)
           worldconf:*terrain-set*))
   :tileset))

(defroute "/terrain-tilemap/:name" (&key name)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (tiledmap:map-to-json
   (tiledmap:make-tileset-tilemap
    (get-terr-tileset name))))

(defroute "/terrain-tileset/:terr" (&key terr)
  (render-json
   (tiledmap:map-to-plist
    (get-terr-tileset terr))))

(defroute "/terrain-image/:terr" (&key terr)
  (let* ((ts (get-terr))
         (d (alexandria:read-file-into-byte-vector
             (format nil "~a~a" "/home/mik/joegame/assets/images/" file)))
         )
    `(200 (:content-type "image/png") ,d)))

(defroute "/new-terrain" ()
  (render "new-terrain.djhtml"))

(defroute "/area-set" ()
  (render-json
   (mapcan
    #'(lambda (item)
        (let ((itempl (append (cdr item))))
          (setf (getf itempl :signal)
                (worldconf:serialize (getf itempl :signal)))
          (list (car item) itempl)))
    worldconf:*area-set*)))

(defroute "/image/:file" (&key file)
  (let ((d (alexandria:read-file-into-byte-vector (format nil "~a~a" "/home/mik/joegame/assets/images/" file))))
    `(200 (:content-type "image/png") ,d)))


(defroute "/imageplus/:file" (&key file)
  (let* ((p
           (png:decode-file
            (format nil "~a~a" "/home/mik/joegame/assets/images/" file)
            :preserve-alpha 't))
         (data (flexi-streams:with-output-to-sequence (s)
                 (png:encode
                  (render:extrude-tileset-image p
                                                :margin 0
                                                :spacing 0
                                                :extrusion 16)
                  s))))
    `(200 (:content-type "image/png") ,data)))

(defun make-tile-stream (x y z)
  (flexi-streams:with-output-to-sequence (s)
    (png:encode
     (worldtiles:world-tile x y z)
     s)
    s))
(defvar make-tile-stream-memo (utils:memoize #'make-tile-stream))
(setf make-tile-stream-memo (utils:memoize #'make-tile-stream))


(defroute "/worldtile/:z/:x/:y" (&key z x y)
  `(200 (:content-type "image/png" :access-control-allow-origin  "*")
        ,(funcall make-tile-stream-memo (parse-integer x) (parse-integer y) (parse-integer z))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            asset manager            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-images-page ()
  (let ((search
          (cdr
           (assoc "search"
                  (request-query-parameters *request*)
                  :test #'equalp))))
    (render #P"images.djhtml"
            (list :images
                  (images search)))))

(defroute "/db/images" ()
  (render-images-page))

(defroute "/db/image-search" ()
  (let* ((search
           (cdr
            (assoc "search"
                   (request-query-parameters *request*)
                   :test #'equalp)))
         (results (images search)))
    (render #P"components/image-table.djhtml" (list :images results))))


;; (defroute "/db/image/:hash" (&key hash)
;;   `(200 (:content-type "image/png") ,(car
;;                                       (magicklib:get-png
;;                                        (image-data hash)))))
(defroute "/db/image/:hash" (&key hash)
  `(200 (:content-type "image/png") ,(image-data hash)))

(defroute "/db/tilemap/:hash" (&key hash)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (get-tileset-tilemap hash))

(defroute "/db/image-tiled/:hash" (&key _parsed hash)
  (let* ( (meta (image-meta hash))
          (width (getf meta :width))
          (height (getf meta :height))
          (spacing (getf meta :spacing))
          (margin (getf meta :margin))
          (tilewidth (getf meta :framewidth))
          (tileheight (getf meta :frameheight)))
    (if (and (= tilewidth width)
             (= tileheight height))
        `(200 (:content-type "image/png") ,(image-data hash))
        `(200 (:content-type "image/png") ,(magicklib:draw-tile-lines-blob
                                            (image-data hash)
                                            tilewidth
                                            tileheight
                                            :margin margin
                                            :spacing spacing)))))

(defroute "/db/image-icon-show/:hash" (&key _parsed hash)
  (with-html-output-to-string (os)
    (:img :class "icon" :src (utils:fmt "/db/image/~a" hash) (image-name hash))))

(defroute "/db/image-show/:hash" (&key _parsed hash)
  (let ((iname (image-name hash)))
    (with-html-output-to-string (os)
      (:img
       :alt (utils:fmt "joegame image: ~a (~a)" iname hash )
       :style "margin: auto"
       :src (utils:fmt "/db/image/~a" hash)
       iname))))

(defroute "/db/image-tiled-show/:hash" (&key _parsed hash)
  (let* ((iname (image-name hash))
         (nocache (cdr (assoc "nocache" _parsed :test #'string=))))
    (with-html-output-to-string (os)
      (:img
       :class "image-preview"
       :alt (utils:fmt "joegame image: ~a (~a)" iname hash )
       :style "margin: auto; margin-top: 4rem; margin-bottom: 4rem"
       :src (utils:fmt "/db/image-tiled/~a?time=~a" hash (get-universal-time))
       iname))))

(defroute "/db/image-tile/:hash/:index" (&key _parsed hash index)
  (let* ((iname (image-name hash))
         (meta (image-meta hash))
         (width (getf meta :width))
         (height (getf meta :height))
         (spacing (getf meta :spacing))
         (margin (getf meta :margin))
         (tilewidth (getf meta :framewidth))
         (tileheight (getf meta :frameheight))
         (columns (getf meta :columns)))
    (destructuring-bind (xval yval) (ixy ))
    `(200 (:content-type "image/png") ,(magicklib:crop-image-blob
                                        (image-data hash)
                                        tilewidth
                                        tileheight
                                        :margin margin
                                        :spacing spacing))))

(defun render-image-meta-form (image)
  (let ((image-id (if (stringp image)
                      (image-id image)
                      image)))
    (render #P"components/image-meta-form.djhtml"
            `(:image ,(image-info image-id) :sources ,(sources)
              :objects ,(image-objects image-id)))))

(defroute "/db/image-form/:hash" (&key hash)
  (render-image-meta-form hash))

(defroute ("/db/image/:hash" :method :PATCH) (&key hash _parsed)
  (let* ((source-website (cdr (assoc "source-website" _parsed :test #'string=)))
         (source-name (cdr (assoc "source-name" _parsed :test #'string=)))
         (fw (cdr (assoc "framewidth" _parsed :test #'string=)))
         (fh (cdr (assoc "frameheight" _parsed :test #'string=)))
         (source
           (if (and source-name source-website)
               (new-source source-name source-website)
               (cdr (assoc "source" _parsed :test #'string=))))
         (margin (cdr (assoc "margin" _parsed :test #'string=)))
         (spacing (cdr (assoc "spacing" _parsed :test #'string=))))
    (print (image-id hash))
    (set-meta (image-id hash)
              (
               :framewidth fw
               :frameheight fh
               ;;:source source
               :margin margin
               :spacing spacing))
    (render-image-meta-form hash)))

(defroute "/db/html/create-source-form" ()
  (with-html-output-to-string (os)
    (:div :style "margin-left: 3rem; margin-top: 1rem;"
          (:label :for "source-name" "Name"
                  (:input :type "text" :name "source-name"))
          (:label :for "source-website" "Site"
                  (:input :type "text" :name "source-website")))))

(defun sassoc (item alist)
  (cdr
   (assoc item alist :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              objects             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute ("/db/object" :method :POST) (&key _parsed)
  (let ((image-id (parse-integer
                   (sassoc "image-id" _parsed)))
        (tiles (sassoc "tiles" _parsed))
        (name (sassoc "name" _parsed))
        (tiles-width (parse-integer
                      (sassoc "tiles-width" _parsed))))
    (server.asset-db:insert-object name image-id tiles tiles-width)
    (render-image-meta-form image-id)))

(defroute ("/db/object" :method :PUT) (&key _parsed)
  (let ((image-id (parse-integer
                   (sassoc "image-id" _parsed)))
        (obj-id (parse-integer
                 (sassoc "obj-id" _parsed)))
        (name (sassoc "name" _parsed))
        (tiles (sassoc "tiles" _parsed))
        (tiles-width (parse-integer
                      (sassoc "tiles-width" _parsed))))
    (server.asset-db:update-object obj-id name image-id tiles tiles-width)
    (render-image-meta-form image-id)))

(defroute ("/db/object" :method :DELETE) (&key _parsed)
  (let ((object-id (parse-integer
                    (sassoc "object-id" _parsed)))
        (image-id (parse-integer
                   (sassoc "image-id" _parsed))))
    (delete-object  object-id )
    (render-image-meta-form image-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              frame anim             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defroute ("/db/frameanim" :method :POST) (&key _parsed)
  (let ((image-id (parse-integer
                   (sassoc "image-id" _parsed)))
        (frames (sassoc "frames" _parsed))
        (name (sassoc "name" _parsed)))
    (server.asset-db:insert-frameanim name image-id frames)
    (render-image-meta-form image-id)))

(defroute ("/db/frameanim" :method :PUT) (&key _parsed)
  (let ((image-id (parse-integer
                   (sassoc "image-id" _parsed)))
        (frame-id (parse-integer
                   (sassoc "frame-id" _parsed)))
        (name (sassoc "name" _parsed))
        (frames (sassoc "frames" _parsed)))
    (server.asset-db:update-frameanim frame-id name image-id frames)
    (render-image-meta-form image-id)))

(defroute ("/db/frameanim" :method :DELETE) (&key _parsed)
  (let ((frame-id (parse-integer
                   (sassoc "frame-id" _parsed)))
        (image-id (parse-integer
                   (sassoc "image-id" _parsed))))
    (delete-frameanim frame-id)
    (render-image-meta-form image-id)))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.djhtml"
                   *template-directory*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             imagesubmit             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute "/db/image-submit" ()
  (render #P"components/image-submit-form.djhtml"))

;; (set-meta (image-id "c831e9479019f25518866b08b63e546d7d78a8a1647cddfd34134adcf264df1a") (:framewidth 16))
