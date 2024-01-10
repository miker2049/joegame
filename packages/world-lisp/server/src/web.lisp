(in-package :cl-user)
(defpackage server.web
  (:import-from :cl-who
                with-html-output-to-string
                html-mode)
  (:use cl
        caveman2
        server.config
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
    (render #P"index.html" `(:full-world-image-url "/tiles/world.png"
                             :full-world-image-alt "The entire joegame map."
                             :tile-urls "/zone/${x}/${y}"
                             :x-map-coords ,mapcoords
                             :y-map-coords ,mapcoords))))

(defroute "/zone/:x/:y" (&key x y)
  (let ((mapcoords (loop for n below 160 :collect (cons (* n 10)  n))))
    (render #P"zone.html" `(:image-url ,(format nil "/tiles/zone_~a_~a.png" x y)
                            :image-alt ,(format nil "The ~a,~a zone" x y)
                            :map-urls-prefix ,(format nil "/map/~a/~a/" x y)
                            :x-map-coords ,mapcoords
                            :y-map-coords ,mapcoords))))

(defroute "/map/:zx/:zy/:x/:y" (&key zx zy x y)
  (render #P"layouts/game-view.html" `(:zoneX ,zx :zoneY ,zy :x ,x :y ,y)))

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

(defroute "/terrain-set" ()
  (render #P"terrain-set.html"
          (list :terrain-indexes
                (loop :for idx :below (length worldconf:*terrain-set*)
                      :collect idx))))

(defroute "/get-terrain" ()
  (render #P"terrain.html"))

(defroute "/area-set" ()
  (render-json
   (mapcan
    #'(lambda (item)
        (let ((itempl (append (cdr item))))
          (setf (getf itempl :signal)
                (worldconf:serialize (getf itempl :signal)))
          (list (car item) itempl)))
    worldconf:*area-set*)))

(defun get-binary-data (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      data)))

(defroute "/image/:file" (&key file)
  (let ((d (get-binary-data (format nil "~a~a" "/home/mik/joegame/assets/images/" file))))
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

(defroute "/worldtile/image/:row/:tile" (&key row tile)
  (with-html-output-to-string (output)
    (:div :class "clicked noselect"
          (:img :class "noselect"
                :draggable nil
                :src (format nil "/mwtiles/mw-~A-~A.png"
                             (* 256 (parse-integer tile))
                             (* 256 (parse-integer row)))))))


(defun filter (elements test)
  (loop
    for e in elements
    when (funcall test e)
      collect e))

(defun take (n col)
  (loop for it below n collect it))


(defroute "/db/images" ()
  (let ((search
          (cdr
           (assoc "search"
                  (request-query-parameters *request*)
                  :test #'equalp))))
    (render #P"images.html"
            (list :images
                  (images search)))))

(defroute "/db/image-search" ()
  (let* ((search
           (cdr
            (assoc "search"
                   (request-query-parameters *request*)
                   :test #'equalp)))
         (results (images search)))
    (render #P"components/image-table.html" (list :images results))))


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

(defroute "/db/image-form/:hash" (&key _parsed hash)
  (render #P"components/image-meta-form.html"
          `(:image ,(image-meta hash) :sources ,(sources))))

(defroute ("/db/image/:hash" :method :POST) (&key hash _parsed)
  (print _parsed)
  (let* (
         (source-website (cdr (assoc "source-website" _parsed :test #'string=)))
         (source-name (cdr (assoc "source-name" _parsed :test #'string=)))
         (fw (cdr (assoc "framewidth" _parsed :test #'string=)))
         (fh (cdr (assoc "frameheight" _parsed :test #'string=)))
         (source
           (if (and source-name source-website)
               (new-source source-name source-website)
               (cdr (assoc "source" _parsed :test #'string=))))
         (margin (cdr (assoc "margin" _parsed :test #'string=)))
         (spacing (cdr (assoc "spacing" _parsed :test #'string=))))
    (set-meta (image-id hash)
              (:source source
               :framewidth fw
               :frameheight fh
               :margin margin
               :spacing spacing))
    (render #P"components/image-meta-form.html"
            `(:image ,(image-meta hash) :sources ,(sources)))))

(defroute "/db/html/create-source-form" ()
  (with-html-output-to-string (os)
    (:div :style "margin-left: 3rem; margin-top: 1rem;"
          (:label :for "source-name" "Name"
                  (:input :type "text" :name "source-name"))
          (:label :for "source-website" "Site"
                  (:input :type "text" :name "source-website")))))

;; (defroute ("/db/upload-images" :method :POST) (&key _parsed)
;;   (print
;;    (request-body-parameters *request*))
;;   (with-html-output-to-string (os) (:p "hey")))
;;
;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
