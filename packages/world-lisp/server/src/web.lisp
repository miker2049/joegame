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

(defroute "/db/images" ()
  (let ((search
          (cdr
           (assoc "search"
                  (request-query-parameters *request*)
                  :test #'equalp))))
    (render #P"images.html"
            (list :images
                  (utils:filter  (images)
                                 (if search
                                     (lambda (it) (utils:fuzzmatch
                                                   search
                                                   (getf it :name)))
                                     (lambda (it) t)))))))

(defroute "/db/image-search" ()
  (let* ((search
           (cdr
            (assoc "search"
                   (request-query-parameters *request*)
                   :test #'equalp)))
         (results
           (utils:filter (images)
                         (if search
                             (lambda (it) (utils:fuzzmatch
                                           search
                                           (getf it :name)))
                             (lambda (it) t)))))
    (render #P"components/image-table.html" (list :images results))))


(defroute ("/db/images" :method :POST) (&key _parsed)
  (print _parsed)
  (redirect (utils:fmt "/db/images?search=~a"
                       (cdr (assoc "search" _parsed :test #'equalp)))))

(defroute "/db/image/:hash" (&key _parsed hash)
  `(200 (:content-type "image/png") ,(image-data hash)))

(defroute "/db/image-show/:hash" (&key _parsed hash)
  (with-html-output-to-string (os)
    (:img :class "icon" :src (utils:fmt "/db/image/~a" hash) (image-name hash))))


;;
;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
