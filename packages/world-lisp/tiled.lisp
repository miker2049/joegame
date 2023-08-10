(defpackage tiledmap
  (:use :cl)
  (:import-from utils
    get-json-from-serializable
    define-serializable)
  (:export save-file))

(in-package tiledmap)



(define-serializable propertied nil
  (properties nil))

(define-serializable tiled-file (propertied)
  ;; (backgroundcolor "")
  ;; (tiled-class "" class)
  (tiledversion config:*tiled-version*)
  (version config:*tiled-json-version*))


(define-serializable tiled-map (tiled-file)
  (compressionlevel -1)
  (height 0)
  (infinite :false)
  (layers nil)
  (nextlayerid 0)
  (nextobjectid 0)
  (orientation "orthogonal")
  (parallaxoriginx 0)
  (parallaxoriginy 0)
  (renderorder "right-down")
  (tileheight 16)
  (tilesets nil)
  (tilewidth 16)
  (tiled-type "map" "type")
  (width 0))

(define-serializable tileset (tiled-file)
  (columns 0)
  ;; (fillmode "stretch")
  (firstgid 0)
  (image "")
  (imageheight 0)
  (imagewidth 0)
  (margin 0)
  (name "tileset")
  ;; (objectalignment "")
  ;; (source "")
  (spacing 0)
  ;;(terrains nil)
  (tilecount 0)
  (tileheight 16)
  (tilewidth 16)
  ;; (tilerendersize "tile")
  ;; (tiles nil)
  (tiled-type "tileset" "type"))

(defmethod add-tileset ((tm tiled-map) (ts tileset))
  (setf (firstgid ts)
    (let ((last-ts
            (car
              (last (tilesets tm)))))
      (if last-ts
        (+ (firstgid last-ts)
          (tilecount last-ts))
        0)))
  (push ts (tilesets tm)))

(define-serializable layer nil
  (tiledclass "" "class")
  (id 0)
  (locked :false)
  (name "tiled-layer")
  (offsetx 0)
  (offsety 0)
  (opacity 1)
  (parallaxx 1)
  (parallaxy 1)
  (properties nil)
  (tintcolor  "")
  (visible 't)
  (x 0)
  (y 0))

(define-serializable tilelayer (tiled-layer)
  (compression "" "empty")
  (data nil)
  (encoding "csv")
  (tiled-type  "tilelayer" "type")
  (height 0)
  (width 0))

(define-serializable imagelayer (tiled-layer)
  (image "")
  (tiled-type  "imagelayer" "type")
  (repeatx :false)
  (repeaty :false))

(define-serializable objectlayer (tiled-layer)
  (draworder "topdown")
  (objects nil)
  (tiled-type  "objectgroup" "type"))

(define-serializable grouplayer (tiled-layer)
  (layers nil)
  (tiled-type  "group" "type"))


(define-serializable tile-json (propetied)
  (animation nil)
  (id 0)
  (image "")
  (imageheight 0)
  (imagewidth 0)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (objectgroup nil)
  (probability nil)
  (tiled-type "tile" "type"))

(define-serializable property nil
  (name "prop")
  (prop-type "string" "type")
  (propertytype "")
  (value ""))

(define-serializable tile-frame ()
  (duration 100)
  (tileid 0))

(define-serializable point ()
  (x 0)
  (y 0))

(define-serializable tiled-object (propertied point)
  (height 0)
  (id 0)
  (name 0)
  (rotation 0)
  (template "")
  (tiled-object-type "" "type")
  (tiled-object-class "" "class")
  (visible :true)
  (width 0))

(define-serializable tiled-object-tile (tiled-object)
  (gid 0))
(define-serializable tiled-object-point (tiled-object)
  (point :true))

(define-serializable tiled-object-rect (tiled-object))

(define-serializable tiled-object-ellipse (tiled-object)
  (ellipse :true))

(define-serializable tiled-object-polygon (tiled-object)
  (polygon nil))

(define-serializable tiled-object-polyline (tiled-object)
  (polyline nil))

(define-serializable tiled-object-text (tiled-object)
  (text nil))

(define-serializable tiled-text-config (propertied point)
  (bold :false)
  (color "#000000")
  (fontfamily "sans-serif")
  (halign "left")
  (italic :false)
  (kerning :true)
  (pixelsize 16)
  (strikeout :false)
  (text "")
  (underline :false)
  (valign "top")
  (wrap :false))

(define-serializable tiled-object-template ()
  (object-type "" "type")
  (tileset nil)
  (object nil))


(defun tile-space (amt tilesize margin spacing)
  (if (< amt 0)
    (error "Invalid amt, needs to be positive integer"))
  (if (eql amt 0)
    0
    (if (eql amt 1)
      (+ tilesize margin margin)
      (+
        (+ margin tilesize)
        (* (- amt 2) (+ tilesize spacing))
        (+ spacing tilesize margin)))))

(defun tiles-in-dimension (size tilesize margin spacing)
  (let ((i 0))
    (loop while
      (<=
        (tile-space i tilesize margin spacing)
        size)
      do (incf i))
    (- i 1)))



(defun make-tileset-from-image
  (imgpath &key (name "tileset") (margin 0) (tilesize 16) (spacing 0))
  (let* ((img (png:decode-file imgpath))
          (imgwidth (png:image-width img))
          (imgheight (png:image-height img))
          (cols (tiles-in-dimension imgwidth tilesize margin spacing))
          (rows (tiles-in-dimension imgheight tilesize margin spacing)))
    (make-instance 'tileset
      :name name
      :image (file-namestring imgpath)
      :imageheight imgheight
      :imagewidth imgwidth
      :margin margin
      :spacing spacing
      :tileheight tilesize
      :tilewidth tilesize
      :columns cols
      :tilecount (* cols rows))))

(defun save-file (path b)
  (with-open-file (s path
                    :direction :output
                    :if-exists :supersede)
    (format s "~a" b)))

(save-file
  "../../assets/images/terr_grass.tsj"
  (get-json-from-serializable
    (make-tileset-from-image
      "../../assets/images/terr_grass.png"
      :name "tooken")))

(get-json-from-serializable
  (make-tileset-from-image
    "../../assets/images/terr_grass.png"
    :name "tooken"))


(defclass tileset-tile ()
  ((id
     :initarg :id
     :initform 0
     :accessor tileset-id)
    (tileset
      :initarg :tileset
      :initform ""
      :accessor tileset)))


(defun random-file (&key (extension "json"))
  (format nil "~X.~a"
    (simplex:xxh64 (format nil "~X" (random 100.0)))
    extension))

(defun prepare-run-tiled ()
  (setf
    (uiop:getenv "QT_QPA_PLATFORM")
    "wayland"))


(defun valid-tilemap (tm)
  (let ((in (random-file))
         (out (random-file)))
    (prepare-run-tiled)
    (save-file in (get-json-from-serializable tm))

    (multiple-value-bind (output error-output exit-code)
      (uiop:run-program
        (list
          "tiled"
          "--resolve-types-and-properties"
          "--embed-tilesets"
          "--export-map"
          "json"
          in out)
        :output :string
        :error-output :string
        :ignore-error-status t)
      (uiop:delete-file-if-exists in)
      (uiop:delete-file-if-exists out)
      (unless (zerop exit-code)
        (format t "error output is: ~a" error-output))
      (zerop exit-code))))



(valid-tilemap (make-instance 'tiled-map
                 :width 10 :height 10))

(defun load-tiled-map (path)
  (let ((parsed
          (deserialize-tiled-map
            (jojo:parse
              (uiop:read-file-string path)))))
    (setf
      (tilesets parsed)
      (mapcar #'deserialize-tileset (tilesets parsed)))
    (setf
      (layers parsed)
      (mapcar #'deserialize-layer (layers parsed)))
    parsed))
