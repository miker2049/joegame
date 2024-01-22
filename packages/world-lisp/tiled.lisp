(defpackage tiledmap
  (:use :cl)
  (:import-from alexandria
                switch)
  (:import-from utils
                ;; wang-
                to-plist
                define-deserialization
                get-json-from-serializable
                define-serializable)
  (:export
   assure-unique-layer-names
   fix-map-tilesets-path
   fix-map-tilesets-path-from-file
   load-tiled-map
   width height
   map-to-json
   map-to-plist
   backgroundcolor
   tiled-map
   tileset
   margin
   spacing
   tilesets
   install-tileset
   make-tileset-from-image
   make-tileset-from-image
   make-lazy-tileset
   make-tileset-from-image-embed
   make-tileset-tilemap
   name
   wang-grid
   wang-tileset
   tileset-in-map
   add-tileset
   layers
   firstgid
   get-next-layer-id
   tilelayer
   grouplayer
   objectlayer
   imagelayer
   image
   get-image
   save-file
   deserialize-tiled-map
   valid-tilemap
   add-property
   fix-map-tilesets-path
   add-pack-to-map
   asset-pack
   image-pack-config
   pack-files
   spritesheet-pack-config
   map-to-json
   map-to-plist
   tiles-in-dimension))

(in-package tiledmap)


(define-serializable property nil
  (name "prop")
  (property-type "string" "type")
  (prop-value "" "value"))


(define-serializable propertied nil
  (properties nil))

(defmethod to-plist nconc ((obj propertied))
  (list :|properties| (mapcar #'to-plist (properties obj)) ))

(defmethod add-property ((obj propertied) ptype name pvalue)
  (setf (properties obj)
        (append (properties obj)
                (list
                 (make-instance 'property :property-type ptype :prop-value pvalue :name name)))))


(define-serializable tiled-file (propertied)
  (backgroundcolor "")
  (tiled-class "" class)
  (tiledversion config:*tiled-version*)
  (version config:*tiled-json-version*))

(defvar tiled-map-slots
  '( (compressionlevel -1)
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
    (width 0)))

(define-serializable tiled-map (propertied)
  (properties nil)

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



(define-serializable external-tileset (tiled-file)
  (firstgid 0)
  (source "file.png"))

(define-serializable tileset (tiled-file)
  (properties nil)
  (columns 0)
  ;; (fillmode "stretch")
  (firstgid 1)
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

(defmethod get-image ((ts tileset))
  (image ts))

(defmethod install-tileset-image ((ts tileset) &key (dir (namestring
                                                          config:*static-directory*)))
  (utils:cp (namestring (truename (image ts))) dir))

(defmethod install-tileset-json ((ts tileset) &key (dir (namestring
                                                         config:*static-directory*)))
  (utils:save-file (merge-pathnames (utils:fmt "~a.json" (tiledmap:name ts))
                                    dir)
                   (let ((pm (tiledmap:map-to-plist ts)))
                     (setf (getf pm :|image|)
                           (namestring
                            (merge-pathnames (file-namestring (image ts)) dir)))
                     (jojo:to-json pm))))

(defmethod install-tileset ((ts tileset) &key (dir (namestring
                                                    config:*static-directory*)))
  (install-tileset-image ts :dir dir)
  (install-tileset-json ts :dir dir))


(defmethod wang-grid ((ts tileset))
  (error "Not a wang-tileset."))

(define-serializable wang-tileset (tileset)
  (wang-grid nil))

(defclass lazy-tileset (tileset utils:lazy-generated-file) ())


(defmethod get-image ((ts lazy-tileset))
  (utils:get-lgf ts))

(defmethod install-tileset-image ((ts lazy-tileset) &key (dir (namestring config:*static-directory*)))
  (utils:move-lgf ts
                  (namestring (merge-pathnames
                               (file-namestring (utils:lgf-path ts))
                               dir)))
  (get-image ts))


(defclass tileset-with-image (tileset)
  ((imagedata
    :accessor imagedata
    :initarg :imagedata)))

(defmethod render-tileset-image ((ts tileset-with-image) path)
  (png:encode-file (slot-value ts :imagedata) path)
  (setf (image ts) path))

(defun tileset-in-map (map n)
  (some #'(lambda (item) (equal (name item) n))
        (tilesets map)))

(defmethod max-firstgid ((tm tiled-map))
  (reduce #'(lambda (a b) (if (> (firstgid b) (firstgid a))
                              b a))
          (tilesets tm)))

(defmethod add-tileset ((tm tiled-map) (ts tileset))
  (unless (tileset-in-map tm (name ts))
    (progn
      (setf (firstgid ts)
            (let ((ts-size
                    (length (tilesets tm))))
              (if (> ts-size 0)
                  (let ((mts (max-firstgid tm)))
                    (+ (firstgid mts)
                       (tilecount mts)))
                  1)))
      (setf (tilesets tm)
            (nconc (list ts) (tilesets tm))))))


(define-serializable tiled-layer nil
  ;; (tiledclass "" "class")
  (id 0)
  (locked :false)
  (name "tiledlayerfoo")
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
  (properties nil)
  (layerempty :false "empty")
  (compression "")
  (name "foo")
  (data nil)
  (encoding "csv")
  (tiled-type  "tilelayer" "type")
  (height 0)
  (width 0))


(defun get-next-layer-id (map)
  (+ 1 (reduce #'max (mapcar #'id (layers map)) :initial-value 0)))


(define-serializable imagelayer (tiled-layer)
  (properties nil)
  (image "")
  (name "")
  (tiled-type  "imagelayer" "type")
  (repeatx :false)
  (repeaty :false))

(define-serializable objectlayer (tiled-layer)
  (properties nil)
  (draworder "topdown")
  (objects nil)
  (tiled-type  "objectgroup" "type"))

(define-serializable grouplayer (tiled-layer)
  (properties nil)
  (layers nil)
  (tiled-type  "group" "type"))


(define-serializable tile-json (propertied)
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



;; HACK this is in worldconf too
(defvar *maps-dir* nil
  "Dir with images.")
(setf *maps-dir*  "~/joegame/assets/maps/")
;;

(defun make-tileset-from-image
    (imgpath &key name (margin 0) (tilesize 16) (spacing 0) (lazy nil))
  "imgg can be either a path to a file or a already-decoded array"
  (let* ((img (png:decode-file imgpath :preserve-alpha t))
         (imgwidth (png:image-width img))
         (imgheight (png:image-height img))
         (cols (tiles-in-dimension imgwidth tilesize margin spacing))
         (rows (tiles-in-dimension imgheight tilesize margin spacing)))
    (make-instance (if lazy 'lazy-tileset 'tileset)
                   :name (or name (pathname-name imgpath))
                   :image (format nil "~a" imgpath)
                   :imageheight imgheight
                   :imagewidth imgwidth
                   :margin margin
                   :spacing spacing
                   :tileheight tilesize
                   :tilewidth tilesize
                   :columns cols
                   :tilecount (* cols rows))))

(defun make-tileset-from-image-blob
    (img &key name (margin 0) (tilesize 16) (spacing 0) (lazy nil))
  (let* ((imgwidth (png:image-width img))
         (imgheight (png:image-height img))
         (cols (tiles-in-dimension imgwidth tilesize margin spacing))
         (rows (tiles-in-dimension imgheight tilesize margin spacing)))
    (make-instance (if lazy 'lazy-tileset 'tileset)
                   :name name
                   :image name
                   :imageheight imgheight
                   :imagewidth imgwidth
                   :margin margin
                   :spacing spacing
                   :tileheight tilesize
                   :tilewidth tilesize
                   :columns cols
                   :tilecount (* cols rows))))

(defun make-lazy-tileset
    (imgpath width height genfun &key (name "tileset") (margin 0) (tilesize 16) (spacing 0) (lazy nil))
  (let ((cols (tiles-in-dimension width tilesize margin spacing))
        (rows (tiles-in-dimension height tilesize margin spacing)))
    (make-instance (if lazy 'lazy-tileset 'tileset)
                   :name name
                   :image (format nil "~a" imgpath)
                   :outpath (format nil "~a" imgpath)
                   :imageheight height
                   :imagewidth width
                   :margin margin
                   :spacing spacing
                   :tileheight tilesize
                   :tilewidth tilesize
                   :columns cols
                   :tilecount (* cols rows)
                   :gen-fun genfun)))

(defun make-tileset-from-image-embed
    (imgg &key (name "tileset") (margin 0) (tilesize 16) (spacing 0))
  "imgg can be either a path to a file or a already-decoded array"
  (let* ((img (if (stringp imgg)
                  (png:decode-file imgg :preserve-alpha t)
                  imgg))
         (imgwidth (png:image-width img))
         (imgheight (png:image-height img))
         (cols (tiles-in-dimension imgwidth tilesize margin spacing))
         (rows (tiles-in-dimension imgheight tilesize margin spacing)))
    (make-instance 'tileset-with-image
                   :name name
                   :image ""
                   :imagedata img
                   :imageheight imgheight
                   :imagewidth imgwidth
                   :margin margin
                   :spacing spacing
                   :tileheight tilesize
                   :tilewidth tilesize
                   :columns cols
                   :tilecount (* cols rows))))


(make-tileset-from-image-embed "~/joegame/assets/images/browserquest.png")

(defun save-file (path b)
  (with-open-file (s path
                     :direction :output
                     :if-exists :supersede)
    (format s "~a" b)))

;; (save-file
;;   "../../assets/images/terr_grass.tsj"
;;   (get-json-from-serializable
;;     (make-tileset-from-image
;;       "../../assets/images/terr_grass.png"
;;       :name "tooken")))

;; (get-json-from-serializable
;;   (make-tileset-from-image
;;     "../../assets/images/terr_grass.png"
;;     :name "tooken"))


(defclass tileset-tile ()
  ((id
    :initarg :id
    :initform 0
    :accessor tileset-id)
   (tileset
    :initarg :tileset
    :initform ""
    :accessor tileset)))




(defun load-parsed-tiled-map-json (path)
  (deserialize-tiled-map
   (jojo:parse
    (uiop:read-file-string path))))

(defmacro deserialize-properties (item)
  `(progn
     (setf (properties ,item)
           (mapcar #'deserialize-property (properties ,item)))
     ,item))

(defun load-tiled-map (path)
  (let ((jojo:*false-value* :false))
    (let ((parsed
            (deserialize-tiled-map
             (jojo:parse
              (uiop:read-file-string path)))))
      ;; (print (properties parsed))
      (setf
       (tilesets parsed)
       (mapcar #'(lambda (ts)
                   (deserialize-properties
                    (if (getf ts :|source|)
                        (deserialize-external-tileset ts)
                        (deserialize-tileset ts))))
               (tilesets parsed))
       (layers parsed)
       (mapcar
        #'(lambda (l)
            ;; (print (getf l :|name|))
            (let ((lay
                    (switch ((getf l :|type|) :test #'equal)
                      ("tilelayer" (deserialize-tilelayer l))
                      ("imagelayer" (deserialize-imagelayer l))
                      ("objectgroup"
                       (let ((olay (deserialize-objectlayer l)))
                         ;; need to be special about objects
                         (setf (objects olay)
                               (mapcar
                                #'(lambda (to)
                                    (let ((dto (deserialize-tiled-object to)))
                                      (deserialize-properties dto)))
                                (objects olay)))
                         olay))
                      ("group" (deserialize-grouplayer l))
                      (otherwise (error "What kind of layer?")))))
              (deserialize-properties lay)))
        (layers parsed)))
      (deserialize-properties parsed))))

(defun map-to-plist (mmap)
  (let ((m (to-plist mmap)))
    (reverse
     (mapcar #'(lambda (it)
                 (setf
                  (getf m it)
                  (mapcar #'to-plist
                          (getf m it))))
             '(:|layers| :|tilesets| :|properties|))) ;; TODO Need to look for properties everywhere
    m))

(defun map-to-json (mmap)
  (jojo:to-json (map-to-plist mmap)))


;; (save-file "~/joegame/assets/maps/dds3.json"
;;   (jojo:to-json
;;     (map-to-plist
;;       (load-tiled-map 
;;         "~/joegame/assets/maps/desert-stamps2.json"))))

(defun random-file (&key (extension "json"))
  (format nil "~X.~a"
          (simplex:xxh64 (format nil "~X" (random 100.0)))
          extension))

(defun prepare-run-tiled ()
  (setf
   (uiop:getenv "QT_QPA_PLATFORM")
   "minimal"))

(defun preview-tilemap-in-tiled (mappath)
  ;; (prepare-run-tiled)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program
       (list
        "tiled"
        mappath)
       :output :string
       :error-output :string
       :ignore-error-status t)
    (unless (zerop exit-code)
      (format t "error output is: ~a" error-output)
      (zerop exit-code))))

(defun valid-tilemap (tm)
  (let ((in (random-file))
        (out (random-file)))
    (prepare-run-tiled)
    (save-file in (map-to-json tm))

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

(defun assure-unique-layer-names (map)
  (let ((namecount (make-hash-table :test #'equal)))
    (dolist (lay (layers map))
      (let ((record (gethash (name lay) namecount)))
        (if record
            (progn
              (setf (name lay)
                    (format nil "~a-~a"
                            (name lay)
                            (+ 1 record)))
              (incf record))
            (setf (gethash (name lay) namecount) 0))))))

(defun fix-map-tilesets-path (map new-dir)
  "Takes a json map path and a new dir and changes all tileset images to that dir.
For debugging tilemap files directly in tiled."
  (setf
   (tilesets map)
   (mapcar #'(lambda (ts)
               (setf (image ts)
                     (concatenate 'string new-dir (file-namestring (image ts))))
               ts)
           (tilesets map))))

(defun fix-map-tilesets-path-from-file (mappath new-dir)
  "Takes a json map path and a new dir and changes all tileset images to that dir.
For debugging tilemap files directly in tiled."
  (let ((m (jojo:parse (uiop:read-file-string mappath))))
    (setf
     (getf m :|tilesets|)
     (mapcar #'(lambda (ts) (setf (getf ts :|image|)
                                  (concatenate 'string new-dir (file-namestring (getf ts :|image|))))
                 ts)
             (getf m :|tilesets|)))
    (save-file
     mappath
     (jojo:to-json m))))


(defclass asset-pack ()
  ((files
    :initarg :files
    :initform nil
    :accessor pack-files)))

(defmethod serialize-asset-pack ((pack asset-pack))
  `(:|meta| (:|url| "joegame")
    :|main| (:|files| ,(pack-files pack))))

(defun image-pack-config (key url)
  `(:|key| ,key :|url| ,url :|type| "image" ))

(defun spritesheet-pack-config (key url
                                &key (frameWidth 16) (frameHeight 16)
                                  (margin 0) (spacing 0))
  `(:|key| ,key :|url| ,url :|type| "spritesheet"
                :|frameConfig| (:|frameWidth| ,frameWidth
                                :|frameHeight| ,frameHeight
                                :|margin| ,margin
                                :|spacing| ,spacing) ))

(defmethod add-pack-to-map ((mmap tiled-map) (pack asset-pack))
  (add-property mmap "string" "pack" (jojo:to-json (serialize-asset-pack pack))))

(defmethod make-tileset-tilemap ((ts tileset))
  (let* ((columns (columns ts))
         (tcount (tilecount ts))
         (height (/ tcount columns))
         (tmap
           (make-instance 'tiled-map
                          :layers (list
                                   (make-instance
                                    'tilelayer
                                    :name "main"
                                    :width columns
                                    :height height
                                    :data (utils:range (+ 1 tcount) :start 1)))
                          :tilesets (list ts)
                          :width columns
                          :height height
                          :tilewidth (tilewidth ts)
                          :tileheight (tileheight ts))))
    tmap))

(defmacro make-tilemap-from-image (imgpath &key keyss &allow-other-keys)
  `(make-tileset-tilemap
    (make-tileset-from-image-embed ,imgpath ,@keyss)))


(defmethod preview-map ((tilemap tiled-map))
  (let* ((dir (utils:mktempd))
         (dirreal (truename (pathname dir)))
         (mappath  (merge-pathnames dirreal "map.json"))
         (tspath (merge-pathnames dirreal "tileset.png")))

    (setf
     (uiop:getenv "QT_QPA_PLATFORM")
     "wayland")
    (loop :for ts :in (tilesets tilemap)
          :do (setf (image ts)
                    (uiop:native-namestring
                     tspath)))
    (png:encode-file
     (imagedata (car (tilesets tilemap)))
     tspath)
    (loop :for ts :in (tilesets tilemap)
          :do (setf (imagedata ts) nil))
    (utils:save-file mappath (map-to-json tilemap))
    (preview-tilemap-in-tiled
     (uiop:native-namestring
      mappath))
    (uiop:delete-directory-tree
     (truename
      (pathname dir))
     :validate t)))
