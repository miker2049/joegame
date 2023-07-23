(defpackage tiledmap
  (:use :cl :jonathan))

(in-package tiledmap)


(defmacro define-class-from-spec (class-name supers &body slots)
  `(defclass ,class-name ,supers
     ,(mapcar (lambda (slot)
                `(,(car slot)
                   :initarg ,(intern (string-upcase (symbol-name (car slot))) :keyword)
                   :initform ,(cadr slot)
                   :accessor ,(car slot)))
        slots)))


(defmacro define-json-method (class-name &body slots)
  `(defmethod jojo:%to-json ((obj ,class-name))
     (jojo:with-object
       ,@(mapcar (lambda (slot)
                   `(jojo:write-key-value
                      ,(string-downcase
                         (if (eql (length slot) 3)
                           (nth 2 slot)
                           (symbol-name (car slot))))
                      (slot-value obj ',(car slot))))
           slots))))
(defgeneric to-plist (obj)
  (:method-combination nconc))

(defmacro define-plist-serialize (class-name &body slots)
  `(defmethod to-plist nconc ((obj ,class-name))
     (list ,@(mapcan (lambda (slot)
                       `(,(intern
                            (string-downcase
                              (if (eql (length slot) 3)
                                (nth 2 slot)
                                (symbol-name (car slot))))
                            "KEYWORD")
                          (slot-value obj ',(car slot))))
               slots))))


(defun deduplicate-keys (plist)
  "Remove duplicate keys from a plist."
  (let ((result '())
         (seen (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr do
      (when (not (gethash key seen))
        (setf (gethash key seen) t)
        (push key result)
        (push value result)))
    (nreverse result)))


(defmacro define-tiled-object (class-name supers &body slot-spec)
  `(progn
     (define-class-from-spec ,class-name ,supers ,@slot-spec)
     (define-plist-serialize ,class-name ,@slot-spec)
     (defmethod tiled-to-json ((obj ,class-name))
       (jojo:to-json (deduplicate-keys (to-plist obj))))))

(define-tiled-object propertied nil
  (properties nil))

(define-tiled-object tiled-file (propertied)
  (backgroundcolor "")
  (tiled-class "" class)
  (tiledversion config:*tiled-version*)
  (version config:*tiled-json-version*))


(define-tiled-object tiled-map (tiled-file)
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

(define-tiled-object tiled-tileset (tiled-file)
  (columns 0)
  (fillmode "stretch")
  (firstgid 0)
  (image "")
  (imageheight 0)
  (imagewidth 0)
  (margin 0)
  (name "tileset")
  (objectalignment "")
  (source "")
  (spacing 0)
  ;;(terrains nil)
  (tilecount 0)
  (tileheight 0)
  (tilerendersize "tile")
  (tiles nil)
  (tiled-type "tileset" "type")
  (wangsets nil))

(define-tiled-object tiled-layer nil
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

(define-tiled-object tile-layer (tiled-layer)
  (compression "" "empty")
  (data nil)
  (encoding "csv")
  (tiled-type  "tilelayer" "type")
  (height 0)
  (width 0))

(define-tiled-object image-layer (tiled-layer)
  (image "")
  (tiled-type  "imagelayer" "type")
  (repeatx :false)
  (repeaty :false))

(define-tiled-object object-layer (tiled-layer)
  (draworder "topdown")
  (objects nil)
  (tiled-type  "objectgroup" "type"))

(define-tiled-object group-layer (tiled-layer)
  (layers nil)
  (tiled-type  "group" "type"))


(define-tiled-object tile (propetied)
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

(define-tiled-object property nil
  (name "prop")
  (prop-type "string" "type")
  (propertytype "")
  (value ""))

(define-tiled-object tile-frame ()
  (duration 100)
  (tileid 0))

(define-tiled-object point ()
  (x 0)
  (y 0))

(define-tiled-object tiled-object (propertied point)
  (height 0)
  (id 0)
  (name 0)
  (rotation 0)
  (template "")
  (tiled-object-type "" "type")
  (tiled-object-class "" "class")
  (visible :true)
  (width 0))

(define-tiled-object tiled-object-tile (tiled-object)
  (gid 0))
(define-tiled-object tiled-object-point (tiled-object)
  (point :true))

(define-tiled-object tiled-object-rect (tiled-object))

(define-tiled-object tiled-object-ellipse (tiled-object)
  (ellipse :true))

(define-tiled-object tiled-object-polygon (tiled-object)
  (polygon nil))

(define-tiled-object tiled-object-polyline (tiled-object)
  (polyline nil))

(define-tiled-object tiled-object-text (tiled-object)
  (text nil))

(define-tiled-object tiled-text-config (propertied point)
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

(define-tiled-object tiled-object-template ()
  (object-type "" "type")
  (tileset nil)
  (object nil))
