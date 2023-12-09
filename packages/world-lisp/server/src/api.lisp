(in-package :server.web)

(defvar terr-plist
  (mapcar #'(lambda (it)
              (let ((itt (cdr it)))
                (list
                 :name (getf itt :name)
                 :color (getf itt :color)
                 :priority (getf itt :priority)
                 :wang-tiles (getf itt :wang-tiles)
                 :tileset
                 (utils:to-plist
                  (getf itt :tileset)))))
          worldconf:*terrain-set*))

(defun get-terr (n)
  (nth
   (if (stringp n)
       (parse-integer n)
       (if (not n)
           0 n))
   terr-plist))

(defroute "/terrain-config-set" ()
  (render-json terr-plist))

(defroute "/terrain-config/:n" (&key n)
  (render-json (get-terr n)))

(defroute "/terrain-image/:n" (&key n)
  `(200 (:content-type "image/png")
        ,(get-binary-data
          (tiledmap:get-image
           (getf
            (cdr
             (nth (parse-integer n)
                  worldconf:*terrain-set*))
            :tileset)))))
