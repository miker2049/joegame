(in-package worldconf)

;; general testing
(defun map-debug (out conf ww hh &key (x 0) (y 0) (image-path "../images/"))
  (let ((mpath out)
         (mm (tiledmap:map-to-json
               (let
                 ((map
                    (make-map-from-wang-val-grids
                      (collect-terrain-wang-vals conf x y ww hh))))
                 ;; extra config
                 ;; (setf (tiledmap:backgroundcolor map) "#21ab60")
                 map))))
    (utils:save-file mpath mm)
    (tiledmap:fix-map-tilesets-path mpath image-path)))

(defun map-debug* ()
  (let ((mpath "/home/mik/joegame/assets/maps/conf_test.json")
         (mm (tiledmap:map-to-json
               (let ((map
                       (make-map-from-wang-val-grids (collect-terrain-wang-vals
                                                       (__ :ocean
                                                         (perlin~ 1.6 11
                                                           (list

                                                             (__ :field)
                                                             (__ :ocean))))
                                                       0 0 5 5))))
                 ;; extra config
                 ;; (setf (tiledmap:backgroundcolor map) "#21ab60")
                 map))))
    (utils:save-file mpath mm)
    (tiledmap:fix-map-tilesets-path mpath "../images/")))


(map-debug*)
;; wang map debug

(defun wang-map-debug ()
  (let* ((mpath "/home/mik/joegame/assets/maps/wang_test.json")
          (map (make-instance 'tiledmap:tiled-map :width 16 :height 16 ))
          (mm (tiledmap:map-to-json
                (progn
                  (tiledmap:add-tileset map
                    (tiledmap:make-tileset-from-image "/home/mik/joegame/assets/images/terr_water.png"))
                  (setf (tiledmap:layers map)
                    (list (make-instance 'tiledmap:tilelayer
                            :width 16 :height 16
                            :data (mapcar #'(lambda (i) (if (eql i 0) 0 (+ 1 i))) *thick-terrain-wang-raw*))))
                  map))))
    (utils:save-file mpath mm)
    (tiledmap:fix-map-tilesets-path mpath "../images/")))

(defun show-terr-names ()
  (mapcar #'(lambda (row) (mapcar #'(lambda (items) (mapcar #'name items)) row))
    (grid-to-list
      (collect-terrain-stacks
        (__ :field
          (perlin~ 1.6 10
            (list
              (__ :ocean)
              (__ :field)
              (__ :forest)
              (__ :desert))))
        0 0  4 4))))


(defun get-terr-debug-img (out conf)
  (save-image-file
    out
    (render-conf-terr-img
      conf 5 5))
  (magicklib:scale-image out (* 4 16) out))

(get-terr-debug-img
  "tt.png"
  (__ :dirt
    (perlin~ 1.6 11
      (list
        (__ :desert)
        (__ :ocean)))))


(defun get-terr-debug-img* (w h conf)
  (progn
    (save-image-file
      "tt.png"
      (render-conf-terr-img
        conf
        w
        h))
    (magicklib:scale-image "tt.png" (* 4 16) "tt.png")))

(defun get-terr-debug-img** (outpath w h conf)
  (progn
    (save-image-file
      outpath
      (render-conf-img
        conf
        w
        h))
    (magicklib:scale-image outpath(* 4 16) outpath)))




(defun render-terr-images ()
  (ensure-directories-exist "./terr-images/")
  (loop
    :for i
    :in *area-set*
    :do (get-terr-debug-img**
          (format nil "terr-images/~a.png"
            (getf (cdr i) :name))
          5 5
          (getf (cdr i) :signal)))
  (uiop:run-program '("./tile_terrs.sh"))
  (uiop:run-program '("rm" "-rf" "terr-images/")))
