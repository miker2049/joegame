(in-package worldconf)

;; general testing
(defun map-debug (out conf ww hh &key (x 0) (y 0) (image-path "../images/"))
  (let ((mpath out)
        (mm (tiledmap:map-to-json
             (get-tiled-map-from-conf conf x y ww hh))))
    (utils:save-file mpath mm)
    (tiledmap:fix-map-tilesets-path mpath image-path)))

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

(defun get-wang-tiles-from-map (mappath)
  "Expects it to be the first layer. Argument is path to map.
Just spits it out."
  (mapcar
   #'(lambda (item) (if (eql item 0) 0 (- item 1)))
   (getf
    (car
     (getf
      (jojo:parse
       (read-file-into-string mappath))
      :|layers|))
    :|data|)))

(defun show-terr-names (conf x y w h)
  (mapcar #'(lambda (row) (mapcar #'(lambda (items) (mapcar #'name items)) row))
          (grid-to-list
           (collect-terrain-stacks
            conf x y w h))))


(defun render-and-scale (out conf w h &key (scale 64) (x 0) (y 0))
  "Scales the output image, does not sample from conf. Meant for small to large."
  (save-image-file
    out
    (render-conf-img
      conf w h x y))
  ;; (magicklib:scale-image out scale out)
  (uiop:run-program (list "magick" "convert" out "-scale"
                      (format nil "~s%" (* scale 100)) out)))


(defun terr-images-debug ()
  "Produces an image which shows a representation of each terrain signal,
showing a colorized wang-value-grid."
  (ensure-directories-exist "./terr-images/")
  (loop
    :for i
    :in *area-set*
    :do (render-and-scale
          (format nil "terr-images/~a.png"
            (getf (cdr i) :name))
          (getf (cdr i) :signal)
          5 5))
  (uiop:run-program '("./tile_terrs.sh"))
  (uiop:run-program '("rm" "-rf" "terr-images/")))


(defun show-terr-names-from-conf (conf x y w h)
  (mapcar #'(lambda (item) (map-grid-values item #'name))
    (get-terrain-grids conf x y w h)))

(progn
  (setf *tc* (__ :soil
               (perlin~ 1.6 11
                 (list
                   (__ :desert)
                   (__ :field)
                   (__ :lake)))))
  ;; (render-and-scale "tt.png" *tc* 8 8)
  (map-debug  "/home/mik/joegame/assets/maps/conf_test.json" *tc* 8 8 )
  )


(defun full-world-pic ()
  (render-big-img *worldconf* 1600 1600 "full-pic.png" :threads 16 :scale 1/16 :yoff -150 :xoff -150))


(setf *tm-with-props* (let ((tm (get-tiled-map 10 10 10 10)))
                        (tiledmap:add-property tm "integer" "foo" 420)
                        (tiledmap:add-property tm "integer" "foor" 820)
                        tm))
