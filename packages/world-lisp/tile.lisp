(defpackage :tile (:use :cl :cl-tiled))

(in-package :tile)

(defparameter *wangs*
    (load-map "/home/mik/joegame/assets/maps/desert-stamps2.json"))

(map 'list #'(lambda (l) (layer-name l))
    (map-layers *wangs*))

(tile-id
    (cell-tile
        (car (layer-cells (nth 2 (map-layers *wangs*))))))



(defun get-tileset-and-id (layer x y)
    (let ((tile
              (cell-tile
                  (nth (xyi x y (map-width (layer-map layer))) (layer-cells layer)))))
        (list
            (tileset-name (tile-tileset tile))
            (tile-id tile))))

(map 'list #'(lambda (l) (get-tileset-and-id l 1 3)) (map-layers *wangs*))
