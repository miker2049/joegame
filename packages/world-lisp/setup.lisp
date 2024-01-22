(in-package cl-user)
(defpackage setup-game (:use :cl :worldconf) (:export setup))
(in-package setup-game)

(defun make-world-view-tiles (wv scale &key (cols 8) (rows 8) (dir "./") (suffix "tile"))
  "Creates cols x rows images in dir.  Images are tiles, each sized based on
the worldview dimension / cols or rows respectively.  The tiles form a picture of
the signal in worldview. wv is a world-view"
  (let ((tile-width (floor (/ (wv-width wv) cols)))
        (tile-height (floor (/ (wv-height wv) rows))))
    (ensure-directories-exist dir)
    (loop :for tile-y :below rows
          :do (loop :for tile-x :below cols
                    :do (render:save-image-file
                         (format nil "~a~a_~a_~a.png"
                                 dir tile-x tile-y suffix)
                         (make-world-image-scaled
                          (wv-sig wv)
                          tile-width
                          tile-height
                          scale
                          (+ (wv-xoff wv) (* tile-width  tile-x))
                          (+ (wv-yoff wv) (* tile-height tile-y))))))))
(defun make-big-world-view-tiles (wv &optional dir)
  "This makes zones pictures."
  (make-world-view-tiles wv 1/16 :cols 16 :rows 16 :dir dir :suffix "zone"))

(defun make-zone-tiles (wv zx zy &optional dir)
  "Make the non scaled tiles of a particular zone."
  (let ((wsize (wv-width wv)))
    (let ((xo
            (+ (* zx wsize)
               (* (wv-xoff wv) 16)))
          (yo (+ (* zy wsize)
                 (* (wv-yoff wv) 16))))
      (make-world-view-tiles
       (make-world-view (wv-sig wv) xo yo wsize wsize)
       1
       :cols 16
       :rows 16
       :dir dir
       :suffix (format nil "~a_~a_zone" zx zy)))))


(defun make-all-tiles (wv &optional dir)
  (make-big-world-view-tiles wv dir)
  (loop :for y :below 16
        :do (loop :for x :below 16
                  :do (make-zone-tiles wv x y dir))))

(defmacro threaded-all-zone-tiles (wv &optional dir)
  `(promise-all
    (map
     'list
     #'(lambda (y)
         (await
          (loop :for x :below 16
                :do (make-zone-tiles ,wv x y ,dir))))
     (loop :for i :below 16 :collect i))
    (lambda ()
      (print "done!"))))

(defun make-all-tiles* (wv &optional dir)
  (threaded-all-zone-tiles wv dir)
  (make-big-world-view-tiles wv dir))

(defun make-big-pictures (wv dir &key force)
  (ensure-directories-exist dir)
  (let ((filepath (format nil "~a~a" dir "world.png")))
    (unless (or (probe-file filepath) force)
      (render-big-img
       (wv-sig wv)
       (wv-width wv)
       (wv-height wv)
       filepath
       :threads 16
       :scale 1/16
       :xoff (wv-xoff wv)
       :yoff (wv-yoff wv))))
  (loop :for y :below 16
        :do (loop :for x :below 16
                  :do
                     (let ((filepath (format nil "~azone_~a_~a.png" dir x y)))
                       (unless (or (probe-file filepath) force)
                         (render-big-img
                          (wv-sig wv)
                          (wv-width wv)
                          (wv-height wv)
                          filepath
                          :threads 16
                          :scale 1
                          :xoff (* (+ (* x 100) (wv-xoff wv )) 16) ;; the tile size of world.png is 100x100, and it is scaled 1/16
                          :yoff (* (+ (* y 100) (wv-yoff wv )) 16) ))))))



(defun install-terrains (dir)
  (loop :for ts
          :in
          (mapcar #'
           (lambda (it)
             (getf (cdr it) :tileset))
           *terrain-set*)
        :do (tiledmap:install-tileset ts :dir dir)))


(defun setup ()
  (ensure-directories-exist config:*terrain-directory*)
  (ensure-directories-exist config:*tiles-directory*)
  (install-terrains (namestring config:*terrain-directory*))
  (make-big-pictures *world-view* (namestring config:*tiles-directory*)))

