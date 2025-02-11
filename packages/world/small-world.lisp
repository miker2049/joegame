(in-package :worldconf)

(setf *area-set*
      (make-area-set
       `((:ocean . (:name "ocean" :color "#4aa0df"
                    :signal ,(_ "ocean" :ocean)
                    :objects ()
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_ocean.png"))))
         (:grass . (:name "grass" :color "#1A9C4F"
                    :signal ,(_ "grass" :ocean)
                    :objects (
                              (1 . :|leafy-tree|)
                              (0.09 . :|shrub|)
                              (0.05 . :|grass-boulder|)
                              (0.05 . :|grass-floor-rock|)
                              (0.05 . :|grass-floor-rock-2|)
                              (0.1 . :|rock|)
                              (1 . :|space|)
                              )
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_grass.png"))))
         (:sand . (:name "sand" :color "#ead2bd"
                   :signal ,(_ "sand" :ocean)

                   :objects (
                             (0.2 . :|dead-tree|)
                             (2 . :|space|)
                             (0.2 . :|cow-skull|)
                             (0.3 . :|cactus|)
                             )
                   :tileset ,(tiledmap:make-tileset-from-image
                              (get-asset-path "images/terr_sand.png")))))))


(let ((size (expt 2 16)))
  (setf *worldconf*
        (__ :ocean
            (<>

             (circle&
              (circle&
               (not-circle&
                (in-circle&
                 (perlin~ 0.000008 208 '())
                 (point (/ size 2) (/ size 2))
                 (* (/ size 2) 2/3) 1.2)
                (point (* size 1/3) (* size 2/3))
                (/ size 3) 0.5)
               (point (* size 1/3) (* size 3/4))
               (* size 1/5) 1.3)
              (point (* size 2/3) (* size 1/2))
              (* size 1/3) 0.6)

             0.0 (__ :ocean)
             1/2 (   0 (__ :sand)
                       1/64 (__ :grass))))))
