(in-package :worldconf)

(setf *terrain-set*
      (make-terrain-set
       `((:ocean . (:name "ocean"
                    :color "#B7C4CF"
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_ocean.png"))
                    :wang-tiles :thick-terrain))
         (:grass-blade . (:name "grass-blade" :color "#1A9C4F" :tileset
                                ,(tiledmap:make-tileset-from-image
                                  (get-asset-path "images/terr_grass.png"))
                          :wang-tiles :terrain)))))

(setf *area-set*
      (make-area-set
       `((:ocean . (:name "ocean" :color "#B7C4CF" :signal ,(_ "ocean" :ocean)))
         (:grass-and-sand . (:name "grass-and-sand" :color "#839450" :signal ,(_ "grass-and-sand" :grass-blade))))))


(let ((size (expt 256 2)))
  (setf *worldconf*
        (__ :ocean
            (<>
             (circle&
              (circle&
               (not-circle&
                (in-circle&
                 (perlin~ 0.00008 208 '())
                 (point (/ size 2) (/ size 2))
                 (* (/ size 2) 2/3) 1.2)
                (point (* size 1/3) (* size 2/3))
                (/ size 3) 0.5)
               (point (* size 1/3) (* size 3/4))
               (* size 1/5) 1.3)
              (point (* size 2/3) (* size 1/2))
              (* size 1/3) 0.6)
             0.0 (__ :ocean)
             0.5 (__ :grass-and-sand)))))
