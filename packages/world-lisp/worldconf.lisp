(in-package worldconf)

(defvar *worldconf* nil
  "The active terrain/signal graph for generating worlds (area pixels)")

(defvar *image-dir* nil
  "Dir with images.")


(setf *image-dir*  "~/joegame/assets/images/")

(defvar *land-signal* nil
  "Main continent/land signal, answers the question:
\"Is this spot on ocean, or is this spot on land?\".")

(defvar *continent-signal* nil
  "Inner signal of land.")

(defvar *terrain-set* nil)
(defvar *area-set* nil)


(defun make-area-config (&key sym color id signal)
  `(,sym . (:id ,id
             :name ,(string-downcase
                      (symbol-name sym))
             :color ,color
             :signal ,signal)))

(defun terrain-filler-from-collection (terrain collection children)
  (if-let ((terr
             (cdr
               (assoc terrain collection))))
    (filler~ (list
               (make-instance 'terrain
                 :id (getf terr :id)
                 :name (getf terr :name)
                 :color (getf terr :color)
                 :children  children )))))

(defun area-filler-from-collection (terrain collection children)
  (if-let ((terr
             (cdr
               (assoc terrain collection))))
    (filler~ (list
               (make-instance 'area
                 :id (getf terr :id)
                 :name (getf terr :name)
                 :color (getf terr :color)
                 :signal (getf terr :signal)
                 :children  children )))))

(defparameter *conf-output* "worldconf.json")
(defvar *terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(setf *terrain-wang-raw*
  (list
    0  0  0  0    0  14 19 20   0  0  0  0     0  6  27 28
    0  0  0  0    0  0  13 26   0  0  0  8     0  12 21 22
    0  0  0  0    0  0  0  14   0  0  7  20    0  18 27 28
    0  0  0  0    0  0  0  0    0  8  25 26    0  24 21 22

    0  0  0  0    0  13 21 22   0  0  0  0     7  26 27 28
    9  0  0  0    9  0  13 28   1  2  3  4     19 20 21 22
    25 9  0  0    19 10 0  14   19 20 21 22    25 26 27 28
    19 20 5  0    25 26 9  0    25 26 27 28    19 20 21 22

    25 26 35 0    19 20 21 22   19 20 15 0     25 26 27 28
    19 16 0  0    25 26 27 28   25 15 0  7     19 20 21 22
    15 0  0  0    31 32 33 34   15 0  7  22    25 26 27 28
    0  0  0  0    0  0  0  0    0  7  27 28    14 20 21 22

    19 20 11 0    20 21 22 19   20 21 22 9     25 26 27 28
    25 26 17 0    26 27 28 25   26 27 28 19    19 20 21 22
    19 20 23 0    20 21 22 19   19 20 21 22    25 26 27 28
    25 26 29 0    26 27 28 16   25 26 27 28    19 20 21 22))

(defvar *thick-terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(setf *thick-terrain-wang-raw* (list 0 0 0 0 0 14 38 20 0 0 0 0 0 6 27 28 0 0 0 0 0 0 14 38 0 0 0 8 0 12 21 22 0 0
                                 0 0 0 0 0 14 0 0 7 36 0 18 27 28 0 0 0 0 0 0 0 0 0 8 36 26 0 24 21 22 0 0 0 0
                                 0 14 38 22 0 0 0 0 7 26 27 28 9 0 0 0 9 0 14 38 1 2 3 4 19 20 21 22 39 9 0 0
                                 39 10 0 14 19 20 21 22 25 26 27 28 19 39 5 0 25 39 9 0 25 26 27 28 19 20 21
                                 22 25 37 15 0 19 20 21 22 19 37 15 0 25 26 27 28 37 15 0 0 25 26 27 28 37 15
                                 0 7 19 20 21 22 15 0 0 0 31 32 33 34 15 0 7 36 25 26 27 28 0 0 0 0 0 0 0 0 0
                                 7 36 28 14 20 21 22 19 20 11 0 20 21 22 19 20 21 22 9 25 26 27 28 25 26 17 0
                                 26 27 28 25 26 27 28 19 19 20 21 22 19 20 23 0 20 21 22 19 19 20 21 22 25 26
                                 27 28 25 26 29 0 26 27 28 16 25 26 27 28 19 20 21 22))
(defvar *terrain-wang-tiles* nil)
(setf *terrain-wang-tiles*
  (grid:chunk-list-to-grid
    *terrain-wang-raw*
    16))

(defvar *thick-terrain-wang-tiles* nil)
(setf *thick-terrain-wang-tiles*
  (grid:chunk-list-to-grid
    *thick-terrain-wang-raw*
    16))

(defvar *cliff-wang-tiles* nil)

(defun __ (terrain &rest children)
  (area-filler-from-collection terrain *area-set* children))
(defun _ (terrain &rest children)
  "the  is unicode hex code 2593"
  (terrain-filler-from-collection terrain *terrain-set* children))

(setf *terrain-set*
  `((:deep-underwater . ( :id 0
                          :name "deep-underwater"
                          :color #xB7C4CF
                          :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                     :margin 0 :spacing 0)
                          :wang-tiles ,*terrain-wang-tiles*))
     (:ocean . (:id 0 :name "ocean" :color #xB7C4CF :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                               :margin 0 :spacing 0)
                 :wang-tiles ,*thick-terrain-wang-tiles*))
     (:dirt . (:id 1 :name "dirt" :color #x007E76 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_dirt.png")
                                                             :margin 0 :spacing 0)
                :wang-tiles ,*terrain-wang-tiles*))
     (:grass . (:id 2 :name "grass" :color #xA0D8B3 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_grass.png")
                                                               :margin 0 :spacing 0)
                 :wang-tiles ,*terrain-wang-tiles*))
     (:deep-grass . (:id 3 :name "deep-grass" :color #xA2A378 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                                         :margin 0 :spacing 0)
                      :wang-tiles ,*terrain-wang-tiles*))
     (:sand . (:id 4 :name "sand" :color #xEEE3CB :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                             :margin 0 :spacing 0)
                :wang-tiles ,*terrain-wang-tiles*))
     (:hard-sand . ( :id 5
                     :name "hard-sand"
                     :color #xD7C0AE
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_sand.png")
                                :margin 0 :spacing 0)
                     :wang-tiles ,*terrain-wang-tiles*))
     (:stone . (:id 6 :name "stone" :color #xD6E8DB :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_sand.png")
                                                               :margin 0 :spacing 0)
                 :wang-tiles ,*terrain-wang-tiles*))
     (:cliff . (:id 7 :name "cliff" :color #x000000 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                               :margin 0 :spacing 0)
                 :wang-tiles ,*terrain-wang-tiles*))
     (:stone . (:id 8 :name "stone" :color #xF6F1F1 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                               :margin 0 :spacing 0)
                 :wang-tiles ,*terrain-wang-tiles*))
     (:ice . (:id 9 :name "ice" :color #xAFD3E2 :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                                           :margin 0 :spacing 0)
               :wang-tiles ,*terrain-wang-tiles*))
     (:clay . ( :id 10
                :name "clay"
                :color #xC38154
                :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_clay.png")
                           :margin 0 :spacing 0)
                :wang-tiles ,*terrain-wang-tiles*))))

(setf *area-set*
  (mapcar
    #'(lambda (item)
        (make-area-config
          :sym (cadr item)
          :color (parse-integer
                   (string-left-trim "#" (getf  (cddr item) :color))
                   :radix 16)
          :id (car item)
          :signal (getf (cddr item) :signal)))
    (utils:enumerate
      `((:depths . (:name "depths"
                     :color "#313e49"
                     :signal ,(_ :deep-underwater) ))
         (:trench . (:name "trench"
                      :color "#5c758a"
                      :signal ,(_ :trench)))
         (:ocean . (:name "ocean" :color "#B7C4CF" :signal ,(_ :ocean)))
         (:shore . (:name "shore" :color "#e0b483" :signal ,(_ :sand)))
         (:late-shore . (:name "late-shore" :color "#c69763" :signal ,(_ :hard-sand)))
         (:coastal . (:name "coastal" :color "#c6ad74" :signal ,(_ :hard-sand)))
         (:grass-and-sand . (:name "grass-and-sand" :color "#839450" :signal ,(_ :grass)))
         (:rocky-sand . (:name "rocky-sand" :color "#B18E68" :signal ,(_ :sand)))
         (:desert . ( :name "desert"
                      :color "#ffffd3"
                      :signal ,(warp& (perlin~ 0.31 108 (list (_ :dirt) (_ :clay))) :amount 200)))
         (:desert-graveyard . (:name "desert-graveyard" :color "#faa06b" :signal ,(_ :hard-sand)))
         (:dead-forest . (:name "dead-forest" :color "#f4c992" :signal ,(_ :hard-sand)))
         (:old-pavement-desert . (:name "old-pavement-desert" :color "#b89a74" :signal ,(_ :hard-sand)))
         (:boulder-meadow-desert . (:name "boulder-meadow-desert" :color "#96794d" :signal ,(_ :hard-sand)))
         (:water-desert . (:name "water-desert" :color "#c5e9bd" :signal ,(_ :hard-sand)))
         (:field . (:name "field" :color "#3590e" :signal ,(warp&
                                                             (perlin~ 0.11 1
                                                               (list (_ :hard-sand) (_ :grass)))
                                                             :amount 100)))
         (:old-pavement-field . (:name "old-pavement-field" :color "#8f8f51" :signal ,(_ :hard-sand)))
         (:forest . (:name "forest" :color "#293b09" :signal ,(_ :grass)))
         (:forest-magic . (:name "forest-magic" :color "#2e4114" :signal ,(_ :hard-sand)))
         (:water-forest . (:name "water-forest" :color "#2e352e" :signal ,(_ :hard-sand)))
         (:old-pavement-forest . (:name "old-pavement-forest" :color "#444353" :signal ,(_ :hard-sand)))))))

(setf *land-signal*
  (circle&
    (circle&
      (not-circle&
        (in-circle&
          (*&
            (perlin~ 0.0004 108 '())
            1.45)
          (point 10000 10000)
          4000 -0.5)
        (point 12500 7500)
        8000 1)
      (point 6000 5200)
      5000 2 0.76)
    (point 5500 17000)
    5000 1.7))

(setf *continent-signal*
  (router&&
    (warp& (stretch& *land-signal* :n 0.5 :end 1.0) :amount 500)
    (0.1 . (__ :shore))
    (0.2 . (__ :late-shore))
    (0.5 .
      (router&&
        (warp&
          (stretch&
            (stretch& *land-signal* :n 0.5 :end 1.0)
            :n 0.2 :end 0.5)
          :amount 4000
          :offset-a1 (point 1000 0) :offset-b2 (point 1000 5000))
        (0.5 . (__ :coastal))
        (1.0 . (__ :desert))))
    (0.67 . (router&& (warp&
                        (perlin~ 0.0007 10 '())
                        :amount 1000)
              (0.1 . (__ :field
                       (child-sigg
                         (warp&
                           (stretch&
                             (perlin~ 0.01 10 '())
                             :n 0 :end 0.1)
                           :amount 1000)
                         (list
                           (__ :field)
                           (__ :grass-and-sand)))))
              (0.5 . (__ :late-shore))
              (0.9 . (__ :desert))
              (1.0 . (__ :field))))
    (0.8 . (router&& (warp&
                       (perlin~ 0.004 10 '())
                       :amount 2000)
             (0.2 . (__ :coastal))
             (0.8 . (__ :rocky-sand))
             (1.0 . (__ :desert))))
    (1 .
      (__ :old-pavement-desert
        (child-sigg (stretch& (stretch& *land-signal* :n 0.5 :end 1.0) :n 0.8 :end 1.0)
          (list
            (perlin~ 0.001 108
              (list
                (__ :grass-and-sand)
                (__ :old-pavement-desert)
                (__ :field)))
            (__ :forest
              (child-sigg
                (warp&
                  (perlin~ 0.01 109 nil)
                  :amount 100)
                (list
                  (__ :forest)
                  (__ :old-pavement-field)
                  (__ :old-pavement-forest))))))))))

(setf *worldconf*
  (__ :ocean
    (child-sigg *land-signal*
      (list
        (router&& (stretch& *land-signal* :n 0 :end 0.5)
          (0.8 .
            (__ :depths))
          (0.98 .
            (__ :trench))
          (1 .
            (__ :ocean)))
        *continent-signal*))))


;; general testing
(defun map-debug ()
  (let ((mpath "/home/mik/joegame/assets/maps/conf_test.json")
         (mm (tiledmap:map-to-json
               (let ((map
                       (make-map-from-wang-val-grids (collect-terrain-wang-vals
                                                       (__ :ocean
                                                         (perlin~ 1.6 10
                                                           (list
                                                             (__ :desert)
                                                             (__ :ocean)
                                                             )))
                                                       0 0 5 5))))
                 ;; extra config
                 ;; (setf (tiledmap:backgroundcolor map) "#21ab60")
                 map))))
    (utils:save-file mpath mm)
    (tiledmap:fix-map-tilesets-path mpath "../images/")))


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


(defun get-terr-debug-img ()
  (progn
    (save-image-file
      "tt.png"
      (render-conf-terr-img
        (__ :ocean
          (perlin~ 1.6 10
            (list
              (__ :desert)
              (__ :ocean)
              ))
          :amount 0)
        5        5))
    (magicklib:scale-image "tt.png" (* 4 16) "tt.png")))

;; (map-debug)
;; (get-terr-debug-img)
