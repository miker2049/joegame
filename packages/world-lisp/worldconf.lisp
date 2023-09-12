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

(defun terrain-filler-from-collection (terrain collection children display-name)
  (if-let ((terr
             (cdr
               (assoc terrain collection))))
    (filler~ (list
               (make-instance 'terrain
                 :id (getf terr :id)
                 :name (getf terr :name)
                 :display-name display-name
                 :color (getf terr :color)
                 :children  children )))
    (error "Cant find terrain!")))

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
(defvar *connect-wang-corners* t)
(defvar *terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(defvar *terrain-wang-raw-alt* nil "connecting corners")
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

(setf *terrain-wang-raw-alt*
  '(0 0 0 0 0 14 19 20 0 0 0 0 0 6 27 28 0 0 0 0 0 0 13 26 0 0 0 8 0 12 21 22 0 0
     0 0 0 0 0 14 0 0 7 20 0 18 27 28 0 0 0 0 0 0 0 0 0 8 25 26 0 24 21 22 0 0 0 0
     7 21 21 22 0 0 0 0 7 26 27 28 9 0 0 0 21 19 20 28 1 2 3 4 19 20 21 22 25 9 0 0
     19 25 26 21 19 20 21 22 25 26 27 28 19 20 5 0 25 26 21 15 25 26 27 28 19 20 21
     22 25 26 35 0 19 20 21 22 19 20 19 9 25 26 27 28 19 16 0 0 25 26 27 28 19 20
     25 26 19 20 21 22 15 0 0 0 31 32 33 34 19 20 21 21 21 21 21 21 0 0 0 0 0 0 0 0
     14 26 27 28 14 20 21 22 19 20 11 0 20 21 22 19 20 21 22 9 25 26 27 28 25 26 17
     0 26 27 28 25 26 27 28 19 19 20 21 22 19 20 23 0 20 21 22 19 19 20 21 22 25 26
     27 28 25 26 29 0 26 27 28 16 25 26 27 28 19 20 21 22))

(if *connect-wang-corners*
  (setf *terrain-wang-raw* *terrain-wang-raw-alt*))

(defvar *thick-terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(setf *thick-terrain-wang-raw* '(0 0 0 0 0 14 38 20 0 0 0 0 0 6 27 28 0 0 0 0 0 0 14 38 0 0 0 8 0 12 21 22 0 0
                                  0 0 0 0 0 14 0 0 7 36 0 18 27 28 0 0 0 0 0 0 0 0 0 8 36 26 0 24 21 22 0 0 0 0
                                  7 36 19 22 0 0 0 0 7 26 27 28 9 0 0 0 36 19 19 19 1 2 3 4 19 20 21 22 39 9 0 0
                                  19 19 19 37 19 20 21 22 25 26 27 28 19 39 5 0 25 19 37 15 25 26 27 28 19 20 21
                                  22 25 37 15 0 19 20 21 22 19 19 39 9 25 26 27 28 37 15 0 0 25 26 27 28 19 19
                                  19 39 19 20 21 22 15 0 0 0 31 32 33 34 38 19 19 19 25 26 27 28 0 0 0 0 0 0 0 0
                                  14 38 19 19 14 20 21 22 19 20 11 0 20 21 22 19 20 21 22 9 25 26 27 28 25 26 17
                                  0 26 27 28 25 26 27 28 19 19 20 21 22 19 20 23 0 20 21 22 19 19 20 21 22 25 26
                                  27 28 25 26 29 0 26 27 28 16 25 26 27 28 19 20 21 22))
(defvar *terrain-wang-tiles* nil)
(setf *terrain-wang-tiles*
  (grid:chunk-list-to-grid
    *terrain-wang-raw-alt*
    16))

(defvar *thick-terrain-wang-tiles* nil)
(setf *thick-terrain-wang-tiles*
  (grid:chunk-list-to-grid
    *thick-terrain-wang-raw*
    16))

(defvar *wang-tile-set* nil
  "The collection of different generic wang tile arrangements the world can use")
(setf *wang-tile-set* (list
                        :terrain *terrain-wang-tiles*
                        :thick-terrain *thick-terrain-wang-tiles*))

(defvar *cliff-wang-tiles* nil)

(defun __ (terrain &rest children)
  (area-filler-from-collection terrain *area-set* children))

(defun _ (display-name terrain &rest children)
  "the  is unicode hex code 2593"
  (terrain-filler-from-collection terrain *terrain-set* children
    (or display-name "")))

(setf *terrain-set*
  (mapcar
    #'(lambda (item)
        `(,(cadr item) .
           (:color ,(parse-integer
                      (string-left-trim "#" (getf  (cddr item) :color))
                      :radix 16)
             :name ,(getf (cddr item) :name)
             :id ,(car item)
             :wang-tiles ,(getf (cddr item) :wang-tiles)
             :priority ,(* 1000 (car item))
             :tileset ,(getf (cddr item) :tileset))))
    (utils:enumerate
      `((:deep-underwater . (:name "deep-underwater"
                              :color "#B7C4CF"
                              :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                         :margin 0 :spacing 0)
                              :wang-tiles :terrain))
         (:ocean . (:name "ocean"
                     :color "#B7C4CF"
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                :margin 0 :spacing 0)
                     :wang-tiles :thick-terrain))
         (:simple-dirt . (:name "simple-dirt"
                           :color "#007E76"
                           :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_dirt.png")
                                      :margin 0 :spacing 0)
                           :wang-tiles :terrain))
         (:dirt . (:name "dirt"
                    :color "#007E76"
                    :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_dirt.png")
                               :margin 0 :spacing 0)
                    :wang-tiles :terrain))
         (:grass . (:name "grass"
                     :color "#A0D8B3"
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_grass.png")
                                :margin 0 :spacing 0)
                     :wang-tiles :terrain))
         (:deep-grass . (:name "deep-grass"
                          :color "#A2A378"
                          :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                     :margin 0 :spacing 0)
                          :wang-tiles :terrain))
         (:sand . (:name "sand"
                    :color "#EEE3CB"
                    :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                               :margin 0 :spacing 0)
                    :wang-tiles :terrain))
         (:hard-sand . (:name "hard-sand"
                         :color "#D7C0AE"
                         :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_sand.png")
                                    :margin 0 :spacing 0)
                         :wang-tiles :terrain))
         (:stone . (:name "stone"
                     :color "#D6E8DB"
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_sand.png")
                                :margin 0 :spacing 0)
                     :wang-tiles :terrain))
         (:cliff . (:name "cliff"
                     :color "#000000"
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                :margin 0 :spacing 0)
                     :wang-tiles :terrain))
         (:stone . ( :name "stone"
                     :color "#F6F1F1"
                     :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                                :margin 0 :spacing 0)
                     :wang-tiles :terrain))
         (:ice . ( :name "ice"
                   :color "#AFD3E2"
                   :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                              :margin 0 :spacing 0)
                   :wang-tiles :terrain))
         (:clay . (:name "clay"
                    :color "#C38154"
                    :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_clay.png")
                               :margin 0 :spacing 0)
                    :wang-tiles :terrain))
         (:lake . (:name "lake"
                    :color  "#AFD3E2"
                    :tileset (:imagepath ,(truename "~/joegame/assets/images/terr_water.png")
                               :margin 0 :spacing 0)
                    :wang-tiles :thick-terrain))))))

(setf *area-set*
  (mapcar
    #'(lambda (item)
        `(,(cadr item) .
           (:color ,(parse-integer
                      (string-left-trim "#" (getf  (cddr item) :color))
                      :radix 16)
             :name ,(getf (cddr item) :name)
             :id ,(car item)
             :signal ,(getf (cddr item) :signal))))
    (utils:enumerate
      `((:depths . (:name "depths"
                     :color "#313e49"
                     :signal ,(_ "depths" :deep-underwater) ))

         (:trench . (:name "trench"
                      :color "#5c758a"
                      :signal ,(_ "trench" :deep-underwater)))

         (:soil . (:name "soil"
                    :color "#644117"
                    :signal ,(_ "soil" :simple-dirt)))

         (:ocean . (:name "ocean" :color "#B7C4CF" :signal ,(_ "ocean" :ocean)))

         (:shore . (:name "shore" :color "#e0b483" :signal ,(_ "shore" :sand)))

         (:late-shore . (:name "late-shore" :color "#c69763" :signal ,(_ "late-shore" :hard-sand)))

         (:coastal . (:name "coastal" :color "#c6ad74" :signal ,(_ "coastal" :hard-sand)))

         (:grass-and-sand . (:name "grass-and-sand" :color "#839450" :signal ,(_ "grass-and-sand" :grass)))

         (:rocky-sand . (:name "rocky-sand" :color "#B18E68" :signal ,(_ "rocky-sand" :sand)))

         (:desert . ( :name "desert"
                      :color "#ffffd3"
                      :signal ,(warp& (perlin~ 0.31 108 (list (_ "desert" :dirt) (_ "desert" :clay))) :amount 200)))

         (:desert-graveyard . (:name "desert-graveyard" :color "#faa06b" :signal ,(_ "desert-graveyard" :hard-sand)))

         (:dead-forest . (:name "dead-forest" :color "#f4c992" :signal ,(_ "dead-forest" :hard-sand)))

         (:old-pavement-desert . (:name "old-pavement-desert" :color "#b89a74" :signal ,(_ "old-pavement-desert" :hard-sand)))

         (:boulder-meadow-desert . (:name "boulder-meadow-desert" :color "#96794d" :signal ,(_ "boulder-meadow-desert" :hard-sand)))

         (:water-desert . (:name "water-desert" :color "#c5e9bd" :signal ,(_ "water-desert" :hard-sand)))

         (:field . (:name "field" :color "#3590e" :signal ,(warp&
                                                             (perlin~ 0.11 1
                                                               (list (_ "field" :hard-sand) (_ "field" :grass)))
                                                             :amount 100)))

         (:old-pavement-field . (:name "old-pavement-field" :color "#8f8f51" :signal ,(_ "old-pavement-field" :hard-sand)))

         (:forest . (:name "forest" :color "#293b09" :signal ,(_ "forest" :grass)))

         (:forest-magic . (:name "forest-magic" :color "#2e4114" :signal ,(_ "forest-magic" :hard-sand)))

         (:water-forest . (:name "water-forest" :color "#2e352e" :signal ,(_ "water-forest" :hard-sand)))

         (:old-pavement-forest . (:name "old-pavement-forest" :color "#444353" :signal ,(_ "old-pavement-forest" :hard-sand)))
         (:lake . (:name "lake" :color "#444353" :signal ,(_ "lake" :lake)))))))

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

