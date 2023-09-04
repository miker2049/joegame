(in-package worldconf)

(defvar *worldconf* nil
  "The active terrain/signal graph for generating worlds (area pixels)")

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

(defvar *terrain-wang-tiles* nil)
(setf *terrain-wang-tiles*
  (grid:chunk-list-to-grid
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
      25 26 29 0    26 27 28 16   25 26 27 28    19 20 21 22)
    16))

(defvar *cliff-wang-tiles* nil)

(defun __ (terrain &rest children)
  (area-filler-from-collection terrain *area-set* children))
(defun _ (terrain &rest children)
  "the  is unicode hex code 2593"
  (terrain-filler-from-collection terrain *terrain-set* children))

(setf *terrain-set* '((:deep-underwater . (:id 0 :name "deep-underwater" :color #xB7C4CF))
                       (:ocean . (:id 0 :name "ocean" :color #xB7C4CF))
                       (:dirt . (:id 1 :name "dirt" :color #x967E76))
                       (:grass . (:id 2 :name "grass" :color #xA0D8B3))
                       (:deep-grass . (:id 3 :name "deep-grass" :color #xA2A378))
                       (:sand . (:id 4 :name "sand" :color #xEEE3CB))
                       (:hard-sand . (:id 5 :name "hard-sand" :color #xD7C0AE))
                       (:stone . (:id 6 :name "stone" :color #xD6E8DB))
                       (:cliff . (:id 7 :name "cliff" :color #x000000))
                       (:stone . (:id 8 :name "stone" :color #xF6F1F1))
                       (:ice . (:id 9 :name "ice" :color #xAFD3E2))
                       (:clay . (:id 10 :name "clay" :color #xC38154))))

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
         (:desert . (:name "desert" :color "#ffffd3"  :signal ,(_ :hard-sand)))
         (:desert-graveyard . (:name "desert-graveyard" :color "#faa06b" :signal ,(_ :hard-sand)))
         (:dead-forest . (:name "dead-forest" :color "#f4c992" :signal ,(_ :hard-sand)))
         (:old-pavement-desert . (:name "old-pavement-desert" :color "#b89a74" :signal ,(_ :hard-sand)))
         (:boulder-meadow-desert . (:name "boulder-meadow-desert" :color "#96794d" :signal ,(_ :hard-sand)))
         (:water-desert . (:name "water-desert" :color "#c5e9bd" :signal ,(_ :hard-sand)))
         (:field . (:name "field" :color "#3590e" :signal ,(_ :hard-sand)))
         (:old-pavement-field . (:name "old-pavement-field" :color "#8f8f51" :signal ,(_ :hard-sand)))
         (:forest . (:name "forest" :color "#293b09" :signal ,(_ :hard-sand)))
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
