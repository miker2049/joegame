(in-package worldconf)

(defvar *worldconf* nil
  "The active terrain/signal graph for generating worlds (area pixels)")

(defvar *image-dir* nil
  "Dir with images.")


(setf *image-dir*  "~/joegame/packages/joegame-assets/images/")

(defvar *land-signal* nil
  "Main continent/land signal, answers the question:
\"Is this spot on ocean, or is this spot on land?\".")

(defvar *continent-signal* nil
  "Inner signal of land.")

(defvar *terrain-set* nil)
(defvar *area-set* nil)
(defvar *jp* nil
  "Joegame particles")

(setq *jp*
      '((:name "depths-drop" :c1 "#313e49" :c2 "#313e49"
         :mask "smooth" :variations :simple)
        (:name "trench-drop" :c1 "#5c758a" :c2 "#5c758a"
         :mask "smooth" :variations :simple)
        (:name "ocean-drop" :c1 "#b7c4cf" :c2 "#b7c4cf"
         :mask "smooth" :variations :simple)
        (:name "lake-drop" :c1 "#4aa0df" :c2 "#4aa0df"
         :mask "smooth" :variations :simple)
        (:name "grass-blade" :c1 "#1a9c4f" :c2 "#32d083"
         :mask "terrain" :variations :all)
        (:name "dead-grass-blade" :c1 "#897f38" :c2 "#b7ab55"
         :mask "terrain" :variations :all)
        (:name "dirt-speck" :c1 "#967054" :c2 "#9f785a"
         :mask "smooth" :variations :all)
        (:name "rock-speck" :c1 "#464646" :c2 "#bfbfbf"
         :mask "terrain" :variations :sparse)
        (:name "gem" :c1 "#0055b6" :c2 "#003a9e"
         :mask "terrain" :variations :sparse)
        (:name "quartz" :c1 "#74453b" :c2 "#be9c92"
         :mask "terrain" :variations :sparse)
        (:name "clay" :c1 "#905932" :c2 "#905932"
         :mask "smooth" :variations :all)
        (:name "stone" :c1 "#9da8a9" :c2 "#adb8b9"
         :mask "smooth" :variations :all)
        (:name "glass" :c1 "#a8b77e" :c2 "#a8b77e"
         :mask "terrain" :variations :sparse)
        (:name "sand" :c1 "#e5bea6" :c2 "#ecd0b8"
         :mask "smooth" :variations :all)
        (:name "wet-sand" :c1 "#dbab69" :c2 "#daac70"
         :mask "smooth" :variations :all)
        (:name "pine-needle" :c1 "#7a3703" :c2 "#7b4602"
         :mask "terrain" :variations :sparse)
        (:name "piece-of-plastic-blue" :c1 "#0078f8" :c2 "#007bf9"
         :mask "terrain" :variations :sparse)
        (:name "piece-of-plastic-red" :c1 "#b51800" :c2 "#941b19"
         :mask "terrain" :variations :sparse)
        (:name "piece-of-plastic-yellow" :c1 "#ae9d11" :c2 "#c5b81d"
         :mask "terrain" :variations :sparse)
        (:name "bark" :c1 "#5c3624" :c2 "#ae785e"
         :mask "terrain" :variations :sparse)))

(defun terrain-filler-from-collection (terrain collection children display-name)
  (if-let ((terr
            (cdr
             (assoc terrain collection))))
    (make-instance 'terrain
                   :id (getf terr :id)
                   :name (getf terr :name)
                   :display-name display-name
                   :color (getf terr :color)
                   :children  children )
    (error "Cant find terrain!")))

(defun area-filler-from-collection (terrain collection children)
  (if-let ((terr
            (cdr
             (assoc terrain collection))))
    (make-instance 'area
                   :id (getf terr :id)
                   :name (getf terr :name)
                   :color (getf terr :color)
                   :signal (getf terr :signal)
                   :children  children )))

(defparameter *conf-output* "worldconf.json")
(defvar *connect-wang-corners* t)
(defvar *terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(defvar *terrain-wang-raw-alt* nil "connecting corners")
(defvar *terrain-wang-empty* nil "all zeros")
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
      '(0  0  0  0     0  14 19 20    0  0   0   0     0   6   27   28
        0  0  0  0     0  0  13 26    0  0   0   8     0   12  21   22
        0  0  0  0     0  0  0  14    0  0   7   20    0   18  27   28
        0  0  0  0     0  0  0  0     0  8   25  26    0   24  21   22

        0  0  0  0     7  21 21 22    0  0   0   0     7   26  27   28
        9  0  0  0     21 19 20 28    1  2   3   4     19  20  21   22
        25 9  0  0     19 25 26 21    19 20  21  22    25  26  27   28
        19 20 5  0     25 26 21 15    25 26  27  28    19  20  21   22

        25 26 35 0     19 20 21 22    19 20   19   9  25   26   27  28
        19 16 0  0     25 26 27 28    19 20   25   26 19   20   21  22
        15 0  0  0     31 32 33 34    19 20   21   21 21   21   21  21
        0  0  0  0     0  0  0  0     14 26   27   28 14   20   21  22

        19 20 11 0     20 21 22 19    20 21   22   9  25   26   27  28
        25 26 17 0     26 27 28 25    26 27   28   19 19   20   21  22
        19 20 23 0     20 21 22 19    19 20   21   22 25   26   27  28
        25 26 29 0     26 27 28 16    25 26   27   28 19   20   21  22))

(setf *terrain-wang-empty*
      (grid:chunk-list-to-grid
       (loop :for i :below 256 :collect 0)
       16))

(if *connect-wang-corners*
    (setf *terrain-wang-raw* *terrain-wang-raw-alt*))

(defvar *thick-terrain-wang-raw* nil "raw tiledata for wang arrangement of standard terrain image/tileset")
(setf *thick-terrain-wang-raw* '(
                                 0 0 0 0 0 14 38 20 0 0 0 0 0 6 27 28
                                 0 0 0 0 0 0 14 38 0 0 0 8 0 12 21 22
                                 0 0 0 0 0 0 0 14 0 0 7 36 0 18 27 28
                                 0 0 0 0 0 0 0 0 0 8 36 26 0 24 21 22
                                 0 0 0 0 7 36 19 22 0 0 0 0 7 26 27 28
                                 9 0 0 0 36 19 19 19 1 2 3 4 19 20 21 22
                                 39 9 0 0 19 19 19 37 19 20 21 22 25 26 27
                                 28 19 39 5 0 25 19 37 15 25 26 27 28 19 20
                                 21 22 25 37 15 0 19 20 21 22 19 19 39 9 25
                                 26 27 28 37 15 0 0 25 26 27 28 19 19 19 39
                                 19 20 21 22 15 0 0 0 31 32 33 34 38 19 19
                                 19 25 26 27 28 0 0 0 0 0 0 0 0 14 38
                                 19 19 14 20 21 22 19 20 11 0 20 21 22 19 20
                                 21 22 9 25 26 27 28 25 26 17 0 26 27 28 25
                                 26 27 28 19 19 20 21 22 19 20 23 0 20 21 22
                                 19 19 20 21 22 25 26 27 28 25 26 29 0 26 27
                                 28 16 25 26 27 28 19 20 21 22
                                 ))
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
                       :empty *terrain-wang-empty*
                       :terrain *terrain-wang-tiles*
                       :thick-terrain *thick-terrain-wang-tiles*))

(defvar *cliff-wang-tiles* nil)

(defun __ (terrain &rest children)
  (area-filler-from-collection terrain *area-set* children))

(defun _ (display-name terrain &rest children)
  "the  is unicode hex code 2593"
  (terrain-filler-from-collection terrain *terrain-set* children
                                  (or display-name "")))

(defvar *terrain-masks* nil
  "A list of paths to generic masks and their tiles that can be used to make
terrain images that will work with some wang-tile collection.")

(setf *terrain-masks* '(("./wang-masks/terr_wang-mask.png" :terrain)
                        ( "./wang-masks/terr_smooth_wang-mask.png" :terrain)))



(defun get-mask-file (mask-idx &optional level)
  (let ((maskfile (car (nth mask-idx *terrain-masks*))))
    (if level
        (render:noise-series-get-file maskfile level)
        maskfile)))

(defun get-mask-file-tiles (mask-idx)
  (cadr (nth mask-idx *terrain-masks*)))


(defmacro make-lazy-input-tileset
    (in-file mask path terr-options &rest args &key &allow-other-keys)
  `(tiledmap:make-lazy-tileset ,path 96 96
    #'(lambda (it)
        (render:create-terrain-file ,in-file ,mask (lgf-path it) ,@terr-options))
    :lazy t
    ,@args))

(defmacro make-lazy-noise-tileset
    (c1 c2 mask path terr-options &rest args &key &allow-other-keys)
  `(tiledmap:make-lazy-tileset
    ,path 96 96
    #'(lambda (it)
        (render:create-noise-terrain-file ,c1 ,c2 ,mask
                                          (lgf-path it)
                                          ,@terr-options))
    :lazy t
    ,@args))

(defmacro make-lazy-noise-tileset*
    (c1 c2 mask-idx level path terr-options &rest args &key &allow-other-keys)
  `(make-lazy-noise-tileset
    ,c1
    ,c2
    ,(get-mask-file mask-idx level)
    ,path
    ,terr-options
    ,@args))


(defmacro gen-terrain-noise-series
    (&key name c1 c2 (mask-idx 1) terr-options tileset-options enable-flags (dir "./"))
  "Generate a series of terrains from one template based on noise reduction of the mask.
`mask-idx' is an index of `*terrain-masks*'
`enable-flags' turn on and off the various new masks to be generated.
`terr-options' are extra args to `create-noise-terrain-file'.
`tileset-options' are for `make-lazy-noise-tileset'"
  (let ((thisname (gensym))
        (strname
          (string-downcase (symbol-name name))))
    (print name)
    `(append
      (mapcar
       #'(lambda (mask)
           (let ((,thisname (utils:fmt "~a_~a"
                                       ,strname
                                       (car
                                        (last (cl-ppcre:split "(\\-)" (pathname-name mask)))))))
             (cons
              (intern
               (string-upcase ,thisname)
               "KEYWORD")
              (list
               :name ,thisname
               :wang-tiles (get-mask-file-tiles ,mask-idx)
               :color ,c1
               :tileset (make-lazy-noise-tileset
                         ,c1
                         ,c2
                         mask
                         (namestring
                          (merge-pathnames
                           (utils:fmt "~a_~a" (string-downcase ,name)
                                      (car
                                       (last (cl-ppcre:split "(\\-)" mask))))
                           ,dir))
                         ,terr-options
                         ,@tileset-options
                         :name ,thisname)))))
       (render:noise-series-files (get-mask-file ,mask-idx) ,enable-flags))
      (list (cons
             ,name
             (list
              :name ,strname
              :wang-tiles (get-mask-file-tiles ,mask-idx)
              :color ,c1
              :tileset (make-lazy-noise-tileset
                        ,c1
                        ,c2
                        (get-mask-file ,mask-idx)
                        (namestring
                         (merge-pathnames
                          (utils:fmt "~a.png" (string-downcase ,name))
                          ,dir))
                        ,terr-options
                        ,@tileset-options
                        :name (downcase (symbol-name ,name)))))))))

(defmacro gen-terrain-noise-series* (name c1 c2 &rest options &key &allow-other-keys)
  "Like `gen-terrain-noise-series' but with typical defaults and color-number parsing."
  `(gen-terrain-noise-series
    :name ,name
    :c1 ,(utils:parse-html-hex-string c1)
    :c2 ,(utils:parse-html-hex-string c2)
    ,@options))

(defmacro gen-terrain-series-sparse (name c1 c2)
  `(gen-terrain-noise-series* ,name ,c1 ,c2 :enable-flags '(t t t nil nil nil nil nil nil nil nil)
                                            :terr-options (:iter-n 32 :bevel nil)))


(defmacro gen-terrain-series-simple (name c1 c2 &rest args &key &allow-other-keys)
  `(gen-terrain-noise-series* ,name ,c1 ,c2 ,@args
    :enable-flags (list nil nil nil nil nil nil nil nil nil nil nil)))


(defun make-terrain-set-item (item)
  `(,(cadr item) .
    (:color ,(let ((colorval (getf  (cddr item) :color)))
               (if (numberp colorval)
                   colorval
                   (parse-integer
                    (string-left-trim "#" colorval)
                    :radix 16)))
     :name ,(getf (cddr item) :name)
     :id ,(car item)
     :wang-tiles ,(getf (cddr item) :wang-tiles)
     :priority ,(* 1000 (car item))
     :tileset ,(getf (cddr item) :tileset))))

(defun make-terrain-set (terr-set &key (cb #'make-terrain-set-item))
  "Expects values like `(:ocean . (:name \"ocean\" :color \"#B7C4CF\" :signal ,(_ \"ocean\" :ocean)))'
in a quoted list."
  (mapcar cb (utils:enumerate terr-set)))

(setf *terrain-set*
      (make-terrain-set
       `((:deep-underwater . (:name "deep-underwater"
                              :color "#B7C4CF"
                              :tileset ,(tiledmap:make-tileset-from-image
                                         (get-asset-path "images/terr_trench.png"))
                              :wang-tiles :terrain))
         (:ocean . (:name "ocean"
                    :color "#B7C4CF"
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_ocean.png"))

                    :wang-tiles :thick-terrain))

         (:ocean . (:name "algea-ocean"
                    :color "#B7C4CF"
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_water.png"))
                    :wang-tiles :thick-terrain))
         ;; ,@(mapcan #'identity *jp*)
         ,@(gen-terrain-series-simple :depths-drop "#313e49" "#313e49" :mask-idx 1)
         ,@(gen-terrain-series-simple :trench-drop  "#5c758a" "#5c758a" :mask-idx 1 )
         ,@(gen-terrain-series-simple :ocean-drop  "#b7c4cf"  "#b7c4cf" :mask-idx 1 )
         ,@(gen-terrain-series-simple :lake-drop  "#4aa0df"  "#4aa0df" :mask-idx 1)
         ,@(gen-terrain-noise-series*  :dirt-speck "#967054" "#9f785a" :mask-idx 0)
         ,@(gen-terrain-series-sparse :rock-speck "#464646" "#bfbfbf")
         ,@(gen-terrain-series-sparse :gem "#0055b6" "#003a9e")
         ,@(gen-terrain-series-sparse :quartz "#74453b" "#be9c92")
         ;; ,@(gen-terrain-noise-series* :clay "#905932" "#905932" :mask-idx 0)
         ,@(gen-terrain-noise-series* :stone "#9da8a9" "#adb8b9")
         ,@(gen-terrain-noise-series* :glass "#a8b77e" "#a8b77e")
         ,@(gen-terrain-noise-series* :sand "#e5bea6" "#ecd0b8")
         ,@(gen-terrain-noise-series* :sand-hill "#e5bea6" "#ecd0b8")
         ,@(gen-terrain-noise-series* :wet-sand "#dbab69" "#daac70" :mask-idx 0)

         (:clay . (:name "clay"
                   :color "#C38154"
                   :tileset ,(tiledmap:make-tileset-from-image
                              (get-asset-path "images/terr_clay.png"))
                   :wang-tiles :terrain))
         ,@(gen-terrain-noise-series*  :dead-grass-blade "#897f38" "#b7ab55" :mask-idx 0)
         ,@(gen-terrain-series-sparse :pine-needle "#7a3703" "#7b4602")
         ,@(gen-terrain-series-sparse :piece-of-plastic-blue "#0078f8" "#007bf9")
         ,@(gen-terrain-series-sparse :piece-of-plastic-red "#b51800" "#941b19")
         ,@(gen-terrain-series-sparse :piece-of-plastic-yellow "#ae9d11" "#c5b81d")
         ,@(gen-terrain-series-sparse :bark "#5c3624" "#ae785e")
         (:simple-dirt . (:name "simple-dirt"
                          :color "#007E76"
                          :tileset ,(tiledmap:make-tileset-from-image
                                     (get-asset-path "images/terr_dirt.png")
                                     :name "simple-dirt")
                          :wang-tiles :terrain))
         (:dirt . (:name "dirt"
                   :color "#007E76"
                   :tileset ,(tiledmap:make-tileset-from-image
                              (get-asset-path "images/terr_dirt.png")
                              :name "dirt")
                   :wang-tiles :terrain))
         ,@(gen-terrain-noise-series* :grass-blade  "#1a9c4f"  "#32d083" :mask-idx 0)
         (:hard-sand . (:name "hard-sand"
                        :color "#D7C0AE"
                        :tileset ,(tiledmap:make-tileset-from-image
                                   (get-asset-path "images/terr_sand2.png")
                                   :name "hard-sand")
                        :wang-tiles :terrain))
         (:stone . (:name "stone"
                    :color "#D6E8DB"
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_sand.png"))
                    :wang-tiles :terrain))
         (:cliff . (:name "cliff"
                    :color "#000000"
                    :tileset ,(tiledmap:make-tileset-from-image
                               (get-asset-path "images/terr_sand.png"))
                    :wang-tiles :terrain))
         (:stone . ( :name "stone"
                     :color "#F6F1F1"
                     :tileset ,(tiledmap:make-tileset-from-image
                                (get-asset-path "images/terr_cobble.png"))
                     :wang-tiles :terrain))
         (:ice . ( :name "ice"
                   :color "#AFD3E2"
                   :tileset ,(tiledmap:make-tileset-from-image
                              (get-asset-path "images/terr_ice.png"))
                   :wang-tiles :terrain))
         (:lake . (:name "lake"
                   :color  "#AFD3E2"
                   :tileset ,(tiledmap:make-tileset-from-image
                              (get-asset-path "images/terr_water2.png"))
                   :wang-tiles :thick-terrain))
         (:empty . (:name "empty"
                    :color  "#000000"
                    :tileset ,(make-instance 'tiledmap:tileset
                                             :columns 4
                                             :imagewidth 96
                                             :imageheight 96
                                             :image  "./terr_empty.png")
                    :wang-tiles :empty)))))


(defun make-area-set-item (item)
  "Expects values like `(:ocean . (:name \"ocean\" :color \"#B7C4CF\" :signal ,(_ \"ocean\" :ocean)))'."
  `(,(cadr item) .
    (:color ,(parse-integer
              (string-left-trim "#" (getf  (cddr item) :color))
              :radix 16)
     :name ,(getf (cddr item) :name)
     :objects ,(getf (cddr item) :objects)
     :animals ,(getf (cddr item) :animals)
     :id ,(car item)
     :tileset ,(getf (cddr item) :tileset))))

(defun make-area-set (area-set)
  "Expects values like `(:ocean . (:name \"ocean\" :color \"#B7C4CF\" :signal ,(_ \"ocean\" :ocean)))'
in a quoted list."
  (make-terrain-set area-set :cb #'make-area-set-item))

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

          (:late-shore . (:name "late-shore" :color "#c69763"
                          :signal ,(let ((sig (perlin~ 0.11 1 nil )))
                                     (child-sigg sig
                                                 (list
                                                  ;; (_ "late-shore" :empty)
                                                  (_ "late-shore" :sand (_ "late-shore" :empty))
                                                  (_ "foo" :sand-hill
                                                     (child-sigg (stretch& sig :n 0.5 :end 1)
                                                                 (list
                                                                  (_ "late-shore" :grass-blade_50)
                                                                  (_ "late-shore" :grass-blade_100)
                                                                  (_ "late-shore" :lake)))))))))
          (:coastal . (:name "coastal" :color "#c6ad74" :signal ,(let ((sig (perlin~ 0.11 1 nil )))
                                                                   (_ "l" :sand
                                                                      (child-sigg sig
                                                                                  (list
                                                                                   (_ "late-shore" :grass-blade_50)
                                                                                   (_ "late-shore" :grass-blade_300)
                                                                                   (_ "late-shore" :grass-blade_400)
                                                                                   (_ "late-shore" :sand)
                                                                                   (_ "late-shore" :grass-blade_700)
                                                                                   (_ "late-shore" :grass-blade_800)
                                                                                   (_ "late-shore" :grass-blade_999)
                                                                                   (_ "late-shore" :grass-blade)))))))

          (:grass-and-sand . (:name "grass-and-sand" :color "#839450" :signal ,(_ "grass-and-sand" :grass-blade)))

          (:rocky-sand . (:name "rocky-sand" :color "#B18E68" :signal

                                ,(_ "rocky-sand" :dirt
                                    (warped-perlin~  0.4 1242243 (list (_ "rocky-sand" :dirt) (_ "rocky-sand" :grass-blade))))))

          (:desert . ( :name "desert"
                       :color "#ffffd3"
                       :signal ,(warp& (perlin~ 0.31 108 (list (_ "desert" :dirt) (_ "desert" :clay))) :amount 200)))

          (:desert-graveyard . (:name "desert-graveyard" :color "#faa06b" :signal ,(_ "desert-graveyard" :hard-sand)))

          (:dead-forest . (:name "dead-forest" :color "#f4c992" :signal ,(_ "dead-forest" :sand
                                                                            (_ "dead" :grass-blade)
                                                                            (_ "foo" :clay))))

          (:old-pavement-desert . (:name "old-pavement-desert" :color "#b89a74" :signal ,(_ "old-pavement-desert" :hard-sand)))

          (:boulder-meadow-desert . (:name "boulder-meadow-desert" :color "#96794d" :signal ,(_ "boulder-meadow-desert" :hard-sand)))

          (:water-desert . (:name "water-desert" :color "#c5e9bd" :signal ,(_ "water-desert" :hard-sand)))

          (:field . (:name "field" :color "#3590e" :signal ,(warp&
                                                             (perlin~ 0.11 1
                                                                      (list (_ "field" :hard-sand) (_ "field" :grass-blade)))
                                                             :amount 100)))

          (:old-pavement-field . (:name "old-pavement-field" :color "#8f8f51" :signal ,(_ "old-pavement-field" :hard-sand)))

          (:forest . (:name "forest" :color "#293b09" :signal ,(_ "forest" :grass-blade)))

          (:forest-magic . (:name "forest-magic" :color "#2e4114" :signal ,(_ "forest-magic" :hard-sand)))

          (:water-forest . (:name "water-forest" :color "#2e352e" :signal ,(_ "water-forest" :hard-sand)))

          (:old-pavement-forest . (:name "old-pavement-forest" :color "#444353" :signal ,(_ "old-pavement-forest" :hard-sand)))
          (:lake . (:name "lake" :color "#444353" :signal ,(_ "lake" :lake)))))))


(let ((size (* 256 (expt 2 8))))
  (setf *worldconf*
        (__ :ocean
            (<>
             (circle&
              (circle&
               (not-circle&
                (in-circle&
                 (perlin~ 0.00008 108 '())
                 (point (/ size 2) (/ size 2))
                 (* (/ size 2) 2/3) 1.2)
                (point (* size 2/3) (* size 1/3))
                (/ size 3) 0.5)
               (point (* size 1/3) (* size 3/4))
               (* size 1/5) 1.3)
              (point (* size 1/4) (* size 1/2))
              (* size 1/6) 0.6)
             0.0 (
                  0.0 (__ :depths)
                  0.8 (__ :trench)
                  0.98 (__ :ocean))
             0.5 (
                  0.0  (__ :shore)
                  0.1  (__ :late-shore)
                  0.2 (
                       0.0 (__ :coastal)
                       0.5 (__ :desert))
                  0.5 (<> (perlin~ 0.0007 10 '() )
                          0.0  (__ :field
                                   (<> (perlin~ 0.01 10 '())
                                       0.0 (
                                            0.0(__ :field)
                                            1/3 (__ :grass-and-sand)
                                            2/3 (__ :grass))
                                       0.1  (__ :late-shore)
                                       0.5  (__ :desert)
                                       0.9  (__ :field))))
                  0.67   (<> (perlin~ 0.004 10 '())
                             0.0  (__ :coastal)
                             0.2  (__ :rocky-sand)
                             0.8  (__ :desert))

                  0.8 (0.0
                       (<> (perlin~ 0.001 108 '())
                           0.0 (__ :grass-and-sand)
                           1/3 (__ :old-pavement-desert)
                           2/3 (__ :field))
                       0.5
                       (__ :forest
                           (<> (perlin~ 0.01 109 nil)
                               0.0 (__ :forest)
                               1/3 (__ :old-pavement-field)
                               2/3 (__ :old-pavement-forest)))))))))


(defvar *world-size* nil
  "Size of the square, scaled (1/16) world in one dimension")
(setf *world-size* 1600)

(defvar *world-view* nil
  "The active world view for the top-level, highest level map that
will usually be seen scaled 1/16.")
(setf *world-view* (make-world-view *worldconf* -150 -150 (- *world-size* 150) (- *world-size* 150)))

(type-of (typep 256 'fixnum))

(defun cantor (a b)
  (declare (type integer a b))
  (declare  (optimize (speed 3)))
  (+ (the fixnum (/ (* (+ a b 1) (+ a b)) 2)) b))


(defun hashint ( a b c d )
  (declare (type (integer 0 4294967296) a b c d))
  (declare  (optimize (speed 3) (safety 0)))
  (cantor a (cantor b (cantor c  d))))

(defun getuniq ()
  (declare  (optimize (speed 3) (safety 0)))
  (let ((arr nil))
    (loop for x fixnum below 256
          do
             (loop for y fixnum below 256
                   do
                      (loop for file fixnum below 8
                            do
                               (loop for rank fixnum below 8
                                     do (setf arr (adjoin (hashint x y file rank) arr))))))
    (length arr)))
