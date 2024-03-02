(ns game.tiles)

(def +player+ "&")
(def +wall+ "#")
(def +empty+ (char 0x2002))

(def tiles {+player+ {:name "player"}
                     +wall+ {:name "wall"}
                     (char 0x2002) {:name "empty"}})

(defn get-tile-name [t]
  (:name (get tiles t)))
