(ns game.buffer
  (:require [clojure.string :as str]))

(defn xyi [x y width]
  (+ x (* y width)))

(defn ixy [i width]
  (let [x (mod i width)
        y (Math/floor (/ i width))]
    [x y]))

(defn make-grid [h w l]
  (vec (map vec (partition w l))))


(defn ^:export make-empty-grid [w h v]
  (make-grid h w (repeat (* w h) v)))

(defn ^:export chunk-list-to-grid [l row-width]
  (let [length (count l)
        trimmed (subvec l 0 (- length (mod length row-width)))
        grid (make-grid (int (/ (count trimmed) row-width)) row-width (vec trimmed))]
    (reduce
     (fn [grid idx]
       (let [[x y] (ixy idx row-width)]
         (assoc-in grid [y x] (nth trimmed idx))))
     grid
     (range 0 (count trimmed)))))


(defn flatten-grid [grid]
  (apply concat grid))

(defn grid-to-list [grid]
  (map vec grid))

(defn make-grid-from-list [h w l]
  (vec (map vec (partition w l))))

(defn ^:export at [g x y]
  (get-in g [y x]))

(defn set-val [g v x y]
  (assoc-in g [y x] v))

(defn get-width [g]
  (count (first g)))

(defn get-height [g]
  (count g))

(defn get-row [arr row]
  (arr row))

(defn clone-grid [g]
  (map vec g))

(defn iterate-grid [g cb]
  (doseq [y (range 0 (get-height g))
          x (range 0 (get-width g))]
    (cb [x y])))

(defn grid-coords [g]
  (for [y (range 0 (get-height g))
        x (range 0 (get-width g))]
    [x y]))

(defn iterate-grid-values [g cb]
  (iterate-grid g (fn [[x y]]
                    (cb (at g x y)))))

(defn grids-same [g1 g2]
  (every?
   (fn [[x y]]
     (= (at g1 x y) (at g2 x y)))
   (for [y (range 0 (get-height g1))
         x (range 0 (get-width g1))]
     [x y])))

(defn grid-empty [g & {:keys [empty] :or {empty nil}}]
  (every?
   (fn [[x y]]
     (= (or empty nil) (at g x y)))
   (for [y (range 0 (get-height g))
         x (range 0 (get-width g))]
     [x y])))

(defn get-grid-data [g]
  (flatten-grid g))

(defn inject-chunk [g ol xo yo]
  (reduce
   (fn [grid [x y]]
     (if (or (> (+ y yo) (get-height grid))
             (> (+ x xo) (get-width grid)))
       (throw (js/Error. "Trying to inject into nonexistent spot."))
       (set-val grid (at ol x y) (+ x xo) (+ y yo))))
   g
   (for [y (range 0 (get-height ol))
         x (range 0 (get-width ol))]
     [x y])))

(defn attach-chunk-output-dimension [gdim oldim offset]
  (if (< offset 0)
    (Math/max (+ (Math/abs offset) gdim) oldim)
    (Math/max gdim (+ offset oldim))))

(defn attach-chunk [g ol xo yo & {:keys [default] :or {default 0}}]
  (let [w (attach-chunk-output-dimension (get-width g) (get-width ol) xo)
        h (attach-chunk-output-dimension (get-height g) (get-height ol) yo)
        baseXo (if (< xo 0) (Math/abs xo) 0)
        baseYo (if (< yo 0) (Math/abs yo) 0)
        olXo (if (< xo 0) 0 xo)
        olYo (if (< yo 0) 0 yo)]
    (-> (make-empty-grid w h default)
        (inject-chunk g baseXo baseYo)
        (inject-chunk ol olXo olYo))))

(defn ^:export add-chunk [g ol xo yo & {:keys [default] :or {default 0}}]
  (if (or (< xo 0) (< yo 0))
    (attach-chunk g ol xo yo default)
    (if (or (< (get-height g) (+ (get-height ol) yo))
            (< (get-width g) (+ (get-width ol) xo)))
      (attach-chunk g ol xo yo default)
      (inject-chunk g ol xo yo))))

(defn ^:export print-grid [g]
  (doseq [row g] (println (clojure.string/join " " row))))

(defn grid-from-grid [g]
  (vec (map vec g)))

(defn get-center-grid [g]
  [(int (/  (get-width g) 2))
   (int (/  (get-height g) 2))])

(defn grid-pad-row [g amount & {:keys [v bottom] :or {v 0}}]
  (let [newrows (make-empty-grid (get-width g) amount v)]
    (add-chunk g newrows 0 (if bottom (get-height g) (* -1 amount)) v)))

(defn grid-pad-col [g amount & {:keys [v left] :or {v 0}}]
  (let [newcols (make-empty-grid amount (get-height g) v)]
    (add-chunk g newcols (if left  (* -1 amount) (get-width g)) 0 v)))

(defn map-grid [g cb]
  (chunk-list-to-grid
   (reduce
    (fn [acc curr]
      (conj acc (cb curr)))
    [] (grid-coords g))
   (get-width g)))

(defn ^:export get-sub-arr [x y w h g]
  (let [out (make-empty-grid w h 0)]
    (map-grid out (fn [[xx yy]]
                    (at g (+ xx x) (+ yy y))))))

(defn map-grid-values [g cb]
  (map-grid g (fn [coord]
                (cb (at g (first coord) (second coord))))))

(defn scaled-xy [g scale x y]
  (at g
      (Math/max (Math/floor (/ x scale)) 0)
      (Math/max (Math/floor (/ y scale)) 0)))

(defn scale-grid [g scale]
  (let [ow (Math/floor (* (get-width g) scale))
        oh (Math/floor (* (get-height g) scale))]
    (if (or (<= ow 0) (<= oh 0))
      (throw (js/Error. "scale grid failed"))
      (map-grid (make-empty-grid ow oh 0)
                (fn [coord]
                  (scaled-xy g scale (first coord) (second coord)))))))

(defn +grid [g i]
  (map-grid g
            (fn [[x y]]
              (+ (at g x y) i))))

(defn encode-grid [g check]
  (reduce (fn [out coord]
            (bit-or (bit-shift-left out 1)
                    (if (= check (at g (first coord) (second coord)))
                      1
                      0)))
          0
          (for [y (range (get-height g))
                x (range (get-width g))] [x y])))

(defn unique-grid-items [g]
  (set  (apply concat g)))

(defprotocol BufferView (render-view [_]))

(defrecord PlainBufferView [grid width height x y]
  BufferView
  (render-view [_]
    (get-sub-arr x y width height grid)))

(defn split-by-spaces [s]
  (str/split s #"\s+"))

(defn string-join [scoll]
  (str/join (char 0x2002) scoll))

(defn count-split-chars [scoll]
  (+ (- (count scoll) 1)
     (reduce (fn [acc curr] (+ acc (count curr))) 0 scoll)))

(defn conj* [vol v]
  (conj (vec vol) v))

(defn text-flow-grid [text width]
  (map string-join
       (reduce
        (fn [acc curr]
          (let [currline (last acc)]
            (if (>= (+ (count-split-chars currline) (count curr))
                    width)
              (conj* acc [curr])
              (conj* (butlast acc) (conj* currline curr)))))
        []
        (split-by-spaces text))))

(defn pad-list
  "Pad a list l with v to make width w"
  [l w v]
  (if (< (count l) w)
    (recur (conj* l v) w v)
    l))

(defn string-to-grid [s w]
  (chunk-list-to-grid (vec s) w))


(defn grid-assure-height [g v def & {:keys [bottom] :or {bottom true}}]
  (let [diff (- v (count g))]
    (when (> diff 0)
      (grid-pad-row g diff :bottom bottom :v def))))

(defn string-lines-to-grid [sl w def]
  (grid-assure-height
   (vec
    (map #(pad-list % w def) (map vec sl)))
   5 (char 0x2002)))

;; (defn string-lines-to-grid [sl w def]
;;    (map #(pad-list % w def) (map vec sl)))

(defn ^:export text-to-grid [text width]
  (string-lines-to-grid
   (text-flow-grid text width)
   width (char 0x2002)))
