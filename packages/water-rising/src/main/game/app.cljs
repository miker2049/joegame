(ns game.app
  (:require
   [game.buffer :as buff]
   [game.tiles :as tiles]
   [game.world :as world]
   ["simplex-noise" :as simplex]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [reagent.dom.client :as rdom_client]
   [clojure.math :as math]))
(defonce ^:private noise (.createNoise2D simplex))

(defn clog [x]
  (js/console.log x))

(def +width+ 80)
(def +height+ 40)
(def +game-height+ 35)
(def +text-height+ 5)

(def +empty-char+ (char 0x2002))

(def +lorem+ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defonce pos (r/atom [0 0]))
(defonce selection (r/atom [0 0]))
(defonce game-view
  (r/atom (vec (buff/make-empty-grid +width+ +height+ +empty-char+))))

;; (defn update-game [grid text]
;;   (buff/print-grid
;;    (buff/add-chunk
;;     (buff/add-chunk @game-view grid 0 0)
;;     text 0 35)))

(defn update-screen [grid ol]
  (buff/add-chunk grid ol 0 0))

(defn update-screen-from-js-buff [grid ol]
  (update-screen grid
                 (buff/chunk-list-to-grid
                  (vec ol)
                  (buff/get-width grid))))

(defn update-text-view [g text]
  (buff/add-chunk
   g
   (buff/text-to-grid text 79)
   0 35))

(defn clear-screen! []
  (reset! game-view
          (buff/make-empty-grid +width+ +height+ +empty-char+))
  nil)

(defn update-screen! [ol]
  (let [gv @game-view]
    (reset! game-view
            (buff/add-chunk
             gv ol
             0 0))
    nil))

(defn update-screen-js!
  "Takes a js 1D buffer"
  [ol]
  (let [gv @game-view]
    (reset! game-view
            (buff/add-chunk
             gv
             (buff/chunk-list-to-grid
              (vec ol)
              (buff/get-width gv))
             0 0))
    nil))

(defn update-text! [text]
  (let [gv @game-view]
    (reset! game-view
            (buff/add-chunk
             gv
             (buff/text-to-grid text 79)
             0 35))
    nil))

(defn update-game! [ol text]
  (let [gv (deref game-view)]
    (reset! game-view
            (-> gv
                (update-text-view text)
                (update-screen-from-js-buff ol)))
    nil))

(defn update-game!! [ol text]
  (let [gv (deref game-view)]
    (reset! game-view
            (-> gv
                (update-text-view text)
                (buff/add-chunk ol 0 0)))
    nil))

(defn pos-x []
  (nth @pos 0))
(defn pos-y []
  (nth @pos 1))

(defn move-pos [x y]
  (let [ppos @pos]
    (reset! pos [(+ (nth ppos 0) x)
                 (+ (nth ppos 1) y)])))

(defn set-selection [x y]
  (swap! selection [x y]))

(defn move-down
  ([n] (move-pos 0 (abs n)))
  ([] (move-down 1)))

(defn move-up
  ([n] (move-pos 0 (* -1 (abs n))))
  ([] (move-up 1)))

(defn move-right
  ([n] (move-pos (abs n) 0))
  ([] (move-right 1)))

(defn move-left
  ([n] (move-pos (* -1 (abs n)) 0))
  ([] (move-left 1)))

(defn ixy [n width]
  [(mod n width) (math/floor (/ n width))])

(defn get-by-id [id]
  (js/document.getElementById id))

(defn get-root-div []
  (get-by-id "root"))

(defn remove-inner-html [element-id]
  (let [element (get-by-id element-id)]
    (when element
      (set! (.-innerHTML element) ""))))

(defn create-element
  ([tag] (create-element tag {}))
  ([tag attrs]
   (let [el (js/document.createElement tag)]
     (doseq [[k v] attrs]
       (.setAttribute el (name k) v))
     el)))

(defn gen-empty-buffer [w h ch]
  (let [buf (make-array  (* w h))]
    (dotimes [i (* w h)]
      (aset buf i ch))
    buf))

(defn gen-noise-buffer
  ([w h ch choff]
   (gen-noise-buffer w h (pos-x) (pos-y) ch choff))
  ([w h xo yo ch choff]
   (let [buf (make-array  (* w h))]
     (dotimes [i (* w h)]
       (aset buf i
             (let [[x y] (ixy i w)
                   [x y] [(+ x xo) (+ y yo)]
                   nse (noise (* 0.05 x) (* 0.05 y))]
               (if (> nse 0.2)
                 ch choff))))
     buf)))

(defn handle-table-keys [e]
  (case (.-key e)
    "ArrowDown" (do (.preventDefault e)
                    (move-down 20))
    "ArrowUp" (do (.preventDefault e)
                  (move-up 20))
    "ArrowLeft" (do (.preventDefault e)
                    (move-left 40))
    "ArrowRight" (do (.preventDefault e)
                     (move-right 40))
    :nuttin))

(defn game-table [buf w h]
  [:table {:id "game-table" :tabIndex -1 :onKeyDown handle-table-keys}
   (into [:tbody]
         (map (fn [row] (into [:tr {:data-row row}]
                              (map (fn [item]
                                     [(if (>= row +game-height+) :td.text-cell :td.game-cell)
                                      {:data-grid-x item
                                       :data-grid-y row
                                       :onClick #(println "fart!")}
                                      (buff/at buf item row)])
                                   (range w))))
              (range h)))])

(defn gen-table-from-buffer [buff w h]
  (let [table (create-element "table" {:id "game-table"})]
    (doall
     (for [i (range h)]
       (let [row (create-element "tr")]
         (doall
          (for [j (range w)]
            (let [cell (create-element "td"
                                       {:data-grid-x j
                                        :data-grid-y i})]
              (.addEventListener cell "click"
                                 (fn [e]
                                   (.preventDefault e)
                                   (set-selection j i)
                                   (clog (str "(" j "," i ")"))))
              (set! (.-innerText cell) (buff/at buff j i))
              (.appendChild row cell))))
         (.appendChild table row))))
    (.appendChild (get-root-div) table)))

(defn get-cell [x y]
  (js/document.querySelector (str "td[data-grid-x='" x "'][data-grid-y='" y "'")))

(defn set-cell [x y ch]
  (let [cell (get-cell x y)]
    (set! (.-innerText cell) (first ch))))

(defn init-table [w h ch]
  (gen-table-from-buffer (gen-empty-buffer w h ch) w h))

(defn init-noise-table
  ([w h]
   (gen-table-from-buffer (gen-noise-buffer w h "0" (char 0x2002)) w h)))

(defonce root-container (rdom_client/create-root (.getElementById js/document "root")))

(defonce world-buffer (r/atom (gen-noise-buffer 80 30  (char 0x2002) "7")))

(defn live-game-table []
  [game-table @game-view 80 40])

(defn ^:dev/after-load start
  []
  (rdom_client/render root-container
                      [live-game-table]))

(defn ^:export init! []
  (start))

;; (update-game!
;;  (gen-noise-buffer 80 35 tiles/+wall+ tiles/+empty+)
;;  "Hosswsdyssds sd")

;; (add-watch pos :key (fn [k r ov [nx ny]]
;;                       (update-game!
;;                        (.flat
;;                         (world/gen-world-view nx ny 80 35))
;;                        (str "the position currently is: " nx ", " ny))))

(add-watch pos :key (fn [k r ov [nx ny]]
                      (update-game!!
                        (world/gen-world-view nx ny 80 35)
                       (str "the position currently is: " nx ", " ny))))
;; (js/console.log
;;  (-> (world/gen-world-view 1200 -40 80 35)
;;      (.map (fn [it] (.join it)))
;;      (.join "\n")))

;; (update-game!
;;  (.flat
;;   (world/gen-world-view 10 0 80 35))
;;  "Fart")
