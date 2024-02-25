(ns joegame.frontend.app
  (:require ["simplex-noise" :as simplex]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.math :as math]))

(defonce pos (r/atom [0 0]))
(defonce selection (r/atom [0 0]))


(defn pos-x []
  (nth @pos 0))
(defn pos-y []
  (nth @pos 1))

(defn move-pos [x y]
  (let [ppos @pos]
    (swap! pos [(+ (nth ppos 0) x)
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

(defonce ^:private noise (.createNoise2D simplex))

(defn clog [x]
  (js/console.log x))

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
     buf) ))

;; makes a html table in specified div, with specified height/width, filled with centered periods
(defn gen-table [div w h]
  (let [table (create-element "table" {:id "game-table"})]
    (doall 
     (for [i (range h)]
       (let [row (create-element "tr")]
         (doall
          (for [j (range w)]
            (let [cell (js/document.createElement "td")]
              (set! (.-innerText cell) "*")
              (.appendChild row cell))))
         (.appendChild table row))))
    (.appendChild div table)))

(defn game-table [buff w h]
  [:table {:id "game-table"}
   (into [:tbody]
         (map (fn [row] (into [:tr {:data-row row}]
                              (map (fn [item]
                                     [:td {:data-grid-x item :data-grid-y row}
                                      (aget buff (+ (* w row) item))])
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
              (set! (.-innerText cell) (aget buff (+ (* i w) j)))
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

;; (defn ^:dev/after-load start []
;;   (remove-inner-html "root")
;;   (init-noise-table 80 30))

;; (defn init-handle-keys []
;;     (.addEventListener js/document "keydown"
;;                        (fn [e]
;;                          (.preventDefault e)
;;                          (case (.-key e)
;;                            "ArrowDown" (move-down)
;;                            "ArrowUp" (move-up)
;;                            "ArrowLeft" (move-left)
;;                            "ArrowRight" (move-right)
;;                            :nuttin)
;;                          (start))))
;; (defn init! []
;;   (init-handle-keys)
;;   (start))

(defn ^:dev/after-load start
  []
  (rdom/render [game-table (gen-noise-buffer 80 30 "*" "#") 80 30]
                      (.getElementById js/document "root")))

(defn ^:export main
  []
  (start))

(defn init! []
  (start))
