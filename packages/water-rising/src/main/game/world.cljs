(ns game.world
  (:require
   [game.data :refer [world-buffer]]
   [game.buffer :refer [xyi]]
   ["pngjs/browser" :as png]
   [cljs.core.async.impl.timers :refer [timeout]]
   [cljs.core.async.interop :refer-macros [<p!]]
   [cljs.core.async :as async :refer [<! go go-loop]]
   [game.buffer :as buff]))

(def ^:dynamic *img* nil)

(def PNG png/PNG)

(defn make-png []
  (PNG. {:filterType 4}))

;; (.parse (make-png "")
;;         )

(defn fetch [url]
  (<p!
   (js/fetch url)))

(defn load-image [url cb]
  (go
    (let [res (<p! (js/fetch url))
          blb (<p! (.blob res))
          buf (<p! (.arrayBuffer blb))
          parsed (<p! (js/Promise.
                       (fn [res rej]
                         (.parse (make-png) buf
                                 (fn [err idata]
                                   (if err
                                     (do (prn err) (rej err))
                                     (res idata)))))))]
      (cb parsed))))

(defn get-pixel-data [imgsrc]
  (js/Promise. (fn [res rej]
                 (let [image-instance (js/Image.)]
                   (set! (.-onload image-instance)
                         (fn [] (let [width (.-width image-instance)
                                      height (.-height image-instance)
                                      canvas (.createElement js/document "canvas")]
                                  (prn "heydo")
                                  (set! (.-width canvas) width)
                                  (set! (.-height canvas) height)
                                  (let [ctx (.getContext canvas "2d")]
                                    (.drawImage ctx image-instance 0 0)
                                    (let [imageData (.getImageData ctx 0 0 (.-width canvas) (.-height canvas))]
                                      (res imageData))))))
                   (set! (.-src image-instance) imgsrc)))))

(defn get-row [imagedata row]
  (map
   (fn [x]
     (let [idx (* (+ (* row (.-width imagedata)) x) 4)
           red (aget (.-data imagedata) idx)
           green (aget (.-data imagedata) (inc idx))
           blue (aget (.-data imagedata) (+ idx 2))
           alpha (aget (.-data imagedata) (+ idx 3))]
       #js[red green blue alpha]))
   (range (.-width imagedata))))

(defn get-pixel-data-row [imgsrc rown]
  (js/promise. (fn [res rej]
                 (let [image-instance (js/image.)]
                   (set! (.-onload image-instance)
                         (fn [] (let [width (.-width image-instance)
                                      height (.-height image-instance)
                                      canvas (.createelement js/document "canvas")]
                                  (set! (.-width canvas) width)
                                  (set! (.-height canvas) height)
                                  (let [ctx (.getcontext canvas "2d")]
                                    (.drawimage ctx image-instance 0 0)
                                    (let [y rown
                                          imagedata (.getimagedata ctx 0 0 (.-width canvas) (.-height canvas))]
                                      (res
                                       (map
                                        (fn [x]
                                          (let [idx (* (+ (* y (.-width imagedata)) x) 4)
                                                red (aget (.-data imagedata) idx)
                                                green (aget (.-data imagedata) (inc idx))
                                                blue (aget (.-data imagedata) (+ idx 2))
                                                alpha (aget (.-data imagedata) (+ idx 3))]
                                            #js[red green blue alpha]))
                                        (range width))))))))
                   (set! (.-src image-instance) imgsrc)))))

;; (go
;;   (let [d (<p! (get-pixel-data "/img/n34w107.png"))]
;;     (count d)))

;; (.then
;;  (get-pixel-data "/img/n34w107.png")
;;  #(js/console.log
;;    (clj->js
;;     (into []
;;           (get-row % 100)))))

;; (.then
;;  (get-pixel-data "/img/n34w107.png")
;;  #(js/console.log (clj->js (into [] %))))

;; (prn
;;  (js/Array.from [1 2 3 4 5 6]))

;; (load-image
;;  "/img/n34w107.png"
;;  (fn [img]
;;    (prn (.-data img))))

;; (with-image i "daks")

;; (defmacro with42 [x & body]
;;   `(let [~x 42]
;;      ~@body))

;; (with42 :d (println :d))  ;; This will print: 42

;; (go-loop []
;;   (<! (timeout 250))
;;   (println "Hello from process 2")
;;   (recur))

(defn fetch-json [url]
  (go
    (let [res (<p! (js/fetch url))
          text (<p! (.text res))]
      (js/JSON.parse text nil 2))))

(def ^:dynamic *row* "")

(defn store-row []
  (go
    (set! *row*
          (<!
           (fetch-json "/json/n34w107.npy.1150.json")))))

(defn normalize-row-heights [row]
  (let [minn
        (reduce min row)]
    (map #(- % minn) row)))

(defn get-max-height [row]
  (reduce
   (fn [acc curr]
     (js/Math.max acc curr))
   row))

;; (prn
;;  (normalize-row-heights *row*))

(def ^:dynamic *world* [])

(defn gen-world [row]
  (let [eles (normalize-row-heights row)
        height (+ 20 (get-max-height eles))
        rowsrange (clj->js (range height))
        colrange (clj->js (range (count eles)))]
    (amap rowsrange y rows
          (aset rows y
                (amap colrange x row
                      (aset row x
                            (if (< y (nth eles x))
                              (char 0x2002) "#")))))))

(defn -gen-world-view [row xoff yoff width height]
  (let [eles (normalize-row-heights row)
        rowsrange  (clj->js (range yoff (+ yoff height)))
        colrange (clj->js (range xoff (+ xoff width)))]
    (.reverse
     (amap rowsrange y rows
           (aset rows y
                 (amap colrange x row
                       (aset row x
                             (if (> (+ y yoff) (nth eles (+ x xoff)))
                               (char 0x2002) "#"))))))))

(def world-buffer-js (clj->js world-buffer))

(defn gen-world-view [x y w h]
  (-gen-world-view  world-buffer-js x y w h))

(defn gen-world- [url]
  (go
    (prn
     (first
      (clj->js
       (gen-world
        (<! (fetch-json url))))))))

;; (def world (gen-world-view 0 0 80 35))

;; (gen-world-view (.reverse (clj->js world-buffer)) 0 0 80 35)
;;
;; (prn world)
;; (defn get-world-xy [x y]
;;   (nth
;;    (nth world y)
;;    x))

;; (prn (buff/get-sub-arr (- 3601 80) 459 80 35 world))
;; (prn (buff/get-height world))
;; (prn (buff/get-width world))

;; (buff/print-grid
;;  (buff/get-sub-arr 100 100 80 35 world ))
 ;; (gen-world-  "/json/n34w107.npy.1150.json")
;; (set! *world* (gen-world *row*))
