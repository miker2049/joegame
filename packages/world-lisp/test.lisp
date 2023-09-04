(in-package :cl-user)
(defpackage world-tests
  (:export world-test-suite)
  (:use :cl
    :fiveam))

(in-package world-tests)

(def-suite world-test-suite
  :description "The tests of the world")

(setf (uiop:getenv "QT_QPA_PLATFORM") "wayland")
(setf *run-test-when-defined* nil)

(def-suite worldconf-utils
  :in world-test-suite)

(def-suite* circle-filter :in worldconf-utils)



(def-suite* terrain-wangs
  :in worldconf-utils)

(defun --wang-vals (w h)
  (worldconf:collect-terrain-wang-vals worldconf:*worldconf* (* 16 (- 579 100)) (* 16 (- 999 100)) w h))

(defun --terr-grid (w h)
  (worldconf:get-terrain-grids worldconf:*worldconf* (* 16 (- 579 100)) (* 16 (- 999 100)) w h))

(test collect-terrain-wang-vals
  (is (--wang-vals 4 4)
    "collect-terrain-wang-vals should return something"))

(test predictable-wang-grid-sizes
  (is  (eql 3
         (length
           (cdar (--wang-vals 1 1)))))
  (is (eql 7 (length
               (cdar (--wang-vals 2 2)))))
  (is (eql 11 (length
                (cdar (--wang-vals 3 3)))))
  (is (eql 15 (length
                (cdar (--wang-vals 4 4)))))
  (is (eql (- (* 4 83) 1)
        (length
          (cdar (--wang-vals 40 83))))))

(test collect-terrain-grid-vals
  (is (eql (grid:get-height (car (--terr-grid 4 4)))
        16)))


(def-suite* tiled-tools
  :in world-test-suite)

(setf basic-map
  (make-instance 'tiledmap:tiled-map :width 10 :height 10))

(test basic-tilemap-is-valid
  (is (tiledmap:valid-tilemap (make-instance 'tiledmap:tiled-map))))

(test tilemap-with-layer-is-valid
  (is (tiledmap:valid-tilemap
        (make-instance 'tiledmap:tiled-map :width 10 :height 10
          :layers (list  (make-instance 'tiledmap:tilelayer)
                    (make-instance 'tiledmap:objectlayer :name "fart"))))))

(test tilemap-test-is-false-for-corrupt-tile
  (is (not (tiledmap:valid-tilemap
             (make-instance 'tiledmap:tiled-map :width 10 :height 10
               :layers (list  (make-instance 'tiledmap:tilelayer :data '(1))))))))

(test tilemap-valid-after-add-tileset
  (let ((mm (make-instance 'tiledmap:tiled-map :width 10 :height 10)))
    (tiledmap:add-tileset mm (make-instance 'tiledmap:tileset))
    (is (tiledmap:valid-tilemap mm))))


(test tilemap-valid-after-add-layer-from-wang-val-grid
  (let ((mm (make-instance 'tiledmap:tiled-map :width 16 :height 16))
         (ts (make-instance 'tiledmap:tileset :name "grasstest")))
    (tiledmap:add-tileset mm ts)
    (worldconf:add-layer-from-wang-val-grid (grid:chunk-list-to-grid '(1 2 3 4
                                                                        1 2 3 4
                                                                        1 2 3 4
                                                                        1 2 3 4)
                                              4)
      mm
      ts "foo")
    (is (tiledmap:valid-tilemap mm))))



;; (def-suite* tiled-tools
;;   :in world-test-suite)


;; (utils:save-file "test2.json" (tiledmap:map-to-json
;;                                 (let ((mm (make-instance 'tiledmap:tiled-map :width 10 :height 10)))
;;                                   (tiledmap:add-tileset mm (make-instance 'tiledmap:tileset))
;;                                   mm)))
