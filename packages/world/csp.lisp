(defpackage :worldconf.csp
  (:use :cl :grid))
(in-package :worldconf.csp)


(defvar *objects* nil)

(setf *objects*
      '((:name "tree" :texture "browserquestextrude.png" :width 4
         :tiles (212 213 214 215
                 232 233 234 235
                 252 253 254 255
                 272 273 274 275
                 0 293 294   0)
         :collision (0 0 0 0
                     0 0 0 0
                     0 0 0 0
                     0 1 1 0
                     0 1 1 0))
        (:name "grass-boulder" :texture "browserquestextrude.png" :width 3
         :tiles (17 18 19
                 37 38 39
                 57 58 59
                 77 78 79)
         :collision (0 0 0
                     1 1 1
                     1 1 1
                     0 0 0))))

(defun can-place-object? (grid-map object x y)
  "Check if object can be placed at x,y in grid-map"
  (let* ((obj-width (getf object :width))
         (collision (chunk-list-to-grid (getf object :collision) obj-width))
         (height (floor (length (getf object :collision)) obj-width)))

    ;; First check bounds
    (when (or (> (+ x obj-width) (get-width grid-map))
              (> (+ y height) (get-height grid-map)))
      (return-from can-place-object? nil))

    ;; Check collision map against existing grid
    (block check-collision
      (iterate-grid collision
                    (lambda (cx cy)
                      (when (and (= 1 (@ collision cx cy))  ; collision tile
                                 (/= 0 (@ grid-map (+ x cx) (+ y cy)))) ; existing tile
                        (return-from check-collision nil))))
      t)))

(defun place-object (grid-map object x y)
  "Place object at x,y if possible, return new grid if successful or nil"
  (when (can-place-object? grid-map object x y)
    (let* ((obj-width (getf object :width))
           (tiles (chunk-list-to-grid (getf object :tiles) obj-width))
           (new-grid (clone-grid grid-map)))
      ;; Place the tiles
      (iterate-grid tiles
                    (lambda (tx ty)
                      (let ((tile-val (@ tiles tx ty)))
                        (unless (= 0 tile-val)  ; Don't place empty tiles
                          (set-val new-grid tile-val (+ x tx) (+ y ty))))))
      new-grid)))

(defun get-valid-positions (grid-map object)
  "Get list of valid x,y positions where object can be placed"
  (let ((positions nil))
    (iterate-grid grid-map
                  (lambda (x y)
                    (when (can-place-object? grid-map object x y)
                      (push (list x y) positions))))
    positions))



(defun solve-placement (grid-map objects &optional (min-objects 1))
  "Try to place objects on grid-map, ensuring at least min-objects are placed"
  (labels ((try-placing (current-grid remaining-objects placed-count)
             (if (and (>= placed-count min-objects)
                      (null remaining-objects))
                 current-grid ; success case
                 (let* ((object (first remaining-objects))
                        (positions (get-valid-positions current-grid object)))
                   (dolist (pos positions)
                     (let ((new-grid (place-object current-grid object
                                                   (first pos) (second pos))))
                       (when new-grid
                         (let ((result (try-placing new-grid
                                                    (rest remaining-objects)
                                                    (1+ placed-count))))
                           (when result
                             (return-from try-placing result))))))
                   nil)))) ; no valid placement found
    (try-placing grid-map objects 0)))
