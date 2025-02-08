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
                     1 1 1 1
                     1 1 1 1
                     0 0 0 0))
        (:name "grass-boulder" :texture "browserquestextrude.png" :width 3
         :tiles (17 18 19
                 37 38 39
                 57 58 59
                 77 78 79)
         :collision (0 0 0
                     1 1 1
                     1 1 1
                     0 0 0))))


(defun find-obj (name)
  (find name *objects*
        :key #'(lambda (obj) (getf obj :name))
        :test #'string=))

(defun get-corners (obj &aux points)
  "Given a grid of something or 0 get the topleft and bottom
right corners of its bounding box."
  (iterate-grid obj
                (lambda (x y)
                  (unless (eql 0 (@ obj x y))
                    (setf points (push (worldconf:point x y) points)))))
  (values
   (reduce #'worldconf:min-point points)
   (reduce #'worldconf:max-point points)))


(defun get-collision-coords (obj x y)
  "Given an object at x,y get the topleft and botto
right corners of its collision box."
  (let ((offset (worldconf:point x y)))
    (multiple-value-bind (tl br)
        (get-corners
         (chunk-list-to-grid
          (getf obj :collision) (getf obj :width)))
      (values
       (worldconf:+p offset tl)
       (worldconf:+p offset br)))))

(defun collision-rect (obj x y)
  "Takes top left, bottom right values and returns
top,left,right,bottom"
  (multiple-value-bind (tl br) (get-collision-coords obj x y)
    (list
     :top (worldconf:get-y tl)
     :left (worldconf:get-x tl)
     :right (worldconf:get-x br)
     :bottom (worldconf:get-y br))))


;; function intersectRect(r1, r2) {
;;  return !(r2.left > r1.right ||
;;    r2.right < r1.left ||
;;    r2.top > r1.bottom ||
;;    r2.bottom < r1.top);
;;}

(defun intersect-rect (rA rB)
  "Takes rects described in a plist like
'(:top 1 :right: 2 :left 0 :bottom 12)"
  (destructuring-bind
      (&key ((:left leftA)) ((:right rightA)) ((:top topA)) ((:bottom bottomA))) rA
    (destructuring-bind
        (&key ((:left leftB)) ((:right rightB)) ((:top topB)) ((:bottom bottomB))) rB
      (not (or
            (> leftB rightA)
            (< rightB leftA)
            (> topB bottomA)
            (< bottomB topA))))))

(defun intersect-objects (objA xA yA objB xB yB)
  (let ((object-a (find-obj objA))
        (object-b (find-obj objB)))
    (intersect-rect
     (collision-rect object-a xA yA)
     (collision-rect object-b xB yB))))


(defclass populated-terrain ()
  ((terr :initarg :terr
         :accessor terr)
   (objects :accessor pt-objects :initform nil)
   (spaces :accessor pt-spaces :initform nil)))



;; constraints
(defmethod no-intersection? ((pt populated-terrain) obj x y)
  (loop for (placed-name placed-x placed-y) in (pt-objects pt)
        never (intersect-objects placed-name placed-x placed-y obj x y)))
(defmethod inside-terrain? ((pt populated-terrain) obj x y)
  (let* ((obj-ref (find-obj obj))
         (obj-width (getf obj-ref :width))
         (obj-height (floor (/ (length (getf obj-ref :tiles)) obj-width)))
         (considered-chunk (get-sub-arr x y (+ x obj-width) (+ y obj-height) (terr pt))))
    (block grid-iter
      (iterate-grid-values considered-chunk
                           #'(lambda (val)
                               (if (not (eql val 1))
                                   (return-from grid-iter nil))))
      t)))


(defmethod add-object ((pt populated-terrain) obj x y)
  (setf (pt-objects pt)
        (cons  (list obj x y)
               (pt-objects pt))))


(defmethod populate (pt &aux space-round)
  (iterate-grid
   (terr pt)
   #'(lambda (x y)
       (let ((new-obj
               (getf (nth (random (length *objects*)) *objects*) :name)))
         (if (and
              (inside-terrain? pt new-obj x y)
              (no-intersection? pt new-obj x y))
             (add-object pt new-obj x y))))))
