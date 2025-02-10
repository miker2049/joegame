(defpackage :worldconf.csp
  (:use :cl :grid)
  (:export
   get-objects))
(in-package :worldconf.csp)

(defun make-space (w h)
  (let ((nn (* w h)))
    (list
     :width w
     :tiles (utils:range-fill nn 0)
     :collision (utils:range-fill nn 1)
     :texture "none"
     :is-space t
     :name (intern (format nil "space-~d-~d" w h) 'keyword))))


(intern "hey" 'keyword)
(defvar *sparse-n* 5)

(defvar *spaces* nil)
(setf *spaces*
      (loop for height
              from 1 below *sparse-n*
            append (loop for width
                           from 1 below *sparse-n*
                         collect (make-space width height))))



(defvar *objects* nil)

(setf *objects*
      '((:name :|leafy-tree| :texture :|browserquestextrude| :width 4
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
        (:name :|grass-boulder| :texture :|browserquestextrude| :width 3
         :tiles (17 18 19
                 37 38 39
                 57 58 59
                 77 78 79)
         :collision (0 0 0
                     1 1 1
                     1 1 1
                     0 0 0))))


(defun find-obj-from (name obj-set)

  (find name obj-set
        :key #'(lambda (obj) (getf obj :name))
        :test #'eql))



(defun find-obj (name)
  "Find object OR space."
  (or (find-obj-from name *objects*)
      (find-obj-from name *spaces*)))

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
    (declare (type fixnum leftA rightA topA bottomA))
    (destructuring-bind
        (&key ((:left leftB)) ((:right rightB)) ((:top topB)) ((:bottom bottomB))) rB
      (declare (type fixnum leftB rightB topB bottomB))
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
   (objects :accessor pt-objects :initform nil)))


(defun init-random (seed)
  (declare (type fixnum seed))
  (setf *random-state*
        (sb-kernel::seed-random-state seed)))

;; constraints
(defmethod no-intersection? ((pt populated-terrain) (obj string) (x fixnum) (y fixnum))
  (loop for (placed-name placed-x placed-y) in (pt-objects pt)
        never (intersect-objects placed-name placed-x placed-y obj x y)))
(defmethod inside-terrain? ((pt populated-terrain) (obj string) (x fixnum) (y fixnum))
  (let* ((obj-ref (find-obj obj))
         (obj-width (getf obj-ref :width))
         (obj-height (floor (/ (length (getf obj-ref :tiles)) obj-width))))
    (block grid-iter
      (dotimes (yy obj-height)
        (dotimes (xx obj-width)
          (let ((xi (+ x xx)) (yi (+ y yy)) (this-terr (terr pt)))
            (if (or
                 (>= xi (get-width this-terr))
                 (>= yi (get-height this-terr))
                 (not (eql (@ this-terr xi yi) 1)))
                (return-from grid-iter nil)))))
      t)))


(defmethod add-object ((pt populated-terrain) obj x y)
  (setf (pt-objects pt)
        (cons  (list obj x y)
               (pt-objects pt))))


(defun jitter (val n)
  (declare (type fixnum val n))
  (+ val (- (random (* 2 n)) n)))

(defmethod populate (pt &aux space-round)
  (iterate-grid
   (terr pt)
   #'(lambda (x y)
       (let* ((jx (max 0 (jitter x 3)))
              (jy (max 0 (jitter y 3)))
              (curr-set (if space-round *spaces* *objects*))
              (new-obj
                (getf (nth (random (length curr-set)) curr-set) :name)))
         (setf space-round (not space-round))
         (if (and
              (inside-terrain? pt new-obj jx jy)
              (no-intersection? pt new-obj jx jy))
             (add-object pt new-obj jx jy))))))



(defun object-sorter (obja objb)
  (< (caddr obja) (caddr objb)))

(defmethod get-tile-stacks ((pt populated-terrain))
  (let ((out (make-empty-grid
              (get-width (terr pt))
              (get-height (terr pt))
              nil)))
    (dolist (obj-ref (sort (pt-objects pt) #'object-sorter))
      (destructuring-bind (terr-name x y) obj-ref
        (let ((obj (find-obj terr-name)))
          (destructuring-bind
              (&key tiles width &allow-other-keys)
              obj
            (dotimes (tile-idx (length tiles))
              (multiple-value-bind (local-x local-y) (ixy tile-idx width)
                (let ((xx (+ x local-x)) (yy (+ y local-y)) (tile (nth tile-idx tiles)))
                  (set-val
                   out
                   (append (@ out xx yy) (list tile))
                   xx yy))))))))
    out))




(defun get-object-tiles (terr terr-type &optional (seed 0))
  (declare (ignore terr-type))
  (init-random seed)
  (let ((pt (make-instance 'populated-terrain :terr terr)))
    (populate pt)
    (worldconf:expand-stacks
     (worldconf:normalize-stacks
      (get-tile-stacks pt)))))

(defun get-object-tiles* (x y file rank)
  (worldconf:collect-terrain-wang-vals worldconf:*worldconf*
                                       (+ (* 256 x) (* 32 file))
                                       (+ (* 256 y) (* 32 rank))
                                       33
                                       33))

(defun get-objects (terr terr-type &optional (seed 0))
  (declare (ignore terr-type))
  (init-random seed)
  (let ((pt (make-instance 'populated-terrain :terr terr)))
    (populate pt)
    (utils:filter
     (pt-objects pt)
     (lambda (object)
       (if (getf (find-obj (car object)) :is-space)
           nil
           t)))))
