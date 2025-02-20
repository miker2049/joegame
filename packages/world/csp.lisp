(defpackage :worldconf.csp
  (:use :cl :grid)
  (:export
   get-chars
   get-objects))
(in-package :worldconf.csp)
(declaim (optimize (speed 3)))


(defun make-space (w h)
  (declare (type fixnum w h))
  (let ((nn (* w h)))
    (list
     :|tile_config| (list
                     :|width| w
                     :|tiles| (utils:range-fill nn 0)
                     :|collision| (utils:range-fill nn 1)
                     :|texture| "none")
     :is-space t
     :|name| (intern (format nil "space-~d-~d" w h) 'keyword))))


(defvar *sparse-n* 5)

(defvar *spaces* nil)
(setf *spaces*
      (loop for height
              from 3 below *sparse-n*
            append (loop for width
                           from 3 below *sparse-n*
                         collect (make-space width height))))



(defvar *objects* nil)

(setf *objects*
      '((:|name| :|leafy-tree|
         :|tile_config| (
                         :|texture| :|browserquestextrude|
                         :|width| 4
                         :|tiles| (212 213 214 215
                                   232 233 234 235
                                   252 253 254 255
                                   272 273 274 275
                                   0 293 294   0)
                         :|collision| (0 0 0 0
                                       0 0 0 0
                                       1 1 1 1
                                       1 1 1 1
                                       0 0 0 0)))
        (:|name| :|grass-boulder|
         :|tile_config| (
                         :|texture| :|browserquestextrude|
                         :|width| 3
                         :|tiles| (17 18 19
                                   37 38 39
                                   57 58 59
                                   77 78 79)
                         :|collision| (0 0 0
                                       1 1 1
                                       1 1 1
                                       0 0 0)))))


(defun find-obj-from (name obj-set)
  (declare (type list obj-set))
  (find name obj-set
        :key #'(lambda (obj) (getf obj :|name|))
        :test #'eql))



(declaim (inline find-obj-jdb))
(defun find-obj-jdb (key)
  (getf
   (getf jdb:*world-data* :|mapobjects|)
   key))
(defvar *obj-cache* (make-hash-table))

(declaim (inline find-obj))
(defun find-obj (name)
  "Find object OR space."
  (or (gethash name *obj-cache*)
      (let ((obj (or (find-obj-jdb name)
                     (find-obj-from name *spaces*))))
        (setf (gethash name *obj-cache*) obj)
        obj)))
;; (defun find-obj (name)
;;   "Find object OR space."
;;   (or (find-obj-jdb name)
;;       (find-obj-from name *spaces*)))

(defun get-corners-old (obj &aux points)
  "Given a grid of something or 0 get the topleft and bottom
right corners of its bounding box."
  (iterate-grid obj
                (lambda (x y)
                  (unless (eql 0 (@ obj x y))
                    (setf points (push (worldconf:point x y) points)))))
  (if (eql 0 (length points))
      (throw 'no-collision nil)
      (values
       (reduce #'worldconf:min-point points)
       (reduce #'worldconf:max-point points))))


(defun get-corners (obj)
  "Given a grid of something or 0 get the topleft and bottom
right corners of its bounding box."
  (let ((min-x most-positive-fixnum)
        (min-y most-positive-fixnum)
        (max-x 0)
        (max-y 0))
    (declare (type fixnum min-x min-y max-x max-y))
    (iterate-grid obj
                  (lambda (x y)
                    (declare (type fixnum x y))
                    (unless (eql 0 (@ obj x y))
                      (setf min-x (min min-x x)
                            min-y (min min-y y)
                            max-x (max max-x x)
                            max-y (max max-y y)))))
    (if (= min-x most-positive-fixnum)
        (throw 'no-collision nil)
        (values
         (worldconf:point min-x min-y)
         (worldconf:point max-x max-y)))))

(defun get-collision-coords-nocache (obj x y)
  "Given an object at x,y get the topleft and botto
right corners of its collision box."
  (let ((tile-config (getf obj :|tile_config|))
        (offset (worldconf:point x y)))
    (multiple-value-bind (tl br)
        (get-corners
         (chunk-list-to-grid
          (getf tile-config :|collision|)
          (getf tile-config :|width|)))
      (values
       (worldconf:+p offset tl)
       (worldconf:+p offset br)))))


(defvar *collision-cache* (make-hash-table))

(defun get-collision-coords (obj x y)
  (declare (type fixnum x y)
           (type list obj)
           (optimize (speed 3) (safety 1)))
  (let* ((name (getf obj :|name|))
         (cached (gethash name *collision-cache*)))
    (if cached
        (let ((offset (worldconf:point x y)))
          (values
           (worldconf:+p offset (first cached))
           (worldconf:+p offset (second cached))))
        (let ((tile-config (getf obj :|tile_config|))
              (offset (worldconf:point x y)))
          (multiple-value-bind (tl br)
              (get-corners
               (chunk-list-to-grid
                (getf tile-config :|collision|)
                (getf tile-config :|width|)))
            (setf (gethash name *collision-cache*) (list tl br))
            (values
             (worldconf:+p offset tl)
             (worldconf:+p offset br)))))))

(declaim (inline collision-rect))
(defun collision-rect (obj x y)
  "Takes top left, bottom right values and returns
top,left,right,bottom"
  (multiple-value-bind (tl br) (get-collision-coords obj x y)
    (list
     :top (floor (worldconf:get-y tl))
     :left (floor (worldconf:get-x tl))
     :right (floor (worldconf:get-x br))
     :bottom (floor (worldconf:get-y br)))))


;; function intersectRect(r1, r2) {
;;  return !(r2.left > r1.right ||
;;    r2.right < r1.left ||
;;    r2.top > r1.bottom ||
;;    r2.bottom < r1.top);
;;}

(declaim (inline intersect-rect))
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
  (declare (type fixnum xA yA xB yB)
           (type symbol objA objB)
           (optimize (speed 3) (safety 1)))
  (let ((object-a (find-obj objA))
        (object-b (find-obj objB)))
    (catch 'no-collision
      (intersect-rect
       (collision-rect object-a xA yA)
       (collision-rect object-b xB yB)))))


(defclass populated-terrain ()
  ((terr-type :initarg :terr-type
              :accessor terr-type)
   (terr :initarg :terr
         :accessor terr)
   (objects :accessor pt-objects :initform nil)))


(defun init-random (seed)
  (declare (type fixnum seed))
  (setf *random-state*
        (sb-kernel::seed-random-state seed)))

;; constraints
(defmethod no-intersection? ((pt populated-terrain) (obj symbol) (x fixnum) (y fixnum))
  (loop for (placed-name placed-x placed-y) in (pt-objects pt)
        never (intersect-objects placed-name placed-x placed-y obj x y)))

(defmethod inside-terrain? ((pt populated-terrain) (obj-ref symbol) (x fixnum) (y fixnum))
  (let* ((obj (find-obj obj-ref))
         (obj-width (getf (getf obj :|tile_config|) :|width|))
         (obj-height (floor (/ (length (getf (getf obj :|tile_config|) :|tiles|)) obj-width))))
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


(declaim (ftype (function (fixnum fixnum) fixnum) jitter))
(defun jitter (val n)
  (declare (type fixnum val n))
  (let ((this-mod
          (- (random (* 2 n)) n)))
    (declare (type fixnum this-mod))
    (+ val this-mod)))



(defmethod populate ((pt populated-terrain) &aux space-round)
  (let ((terr-objects
          (getf
           (cdr (assoc (terr-type pt) worldconf:*area-set*))
           :objects))
        (terr (terr pt)))
    (declare (type list terr-objects))
    (unless (eql 0 (length terr-objects))
      (iterate-grid
       terr
       #'(lambda (x y)
           (if (eql 0 (mod (+ x (* y (get-width terr))) 10))
               (let* ((jx (max 0 (jitter x 3)))
                      (jy (max 0 (jitter y 3)))
                      (new-obj
                        (let ((wres (utils:weighted-random terr-objects)))
                          (if (or (eql :|space| wres) space-round)
                              (getf (nth (random (length *spaces*)) *spaces*) :|name|)
                              wres))))
                 (setf space-round (not space-round))
                 (if (and
                      (inside-terrain? pt new-obj jx jy)
                      (no-intersection? pt new-obj jx jy))
                     (add-object pt new-obj jx jy)))))))))



(defun object-sorter (obja objb)
  (let ((obja-data (funcall find-obj-jdb (car obja)))
        (objb-data (funcall find-obj-jdb (car objb))))
    (flet ((get-height (data)
             (let ((tile-width (getf (getf data :|tile_config|) :|width|))
                   (tile-amt
                     (length (getf (getf data :|tile_config|) :|tiles|))))
               (declare (type fixnum tile-amt tile-width))
               (/ tile-amt
                  tile-width))))
      (< (+ (get-height obja-data)
            (caddr obja))
         (+ (get-height objb-data)
            (caddr objb))))))

(defun get-objects (terr terr-type seed)
  "Takes terr mask (bitgrid), terr type, and a seed and returns a list of objects and placements.
Placements are relative to the terr mask."
  (init-random seed)
  (let ((pt (make-instance 'populated-terrain
                           :terr terr
                           :terr-type (intern (string-upcase terr-type) 'keyword))))
    (populate pt)
    (utils:filter
     (pt-objects pt)
     (lambda (object)
       (if (getf (find-obj (car object)) :is-space)
           nil
           t)))))

(defun get-chars (terr-type seed n)
  (declare
   (type fixnum seed n)
   (type string terr-type))
  "Get n number of pseudo-random animals based off terr-type."
  (init-random seed)
  (loop for i below n collect
                      (alexandria:random-elt
                       (getf
                        (cdr
                         (assoc
                          (intern (string-upcase terr-type) 'keyword)
                          worldconf:*area-set*))
                        :animals))))

