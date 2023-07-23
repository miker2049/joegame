(defpackage worldconf
  (:use
    :cl

    :async
    :render
    :simplex
    :utils
    :grid

    :jonathan
    :alexandria)
  (:export run
    dbsync
    render-big-img
    render-img
    render-signal-file
    split-rect
    serialize
    dump-csv
    *worldconf*))
(in-package worldconf)
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;utilities;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmacro next-m ()
  '(when (next-method-p) (call-next-method)))

(defun place-signal (val n)
  "Given a signal value val, place it among one of n children.
Returning nil means don't place."
  (let ((val (clamp val 0 0.99999)))
    (if (equal n 0)
      nil
      (floor
        (/ val (/ 1 n))))))




(defun downcase (s)
  (map 'string #'(lambda (char) (char-downcase char))
    s))

(defun q2s (q) (downcase (string q)))

(defun j (plist)
  "Convert plist to json"
  (downcase
    (to-json plist)))



                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;classes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric serialize (obj)
  (:documentation "Serialize object")
  (:method  (obj)
    nil))
(defmethod serialize ((n number))
  n)
(defmethod serialize ((n string))
  n)
(defmethod serialize ((n list))
  (mapcar #'serialize n))

(defgeneric resolve-terrains (obj x y)
  (:documentation "
Resolving terrain means, given a signal/terrain tree, collect all
terrains moving down the tree at a particular point. For a Sig
")
  (:method (obj x y) nil))

                                        ;point;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass point ()
  ((x
     :initarg :x
     :accessor get-x)
    (y
      :initarg :y
      :accessor get-y)))
(defun point (x y)
  (make-instance 'point :x x :y y))

(defmethod print-point ((p point))
  (list (get-x p) (get-y p)))

(defmacro get-random-point-type% (p-min p-max type)
  `(let ((xmin (get-x ,p-min))
          (xmax (get-x ,p-max))
          (ymin (get-y ,p-min))
          (ymax (get-y ,p-max)))
     (if (or (< xmax xmin)
           (< ymax ymin))
       (error "Point max should be below/right of point min.")
       (point
         (+ xmin (random (,type (- xmax xmin))))
         (+ ymin (random (,type (- ymax ymin))))))))

(defmethod get-random-point ((p-min point) (p-max point))
  (get-random-point-type% p-min p-max float))
(defmethod get-random-point-int ((p-min point) (p-max point))
  (get-random-point-type% p-min p-max floor))


(defmethod get-random-points ((p-min point) (p-max point) n)
  (loop for i from 0 to n collect (get-random-point p-min p-max)))

(defmacro op-point (p1 p2 op)
  `(point
     (,op (get-x ,p1) (get-x ,p2))
     (,op (get-y ,p1) (get-y ,p2))))

(defmacro op-point-val (p1 x y op)
  `(point
     (,op (get-x ,p1) ,x)
     (,op (get-y ,p1) ,y)))

(defmacro op-point-sval (p1 n op)
  `(point
     (,op (get-x ,p1) ,n)
     (,op (get-y ,p1) ,n)))

(defmethod +p ((p1 point) (p2 point) )
  (op-point p1 p2 +))
(defmethod ++p ((p1 point) (n number) )
  (op-point-sval p1 n +))
(defmethod +pv ((p1 point) (x number) (y number) )
  (op-point-val p1 x y +))

(defmethod -p ((p1 point) (p2 point) )
  (op-point p1 p2 -))
(defmethod --p ((p1 point) (n number) )
  (op-point-sval p1 n -))
(defmethod -pv ((p1 point) (x number) (y number) )
  (op-point-val p1 x y -))

(defmethod ==p ((p1 point) (p2 point) )
  (and
    (eq (get-x p1) (get-x p2))
    (eq (get-y p1) (get-y p2))))

(defmethod *p ((p1 point) (p2 point) )
  (op-point p1 p2 *))

(defmethod *p ((p point) (n number) )
  (point
    (* n (get-x p))
    (* n (get-y p))))

(defmethod *p ((n number) (p point))
  (*p p n))

(defmethod /p ((p1 point) (p2 point) )
  (op-point p1 p2 /))


                                        ;value;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass value () ())

(defmethod serialize ((obj value))
  (next-m))

(defgeneric get-val (obj p)
  (:method (obj p) 0))

(defmethod sample-val ((v value) (p point) n amount)
  (let
    ((pointmin p)
      (pointmax (++p p n)))
    (let ((vals (map 'list #'(lambda (thisp) (get-val v thisp))
                  (get-random-points pointmin pointmax amount))))
      (mean vals))))


                                        ;parent;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass parent ()
  ((children
     :initform '()
     :initarg :children
     :accessor children)))
(defmethod serialize ((obj parent))
  (append
    (list :children (map 'list #'(lambda (d) (serialize d)) (children obj))) (next-m)))
                                        ;named;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass named ()
  ((name
     :initarg :name
     :accessor name)))
(defmethod serialize ((obj named))
  (append (list :name (name obj))  (next-m)))
                                        ;params;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass parameterized ()
  ((params
     :initarg :params
     :accessor params)))
(defclass param ()
  ((name
     :initarg :name
     :accessor name)
    (val
      :initarg :val
      :accessor val)))
(defun param (name val)
  (make-instance 'param :name name :val val))
(defmethod serialize ((obj parameterized))
  (append (next-m)
    (list :params
      (map 'list #'(lambda (d) (serialize d)) (params obj)))))

(defmethod serialize ((obj param))
  (list (name obj)  (serialize (val obj))))


(defmethod get-param ((p param) nname)
  (let ((found (name p)))
    (if (equal nname found)
      (val p))))

(defmethod find-param ((obj parameterized) k)
  (let ((ps (params obj)) out)
    (dolist (p ps)
      (if-let ((found (get-param p k)))
        (setf out found)))
    out))

                                        ;router;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass router (named children)
  ((handler
     :initarg :handler
     :accessor handler)
    (sigg
      :initarg :sigg
      :accessor sigg)))


                                        ;terrain;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass terrain (named parent value)
  ((color
     :initarg :color
     :accessor color)
    (id
      :initarg :id
      :accessor terr-id)))

(defun terr (name color &rest children)
  (make-instance 'terrain :name name :color color :children children))
(defmethod serialize ((obj terrain))
  (append (list :type "terrain" :color (color obj)) (next-m)))
(defgeneric is-terrain (obj)
  (:method (obj)
    nil))
(defmethod is-terrain ((obj terrain))
  t)

(defmethod resolve-terrains ((obj terrain) (x number) (y number))
  (let ((out (list obj)))
    (dolist (child (children obj))
      (setf out (append out (resolve-terrains child x y))))
    out))

(defmethod resolve-value ((obj terrain) (p point))
  (let ((out (list)))
    (dolist (child (children obj))
      (setf out
        (append out
          (list (resolve-value child p)))))
    out))
                                        ;filters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass filter (named parameterized parent value)
  ((source :initarg :source
     :accessor source)))

(defmethod children ((f filter))
  (children (source f)))


(defclass circle-filter (filter)
  ())
(defun circle& (s p &optional r amount (max 1))
  (make-instance 'circle-filter
    :name "circle"
    :source s
    :params (list
              (param "amount" (or amount 0.5))
              (param "r" (or r 100))
              (param "x" (get-x p))
              (param "max" max)
              (param "y" (get-y p)))))

(defmethod get-val ((f circle-filter) (p point))
  (let
    ((s (source f))
      (amount (find-param f "amount"))
      (ox (find-param f "x"))
      (oy (find-param f "y"))
      (max (find-param f "max"))
      (x (get-x p))
      (y (get-y p))
      (r (find-param f "r")))
    (let* ((dist (e-distance x y ox oy))
            (val (get-val s p))
            (fact (if (< dist r)
                    (+ 1 (* amount
                           (- 1 (/ dist r))))
                    1)))
      (clamp (* fact val) 0.07 max))))

(defclass inside-circle-filter (filter)
  ())
(defun in-circle& (s p &optional r amount)
  (make-instance 'inside-circle-filter
    :name "circle"
    :source s
    :params (list
              (param "amount" (or amount 0.5))
              (param "r" (or r 100))
              (param "x" (get-x p))
              (param "y" (get-y p)))))

(defmethod get-val ((f inside-circle-filter) (p point))
  (let
    ((s (source f))
      (amount (find-param f "amount"))
      (ox (find-param f "x"))
      (oy (find-param f "y"))
      (x (get-x p))
      (y (get-y p))
      (r (find-param f "r")))
    (let* ((dist (e-distance x y ox oy))
            (val (get-val s p))
            (fact (if (< dist r)
                    1
                    (/ r dist))))
      (clamp (* fact val) 0.07 0.94))))

(defclass not-circle-filter (filter)
  ())
(defun not-circle& (s p &optional r amount)
  (make-instance 'not-circle-filter
    :name "circle"
    :source s
    :params (list
              (param "amount" (or amount 0.5))
              (param "r" (or r 100))
              (param "x" (get-x p))
              (param "y" (get-y p)))))

(defmethod get-val ((f not-circle-filter) (p point))
  (let
    ((s (source f))
      (amount (find-param f "amount"))
      (ox (find-param f "x"))
      (oy (find-param f "y"))
      (x (get-x p))
      (y (get-y p))
      (r (find-param f "r")))
    (let* ((dist (e-distance x y ox oy))
            (val (get-val s p))
            (fact (if (> dist r)
                    1
                    (/ dist r))))
      (clamp (* fact val) 0.07 0.94))))

(defclass binary-filter (filter)
  ())

(defun binary& (s &optional n)
  (make-instance 'binary-filter
    :name "binary"
    :source s
    :params (list
              (param "n" (or n 0.5)))))

(defmethod get-val ((f binary-filter) (p point))
  (let ((n (find-param f "n"))
         (s (source f)))
    (if (>= (get-val s p) n)
      1
      0
      ;; (get-val s p)
      )))

(defclass signal-mask (filter)
  ())
(defun mask& (s mask-sig &optional n)
  (make-instance 'signal-mask
    :name "signal-mask"
    :source s
    :params (list
              (param "n" (or n 1))
              (param "sig" mask-sig))))

(defmethod get-val ((f signal-mask) (p point))
  (let ((n (find-param f "n"))
         (sig (find-param f "sig"))
         (s (source f)))
    (if (equal (get-val sig p) n)
      (get-val s p) 0)))


(defclass edge-filter (filter)
  ())
(defun edge& (n s)
  (make-instance 'edge-filter
    :name "edge"
    :source s
    :params (list
              (param "n" n))))

(defun edge-checker- (x y n ss)
  "If true, this spot is close to an edge"
  (not
    (and
      (not (eq 0 (get-val ss (point (- x n) (- y n)))))
      (not (eq 0 (get-val ss (point (- x n) y))))
      (not (eq 0 (get-val ss (point (- x n) (+ y n)))))
      (not (eq 0 (get-val ss (point x (- y n)))))
      (not (eq 0 (get-val ss (point x (+ y n)))))
      (not (eq 0 (get-val ss (point (+ x n) (- y n)))))
      (not (eq 0 (get-val ss (point (+ x n) y))))
      (not (eq 0 (get-val ss (point (+ x n) (+ y n))))))))

;; (defvar edge-checker-memo (utils:memoize #'edge-checker-))
(defvar edge-checker-memo  #'edge-checker-)

(defun edge-checker (x y n ss)
  (funcall edge-checker-memo x y n ss))



(defmethod get-val ((f edge-filter) (p point))
  (let ((check 't))
    (progn
      (dotimes (i (find-param f "n"))
        (setf check
          (funcall #'edge-checker
            (get-x p)
            (get-y p)
            (+ 1 i)
            (source f))))
      (if check (get-val (source f) p) 0))))




(defclass not-edge-filter (filter)
  ())
(defun nedge& (n s)
  (make-instance 'edge-filter
    :name "not-edge"
    :source s
    :params (list
              (param "n" n))))


;; ...
(defmethod get-val ((f not-edge-filter) (p point))
  (let* (
          (x (get-x p))
          (y (get-y p))
          (check nil)
          (ss (source f))
          (nn (find-param f "n")))
    (progn
      (loop for i from 1 to nn
        do (setf check (funcall #'edge-checker x y i ss)))
      (if check 0 (get-val ss p)))))


(defclass stretch-val-filter (filter)
  ())

(defun stretch& (s &key (n 0.5) (end 1))
  (if (< end n)
    (error "End must be greater than n")
    (make-instance 'stretch-val-filter
      :name "stretch"
      :source s
      :params (if (and
                    (and (not (< end 1))
                      (not (> end 0)))
                    (and (not (< n 1))
                      (not (> n 0))))
                (error "Both n and end must be between 0.0 and 1.0")
                (list (param "n" n) (param "end" end))))))

(defun stretchN& (s amt &key (n 0.5) (end 1))
  (if (eql amt 0)
    s
    (stretchN& (stretch& s :n n :end end) (- amt 1) :n n :end end)))

(defun map-to-range (is ie os oe val)
  (clamp
    (let ((slope (/ (- oe os) (- ie is))))
      (+ os (* slope (- val is))))
    0 1))

;; (map-to-range 0.5 1 0 1 0.3)

(defmethod get-val ((f stretch-val-filter) (p point))
  "N of 0.5 means we map 0.5-1.0 to 0.0-1.0"
  (let ((n (find-param f "n"))
         (end (find-param f "end"))
         (ss (source f)))
    (map-to-range n end 0 1 (get-val ss p))))

(defclass *-filter (filter)
  ())

(defun *& (s &optional n)
  (let ((n (or n 0.5)))
    (make-instance '*-filter
      :name "multiplier"
      :source s
      :params (list (param "n" n)))))

(defmethod get-val ((f *-filter) (p point))
  (let ((n (find-param f "n"))
         (ss (source f)))
    (clamp (* n (get-val ss p)) 0.01 0.99)))

(defclass router-filter (filter)
  ())

(defun router& (s routes)
  (make-instance 'router-filter
    :name "router"
    :source s
    :params (list
              (param "routes" routes))))


(defun make-ranges-from-end (ends)
  (mapcar
    #'(lambda (enumerated-end)
        (let ((idx (car enumerated-end))
               (val (clamp
                      (cdr enumerated-end)
                      0 1)))
          (cons
            ;; if the first one, 0, else the value of the last one
            (if (eql idx 0)
              0
              (nth (- idx 1) ends))
            val)))
    (utils:enumerate (sort (copy-list ends) #'<))))

(defun find-in-range (value ranges)
  (dolist (enumerated-range
            (enumerate ranges))
    (let ((idx (car enumerated-range))
           (max (cddr enumerated-range))
           (min (cadr enumerated-range)))
      (if (and (>= value min )
            (<= value max))
        (return idx)))))



(defmethod get-val ((f router-filter) (p point))
  (* (/ 1 (length (children (source f))))
    (find-in-range
      (get-val (source f) p)
      (make-ranges-from-end
        (find-param f "routes")))))

(defclass warp-filter (filter) ())

(defun warp& (s &key
               (offset-a1 (point 0 0))
               (offset-a2 (point 0 0))
               (offset-b1 (point 0 0))
               (offset-b2 (point 0 0))
               (amount 100)
               (simple nil))
  (make-instance 'warp-filter
    :name "warp-filter"
    :source s
    :params (list
              (param "offset-a1" offset-a1)
              (param "offset-a2" offset-a2)
              (param "offset-b1" offset-b1)
              (param "offset-b2" offset-b2)
              (param "simple" simple)
              (param "amount" amount))))



(defun get-warped-value (sig f amount simple px py)
  (let* ( (p (point px py))
          (q (point
               (get-val sig (+p p (find-param f "offset-a1")))
               (get-val sig (+p p (find-param f "offset-a2")))))
          (r (if (not simple)
               (point
                 (get-val sig (+p p
                                (+p (*p amount q)
                                  (find-param f "offset-b1"))))
                 (get-val sig (+p p
                                (+p (*p amount q)
                                  (find-param f "offset-b2"))))))))
    (get-val sig (+p p (*p amount (if simple q r))))))

(defvar get-warped-memo (memoize #'get-warped-value))
(defmethod get-val ((f warp-filter) (p point))
  (let ( (sig (source f))
         (amount (find-param f "amount"))
         (simple (find-param f "simple")))
    (get-warped-value sig f amount simple (get-x p) (get-y p))))


(defmethod serialize ((obj filter))
  (list :name (name obj)
    :source (serialize (source obj))
    :params (mapcar #'(lambda (d) (serialize d)) (params obj))))


                                        ;signal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass sig (named parameterized parent value)
  ())

(defun sig (name params children)
  (let ((ps (map 'list
              #'(lambda (p) (param (car p) (cadr p)))
              params)))
    (make-instance 'sig
      :name name
      :params ps
      :children children)))

(defmethod serialize ((obj sig))
  (append  (list :type "signal") (next-m)))
(defgeneric is-signal (obj)
  (:method (obj)
    nil))
(defmethod is-signal ((obj sig))
  t)

(defmethod resolve-terrains ((obj value) (x number) (y number))
  (let* ((sig-val (get-val obj (point x y)))
          (placement (place-signal sig-val (length (children obj)))))
    (if placement
      (resolve-terrains
        (nth placement
          (children obj)) x y))))

(defmethod resolve-value ((obj value) (p point))
  (let* ((sig-val (get-val obj p))
          (out (list sig-val))
          (placement (place-signal sig-val (length (children obj)))))
    (if (and placement (> (length (children obj) ) 0))
      (setf out (append out
                  (resolve-value
                    (nth placement
                      (children obj))
                    p)))
      )
    (flatten out)))


(defmethod resolve-sampled-terrains
  ((obj value) (x number) (y number) (n number) (amount number))
  (let* ((sig-val (sample-val obj (point x y) n amount))
          (placement (place-signal sig-val (length (children obj)))))
    (if placement
      (resolve-terrains
        (nth placement
          (children obj)) x y))))



(defmethod find-param ((s sig) nname)
  (let (found)
    (dolist (p (params s))
      (if (equal nname (name p))
        (setf found (val p))))
    found))

(defclass perlin (sig)
  ())

(defun perlin~ (freq seed children &optional octave)
  (make-instance 'perlin
    :name "perlin"
    :params (list
              (param "freq" freq)
              (param "depth" (or octave 16))
              (param "seed" seed))
    :children children ))

(defvar simpx-memo (memoize #'simplex:simpx))
;; (defvar simpx-memo  #'simplex:simpx)

(defmethod get-val ((obj perlin) (p point))
  (simplex:simpx
    (get-x p)
    (get-y p)
    :seed (find-param obj "seed")
    :freq (find-param obj "freq")
    :octaves (find-param obj "depth")))

(defclass w-perlin (sig)
  ())

(defun w-perlin~ (ox1 oy1 ox2 oy2 freq seed children &optional octave)
  (make-instance 'w-perlin
    :name "w-perlin"
    :params (list
              (param "offset1" (point ox1 oy1))
              (param "offset2" (point ox2 oy2))
              (param "freq" freq)
              (param "depth" (or octave 16))
              (param "seed" seed))
    :children children ))


(defmethod get-val ((obj w-perlin) (p point))
  (let ((os1 (find-param obj "offset1"))
         (os2 (find-param obj "offset2")))
    (simplex:simpx-warped
      (get-x p)
      (get-y p)
      (get-x os1)
      (get-y os1)
      (get-x os2)
      (get-y os2)
      199
      :seed (find-param obj "seed")
      :freq (find-param obj "freq")
      :octaves (find-param obj "depth"))))

(defclass filler (sig)
  ())

(defun filler~ (&optional children n)
  (make-instance 'filler
    :name "filler"
    :params (list (param "n" (or n 1)))
    :children (or children '())))

(defmethod get-val ((obj filler) (p point))
  (let ((n (find-param obj "n")))
    n))

(defclass white-noise (sig)
  ())

(defun wnoise~ (&optional children)
  (make-instance 'white-noise
    :name "white-noise"
    :children (or children '())))

(defmethod get-val ((obj white-noise) (p point))
  (random 1.0))

                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;render stuff;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-top-color (conf x y)
  (progn
    ;;(print (serialize conf))
    (let* ((resolved
             (resolve-terrains conf x y))
            (c (render:parse-rgb-color
                 (if resolved
                   (color (car (last resolved)))
                   #xeeeeee))))
      (list
        (getf c :r)
        (getf c :g)
        (getf c :b)))))

(defun get-top-color-sampled (conf x y n amount)
  (progn
    (let* ((resolved
             (resolve-sampled-terrains conf x y n amount))
            (c (render:parse-rgb-color
                 (if resolved
                   (color (car (last resolved)))
                   #xeeeeee))))
      (list
        (getf c :r)
        (getf c :g)
        (getf c :b)))))


(defun render-conf (conf out w h &optional xoff yoff)
  (render-image-file out w h
    #'(lambda (x y)
        (progn
          (get-top-color
            conf
            (+ x (or xoff 0))
            (+ y (or yoff 0)))))))

(defun render-conf-img (conf w h &optional xoff yoff)
  (render-image w h
    #'(lambda (x y)
        (progn
          (get-top-color
            conf
            (+ x (or xoff 0))
            (+ y (or yoff 0)))))))


;; When a 100x100 grid is scaled by 0.5, we want to check only half as
;; many items. So, the grid is checked in nxn sections, where we sample
;; n^2 * 0.5 points from the section and return a mean. Lets say the higher
;; the resolution, the less the sample-size needs to be, and also that sample size
;; is always related to base size.


(defun render-conf-scaled (conf out w h sample-n amount &optional xoff yoff)
  (render-image-file out w h
    #'(lambda (x y)
        (progn
          (get-top-color-sampled
            conf
            (* sample-n (+ x (or xoff 0)))
            (* sample-n (+ y (or yoff 0)))
            sample-n
            amount)))))


(defun render-conf-scaled-img (conf w h sample-n amount &optional xoff yoff)
  (render-image w h
    #'(lambda (x y)
        (progn
          (get-top-color-sampled
            conf
            (* sample-n (+ x (or xoff 0)))
            (* sample-n (+ y (or yoff 0)))
            sample-n
            amount)))))


                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;write conf;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
            (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defmethod child-sigg ((s value) (children list))
  (if (slot-exists-p s 'source)
    (copy-instance s :source
      (child-sigg (source s) children))
    (let ((out (copy-instance s)))
      (setf (children out) children)
      out)))

                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;pieces;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun make-world-image (w h &optional x y)
  (render-conf-img
    (ocean_)
    w h))

(defun make-world-image-scaled (conf w h scale &optional x y)
  (if (equal scale 1)
    (render-conf-img conf w h x y)
    (let ((amount (numerator (rationalize scale)))
           (n (denominator (rationalize scale))))
      (render-conf-scaled-img
        conf w h n amount x y))))



;; (defun write-file ()
;;   (progn
;;     (if (probe-file confpath)
;;       (delete-file confpath))
;;     (write-string-into-file
;;       (j (serialize finalconf))
;;       confpath :if-exists :overwrite
;;       :if-does-not-exist :create)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-rect (rect-w rect-h n)
    "Given a rect height and width, return n smaller
rectangles that make it up."
    (let ((seg-height (floor (/ rect-h n))))
      (loop :for i :from 0 :to (- n 1)
        :collect
        (list :h seg-height :w rect-w :x 0 :y (* i seg-height))))))

(defun compose-image-from-rows (imgs)
  "given a bunch of rows of an image, starting at the top and going down,
attach those images together"
  (reduce #'(lambda (i c)
              (render:attach-image i c :bottom))
    (cdr imgs) :initial-value (car imgs)))

(defmacro render-big-img (conf w h outpath &key (threads 8) (scale 1) (xoff 0) (yoff 0))
  `(promise-all
     (map
       'list
       #'(lambda (r)
           (await (make-world-image-scaled ,conf
                    (getf r :w) (getf r :h) ,scale
                    (+ ,xoff (getf r :x))
                    (+ ,yoff (getf r :y)))))
       (split-rect ,w ,h ,threads))
     (lambda (l)
       (render:save-image-file
         ,outpath (compose-image-from-rows l)))))



(defun render-img (outpath conf x y w h)
  (render:save-image-file outpath (make-world-image-scaled conf w h 1 x y)))

(defun collect-tiles (conf x y w h)
  (loop
    :for _y :from y :to (+ y h)
    :collect (loop
               :for _x :from x :to (+ x w)
               :collect (resolve-terrains conf _x _y))))

(defmacro iter-terrs (conf x y w h cb &optional place-cb)
  "Callback uses lambda list (terr-id x y alt)"
  `(loop
     :for _y :from ,y :to (- (+ ,y ,h) 1)
     :do (loop
           :for _x :from ,x :to (- (+ ,x ,w) 1)
           :do (progn
                 (funcall ,cb
                   (terr-id
                     (car (last (resolve-terrains ,conf _x _y))))
                   _x _y)
                 (if ,place-cb
                   (funcall ,place-cb _x _y))))))


(defun render-signal (sig x y w h)
  (render-image w h #'(lambda (xx yy)
                        (let ((val
                                (floor
                                  (* 255
                                    (clamp
                                      (get-val sig (point (+ xx x) (+ yy y))) 0 1)))))
                          (list val val val)))))

(defun render-signal-file (output-pathname &key (x 0) (y 0) width height signal num-channels)
  "Generate image from signal"
  (let ((img (render-signal signal x y width height)))
    (save-image-file output-pathname img)))


(defclass wang-config ()
  ((wang-tiles
     :initarg :wang-tiles
     :accessor wang-tiles)
    (file
      :initarg :file
      :accessor file)))

(defmethod serialize ((wc wang-config))
  (append  (list :type "wang-config"
             :file (file wc)
             :tiles (wang-tiles wc))
    (next-m)))


(defun wangconfig (file tiles)
  (make-instance 'wang-config :file file :wang-tiles tiles))

;;
;; export function calcWangVal(x: number, y: number, grid: Grid, check: number) {
;;   let n = 0;
;;   grid.at(x, y) == check ? (n |= 0b1000) : undefined;
;;   grid.at(x + 1, y) == check ? (n |= 0b1) : undefined;
;;   grid.at(x, y + 1) == check ? (n |= 0b100) : undefined;
;;   grid.at(x + 1, y + 1) == check ? (n |= 0b10) : undefined;
;;   return n;
;; }


(defun check--wang (x y g check)
  (reduce
    #'(lambda (acc curr)
        (let ((xoff (car curr))
               (yoff (cadr curr))
               (n (caddr curr)))
          (if
            (eql check
              (grid:@ g
                (+ x xoff)
                (+ y yoff)))
            (logior acc n)
            acc)))
    '((0 0 #b1000)
       (1 0 #b1)
       (0 1 #b100)
       (1 1 #b10))
    :initial-value 0))
