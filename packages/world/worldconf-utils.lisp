;;;; test 1
(defpackage worldconf
  (:use
   :cl
   :joegame-assets

   :async
   :render
   :simplex
   :utils
   :grid

   :jonathan
   :alexandria)
  (:export run
           render-big-img
           render-img
           render-signal-file
           collect-tiles
           split-rect
           serialize
           dump-csv
           terrain-to-wang-size
           wang-to-terrain-size
           get-tiled-map-from-conf
           add-layer-from-wang-val-grid
           get-terrain-grids
           collect-terrain-wang-vals
           generate-asset-pack
           get-wang-serial
           make-world-view

           install-terrains
           *terrain-wang-tiles*
           *thick-terrain-wang-tiles*
           *worldconf*
           *area-set*
           *terrain-set*
           *world-view*
           *world-size*
           ;; wv-funcs
           make-world-image-scaled
           wv-sig
           wv-xoff
           wv-yoff
           wv-width
           wv-height
           point
           <p
           >p
           +p
           +pv
           max-point
           get-x get-y
           min-point))
(in-package worldconf)

;; (defun get-asset-path (p)
;;   (concatenate 'string (asset-path) p))
;; (declaim (optimize (speed 0) (space 0) (debug 3)))

;;;; utilities



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


;; (unless (eql n 0)
;;   (floor
;;    (/ (clamp val 0 0.99999) (/ 1 n)))))

(defun downcase (s)
  (map 'string #'(lambda (char) (char-downcase char))
       s))
(defun upcase (s)
  (map 'string #'(lambda (char) (char-upcase char))
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

;; Row major
(defgeneric >p (p1 p2)
  (:method (p1 p2)
    (or p1 p2)))
(defmethod >p ((p1 point) (p2 point))
  (or
   (> (get-y p1) (get-y p2))
   (and (eql (get-y p1) (get-y p2))
        (> (get-x p1) (get-x p2)))))

(defgeneric <p (p1 p2)
  (:method (p1 p2)
    (or p1 p2)))
(defmethod <p ((p1 point) (p2 point))
  (>p p2 p1))

(defmethod max-point ((p1 point) (p2 point))
  (if (>p p1 p2)
      p1 p2))

(defmethod min-point ((p1 point) (p2 point))
  (if (<p p1 p2)
      p1 p2))



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

(defun param-hashtable (&rest params)
  (let ((tbl (make-hash-table)))
    (loop for p in params
          do (setf (gethash (intern (upcase (name p)) 'keyword) tbl) p))
    tbl))

(defun param (name val)
  (make-instance 'param :name name :val val))

(defmethod serialize ((obj parameterized))
  (append (next-m)
          (list :params
                (map 'list #'(lambda (d) (serialize d))
                     (alexandria:hash-table-values (params obj))))))

(defmethod serialize ((obj param))
  (list (name obj)  (serialize (val obj))))



(defmethod find-param ((obj parameterized) k)
  (val
   (or
    (gethash k (params obj))
    (param "nn" nil))))

                                        ;router;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass router (named children)
  ((handler
    :initarg :handler
    :accessor handler)
   (sigg
    :initarg :sigg
    :accessor sigg)))


                                        ;terrain;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass metaterrain (named parent value)
  ((display-name
    :initarg :display-name
    :initform ""
    :accessor display-name)
   (color
    :initarg :color
    :accessor color)
   (id
    :initarg :id
    :accessor terr-id)))

(defclass area (metaterrain)
  ((sig
    :initarg :signal
    :accessor area-signal)))

(defmethod print-object ((area area) out)
  (print-unreadable-object (area out :type t)
    (format out "~s" (name area))))


(defclass terrain (metaterrain)
  ())


(defun terr (name color &rest children)
  (make-instance 'terrain :name name :color color :children children))
(defmethod serialize ((obj metaterrain))
  (append (list :type "terrain" :color (color obj)) (next-m)))
(defgeneric is-terrain (obj)
  (:method (obj)
    nil))
(defmethod is-terrain ((obj metaterrain))
  t)

(defmethod resolve-terrains ((obj metaterrain) (x number) (y number))
  (let ((out (list obj)))
    (dolist (child (children obj))
      (setf out (append out (resolve-terrains child x y))))
    out))

(defmethod resolve-value ((obj metaterrain) (p point))
  (let ((out (list)))
    (dolist (child (children obj))
      (setf out
            (append out
                    (list (resolve-value child p)))))
    out))

                                        ;filters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass filter (named parameterized parent value)
  ((source :initarg :source
           :accessor source)
   (type :initform :filter :accessor sig-type)))

(defmethod children ((f filter))
  (children (source f)))


(defclass circle-filter (filter)
  ())
(defun circle& (s p &optional r amount (max 1))
  (make-instance 'circle-filter
                 :name "circle"
                 :source s
                 :params (param-hashtable
                          (param "amount" (or amount 0.5))
                          (param "r" (or r 100))
                          (param "x" (get-x p))
                          (param "max" max)
                          (param "y" (get-y p)))))

(defmethod get-val ((f circle-filter) (p point))
  (let
      ((s (source f))
       (amount (find-param f :amount))
       (ox (find-param f :x))
       (oy (find-param f :y))
       (max (find-param f :max))
       (x (get-x p))
       (y (get-y p))
       (r (find-param f :r)))
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
                 :params (param-hashtable
                          (param "amount" (or amount 0.5))
                          (param "r" (or r 100))
                          (param "x" (get-x p))
                          (param "y" (get-y p)))))

(defmethod get-val ((f inside-circle-filter) (p point))
  (let
      ((s (source f))
       (amount (find-param f :amount))
       (ox (find-param f :x))
       (oy (find-param f :y))
       (x (get-x p))
       (y (get-y p))
       (r (find-param f :r)))
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
                 :params (param-hashtable
                          (param "amount" (or amount 0.5))
                          (param "r" (or r 100))
                          (param "x" (get-x p))
                          (param "y" (get-y p)))))

(defmethod get-val ((f not-circle-filter) (p point))
  (let
      ((s (source f))
       (amount (find-param f :amount))
       (ox (find-param f :x))
       (oy (find-param f :y))
       (x (get-x p))
       (y (get-y p))
       (r (find-param f :r)))
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
                 :params (param-hashtable
                          (param "n" (or n 0.5)))))

(defmethod get-val ((f binary-filter) (p point))
  (let ((n (find-param f :n))
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
                 :params (param-hashtable
                          (param "n" (or n 1))
                          (param "sig" mask-sig))))

(defmethod get-val ((f signal-mask) (p point))
  (let ((n (find-param f :n))
        (sig (find-param f :sig))
        (s (source f)))
    (if (equal (get-val sig p) n)
        (get-val s p) 0)))


(defclass edge-filter (filter)
  ())
(defun edge& (n s)
  (make-instance 'edge-filter
                 :name "edge"
                 :source s
                 :params (param-hashtable
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
      (dotimes (i (find-param f :n))
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
                 :params (param-hashtable
                          (param "n" n))))


;; ...
(defmethod get-val ((f not-edge-filter) (p point))
  (let* (
         (x (get-x p))
         (y (get-y p))
         (check nil)
         (ss (source f))
         (nn (find-param f :n)))
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
                                 (param-hashtable (param "n" n) (param "end" end))))))

(defun stretchN& (s amt &key (n 0.5) (end 1))
  (if (eql amt 0)
      s
      (stretchN& (stretch& s :n n :end end) (- amt 1) :n n :end end)))

(defun map-to-range (is ie os oe val)
  (clamp
   (let ((slope (/ (- oe os) (- ie is))))
     (+ os (* slope (- val is))))
   0 1))


(defmethod get-val ((f stretch-val-filter) (p point))
  "N of 0.5 means we map 0.5-1.0 to 0.0-1.0"
  (let ((n (find-param f :n))
        (end (find-param f :end))
        (ss (source f)))
    (map-to-range n end 0 1 (get-val ss p))))

(defclass *-filter (filter)
  ())

(defun *& (s &optional n)
  (let ((n (or n 0.5)))
    (make-instance '*-filter
                   :name "multiplier"
                   :source s
                   :params (param-hashtable (param "n" n)))))

(defmethod get-val ((f *-filter) (p point))
  (let ((n (find-param f :n))
        (ss (source f)))
    (clamp (* n (get-val ss p)) 0.01 0.99)))

(defclass router-filter (filter)
  ())

(defun router& (s routes)
  (make-instance 'router-filter
                 :name "router"
                 :source s
                 :params (param-hashtable
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
       (find-param f :routes)))))

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
                 :params (param-hashtable
                          (param "offset-a1" offset-a1)
                          (param "offset-a2" offset-a2)
                          (param "offset-b1" offset-b1)
                          (param "offset-b2" offset-b2)
                          (param "simple" simple)
                          (param "amount" amount))))

(defun get-warped-value (sig f amount simple px py)
  (let* ( (p (point px py))
          (q (point
              (get-val sig (+p p (find-param f :offset-a1)))
              (get-val sig (+p p (find-param f :offset-a2)))))
          (r (if (not simple)
                 (point
                  (get-val sig (+p p
                                   (+p (*p amount q)
                                       (find-param f :offset-b1))))
                  (get-val sig (+p p
                                   (+p (*p amount q)
                                       (find-param f :offset-b2))))))))
    (get-val sig (+p p (*p amount (if simple q r))))))

(defvar get-warped-memo (memoize #'get-warped-value))
(defmethod get-val ((f warp-filter) (p point))
  (let ( (sig (source f))
         (amount (find-param f :amount))
         (simple (find-param f :simple)))
    (get-warped-value sig f amount simple (get-x p) (get-y p))))


(defmethod serialize ((obj filter))
  (list :name (name obj)
        :source (serialize (source obj))
        :type "filter"
        :params (mapcar #'(lambda (d) (serialize d)) (alexandria:hash-table-values (params obj)))))


                                        ;signal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass sig (named parameterized parent value)
  ((type :initform :sig :accessor sig-type)))

(defun sig (name params children)
  (let ((ps (map 'list
                 #'(lambda (p) (param (car p) (cadr p)))
                 params)))
    (make-instance 'sig
                   :name name
                   :params (apply #'param-hashtable ps)
                   :children children)))

(defmethod serialize ((obj sig))
  (append  (list :type "signal") (next-m)))
(defgeneric is-signal (obj)
  (:method (obj)
    nil))
(defmethod is-signal ((obj sig))
  t)

(defun _resolve-terrain (curr val children)
  (if (or (not children)
          (> (caar children) val))
      curr
      (_resolve-terrain (car children) val (cdr children))))


(defmethod resolve-terrains ((obj value) (x number) (y number))
  (let* ((children (children obj))
         (next-child
           (_resolve-terrain
            (car children)
            (get-val obj (point x y))
            (cdr children))))
    (append (cdr next-child)
            (resolve-terrains (cadr next-child) x y))))

(defmethod resolve-value ((obj value) (p point))
  (let* ((sig-val (get-val obj p))
         (out (list sig-val))
         (placement (place-signal sig-val (length (children obj)))))
    (if (and placement (> (length (children obj) ) 0))
        (setf out (append out
                          (resolve-value
                           (nth placement
                                (children obj))
                           p))))
    (flatten out)))


(defmethod resolve-sampled-terrains
    ((obj value) (x number) (y number) (n number) (amount number))
  (let* ((sig-val (sample-val obj (point x y) n amount))
         (placement (place-signal sig-val (length (children obj)))))
    (if placement
        (resolve-terrains
         (nth placement
              (children obj)) x y))))



(defmethod find-param ((obj sig) k)
  (val
   (or
    (gethash k (params obj))
    (param "nn" nil))))

(defclass perlin (sig)
  ((fnl
     :accessor perlin-fnl
     :initarg :fastnoise)))

(defun perlin~ (freq seed children &optional octave)
  (make-instance 'perlin
                 :fastnoise (fnl
                              (simplex::noise-type :fnl_noise_perlin)
                              (simplex::fractal-type :fnl_fractal_fbm)
                              (simplex::gain 0.8)
                              (simplex::lacunarity 2.0)
                              (simplex::frequency freq) (simplex::seed seed) (simplex::octaves (or octave 16)))
                 :name "perlin"
                 :params (param-hashtable
                          (param "freq" freq)
                          (param "octaves" (or octave 16))
                          (param "seed" seed))
                 :children children ))

(defvar simpx-memo (memoize #'simplex:simpx))
;; (defvar simpx-memo  #'simplex:simpx)

(defmethod get-val ((obj perlin) (p point))
  (/
   (+ 1
      (fnl-noise-2d
       (perlin-fnl obj)
       (float
        (get-x p))
       (float
        (get-y p))))
   2))

(defclass warped-perlin (perlin)
  ())

(defun warped-perlin~ (freq seed children &optional octave amount)
  (make-instance 'perlin
                 :fastnoise (fnl
                              (simplex::noise-type :fnl_noise_perlin)
                              (simplex::fractal-type :fnl_fractal_domain_warp_progressive)
                              (simplex::lacunarity 2.0)
                              (simplex::domain-warp-amp (or amount 1.0))
                              (simplex::domain-warp-type :fnl_domain_warp_opensimplex2)
                              (simplex::frequency freq) (simplex::seed seed) (simplex::octaves (or octave 16)))
                 :name "perlin"
                 :params (param-hashtable
                          (param "freq" freq)
                          (param "octaves" (or octave 16))
                          (param "amount" (or amount 1.0))
                          (param "seed" seed))
                 :children children ))

(defmethod get-val ((obj warped-perlin) (p point))
  (/
   (+ 1
      (fnl-warp-2d
       (perlin-fnl obj)
       (float
        (get-x p))
       (float
        (get-y p))))
   2))

(defclass filler (sig)
  ())

(defun filler~ (&optional children n)
  (make-instance 'filler
                 :name "filler"
                 :params (param-hashtable (param "n" (or n 1)))
                 :children (or children '())))



(defmethod get-val ((obj filler) (p point))
  (let ((n (find-param obj :n)))
    n))

(defclass white-noise (sig)
  ())

(defun wnoise~ (&optional children)
  (make-instance 'white-noise
                 :name "white-noise"
                 :children (or children '())))

(defmethod get-val ((obj white-noise) (p point))
  (random 1.0))

                                        ;terrain configuration;;;;;;;;;;;;;;;;;
(defclass terrain-config (named)
  ((color
    :initarg :color
    :initform #x000000
    :accessor terrain-color)
   (tileset
    :initarg :tileset
    :accessor terrain-tileset)
   (id
    :initarg :id
    :accessor terrain-id)
   (priority
    :initarg :priority
    :initform 10
    :accessor terrain-priority)
   (wang-template
    :initarg :wang-template
    :initform :terrain
    :accessor terrain-wang-template)))

(defun make-terrain-config (name &key color tileset (wang-template :terrain) id priority)
  (make-instance 'terrain-config
                 :priority priority
                 :id id
                 :color color
                 :wang-template wang-template
                 :tileset tileset
                 :name name))

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


(defun render-conf-terr-img-layer (conf w h &optional xoff yoff)
  (let ((terrs
          (get-terrain-grids conf
                             (or xoff 0) (or yoff 0) w h)))
    (render-image (* 4 w) (* 4 h)
                  #'(lambda (x y)
                      (dolist (layer terrs)
                        (let ((c (parse-rgb-color (color (grid:at layer x y)))))
                          (list
                           (getf c :r)
                           (getf c :g)
                           (getf c :b))))))))

(defun render-conf-terr-img (conf w h &optional xoff yoff)
  (let ((terrs
          (get-terrain-grids conf
                             (or xoff 0) (or yoff 0) w h)))
    (render-image (* 4 w) (* 4 h)
                  #'(lambda (x y)
                      (let ((colors
                              (loop :for layer
                                      :in terrs
                                    :collect
                                    (let ((c (parse-rgb-color (color (grid:at layer x y)))))
                                      (list
                                       (getf c :r)
                                       (getf c :g)
                                       (getf c :b))))))

                        (or
                         (cadr colors)
                         (car colors)
                         '(255 255 255)))))))




(defun render-conf-scaled (conf out w h sample-n amount &optional xoff yoff)
  "When a 100x100 grid is scaled by 0.5, we want to check only half as
many items. So, the grid is checked in nxn sections, where we sample
n^2 * 0.5 points from the section and return a mean. Lets say the higher
the resolution, the less the sample-size needs to be, and also that sample size
is always related to base size."

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

(defmethod child-sigg2 ((s value) (children list))
  (if (slot-exists-p s 'source)
      (setf (source s) (child-sigg (source s) children))
      (setf (children s) children))
  s)

(defun child-sigg (s children)
  (if (eql (sig-type s) :filter)
      (copy-instance s :source
                     (child-sigg (source s) children))
      (let ((out (copy-instance s)))
        (setf (children out) children)
        out)))

(defmacro process-rest-children (&body children)
  `(list ,@(loop for (n val) on children by #'cddr
                 collect `(list ,n ,(if (numberp (car val))
                                        `(process-rest-children ,@val)
                                        val)))))

(defun map-children-to-range (children curr-min curr-max)
  (loop for (n val) in children
        collect (list (map-to-range 0.0 1.0 curr-min curr-max n) val)))

(defun unnest-children (children)
  ;; (loop for item in
  (loop for (curr next) on children by #'cdr
        if (and (listp (cadr curr))
                (numberp (caaadr curr)))
          appending (unnest-children
                     (map-children-to-range
                      (cadr curr) (car curr) (or (car next) 1.0)))
        else appending (list curr)))
;; append (cadr item)))

(defmacro <> (s &rest children)
  `(child-sigg ,s (unnest-children (process-rest-children ,@children))))

                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;pieces;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-world-image-scaled (conf w h scale &optional x y)
  (if (equal scale 1)
      (render-conf-img conf w h x y)
      (let ((amount (numerator (rationalize scale)))
            (n (denominator (rationalize scale))))
        (render-conf-scaled-img
         conf w h n amount x y))))


(defun split-rect (rect-w rect-h n)
  "Given a rect height and width, return n smaller
rectangles that make it up."
  (let ((seg-height (floor (/ rect-h n))))
    (loop :for i :from 0 :to (- n 1)
          :collect
          (list :h seg-height :w rect-w :x 0 :y (* i seg-height)))))

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


(defun iter-terrs-generic (conf x y w h cb &key place-cb (accsr #'terr-id))
  "Callback uses lambda list (terr-id x y alt length), where length is the length of this stack."
  (loop
    :for _y :from y :below (+ y h)
    :do (loop
          :for _x :from x :below (+ x w)
          :do (let* ((terrs (resolve-terrains conf _x _y))
                     (ll (length terrs)))
                (loop
                  :for alt :from 0 :below ll
                  :do (progn
                        (funcall cb
                                 (funcall accsr
                                          (nth alt terrs))
                                 _x _y alt ll)
                        (if place-cb
                            (funcall place-cb _x _y))))))))

(defun iter-terrs-alt (conf x y w h cb &key place-cb (accsr #'terr-id))
  "Callback uses lambda list (terr-id x y alt)"
  (iter-terrs-generic conf x y w h
                      #'(lambda (terr xx yy alt length)
                          (funcall cb terr xx yy alt))
                      :place-cb place-cb
                      :accsr accsr))

(defun iter-terrs (conf x y w h cb &key place-cb (accsr #'terr-id))
  "Callback uses lambda list (terr-id x y), gives the last terr. Suitable for image making."
  (iter-terrs-generic conf x y w h
                      #'(lambda (terr xx yy alt length)
                          (if (eql alt (- 1 length))
                              (funcall cb terr xx yy)))
                      :place-cb place-cb
                      :accsr accsr))

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




(defmethod terr-id ((s sig))
  (terr-id
   (car (children s))))


(defmacro router&& (sigg &rest rs)
  `(router&
    (child-sigg ,sigg
                (list ,@(mapcar #'cdr rs)))
    ',(mapcar #'car rs)))

(defmacro fade% (sig terr-a terr-b &key (iters 8))
  `(list
    ,@(loop :for idx to (- iters 1)
            :collect `(router&& ,sig
                       (,(* idx (/ 1 iters)) . (__ ,terr-a))
                       (1 . (__ ,terr-b))))))




(defun create-routes (ll &key (func #'(lambda (idx) (/ idx ll))))
  (loop
    for ii to (- ll 1)
    :collect (funcall func ii)))

(defun create-compressed-routes (amt n max)
  "Compress the first n children of amt children to stop at max,
where the rest of children have even distance"
  (create-routes amt
                 :func #'(lambda (idx)
                           (if (< idx n)
                               (map-to-range 0 1 0 max (/ (+ 1 idx) amt))
                               (/ (+ 1 idx) amt)))))


(defmacro blend% (ss bss iters from-t to-t amt children )
  `(router&
    (child-sigg
     ,ss
     (append
      (fade% ,bss
             ,to-t ,from-t
             :iters ,iters)
      ,children))
    (create-compressed-routes (+ 1 ,iters) ,iters ,amt)))


(defmacro get-wang-quad (grd wang-n &key (quad-size 4))
  "Wang tiles are stored as regular grid of normalized
tileids (i.e. a single layer on a map with only one tileset of firstgid 0.
You supply this "
  `(grid:get-sub-arr
    ,@(mapcar
       #'(lambda (item)
           (* item quad-size))
       (nth wang-n
            '( (0 0 1 1) (1 0 1 1) (2 0 1 1) (3 0 1 1)
              (0 1 1 1) (1 1 1 1) (2 1 1 1) (3 1 1 1)
              (0 2 1 1) (1 2 1 1) (2 2 1 1) (3 2 1 1)
              (0 3 1 1) (1 3 1 1) (2 3 1 1) (3 3 1 1))))
    ,grd))

(defun get-wang-value
    (x y g &key (accsr #'identity) (check 1) (equal-fn #'eql))

  "A wang value is a number 0-15, derived from
which corners of a 2x2 grid have the currently
considered value check which is by default '1'. A
grid g is checked at the point defined by x,y,
where the topleft corner is x,y top right is
(x+1)/y, etc.

Argument :accessor allows to customize the value to
read from an entity in the array, :equal-fn the
compare function used, and :check the value to
check for"

  (reduce
   #'(lambda (acc curr)
       (let ((xoff (car curr))
             (yoff (cadr curr))
             (n (caddr curr)))
         (if (funcall equal-fn check
                      (funcall accsr
                               (grid:at g
                                        (+ x xoff)
                                        (+ y yoff))))
             (logior acc n)
             acc)))
   '((0 0 #b1000)
     (1 0 #b1)
     (0 1 #b100)
     (1 1 #b10))
   :initial-value 0))

(defun to-wang-value-grid
    (g &key (accsr #'identity) (check 1) (equal-fn #'eql))

  "Loop through overlapping 2x2 chunks of a grid
to create wang chunks, and then map those to wang values"

  (loop :for y
          :from 0
            :below (- (get-height g) 1)
        :collect
        (loop :for x
                :from 0
                  :below (- (get-width g) 1)
              :collect
              (get-wang-value x y g
                              :accsr accsr
                              :check check
                              :equal-fn equal-fn))))

(defun to-wang-value-grid-scale
    (g &key (accsr #'identity) (check 1) (equal-fn #'eql))

  "Loop through overlapping 2x2 chunks of a grid
to create wang chunks, and then map those to wang values"
  (let ((sg
          (get-sub-arr 1 1
                       (- (* 2 (get-width g)) 2)
                       (- (* 2 (get-height g)) 2)
                       (scale-grid g 2))))
    (loop :for y
            :from 0
              :below (- (get-height sg) 0)
                :by 2
          :collect
          (loop :for x
                  :from 0
                    :below (- (get-width sg) 0)
                      :by 2
                :collect
                (get-wang-value x y sg
                                :accsr accsr
                                :check check
                                :equal-fn equal-fn)))))
;; XXXX
;; X0X0X0X0
;; 0X0X0X

(defun collect-terrain-stacks (conf x y w h)
  "Will collect either area or terrain stacks."
  (make-grid w h
             (loop
               :for _y :from y :below (+ y h)
               :collect (loop
                          :for _x :from x :below (+ x w)
                          :collect (resolve-terrains conf _x _y)))))

;; part of the desert
;; (let ((stacks (collect-terrain-stacks *worldconf* (* 430 16) (* 420 16) 10 10)))
;;   (mapcar #'(lambda (row) (mapcar #'(lambda (stack) (mapcar #'name stack)) row)) stacks))

(defgeneric area-to-terrain-chunk (ar p))

(defvar empty-chunk (grid:make-empty-grid 4 4 0))

(defmethod area-to-terrain-chunk ((ar (eql 0)) (p point))
  empty-chunk)



;; "Returns a 4x4 chunk as list of the terrains
;; implied by this position and area.  The area is "
(defmethod area-to-terrain-chunk ((ar area) (p point))
  (grid:chunk-list-to-grid
   (let ((sig (area-signal ar)))
     (mapcar
      #'(lambda (offsets)
          (resolve-terrains sig
                            (+ (car offsets)
                               (get-x p))
                            (+ (cadr offsets)
                               (get-y p))))
      '( (0 0) (0.25 0) (0.5 0) (0.75 0)
        (0 0.25) (0.25 0.25) (0.5 0.25) (0.75 0.25)
        (0 0.5) (0.25 0.5) (0.5 0.5) (0.75 0.5)
        (0 0.75) (0.25 0.75) (0.5 0.75) (0.75 0.75))))
   4))

(defun expand-stacks (sg)
  "Expands a stack grid from
#(((1 2) (1 2))
  ((3 4) (3 4)))
to
#(((1 1) (3 3))
 ((2 2) (4 4)))
Errors if stacks are not the same size"
  (let ((num (length (grid:at sg 0 0))))
    (loop :for n :from 0 :below num
          :collect
          (grid:map-grid
           (grid:make-empty-grid (grid:get-width sg)
                                 (grid:get-height sg)
                                 0)
           #'(lambda (x y)
               (nth n
                    (grid:at sg x y)))))))

(defun get-max-length-in-grid (sg)
  "Takes a grid with items that are themselves lists,
returns the max length of all lists in the grid"
  (let ((out 0))
    (grid:iterate-grid sg
                       #'(lambda (x y)
                           (let ((val (grid:at sg x y)))
                             (setf out (max (if (not (listp val))
                                                0 (length val))
                                            out)))))
    out))

(defun length-or-zero (l)
  (if (listp l)
      (length l) 0))

(defun normalize-stacks (sg &key (pad-direction :prepend) (replacement 0))
  "Makes all stacks in this grid the same size,
by default prepending the replacement value to a stack until
it is the size of the max stack in the input grid"
  (let ((maxlength (get-max-length-in-grid sg)))
    (grid:map-grid sg #'(lambda (x y)
                          (let ((val (grid:at sg x y)))
                            (setf val (if (eql val 0) nil val))
                            (if (< (length-or-zero val) maxlength)
                                (append
                                 (if (eql :append pad-direction)
                                     val)
                                 (loop :for padding
                                         :from 0 :below (- maxlength (length val))
                                       :collect replacement)
                                 (if (eql :prepend pad-direction)
                                     val))
                                val))))))


(defun area-grid-to-terrain-grid (ag)
  "Convert a grid ag of areas into a grid of terrain stacks.
Because each area represents a 4x4 chunk of terrains, we should expect
the new grid to quadrupled in both dimensions. Expects a single grid, one from a
 normalized and expanded list of grids."
  (let
      ((out
         (grid:make-empty-grid
          (* 4
             (grid:get-width ag))
          (* 4
             (grid:get-height ag))
          0)))
    (grid:iterate-grid
     ag
     #'(lambda (x y)
         (grid:add-chunk
          out
          (area-to-terrain-chunk
           (grid:at ag x y)
           (point x y))
          (* x 4)
          (* y 4))))
    out))



(defun get-terrain-grids (conf x y w h)
  "From a toplevel world config, collect an
expanded and normalized grid of terrains."
  (expand-stacks
   (normalize-stacks
    (collect-terrain-stacks conf x y w h))))




(defun --show-terrs (conf x y w h)
  (let ((g (collect-terrain-stacks conf x y w h)))
    (map-grid g #'(lambda (x y) (mapcar #'name (at g x y))))))


(defun get-unique-terrains (tg)
  "Given a terrain grid, get all the unique terrains in it based
on name."
  (unique-grid-items tg
                     :test #'(lambda (a b)
                               (equal (name a) (name b)))))

(defun get-terrain-wang-values (tg terr)
  "Turn a grid of terrains into wang values
based on terr, a name. This will work on areas too,
but will never be used by like that in practice."
  (to-wang-value-grid
   tg
   :accsr #'(lambda (n) (if (eql n 0) nil (name n)))
   :check terr
   :equal-fn #'equal))

(defun get-terr (name)
  "Get a terrain config object by name string."
  (find-if
   #'(lambda (item)
       (equal
        (getf (cdr item) :name) name))
   *terrain-set*))

;; TODO new tileset from terrain-set
(defun get-terr-tileset (name)
  (getf (cdr (get-terr name)) :tileset))


(defclass terrain-wang-layer (named)
  ((data
    :initarg :data
    :accessor data)
   (parent-name
    :initarg :pname
    :accessor pname)))

(defmethod serialize ((twl terrain-wang-layer))
  `(:name ,(name twl) :data ,(data twl)))


(defmethod get-layer-name ((twl terrain-wang-layer))
  (format nil "~a/~a" (parent-name twl) (name twl)))

(defun sort-twl (wvg)
  "Sort a list of 'terrain-wang-layer'."
  (flet ((sort-wvg-func (a b)
           (<
            (getf (cdr (get-terr (name a))) :priority)
            (getf (cdr (get-terr (name b))) :priority))))
    (sort wvg #'sort-wvg-func)))

(defun get-all-terrain-wangs (tg)
  "Given a grid of terrains, return a list of grids
with the format (terrain-name wang-value-grid ...)"
  (mapcar
   #'(lambda (terr)
       (make-instance 'terrain-wang-layer
                      :name (name terr)
                      :pname (or (display-name terr) "foo-terr")
                      :data (get-terrain-wang-values tg (name terr))))
   (mapcar #'identity
           (get-unique-terrains tg))))

(defun terrain-to-wang-size (n)
  "Terrain grids produce wang-value grids
equal to (4*size)-1.  A 2x2 terrain grid produces
a 3x3 wang val grid, 4x4 to 15x15, and so on."
  (- (* 4 n) 1))

(defun wang-to-terrain-size (n)
  "Reverse of `terrain-to-wang-size'."
  (/ (+ 1 n) 4))

;; n = 4p - 1
(defun collect-terrain-wang-vals (conf x y w h)
  "From a top level conf, return a list of resolved terrain
wang values, one list"
  (mapcan
   #'get-all-terrain-wangs
   (get-terrain-grids conf x y w h)))

(defun wang-value-to-tile-subgrid (wg wv)
  "Given a wang grid wg and wang value wv,
get the subgrid represented by the value. Assumes
the grid is even 4x4 division of wg."
  (let ((si (/ (grid:get-height wg) 4)))
    (grid:get-sub-arr
     (* 4 (mod wv 4)) ;;x
     (* 4 (floor (/ wv 4))) ;;y
     si si wg)))


(defun wang-value-grid-to-tile-grid (wg wvg)
  (let ((gg (grid:make-empty-grid
             (* 4 (grid:get-width wvg))
             (* 4 (grid:get-height wvg))
             0)))
    (grid:iterate-grid wvg
                       #'(lambda (x y)
                           (grid:add-chunk
                            gg
                            (wang-value-to-tile-subgrid wg (grid:at wvg x y))
                            (* 4 x) (* 4 y) 0)))
    gg))


(defun add-layer-from-wang-val-grid (wvg map ts lname wang-tiles)
  "Utility for adding layers to a tiledmap MAPP,
where the tileset (TS) is one already added to the map.
The layer will be named NAME.
Should error if the tileset is not in the map."
  ;; (print (tiledmap:firstgid ts))
  (if (not (tiledmap:tileset-in-map map (tiledmap:name ts)))
      (error "Tileset is not detected to be in map.")
      (push
       (make-instance 'tiledmap:tilelayer
                      :data (mapcar
                             #'(lambda (ii)
                                 (if (eql ii 0) 0
                                     (+ (tiledmap:firstgid ts) ii)))
                             (grid:flatten-grid
                              (wang-value-grid-to-tile-grid
                               wang-tiles
                               wvg)))
                      :width (tiledmap:width map)
                      :height (tiledmap:height map)
                      :opacity 1
                      :x 0
                      :y 0
                      :name lname
                      :id (tiledmap:get-next-layer-id map))
       (tiledmap:layers map))))


(defun make-map-from-wang-val-grids (wvg)
  "Argument is a list where each item is a list of rows with a
tileset identifier prepended.  Assumed to be all the same size"
  (let* ((d (data (car wvg)))
         (w (length (car d)))
         (h (length d))
         (m (make-instance 'tiledmap:tiled-map
                           :width (* w 4) :height (* h 4))))
    (dolist (lay wvg)
      (let ((ts (get-terr-tileset (name lay))))
        (tiledmap:add-tileset m ts)
        (add-layer-from-wang-val-grid
         (grid:make-grid w h
                         (data lay))
         m
         ts
         (format nil "~a/~a" (pname lay) (name lay))
         ;; get assigned wvg
         (getf *wang-tile-set*
               (getf (cdr (get-terr (name lay))) :wang-tiles)))))
    ;; reverse layer order
    (setf (tiledmap:layers m) (reverse (tiledmap:layers m)))
    m))


(defmethod set-map-image-dir ((tm tiledmap:tiled-map) dir)
  (setf (tiledmap:tilesets tm)
        (mapcar
         #'(lambda (item)
             (setf (tiledmap:image item)
                   (concatenate 'string
                                dir
                                (if (equal #\/
                                           (car (last (coerce dir 'list))))
                                    "" "/")
                                (file-namestring (tiledmap:image item))))
             item)
         (tiledmap:tilesets tm)))
  tm)

(defun get-tiled-map-from-conf (conf x y w h &key (image-dir "~/joegame/assets/images"))
  (set-map-image-dir
   (make-map-from-wang-val-grids
    (sort-twl
     (collect-terrain-wang-vals conf x y w h)))
   image-dir))

(defun get-tiled-map (x y w h &key (image-dir "~/joegame/assets/images"))
  (get-tiled-map-from-conf *worldconf* x y w h :image-dir image-dir))

(defun list-to-hex-string (l)
  (map 'string #'(lambda (it)
                   (character
                    (format nil "~X" it)))
       l))


(defun get-wang-serial (conf x y file rank)
  (mapcar #'(lambda (it)
              (let ((raw (serialize it)))
                `(:|name| ,(getf raw :name)
                  :|data| ,(list-to-hex-string (mapcan #'identity
                                                       (getf raw :data))))))
          (collect-terrain-wang-vals conf
                                     (+ (* 256 x) (* 32 file))
                                     (+ (* 256 y) (* 32 rank))
                                     33
                                     33)))


;;;; a certain view of a signal, ends up being the main output
;;; of world conf

(defclass world-view ()
  ((width
    :initarg :width
    :accessor wv-width)
   (height
    :initarg :height
    :accessor wv-height)
   (xoff
    :initarg :x
    :accessor wv-xoff)
   (yoff
    :initarg :y
    :accessor wv-yoff)
   (wv-sig
    :initarg :sig
    :accessor wv-sig)))

(defun make-world-view (conf x y w h)
  (make-instance 'world-view :sig conf :width w :height h :x x :y y))





(defun get-terrain-pack-config (terr &optional base-url)
  (if-let ((ft  (cdr (get-terr terr))))
    (let ((path (getf (getf ft :tileset) :imagepath)))
      (utils:fmt "~A" ft)
      (tiledmap:spritesheet-pack-config
       terr
       (if base-url
           (namestring
            (merge-pathnames
             (file-namestring path)
             base-url))
           (namestring path))
       :margin (tiledmap:margin (tiledmap:tileset ft))
       :spacing (tiledmap:spacing (tiledmap:tileset ft))))))

(defun collect-map-tilesets-pack-configs (mmap &optional base-url)
  (mapcar #'(lambda (ts)
              (get-terrain-pack-config (tiledmap:name ts) base-url))
          (tiledmap:tilesets mmap)))

(defun generate-asset-pack (mmap &optional base-url)
  (let ((pack (make-instance 'tiledmap:asset-pack)))
    (setf (tiledmap:pack-files pack)
          (collect-map-tilesets-pack-configs mmap base-url))
    (tiledmap:add-pack-to-map mmap pack)))



(defun get-map (zx zy x y)
  (let ((map (get-tiled-map-from-conf
              *worldconf*
              (+ (* zx 1600 1) (* 10 x))
              (+ (* zy 1600 1) (* 10 y))
              10 10)))
    ;; (generate-asset-pack map "/images/")
    ;; (tiledmap:fix-map-tilesets-path map "/images/")
    (tiledmap:assure-unique-layer-names map)
    map))

#|
Config specific
|#
