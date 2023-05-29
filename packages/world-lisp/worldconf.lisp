(ql:quickload 'jonathan)
(ql:quickload 'alexandria)
(load "./simplex.lisp")
(load "./render.lisp")
(load "./utils.lisp")
(load "./grid.lisp")
(defpackage "worldconf" (:use
                            :cl

                            :render
                            :simplex
                            :utils
                            :grid

                            :jonathan
                            :alexandria))
(in-package "worldconf")
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;utilities;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-terrain (name &optional color file)
    `(progn
         (defun ,name (&rest children)
             (filler~ (list
                          (make-instance 'terrain :name ,(symbol-name name) :color ,color
                              :children children))))))

(defmacro next-m ()
    '(when (next-method-p) (call-next-method)))

(defun place-signal (val n)
    "Given a signal value val, place it among one of n children.
Returning nil means don't place."
    (let ((val (clamp val 0 0.99999)))
        (if (equal n 0)
            nil
            (let* ((curr 1)
                   (div (/ 1 (max n 2))))
                (if (eq n 1)
                    (if (>= val 0.5)
                        0 nil)
                    (loop when
                        (> (* curr div) val)
                        return (- curr 1)
                        do (incf curr)))))))




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
    n)

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

(print-point
    (get-random-point-int (point 1 1) (point 10 10)))

(defun randpoint (n)
    )


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

(defmethod /p ((p1 point) (p2 point) )
    (op-point p1 p2 /))


                                        ;value;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass value () ())

(defmethod serialize ((obj value))
    (next-m))

(defgeneric get-val (obj p)
    (:method (obj p) -1))

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


                                        ;terrain;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass terrain (named parent value)
    ((color
         :initarg :color
         :accessor color)))
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
            ;; (print (format nil "~S HEYY" child))
            (setf out (append out (resolve-terrains child x y))))
        out))
                                        ;filters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass filter (named parameterized value)
    ((source :initarg :source
         :accessor source)))
(defmethod children ((f filter))
    (children (source f)))


(defclass circle-filter (filter)
    ())
(defun circle& (s p &optional r amount)
    (make-instance 'circle-filter
        :name "circle"
        :source s
        :params (list
                    (param "amount" (or amount 0.5))
                    (param "r" (or r 100))
                    (param "x" (get-x p))
                    (param "y" (get-y p)))))

(defmethod get-val ((f circle-filter) (p point))
    (let
        ((s (source f))
            (amount (find-param f "amount"))
            (ox (find-param f "x"))
            (oy (find-param f "y"))
            (r (find-param f "r")))
        (let* ((dist (e-distance x y ox oy))
                  (val (get-val s p))
                  (fact (if (< dist r)
                            (+ 1 (* amount
                                     (- 1 (/ dist r))))
                            1)))
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
            1 0)))

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

(defun edge-checker (x y n ss)
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

(defmethod get-val ((f edge-filter) (p point))
    (let* (
              (x (get-x p))
              (y (get-y p))
              (check 't)
              (ss (source f))
              (nn (find-param f "n"))
              (checker #'edge-checker))
        (progn
            (loop for i from 1 to nn
                do (setf check (funcall checker x y i ss)))
            (if check (get-val ss p) 0))))


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

(defun stretch& (s &optional n)
    (let ((n (or n 0.5)))
        (make-instance 'stretch-val-filter
            :name "stretch"
            :source s
            :params (if (and (not (< n 1))
                            (not (> n 0)))
                        (error "must be between 0.0 and 1.0")
                        (list (param "n" n))))))

(defun map-to-range (is ie os oe val)
    (let ((slope (/ (- oe os) (- ie is))))
        (+ os (* slope (- val is)))))

(defmethod get-val ((f stretch-val-filter) (p point))
    "N of 0.5 means we map 0.5-1.0 to 0.0-1.0"
    (let ((n (find-param f "n"))
             (ss (source f)))
        (map-to-range n 1 0 1 (get-val ss p))))



(defmethod serialize ((obj filter))
    (list :name (name obj)
        :source (serialize (source obj))
        :params (map 'list #'(lambda (d) (serialize d)) (params obj))))


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
                    (param "depth" (or octave 7))
                    (param "seed" seed))
        :children children))

(defmethod get-val ((obj perlin) (p point))
    (simplex:simpx
        (get-x p)
        (get-y p)
        (find-param obj "seed")
        nil
        (find-param obj "freq")))

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



(make-terrain ocean_ #xB7C4CF)
(make-terrain dirt_ #x967E76)
(make-terrain grass_ #xA0D8B3)
(make-terrain deep-grass_ #xA2A378)
(make-terrain sand_ #xEEE3CB)
(make-terrain hard-sand_ #xD7C0AE)
(make-terrain stone_ #xD6E8DB)
(make-terrain cliff_ #x000000)


(defun land~ (&rest children)
    (let ((ss (perlin~ 0.0009 109 '())))
        (child-sigg ss
            (list
                (sand_
                    (stretch&
                        (binary&
                            (child-sigg ss
                                (list
                                    (grass_
                                        (stretch&
                                            (binary&
                                                (child-sigg ss
                                                    children) 0.54))))) 0.52)))))))

(defvar finalconf '()
    "final conf thats turned to json")


(defun cliffs~ ()
    (land~
        (child-sigg (stretch& (land~) 0.4)
            (list
                nil
                (deep-grass_)
                (grass_)
                (hard-sand_)
                (deep-grass_)
                (hard-sand_)
                (grass_)
                (sand_)))))

(setf finalconf
            (ocean_
                (cliffs~)
                (edge& 1
                    (binary& (stretch& (land~ (cliff_)) 0.4) (/ 2 8)))
                (edge& 1
                    (binary& (stretch& (land~ (cliff_)) 0.4) (/ 3 8)))
                (edge& 1
                    (binary& (stretch& (land~ (cliff_)) 0.4) (/ 4 8)))
                (edge& 1
                    (binary& (stretch& (land~ (cliff_)) 0.4) (/ 5 8)))
                (edge& 1
                    (binary& (stretch& (land~ (cliff_)) 0.4) (/ 6 8)))
                ))


(defun make-world-image (w h &optional x y)
    (render-conf-img
        (ocean_)
        w h))

(defun make-world-image-scaled (w h scale &optional x y)
    (if (equal scale 1)
        (render-conf-img finalconf w h x y)
        (let ((amount (numerator (rationalize scale)))
                 (n (denominator (rationalize scale))))
            (render-conf-scaled-img
                finalconf w h n amount))))




(let ((i1 (make-world-image-scaled 200 400 1))
         (i2 (make-world-image-scaled 200 400 1 400 0)))
    ;; (add-chunk i1 i2 400 0 '(0 0 0))
    (save-image-file "cc.png" (attach-image i1 i2)))

(save-image-file "ccc.png" (make-world-image 200 800))

(setf *ti* (make-world-image 20 40))

(paste-pixel *ti* (copy-pixel *ti* 100 100) 50 50)

(defun attach-image (base ol &optional position)
    (if (not position)
        (setf position :right))
    (let* (
              (bw (png:image-width base))
              (bh (png:image-height base))
              (ow (png:image-width base))
              (oh (png:image-height base))
              (vertical (or (eql position :top)
                            (eql position :bottom)))
              (outw (if vertical (max bw ow) (+ bw ow)))
              (outh (if vertical (+ bh oh) (max bh oh)))
              (out (png:make-image outh outw 3))
              (order (if (or (eql position :right)
                             (eql position :bottom))
                         (list base ol)
                         (list ol base))))
        (flet ((gp (out img ox oy x y)
                   (draw-pixel out (copy-pixel img x y) (+ ox x) (+ oy y))))
            (iterate-grid (nth 0 order)
                (curry #'gp out (nth 0 order) 0 0))
            (iterate-grid (nth 1 order)
                (curry #'gp out (nth 1 order)
                    (if vertical
                        0
                        (if (eql position :right)
                            bw
                            ow))
                    (if vertical
                        (if (eql position :bottom)
                            bh
                            oh)
                        0)))
            out)))

(funcall (curry #'(lambda ( x n ) (+ x n)) 3) 1)
(iterate-grid *ti* #'(lambda (x y) (copy-pixel *ti* x y)))

(save-image-file "ccc.png" (render-conf-img finalconf 800 800))


(defvar confpath "worldconf.json")

(defun write-file ()
    (progn
        (if (probe-file confpath)
            (delete-file confpath))
        (write-string-into-file
            (j (serialize finalconf))
            confpath :if-exists :overwrite
            :if-does-not-exist :create)))

(write-file)
