  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage simplex
  (:use :common-lisp :cffi)
  (:export simpx simpx-1 simpx-warped xxh128 xxh64 xxh32))

(in-package simplex)

(define-foreign-library libsimplex
  (:unix  "libsimplex.so")
  (t (:default "libsimplex")))
(use-foreign-library libsimplex)
(defctype size :unsigned-int)

(defcfun "simplex" :float
  (x :float) (y :float)
  (seed size) (octaves size)
  (freq :float) (amp :float)
  (lacuna :float) (persistence :float))


(defcfun "simplex_1" :float
  (x :float)
  (seed size) (octaves size)
  (freq :float) (amp :float)
  (lacuna :float) (persistence :float))

(defun simpx (x y &key (seed 0.0) (octaves 16) (freq 0.1) (amp 1.0) (lac 1.99) (pers 0.5))
  (simplex
   (float x)
   (float y)
   (floor seed)
   (floor octaves)
   (float freq)
   (float amp)
   (float lac)
   (float pers)))

(defun simpx-1 (x &key (seed 0) (octaves 16) (freq 0.1) (amp 1.0) (lac 1.99) (pers 0.5))
  (simplex-1
   (float x)
   (floor seed)
   (floor octaves)
   (float freq)
   (float amp)
   (float lac)
   (float pers)))

(defmacro simpx-compose
    (x y ox oy amt &key
                     (seed 0) (octaves 16) (freq 0.01) (amp 1.0) (lac 1.99) (pers 0.5)
                     ;; (w-seed 0) (w-octaves 16) (w-freq 0.01) (w-amp 1.0) (w-lac 1.99) (w-pers 0.5)
                     ))

(defun simpx-warped
    (x y ox1 oy1 ox2 oy2 amt &key
                               (seed 0) (octaves 16) (freq 0.01) (amp 1.0) (lac 1.99) (pers 0.5)
                               ;; (w-seed 0) (w-octaves 16) (w-freq 0.01) (w-amp 1.0) (w-lac 1.99) (w-pers 0.5)
                               )
  (let ((q
          (list
           (simpx (+ x ox1) (+ y oy1)
                  :seed seed
                  :octaves octaves
                  :freq freq
                  :amp amp
                  :lac lac
                  :pers pers)
           (simpx (+ x ox2) (+ y oy2)
                  :seed seed
                  :octaves octaves
                  :freq freq
                  :amp amp
                  :lac lac
                  :pers pers)
           )
          ))
    (simpx
     (+ x  (* amt (car q)))
     (+ y  (* amt (cadr q)))
     :seed seed
     :octaves octaves
     :freq freq
     :amp amp
     :lac lac
     :pers pers)))

(define-foreign-library libspooky
  (:unix  "libspooky.so")
  (t (:default "libspooky")))
(use-foreign-library libspooky)

(defcfun "spooky_64" :pointer
  (m :string) (length :size)
  (seed :uint64))

(defcfun "strlen" :size
  (m :string))


(defun get-spooky-hash (input seed)
  (foreign-string-to-lisp (foreign-funcall "spooky_128"
                                           :string input
                                           :size (strlen input)
                                           :uint64 seed
                                           :pointer)))


(defun get-spooky-hash-32 (input seed &optional lo)
  (foreign-funcall "spooky_32r"
                   :string input
                   :size (or lo (length input))
                   :uint64 seed
                   :uint32))



(define-foreign-library libxxhash
  (:unix "libxxhash.so")
  (t (:default "libxxhash")))

(use-foreign-library libxxhash)


(defctype xxh64-hash-type :uint64)
(defcstruct xxh128-hash
  (low64 xxh64-hash-type)
  (high64 xxh64-hash-type))


(defun parse-128hash (struct)
  (format nil "~x~x"  (getf struct 'high64) (getf struct 'low64)))

(defun parse-128hash-to-int (struct)
  (logior (ash (getf struct 'high64) 64) (getf struct 'low64)))

(defun XXH128 (input &key (seed 0))
  (let ((output
          (foreign-funcall "XXH3_128bits"
                           :string input
                           :size (strlen input)
                           :uint64 seed
                           (:struct xxh128-hash))))
    (parse-128hash-to-int output)))

(defun XXH64 (input &key (seed 0))
  (foreign-funcall "XXH64"
                   :string input
                   :size (strlen input)
                   :uint64 seed
                   :uint64))

(defun XXH32 (input &key (seed 0))
  (foreign-funcall "XXH32"
                   :string input
                   :size (strlen input)
                   :uint64 seed
                   :uint32))


(defun xyhash (x y &optional &key (seed 0))
  (xxh64
   (format nil "~S-~S" (float x) (float y))
   :seed seed))

(defun jprng (x &optional &key (seed 0))
  (/
   (parse-integer
    (subseq
     (format nil "~X"
             (xxh64
              (format nil "~S" x)
              :seed seed))
     0
     2)
    :radix 16)
   255))

(defun jprng2 (x y &optional &key (seed 0))
  (/
   (parse-integer
    (subseq
     (format nil "~X"
             (xyhash x y :seed seed))
     0 2)
    :radix 16)
   255))



(parse-integer
 (subseq (format nil "~X" (XXH64 "hye")) 0 2)
 :radix 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               voronoi               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun distance-squared (x1 y1 x2 y2)
  (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))

(defun distance (x1 y1 x2 y2)
  (sqrt
   (distance-squared x1 y1 x2 y2)))

(defun distance-manhattan (x1 y1 x2 y2)
  (+
   (abs (- x1 x2))
   (abs (- y1 y2))))

(defun distance-cheby (x1 y1 x2 y2)
  (max
   (abs (- x1 x2))
   (abs (- y1 y2))))

(defun voronoi (x y size distance-func))





;; (define-foreign-library libfastnoise
;;   (:unix  "libfastnoiselite.so")
;;   (t (:default "libfastnoiselite")))
;; (use-foreign-library libfastnoise)

;; (defcenum fnl-noise-type
;;   :fnl_noise_opensimplex2
;;   :fnl_noise_opensimplex2s
;;   :fnl_noise_cellular
;;   :fnl_noise_perlin
;;   :fnl_noise_value_cubic
;;   :fnl_noise_value)

;; (defcenum fnl-rotation-type-3d
;;   :fnl_rotation_none
;;   :fnl_rotation_improve_xy_planes
;;   :fnl_rotation_improve_xz_planes)

;; (defcenum fnl-fractal-type
;;   :fnl_fractal_none
;;   :fnl_fractal_fbm
;;   :fnl_fractal_ridged
;;   :fnl_fractal_pingpong
;;   :fnl_fractal_domain_warp_progressive
;;   :fnl_fractal_domain_warp_independent)

;; (defcenum fnl-cellular-distance-func
;;   :fnl_cellular_distance_euclidean
;;   :fnl_cellular_distance_euclideansq
;;   :fnl_cellular_distance_manhattan
;;   :fnl_cellular_distance_hybrid)

;; (defcenum fnl-cellular-return-type
;;   :fnl_cellular_return_type_cellvalue
;;   :fnl_cellular_return_type_distance
;;   :fnl_cellular_return_type_distance2
;;   :fnl_cellular_return_type_distance2add
;;   :fnl_cellular_return_type_distance2sub
;;   :fnl_cellular_return_type_distance2mul
;;   :fnl_cellular_return_type_distance2div)

;; (defcenum fnl-domain-warp-type
;;   :fnl_domain_warp_opensimplex2
;;   :fnl_domain_warp_opensimplex2_reduced
;;   :fnl_domain_warp_basicgrid)

;; (defcstruct fnl-state
;;   (seed :int)
;;   (frequency :float)
;;   (noise-type fnl-noise-type)
;;   (rotation-type-3d  fnl-rotation-type-3d)
;;   (fractal-type fnl-fractal-type)
;;   (octaves :int)
;;   (lacunarity :float)
;;   (gain :float)
;;   (weighted-strength :float)
;;   (ping-pong-strength :float)
;;   (cell-distance-function fnl-cellular-distance-func)
;;   (cell-return-type fnl-cellular-return-type)
;;   (cell-jitter-mod :float)
;;   (domain-warp-type fnl-domain-warp-type)
;;   (domain-warp-amp :float))

;; (defctype fnl-float :float)
;; (defcfun ("fnlCreateState" fnl-create-state) (:struct fnl-state))
;; (defcfun ("fnlCreateStatePtr" fnl-create-state-ptr) (:pointer fnl-state))
;; (defcfun ("fnlGetNoise2D" fnl-noise-2d) :float
;;   (state (:pointer fnl-state)) (x fnl-float) (y fnl-float))


;; ;; (with-foreign-object (st (:struct fnl-state))
;; ;;   (setf (mem-ref st (:struct fnl-state)) (fnl-create-state)))

;; ;; (let* ((st (fnl-create-state-ptr)))
;; ;;   (fnl-noise-2d st 0.3 1223.2)
;; ;;   ;; (setf (getf st 'noise-type) :fnl_noise_cellular
;; ;;   ;;       (getf st 'seed) 42069)
;; ;;   ;; (render:render-image-file "./fnl.png" 500 500 (lambda (x y)
;; ;;   ;;                                                 (let ((no (integer
;; ;;   ;;                                                            (* 255
;; ;;   ;;                                                               (/
;; ;;   ;;                                                                (+ 1
;; ;;   ;;                                                                   (fnl-noise-2d st (float x) (float y)))
;; ;;   ;;                                                                2)))))
;; ;;   ;;                                                   (list no no no))))
;; ;;   )


;; (let ((state (fnl-create-state)))
;;   (setf (getf state 'frequency) 0.005
;;         (getf state 'cell-return-type) :fnl_cellular_return_type_distance2
;;         (getf state 'noise-type) :fnl_noise_cellular)
;;   (with-foreign-object (st '(:struct fnl-state))
;;     (setf (mem-ref st '(:struct fnl-state)) state)
;;     (render:render-image-file "./fnl.png" 500 500 (lambda (x y)
;;                                                     (let ((no (floor
;;                                                                (* 255
;;                                                                   (/
;;                                                                    (+ 1
;;                                                                       (fnl-noise-2d st (float x) (float y)))
;;                                                                    2)))))
;;                                                       (list no no no))))))
