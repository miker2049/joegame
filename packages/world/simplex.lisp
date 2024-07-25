  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage simplex
  (:use :common-lisp :cffi)
  (:export simpx simpx-1 simpx-warped xxh128 xxh64 xxh32 fnl free-fnl fnl-warp-2d fnl-noise-2d))

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





(define-foreign-library libfastnoise
  (:unix  "libfastnoiselite.so")
  (t (:default "libfastnoiselite")))
(use-foreign-library libfastnoise)

(defcenum fnl-noise-type
  :fnl_noise_opensimplex2
  :fnl_noise_opensimplex2s
  :fnl_noise_cellular
  :fnl_noise_perlin
  :fnl_noise_value_cubic
  :fnl_noise_value)

(defcenum fnl-rotation-type-3d
  :fnl_rotation_none
  :fnl_rotation_improve_xy_planes
  :fnl_rotation_improve_xz_planes)

(defcenum fnl-fractal-type
  :fnl_fractal_none
  :fnl_fractal_fbm
  :fnl_fractal_ridged
  :fnl_fractal_pingpong
  :fnl_fractal_domain_warp_progressive
  :fnl_fractal_domain_warp_independent)

(defcenum fnl-cellular-distance-func
  :fnl_cellular_distance_euclidean
  :fnl_cellular_distance_euclideansq
  :fnl_cellular_distance_manhattan
  :fnl_cellular_distance_hybrid)

(defcenum fnl-cellular-return-type
  :fnl_cellular_return_type_cellvalue
  :fnl_cellular_return_type_distance
  :fnl_cellular_return_type_distance2
  :fnl_cellular_return_type_distance2add
  :fnl_cellular_return_type_distance2sub
  :fnl_cellular_return_type_distance2mul
  :fnl_cellular_return_type_distance2div)

(defcenum fnl-domain-warp-type
  :fnl_domain_warp_opensimplex2
  :fnl_domain_warp_opensimplex2_reduced
  :fnl_domain_warp_basicgrid)

(defcstruct fnl-state
  (seed :int)
  (frequency :float)
  (noise-type fnl-noise-type)
  (rotation-type-3d  fnl-rotation-type-3d)
  (fractal-type fnl-fractal-type)
  (octaves :int)
  (lacunarity :float)
  (gain :float)
  (weighted-strength :float)
  (ping-pong-strength :float)
  (cell-distance-function fnl-cellular-distance-func)
  (cell-return-type fnl-cellular-return-type)
  (cell-jitter-mod :float)
  (domain-warp-type fnl-domain-warp-type)
  (domain-warp-amp :float))

(defctype fnl-float :float)
(defcfun ("fnlCreateState" fnl-create-state) (:struct fnl-state))
(defcfun ("fnlCreateStatePtr" fnl-create-state-ptr) (:pointer (:struct fnl-state)))
(defcfun ("fnlGetNoise2D" _fnl-noise-2d) :float
  (state (:pointer (:struct fnl-state))) (x fnl-float) (y fnl-float))
(defcfun ("fnlGetNoise3D" _fnl-noise-3d) :float
  (state (:pointer (:struct fnl-state))) (x fnl-float) (y fnl-float) (z fnl-float))

(defcfun ("fnlWarp2D" fnl-warp-2d) :float
  (state (:pointer (:struct fnl-state))) (x fnl-float) (y fnl-float))

(defcfun ("fnlWarp3D" fnl-warp-3d) :float
  (state (:pointer (:struct fnl-state))) (x fnl-float) (y fnl-float) (z fnl-float))
(defmacro fnl-set-state (fnl &body settings)
  `(let* ((stt (getf ,fnl :state))
          (ptrr (getf ,fnl :ptr)))
     ,@(mapcar #'(lambda (s) `(setf (getf stt ',(car s)) ,(cadr s))) settings)
     (setf (mem-ref ptrr '(:struct fnl-state)) stt)
     (list :state stt :ptr ptrr)))

(defmacro fnl (&body settings)
  `(let* ((st (fnl-create-state))
          (ptr (foreign-alloc '(:struct fnl-state))))
     (fnl-set-state (list :state st :ptr ptr) ,@settings)))

(defun free-fnl (fnl)
  (let ((ptr (getf fnl :ptr)))
    (if (pointerp ptr)
        (foreign-free ptr))))

(defun fnl-noise-2d (fnl x y)
  (_fnl-noise-2d (getf fnl :ptr) x y))

(defun fnl-test-image (fnl path width height)
  (render::render-image-file path width height
                             #'(lambda ( x y )
                                 (let* ((fnl-raw (fnl-warp-2d (getf fnl :ptr) (float x) (float y)))
                                        (fnl-normal (/ (+ 1 fnl-raw) 2))
                                        (uint (floor (* 255 fnl-normal))))
                                   (list uint uint uint)))))
