  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage simplex
  (:use :common-lisp :cffi)
  (:export simpx simpx-1 simpx-warped xxh128 xxh64 xxh32))

(in-package simplex)

(define-foreign-library libsimplex
  (:unix  "~/joegame/packages/noise/libsimplex.so")
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
  (:unix  "~/joegame/packages/noise/libspooky.so")
  (t (:default "libspooky")))
(use-foreign-library libspooky)

(defcfun "spooky_64" :pointer
    (m :string) (length :size)
     (seed :uint64))

(defcfun "strlen" :size
  (m :string))



(foreign-string-to-lisp (foreign-funcall "echho"
                          :string "H"
                          :size 1
                          :pointer))

(foreign-string-to-lisp (foreign-funcall "echho"
                          :string "H"
                          :size 1
                          :pointer))

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
  (:unix "~/joegame/packages/noise/xxHash/libxxhash.so")
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
