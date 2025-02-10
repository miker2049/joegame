(in-package render)


(defun make-color-range (low high n)
  "Get n colors by interpolating between low and high.
Takes color objects."
  (loop for i from 0 below n
        collect
        (nconc
         (int-to-rgb-list
          (color:encode
           (color:gradient
            (/ i n)
            (list
             (cons 0 low)
             (cons 1 high)))))
         (list 255))))

(defun get-pixel (x y img)
  ;; (declare (type (simple-array unsigned-byte (* * 4)) img))
  (list
   (aref img y x 0)
   (aref img y x 1)
   (aref img y x 2)
   (aref img y x 3)))

(defun set-pixel-from-list (x y img pixel)
  (declare (type png:image img))
  (setf
   (aref img y x 0) (nth 0 pixel)
   (aref img y x 1) (nth 1 pixel)
   (aref img y x 2) (nth 2 pixel)
   (aref img y x 3) (nth 3 pixel)))


(defun replace-image-color (img color replacement)
  "Replace all instances of color COLOR with replacement
color REPLACEMENT in IMG."
  (let ( (rpx (make-array '(4) :initial-contents replacement))
         (out-img (png:copy-image img)))
    (iter-image-pixels img
                       #'(lambda (x y pixel)
                           (if (equal pixel color)
                               (draw-pixel out-img rpx x y))))
    out-img))

(defun replace-image-color-noise (img color replacements)
  "Replace all instances of color COLOR with replacement
colors from REPLACEMENTS in IMG, randomly."
  (let ( (rpxs
           (mapcar
            #'(lambda (c)
                (make-array '(4) :initial-contents c))
            replacements))
         (out-img (png:copy-image img)))
    (iter-image-pixels img
                       #'(lambda (x y pixel)
                           (if (equal pixel color)
                               (draw-pixel out-img
                                           (random-elt
                                            rpxs)
                                           x y))))
    out-img))

(defun black-to-alpha (img)
  (replace-image-color img '(0 0 0 255) '(0 0 0 0)))

(defun empty-image (w h)
  (make-array (list w h 4) :initial-element 255 :element-type '(unsigned-byte 8)))


;; grass
(defun make-raw-terrain (color1 color2 &key (iter-n 64))
  (black-to-alpha
   (replace-image-color-noise
    (empty-image 96 96)
    '(255 255 255 255)
    (make-color-range (color:decode color1) (color:decode color2) iter-n))))

(png:encode-file
 (make-raw-terrain #x33cc77 #x229955 :iter-n 8)
 "test_terr.png")

(png:encode-file
 (make-raw-terrain #x967054 #x9f795b :iter-n 64)
 "test_terr2.png")

(defun create-terrain-file
    (in-img mask-img out-img
     &key (bevel nil) (bevel-color "black")
       (strength "30x1+1+1"))
  (uiop:run-program
   (list
    "./gen_terr"
    "-c" bevel-color
    "-s" strength
    "-i" in-img
    "-m" mask-img
    "-o" out-img
    (if bevel "-b" nil))))

(defun create-noise-terrain-file
    (color1 color2 mask-path out-path
     &key (bevel nil) (bevel-color "black") (shadow-strength "30x1+1+1")
       (iter-n 64))
  "A noise terrain file is produced by
interpolating an `inter-n' number of times between
the first two int inputs treated as RGB colors,
then passing it as a source for a mask and
shadow-add operation. This produces a wang file
based on the input mask file given in the third arg.
Produces a file at path provided in fourth arg.
Options (keyed) "
  (let ((raw-file
          (utils:s+
           (utils:mktemp)
           ".png")))
    ;; make "raw" noise png, the Src of the mask
    (png:encode-file
     (make-raw-terrain color1 color2 :iter-n iter-n)
     raw-file)
    (create-terrain-file
     raw-file mask-path out-path
     :bevel bevel
     :bevel-color bevel-color
     :strength shadow-strength)))


(defun noise-reduce-mask (mask-path out-path amount &optional seed)
  (uiop:run-program
   (list
    "./add_noise"
    mask-path
    amount
    out-path
    seed)))

(defun noise-series-files (mask-path &optional enable-flags)
  (loop :for file
          :in (let
                  ((stem (utils:pathname-no-extension mask-path)))
                (mapcar #'(lambda (suffix)
                            (format nil "~a~a" stem suffix))
                        '("-50.png"
                          "-100.png"
                          "-200.png"
                          "-300.png"
                          "-400.png"
                          "-500.png"
                          "-600.png"
                          "-700.png"
                          "-800.png"
                          "-900.png"
                          "-999.png")))
        :for flag :in (or enable-flags '(t t t t t t t t t t t))
        :if flag :collect file))

(defun noise-series-get-file (mask-path level)
  (nth level (noise-series-files mask-path)))

(defun create-mask-noise-series (mask-path &optional seed enable-flags)
  (let ((seed (or seed
                  (random (expt 2 16))))
        (filenames (noise-series-files mask-path enable-flags))
        (stem (utils:pathname-no-extension mask-path)))
    (flet ((noise-reduce (filen amount)
             (noise-reduce-mask
              mask-path
              (nth filen filenames)
              (format nil "~a" amount)
              (format nil "~a" seed))))
      (noise-reduce 0 5)
      (noise-reduce 1 10)
      (noise-reduce 2 20)
      (noise-reduce 3 30)
      (noise-reduce 4 40)
      (noise-reduce 5 50)
      (noise-reduce 6 60)
      (noise-reduce 7 70)
      (noise-reduce 8 80)
      (noise-reduce 9 90)
      (noise-reduce 10 99))))


(defun fixnum-list-p (l)
  (and (typep l 'list)
       (loop for item in l always (typep item 'fixnum))))

(deftype fixnum-list ()
  `(satisfies fixnum-list-p))


(defun render-from-tiles (source-image tile-list width &optional (tile-size 16))
  (declare (type fixnum width tile-size))
  (declare (type fixnum-list tile-list))
  (declare (type png:image source-image))
  (let* ((src-width (png:image-width source-image))
         (tile-list (mapcar #'1+ tile-list))
         (height (floor (/ (length tile-list) width)))
         (out-image (png:make-image (* tile-size height) (* tile-size width) 4)))
    (declare (type fixnum height))
    (dotimes (tile-idx (length tile-list))
      (let ((tile (nth tile-idx tile-list)))
        (declare (type fixnum tile-idx tile))
        (unless (zerop tile)
          (print tile)
          (multiple-value-bind (tile-x tile-y) (grid:ixy tile-idx width)
            (multiple-value-bind (src-x src-y) (grid:ixy tile (/ src-width tile-size))
              (dotimes (row tile-size)
                (dotimes (pixel tile-size)
                  (let ((src-pixel-x (+ (* src-x tile-size) pixel))
                        (src-pixel-y (+ (* src-y tile-size) row))
                        (out-pixel-x  (+ (* tile-x tile-size) pixel))
                        (out-pixel-y (+ (* tile-y tile-size) row)))
                    (print
                     (format nil "src tile width ~d" (/ src-width tile-size)))
                    (print
                     (format nil "tile ~d, tile-idx ~d" tile tile-idx))
                    (print
                     (format nil "pixel ~d,~d" out-pixel-x out-pixel-y))
                    (print
                     (format nil "source pixel ~d,~d" src-pixel-x src-pixel-y))
                    (set-pixel-from-list
                     out-pixel-x
                     out-pixel-y
                     out-image
                     (get-pixel
                      src-pixel-x
                      src-pixel-y
                      source-image))))))))))
    out-image))


(defun render-from-tiles2 (source-image tile-list width &key
                                                          (tile-size 16)
                                                          (margin 0)
                                                          (spacing 0))
  (declare (type fixnum width tile-size margin spacing))
  (declare (type fixnum-list tile-list))
  (declare (type png:image source-image))
  (let* ((src-width (png:image-width source-image))
         (tile-list (mapcar #'1+ tile-list))
         (height (floor (/ (length tile-list) width)))
         (total-width (+ (* 2 margin) (* tile-size width) (* spacing (1- width))))
         (total-height (+ (* 2 margin) (* tile-size height) (* spacing (1- height))))
         (out-image (png:make-image total-height total-width 4)))
    (declare (type fixnum height))
    (dotimes (tile-idx (length tile-list))
      (let ((tile (nth tile-idx tile-list)))
        (declare (type fixnum tile-idx tile))
        (unless (zerop tile)
          (multiple-value-bind (tile-x tile-y) (grid:ixy tile-idx width)
            (multiple-value-bind (src-x src-y) (grid:ixy tile (/ (- src-width 2) (1+ tile-size)))
              (dotimes (row tile-size)
                (dotimes (pixel tile-size)
                  (let ((src-pixel-x (+ (* src-x tile-size) pixel))
                        (src-pixel-y (+ (* src-y tile-size) row))
                        (out-pixel-x (+ margin
                                        (* tile-x (+ tile-size spacing))
                                        pixel))
                        (out-pixel-y (+ margin
                                        (* tile-y (+ tile-size spacing))
                                        row)))

                    (print
                     (format nil "src tile width ~d" (/ src-width tile-size)))
                    (print
                     (format nil "tile ~d, tile-idx ~d" tile tile-idx))
                    (print
                     (format nil "pixel ~d,~d" out-pixel-x out-pixel-y))
                    (print
                     (format nil "source pixel ~d,~d" src-pixel-x src-pixel-y))
                    (set-pixel-from-list
                     out-pixel-x
                     out-pixel-y
                     out-image
                     (get-pixel
                      src-pixel-x
                      src-pixel-y
                      source-image))))))))))
    out-image))
