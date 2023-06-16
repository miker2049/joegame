(defpackage render (:use cl png alexandria grid)
  (:export render-image
    render-image-file
    attach-image
    save-image-file
    parse-rgb-color
    rgb-to-integer
    copy-pixel
    draw-pixel))
(in-package render)

(defun render-image (width height cb &optional num-channels)
    "Generate image by running callback at each pixel. Callback args are (x y)"
    (let ((img (make-image height width (or num-channels 3) 8)))
        (dotimes (i (image-height img))
            (dotimes (j (image-width img))
                (let ((color (funcall cb j i)))
                    (dotimes (k (image-channels img))
                        (setf (aref img i j k)
                            (nth k color))))))
        img))

(defun save-image-file (output-pathname img &optional num-channels)
    "Generate image by running callback at each pixel. Callback args are (x y)"
    (with-open-file (output output-pathname :element-type '(unsigned-byte 8)
                        :direction :output :if-exists :supersede)
        (png:encode img output)))

(defun render-image-file (output-pathname width height cb &optional num-channels)
  "Generate image by running callback at each pixel. Callback args are (x y)"
  (let ((img (render-image width height cb num-channels)))
    (save-image-file output-pathname img)))

(defun rgb-to-integer (r g b)
  (+ (ash r 16) (ash g 8) b))
(defun parse-rgb-color (c)
  (list
    :r (logand #xFF (ash c -16))
    :g (logand #xFF (ash c -8))
    :b (logand #xFF c)))

(defun copy-pixel (img x y)
    (let ((chans (png:image-channels img))
             (vals (make-array '(3))))
        (dotimes (i chans)
            (setf (aref vals i) (aref img y x i)))
        vals))

(defun draw-pixel (img p x y)
    (dotimes (idx (array-dimension p 0))
        (setf (aref img y x idx) (aref p idx)))
    t)

(defun draw-pixel-from-image (img source ix iy sx sy)
    (dotimes (idx (image-channels img))
        (setf (aref img iy ix idx) (aref source sy sx idx)))
    t)

(defun attach-image (base ol &optional position)
    (if (not position)
        (setf position :bottom))
    (let* (
              (bw (png:image-width base))
              (bh (png:image-height base))
              (ow (png:image-width ol))
              (oh (png:image-height ol))
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
                   (draw-pixel-from-image out img (+ ox x) (+ oy y) x y)))
            ;; (format *standard-output* "bw: ~a, bh: ~a, ow: ~a, oh: ~a, vertical: ~a, outw: ~a, outh: ~a" bw bh ow oh vertical outw outh)
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
