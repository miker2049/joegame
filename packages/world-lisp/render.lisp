(defpackage render (:use cl png alexandria grid)
            (:local-nicknames
             (color org.shirakumo.alloy.colored))
            (:export render-image
                     noise-series-files
                     noise-series-get-file
                     create-mask-noise-series
                     create-noise-terrain-file
                     create-terrain-file
                     render-image-file
                     attach-image
                     save-image-file
                     extrude-tileset-image
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


(defun iter-image-pixels (img cb)
  "Iterate through pixels of img.  On each, run function cb
which expects args X Y PIXEL"
  (loop
    for row from 0 below (png:image-height img)
    do (loop
         for pixel from 0 below (png:image-width img)
         do (funcall cb pixel row
                     (get-pixel pixel row img)))))

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

(defun int-to-rgb-list (c)
  (list
   (logand #xFF (ash c -16))
   (logand #xFF (ash c -8))
   (logand #xFF c)))

(defun parse-rgb-color (c)
  (let ((l (int-to-rgb-list c)))
    (list
     :r (nth 0 l)
     :g (nth 1 l)
     :b (nth 2 l))))

(defun make-pixel (r g b &optional a)
  (if a
      (make-array '(4) :element-type unsigned-byte :initial-contents (list r g b a))
      (make-array '(3) :element-type unsigned-byte :initial-contents (list r g b))))

(defmacro with-rgb-vals (bindings pixel &body body)
  `(let ((,(car bindings) (aref ,pixel 0))
         (,(cadr bindings) (aref ,pixel 1))
         (,(caddr bindings) (aref ,pixel 2)))
     ,@body))
(defmacro with-rgba-vals (bindings pixel &body body)
  `(let ((,(car bindings) (aref ,pixel 0))
         (,(cadr bindings) (aref ,pixel 1))
         (,(caddr bindings) (aref ,pixel 2))
         (,(cadddr bindings)
           (aref ,pixel 3)))
     ,@body))

(defun px (r g b &optional a) (make-pixel r g b a))
(defun px-mult (a b)
  (let ((rgb-list
          (list
           (* (aref a 0)
              (aref b 0))
           (* (aref a 1)
              (aref b 1))
           (* (aref a 2)
              (aref b 2)))))
    (if (or
         (eql 3 (array-dimension a 0))
         (eql 3 (array-dimension b 0)))
        (make-array '(3) :initial-contents rgb-list)
        (make-array '(4)
                    :initial-contents
                    (nconc rgb-list (list (* (aref a 3)
                                             (aref b 3))))))))



(defun copy-pixel (img x y)
  (let* ((chans (png:image-channels img))
         (vals (make-array `(,chans))))
    (dotimes (i chans)
      (setf (aref vals i) (aref img y x i)))
    vals))


;; blending
(defun blend-normal (a b) b)
(defun blend-multiply (a b) (px-mult a b))

(defun draw-pixel (img p x y)
  (dotimes (idx (array-dimension p 0))
    (setf (aref img y x idx) (aref p idx)))
  t)

(defun draw-pixel-from-image (img source ix iy sx sy &optional w h)
  (dotimes (row  (or h 1))
    (dotimes (col  (or w 1))
      (dotimes (idx (image-channels img))
        (let ((finalx (+ col ix)) (finaly (+ row iy)))
          (setf (aref img finaly finalx  idx) (aref source sy sx idx))))))
  t)

(defun draw-rect-from-image (img source ix iy sx sy w h)
  (dotimes (yy h)
    (dotimes (xx w)
      (draw-pixel-from-image img source
                             (+ ix xx)
                             (+ iy yy)
                             (+ sx xx)
                             (+ sy yy))))
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
         (out (png:make-image outh outw (png:image-channels ol)))
         (order (if (or (eql position :right)
                        (eql position :bottom))
                    (list base ol)
                    (list ol base))))
    (flet ((gp (out img ox oy x y)
             (draw-pixel-from-image out img (+ ox x) (+ oy y) x y)))
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


(defun extrude-tileset-image (img &key
                                    (tilewidth 16)
                                    (tileheight 16)
                                    (margin 0)
                                    (spacing 0)
                                    (extrusion 2))
  (flet ((draw-extruded-tile (row col destination)
           (let ((srcx (+ margin (* col (+ tilewidth spacing))))
                 (srcy (+ margin (* row (+ tileheight spacing))))
                 (destx (+ margin (* col (+ tilewidth spacing (* 2 extrusion)))))
                 (desty (+ margin (* row (+ tileheight spacing (* 2 extrusion))))))
             (draw-rect-from-image
              destination img
              (+ destx extrusion) (+ desty extrusion)
              srcx srcy
              tilewidth tileheight)
             (dotimes (i extrusion)
               ;; ;; top row
               (draw-rect-from-image
                destination img
                (+ destx extrusion)
                (+ desty i 0)
                srcx srcy
                tilewidth 1)
               ;; ;; bottom row
               (draw-rect-from-image
                destination img
                (+ destx extrusion)
                (+ desty extrusion tileheight (- extrusion i 1))
                srcx (+ srcy tileheight -1)
                tilewidth 1)
               ;; ;; left column
               (draw-rect-from-image
                destination img
                (+ destx i)
                (+ desty extrusion)
                srcx srcy
                1 tileheight)
               ;; ;; right column
               (draw-rect-from-image
                destination img
                (+ destx extrusion tilewidth (- extrusion i 1))
                (+ desty extrusion)
                (- (+ srcx tilewidth) 1) srcy
                1 tileheight)
               ;; ;; top left corner
               (draw-pixel-from-image destination img destx desty srcx srcy extrusion extrusion)
               ;; ;; top right corner
               (draw-pixel-from-image destination img (+ destx extrusion tilewidth)
                                      desty (+ srcx tilewidth -1) srcy extrusion extrusion)
               ;; ;; bottom left
               (draw-pixel-from-image destination img destx (+ desty extrusion tileheight)
                                      srcx (+ srcy tileheight -1) extrusion extrusion)
               ;; ;; bottom right
               (draw-pixel-from-image destination img
                                      (+ destx extrusion tilewidth)
                                      (+ desty extrusion tileheight)
                                      (+ srcx tilewidth -1) (+ srcy tileheight -1)
                                      extrusion extrusion)))))
    (let ((width (png:image-width img))
          (height (png:image-height img)))
      (let ((cols (floor (/ (+ (- width (* 2 margin)) spacing)
                            (+ tilewidth spacing))))
            (rows (floor (/ (+ (- height (* 2 margin)) spacing)
                            (+ tileheight spacing)))))
        (let ((newwidth (+ (* 2 margin)
                           (* (- cols 1) spacing)
                           (* cols (+ tilewidth (* 2 extrusion)))))
              (newheight (+ (* 2 margin)
                            (* (- rows 1) spacing)
                            (* rows (+ tileheight (* 2 extrusion))))))
          (let ((out (png:make-image newheight newwidth (png:image-channels img))))
            (print
             (format nil "~s" newwidth))
            (dotimes (r rows)
              (dotimes (c cols)
                (draw-extruded-tile r c out)))
            out))))))



;; (delete-file "terr_extruded.png")
;; (png:encode-file
;;   (extrude-tileset-image
;;     (png:decode-file "terr_grass.png" :preserve-alpha 't)
;;     :margin 0
;;     :spacing 0
;;     :tilewidth 16
;;     :tileheight 16
;;     :extrusion 1)
;;   "terr_extruded.png")
