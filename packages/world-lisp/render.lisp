(ql:quickload "png")
(defpackage :render (:use cl png alexandria)
    (:export render-image
        render-image-file
        save-image-file
        parse-rgb-color
        copy-pixel
        draw-pixel))
(in-package render)

(defun render-image (width height cb &optional num-channels)
    "Generate image by running callback at each pixel. Callback args are (x y)"
    (let ((img (make-image width height (or num-channels 3) 8)))
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

(defun parse-rgb-color (c)
        (list
            :r (logand #xFF (ash c -16))
            :g (logand #xFF (ash c -8))
            :b (logand #xFF c)))

(defun copy-pixel (img x y)
    (let ((chans (png:image-channels img))
             (vals '()))
        (dotimes (i chans)
            (setf vals (append
                           vals
                           (list (aref img y x i)))))
        vals))

(defun draw-pixel (img p x y)
    (let ((i 0))
        (dolist (c p)
            (setf (aref img y x i) c)
            (+ 1 i))
        t))
