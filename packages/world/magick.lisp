(defpackage magicklib
  (:use :common-lisp :cffi)
  (:export scale-image image-dimensions draw-tile-lines-blob image-dimensions-blob
           get-png
           crop-image-blob))

(in-package magicklib)
(define-foreign-library magickcore
  (:unix  "libMagick++-7.Q16HDRI.so")
  (t (:default "libMagick++-7.Q16HDRI")))

(define-foreign-library magickwand
  (:unix  "libMagickWand-7.Q16HDRI.so")
  (t (:default "libMagickWand-7.Q16HDRI")))

(use-foreign-library magickcore)
(use-foreign-library magickwand)

(defctype :wand :pointer)
(defctype :draw-wand :pointer)
(defctype :pixel-wand :pointer)
(defctype :image-blob (:pointer :char))

(defcfun "NewMagickWand" :wand)
(defcfun "DestroyMagickWand" :void (wand :wand))
(defcfun "NewDrawingWand" :draw-wand)
(defcfun "DestroyDrawingWand" :void (wand :draw-wand))
(defcfun "NewPixelWand" :pixel-wand)
(defcfun "DestroyPixelWand" :void (wand :pixel-wand))
(defcfun "MagickWandGenesis" :void)
(defcfun "MagickReadImage" :void (wand :wand) (path :string))
(defcfun "MagickReadImageBlob" :uint8 (wand :wand) (ptr :pointer) (size :unsigned-int))
(defcfun "MagickGetImageWidth" :uint64 (wand :wand))
(defcfun "MagickGetImageHeight" :uint64 (wand :wand))
(defcfun "ClearMagickWand" :void (wand :wand))
(defcfun "MagickScaleImage" :void (wand :wand) (cols :uint32) (rows :uint32))
(defcfun "MagickCropImage" :boolean (wand :wand) (width :sizet) (height :sizet) (x :sizet) (y :sizet))
(defcfun "MagickWriteImage" :void (wand :wand) (path :string))
(defcfun "MagickGetImageProperties" :pointer (wand :wand) (pattern :string) (number_properties :sizet))
(defcfun "MagickGetImageProperty" :pointer (wand :wand) (prop :string))
(defcfun "MagickDrawImage" :void (wand :wand) (dwand :draw-wand))
(defcfun "MagickSetFormat" :void (wand :wand) (formt :string))
(defcfun "MagickSetImageFormat" :boolean (wand :wand) (format :string))
(defcfun "MagickGetImageBlob" :image-blob (wand :wand) (length (:pointer :sizet)))

;; drawing

(defcfun "PixelSetColor" :void (pwand :pixel-wand) (color :string))
(defcfun "DrawSetStrokeColor" :void (wand :wand) (pwand :pixel-wand))
(defcfun "DrawSetStrokeWidth" :void (dwand :draw-wand) (n :uint32))
(defcfun "DrawSetAntialias" :void (dwand :draw-wand) (n :double))
(defcfun "DrawSetFillColor" :void (wand :wand) (pwand :pixel-wand))
(defcfun "DrawRectangle" :void (dwand :draw-wand) (x1 :double) (y1 :double) (x2 :double) (y2 :double))
(defcfun "DrawLine" :void (dwand :draw-wand) (x1 :double) (y1 :double) (x2 :double) (y2 :double))
(defcfun "PushDrawingWand" :void (dwand :draw-wand))
(defcfun "PopDrawingWand" :void (dwand :draw-wand))

;; (defcfun "NewMagickWandFromImage" :pointer (img :string))

;; (setf *wand* (newmagickwand))
;; (magickreadimage *wand* "tt.png")
;; (newmagickwandfromimage "tt.png")

;; (magickgetimagewidth *wand*)


(defun alloc-image-array (file)
  (let ((pf
          (alexandria:read-file-into-byte-vector file)))
    (cffi:foreign-array-alloc pf (:array :unsigned-char (length pf)))))

(defmacro with-magick (wand-var imgpath &body body)
  (let ((outvar (gensym)))
    `(progn
       (magickwandgenesis)
       (let ((,wand-var (newmagickwand)))
         (magickreadimage ,wand-var ,imgpath)
         (setf ,outvar
               (progn
                 ,@body))
         (destroymagickwand ,wand-var)
         ,outvar))))

(defmacro with-magick-dimensions ((wand w h) imgpath &body body)
  `(with-magick ,wand ,imgpath
     (setf ,w
           (magickgetimagewidth ,wand))
     (setf ,h
           (magickgetimageheight ,wand))
     ,@body))

(defun image-dimensions (imgpath)
  (with-magick-dimensions (m w h) imgpath
    (list w h)))

(defun scale-image (file scale outfile)
  (with-magick-dimensions (mag w h) file
    (magickscaleimage mag (floor (* w scale)) (floor (* scale h)))
    (magickwriteimage mag outfile)))



(defmacro with-magick-blob (wand-var arr &body body)
  (alexandria:with-gensyms (outvar)
    (alexandria:once-only (arr)
      `(cffi:with-foreign-array (arrptr ,arr (list :array :unsigned-char (length ,arr)))
         (magickwandgenesis)
         (let ((,wand-var (newmagickwand)) ,outvar)
           (magickreadimageblob ,wand-var arrptr (length ,arr))
           (setf ,outvar (progn ,@body))
           (destroymagickwand ,wand-var)
           ,outvar)))))

(defmacro with-magick-dimensions-blob ((wand w h) arr &body body)
  `(with-magick-blob ,wand ,arr
     (setf ,w
           (magickgetimagewidth ,wand))
     (setf ,h
           (magickgetimageheight ,wand))
     ,@body))

(defmacro image-dimensions-blob (arr)
  `(with-magick-dimensions-blob (m w h) ,arr
     (list w h)))

(defmacro with-magick-draw-blob ((wand width height drawer pixel) arr &body body)
  (alexandria:with-gensyms (out l arrout)
    `(with-foreign-pointer (,l (cffi:foreign-type-size :sizet))
       (with-magick-dimensions-blob (,wand ,width ,height)
                                    ,arr
         (setf ,drawer (newdrawingwand))
         (setf ,pixel (newpixelwand))
         (setf *wand* ,wand)
         (setf *pwand* ,pixel)
         (setf *dwand* ,drawer)
         (setf ,out (progn ,@body))

         (magickdrawimage ,wand ,drawer)

         (setf ,arrout
               (magickgetimageblob ,wand ,l))
         (destroypixelwand ,pixel)
         (destroydrawingwand ,drawer)
         (list
          (foreign-array-to-lisp ,arrout
                                 (list :array :unsigned-char (cffi:mem-ref ,l :sizet))
                                 :element-type '(unsigned-byte 8))
          ,width ,height)))))




(defmacro draw-rectangle (x1 y1 x2 y2 &key (stroke-width 1) (stroke "red") (fill "none"))
  `(progn
     (pushdrawingwand *dwand*)
     (pixelsetcolor *pwand* ,stroke)
     (drawsetstrokecolor *dwand* *pwand*)
     (drawsetstrokewidth *dwand* ,stroke-width)
     (pixelsetcolor *pwand* ,fill)
     (drawsetfillcolor *dwand* *pwand*)
     (drawrectangle *dwand*
                    (coerce ,x1 'double-float)
                    (coerce ,y1 'double-float)
                    (coerce ,x2 'double-float)
                    (coerce ,y2 'double-float))
     (popdrawingwand *dwand*)))

(defun calc-tile-axis-amount (tileamt amt margin spacing)
  (floor (/ (+ amt (* -2 margin) spacing)
            (+ tileamt spacing))))



(defun tile-positions (width height tilewidth tileheight margin spacing)
  (mapcar (lambda (iy)
            (mapcar
             (lambda (ix)
               (list
                (+ margin (* ix (+ tilewidth spacing)))
                (+ margin (* iy (+ tileheight spacing)))))
             (utils:range
              (calc-tile-axis-amount tilewidth width margin spacing))))
          (utils:range (calc-tile-axis-amount tileheight height margin spacing))))



(defmacro -draw-tile-lines
    (w h tilew tileh
     &key
       (margin 0) (spacing 0)
       (stroke-width 1) (stroke "red") (fill "none"))
  `(mapcar #'(lambda (it)
               (draw-rectangle
                (first it) (second it)
                (+ ,tilew (first it)) (+ ,tileh (second it))
                :stroke ,stroke
                :fill ,fill
                :stroke-width ,stroke-width))
    (apply #'concatenate 'list
           (tile-positions ,w ,h ,tilew ,tileh ,margin ,spacing))))

(defun draw-tile-lines-blob (blob tilew tileh  &key
                                                 (margin 0) (spacing 0)
                                                 (stroke-width 1) (stroke "red") (fill "none"))
  (car
   (with-magick-draw-blob (wand width height dwand pwand) blob
     (-draw-tile-lines width height tilew tileh :margin margin :spacing spacing
                                                :stroke stroke :fill fill :stroke-width stroke-width))))


(defun crop-image-blob (blob width height xoff yoff)
  (with-magick-blob wand blob
    (magickcropimage wand width height xoff yoff)))




(defun get-png (blob)
  (with-magick-draw-blob (wand w h dr pdr) blob
    (magicksetimageformat wand "PNG")))
