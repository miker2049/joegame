(defpackage magicklib
  (:use :common-lisp :cffi)
  (:export scale-image))

(in-package magicklib)
(define-foreign-library magickcore
    (:unix  "libMagick++-7.Q16HDRI.so")
  (t (:default "libMagick++-7.Q16HDRI")))

(define-foreign-library magickwand
    (:unix  "libMagickWand-7.Q16HDRI.so")
  (t (:default "libMagickWand-7.Q16HDRI")))

(use-foreign-library magickcore)
(use-foreign-library magickwand)

(defcfun "NewMagickWand" :pointer)
(defcfun "MagickWandGenesis" :void)
(defcfun "MagickReadImage" :void (wand :pointer) (path :string))
(defcfun "MagickGetImageWidth" :uint64 (wand :pointer))
(defcfun "MagickGetImageHeight" :uint64 (wand :pointer))
(defcfun "ClearMagickWand" :void (wand :pointer))
(defcfun "DestroyMagickWand" :void (wand :pointer))
(defcfun "MagickScaleImage" :void (wand :pointer) (cols :uint32) (rows :uint32))
(defcfun "MagickWriteImage" :void (wand :pointer) (path :string))
;; (defcfun "NewMagickWandFromImage" :pointer (img :string))

;; (setf *wand* (newmagickwand))
;; (magickreadimage *wand* "tt.png")
;; (newmagickwandfromimage "tt.png")

;; (magickgetimagewidth *wand*)

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

(defun scale-image (file scale outfile)
  (with-magick-dimensions (mag w h) file
    (magickscaleimage mag (floor (* w scale)) (floor (* scale h)))
    (magickwriteimage mag outfile)))


(with-magick mag "generated_terr_clay2.png")
