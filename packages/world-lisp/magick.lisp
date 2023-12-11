(defpackage magicklib
  (:use :common-lisp :cffi)
  (:export scale-image image-dimensions))

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
(defcfun "MagickReadImageBlob" :uint8 (wand :pointer) (ptr :pointer) (size :unsigned-int))
(defcfun "MagickGetImageWidth" :uint64 (wand :pointer))
(defcfun "MagickGetImageHeight" :uint64 (wand :pointer))
(defcfun "ClearMagickWand" :void (wand :pointer))
(defcfun "DestroyMagickWand" :void (wand :pointer))
(defcfun "MagickScaleImage" :void (wand :pointer) (cols :uint32) (rows :uint32))
(defcfun "MagickWriteImage" :void (wand :pointer) (path :string))
(defcfun "MagickGetImageProperties" :pointer (wand :pointer) (pattern :string) (number_properties :sizet))
(defcfun "MagickGetImageProperty" :pointer (wand :pointer) (prop :string))
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
         (let ((,wand-var (newmagickwand)))
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
