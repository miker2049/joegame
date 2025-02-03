(in-package #:worldtool)

(define-constant +window-width+ 800)
(define-constant +window-height+ 600)
(define-constant +key-max+ 227)
(define-constant +key-seen-flag+ 1)
(define-constant +key-released-flag 2)
(define-constant +repl-update-interval+ 0.3d0)

(defvar *resources-path*
  (asdf:system-relative-pathname :worldtool #P"Resources/"))

(deploy:define-hook (:boot set-resources-path) ()
  (setf *resources-path*
        (merge-pathnames #P"Resources/"
                         (uiop:pathname-parent-directory-pathname
                          (deploy:runtime-directory)))))


;; utilities
(defun key (sym)
  (cffi:foreign-enum-value 'al::keycodes sym))

(defun key-state (k)
  (aref *keys* (key k)))

(defmacro if-key (key &body body)
  `(if (> (key-state ,key) 0)
       ,@body))


;; Assets
(defvar *font*)
(defvar *mysha*)
(define-constant +font-path+ "inconsolata.ttf" :test #'string=)
(define-constant +font-size+ 24)
(define-constant +config-path+ "../config.cfg" :test #'string=)


;; dynamic variables
(declaim (type fixnum *fps*))
(defvar *fps* 0)


(defun init ()
  (setf *font* (al:ensure-loaded #'al:load-ttf-font
                                 +font-path+
                                 (- +font-size+) 0))
  (setf *mysha* (al:ensure-loaded #'al:load-bitmap "mysha.png")))


(defun handle-events ()
  (if-key :k
    (print "k hey"))
  (if-key :j
    (print "j hey")))

(defun update (dt)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  ;; TODO : put your game logic here
  )


(defun render ()

  (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                (format nil "~d FPS" *fps*))
  (al:draw-bitmap *mysha* 100 200 0)
  (al:draw-bitmap *mysha* 123 140 0)
  (al:draw-bitmap *mysha* 300 200 0)
  (al:draw-bitmap *mysha* 300 400 0))

(defun shutdown ()
  (al:destroy-font *font*)
  (al:destroy-bitmap *mysha*))

(defvar --enable-sound nil)


(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
            (float-features:with-float-traps-masked
                (:divide-by-zero :invalid :inexact :overflow :underflow)
              (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))
