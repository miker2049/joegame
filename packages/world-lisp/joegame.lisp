(defpackage joegame
  (:use :cl :alexandria))
(in-package joegame)


(declaim (optimize safety))
(defclass game ()
  ((image-folder
    :initform "/images/"
    :documentation "Path to a directory, should serve both a server and tiled context."
    :accessor game-image-folder
    :initarg :image-folder)
   (tile-folder
    :initform "/tiles/"
    :accessor game-tile-folder
    :initarg :tile-folder)
   (mode
    :initform :development
    :accessor game-env-mode
    :initarg :mode)
   (terrains
    :accessor game-terrains
    :initarg :terrains)
   (areas
    :accessor game-areas
    :initarg :areas)
   (terrain-masks
    :accessor game-terrain-masks
    :initarg :masks)
   (wang-tile-data
    :accessor wang-game-tile-data
    :initarg wang-tile-data)
   (world-view
    :accessor game-world-view
    :initarg :signal )))

(defun valid-slot (args key func msg)
  (if (not (funcall func (getf args key)))
      (error msg)))

(defun make-game (&rest args)
  ;; (valid-slot args :terrains #'listp "Terrains must be a list")
  ;; (valid-slot args :areas #'listp "Areas must be a list")
  ;; (valid-slot args :masks #'listp "Terrain masks")
  ;; (valid-slot args :mode
  ;;             #'(lambda (it) (member it '(:development :production)))
  ;;             "Not a valid mode")
  (apply #'make-instance (append '(game) args)))

