(in-package :cl-user)
(defpackage world
  (:use :cl)
  (:import-from :clingon)
  (:import-from worldconf
    dump-csv
    render-big-img
    render-img
    *worldconf*
    dbsync
    run)
  (:export mapimg-cli
    main))

(in-package world)

(defparameter *map-n* 2000)

(defun world-image/options ()
  "Creates and returns the options for the top-level command"
  (list
    (clingon:make-option
      :string
      :description "the output path"
      :initial-value "world.png"
      :short-name #\o
      :long-name "output-path"
      :key :out)
    (clingon:make-option
      :integer
      :initial-value 0
      :description "the x offset"
      :short-name #\x
      :long-name "x-offset"
      :key :x)
    (clingon:make-option
      :integer
      :initial-value 0
      :description "the y offset"
      :short-name #\y
      :long-name "y-offset"
      :key :y)
    (clingon:make-option
      :integer
      :initial-value 256
      :description "the width"
      :short-name #\w
      :long-name "width"
      :key :w)
    (clingon:make-option
      :integer
      :initial-value 256
      :description "the height"
      :short-name #\h
      :long-name "height"
      :key :h)
    (clingon:make-option
      :integer
      :initial-value 1
      :description "threads to use for image"
      :short-name #\j
      :long-name "threads"
      :key :threads)
    ))

(defun world-image/handler (cmd)
  "The top-level handler"
  (let ( (out (clingon:getopt cmd :out))
         (x (clingon:getopt cmd :x))
         (y (clingon:getopt cmd :y))
         (w (clingon:getopt cmd :w))
         (h (clingon:getopt cmd :h))
         (threads (clingon:getopt cmd :threads))
         )
    ;; (format t "x is ~s, y is ~s, width is ~s, height is ~s. Outpath is ~s.~%" x y w h out)
    (if (eql threads 1)
      (render-img out *worldconf* x y w h)
      (render-big-img *worldconf* w h out :threads threads :xoff x :yoff y))))



(defun world-image/command ()
  (clingon:make-command
    :name "world-image"
    :options (world-image/options)
    :handler #'world-image/handler
    :description "Creates a world image at the area tile level"))

(defun world/command ()
  "Creates and returns the top-level command"
  (clingon:make-command
    :name "world"
    :description "Joegame world app"
    :version "0.1.0"
    :license "MIT"
    :authors '("bb")
    :sub-commands (list (world-image/command) )))

(defun main ()
  (let ((app (world/command)))
    (clingon:run app)))
