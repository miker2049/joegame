(defpackage worldtiles
  (:use :cl)
  (:export world-tile world-tile-debug))

(in-package worldtiles)

(defparameter *test-image*
  (png:decode-file
   (joegame-assets:get-asset-path "images/test256.png")))
;; (declaim (optimize (speed 0) (space 0) (debug 3)))
(defun world-tile (x y z)
  (declare (type integer x y z))
  (let* ((tz (expt 2 z))
         (tz-scale (* 1/256 tz)))
    (if (<= tz-scale 1)
        (worldconf:make-world-image-scaled worldconf:*worldconf* 256 256 tz-scale
                                           (* 256 (mod x tz))
                                           (* 256 (mod y tz)))
        *test-image*)))

(defparameter *debug-number-images*
  (mapcar #'png:decode-file
          (list
           (joegame-assets:get-asset-path "images/numbers/0_256.png")
           (joegame-assets:get-asset-path "images/numbers/1_256.png")
           (joegame-assets:get-asset-path "images/numbers/2_256.png")
           (joegame-assets:get-asset-path "images/numbers/3_256.png")
           (joegame-assets:get-asset-path "images/numbers/4_256.png")
           (joegame-assets:get-asset-path "images/numbers/5_256.png")
           (joegame-assets:get-asset-path "images/numbers/6_256.png")
           (joegame-assets:get-asset-path "images/numbers/7_256.png")
           (joegame-assets:get-asset-path "images/numbers/8_256.png")
           (joegame-assets:get-asset-path "images/numbers/9_256.png")
           (joegame-assets:get-asset-path "images/numbers/10_256.png")
           (joegame-assets:get-asset-path "images/numbers/11_256.png")
           (joegame-assets:get-asset-path "images/numbers/12_256.png")
           (joegame-assets:get-asset-path "images/numbers/13_256.png")
           (joegame-assets:get-asset-path "images/numbers/14_256.png")
           (joegame-assets:get-asset-path "images/numbers/15_256.png")
           (joegame-assets:get-asset-path "images/numbers/16_256.png"))))

(defun world-tile-debug (x y z)
  (declare (type integer x y z)
           (ignorable x y))
  (nth z *debug-number-images*))

;; (loop :for y :below (* 256 256) :do
;;   (sqlite:execute-non-query *db* "INSERT INTO world(x,y,val) VALUES (?,?,?)" x y (random 24))
;;   (loop :for x :below (* 256 256) :do (sqlite:execute-non-query *db* "INSERT INTO world VALUES (?,?,?)"
;;                                                                 x y (random 24))))

;; this one ****
(defun test-db-ins-prepared-bytes (size &optional (tile-size 256))
  (declare (type fixnum size tile-size))
  (declare  (optimize (speed 3) (safety 0)))
  (sqlite:with-open-database (db "test-blob.db")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS tiles ( x int, y int, val blob )")
    (let* ((tilesz (expt tile-size 2)))
      (declare (type fixnum tilesz))
      (let* ((arr (make-array tilesz :element-type '(unsigned-byte 8) :initial-element 0))
             (stmt (sqlite:prepare-statement db  "INSERT INTO tiles (x,y,val) VALUES (?,?,?)")))
        (sqlite:execute-non-query db "PRAGMA synchronous = OFF")
        (sqlite:execute-non-query db "PRAGMA journal_mode = WAL")
        (sqlite:with-transaction db
          (loop for x below size do
            (loop for y below size do
              (progn
                (loop for byt below tilesz do (setf (aref arr byt) (random 255)))
                (sqlite:bind-parameter stmt 1 x)
                (sqlite:bind-parameter stmt 2 y)
                (sqlite:bind-parameter stmt 3 arr)
                (sqlite:step-statement stmt)
                (sqlite:clear-statement-bindings stmt)
                (sqlite:reset-statement stmt)))))
        (sqlite:finalize-statement stmt)))))
