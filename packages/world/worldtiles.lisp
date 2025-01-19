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
         (tz-scale (* 1/512 tz)))
    (if (< z 8)
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

;; (time
;;  (let ((size (* 64 64)))
;;    (sqlite:with-open-database (db "test-db.db")
;;      (sqlite:execute-non-query db
;;                                "CREATE TABLE world ( x int, y int, val int )")
;;      (sqlite:with-transaction db
;;        (loop :for y :below size :do
;;          (progn
;;            (print y)
;;            (sqlite:execute-non-query
;;             db
;;             (format nil "INSERT INTO world (x,y,val) VALUES ~{(~{~a~^, ~})~^, ~}"
;;                     (loop :for x :below size
;;                           :collect (list x y (random 17)))))))))))

;; (time
;;  (let ((size (* 64 64)))
;;    (with-open-file (file "./test-db.txt" :direction :output
;;                                          :if-exists :supersede
;;                                          :if-does-not-exist :create)
;;      (loop :for y :below size :do
;;        (progn
;;          (print y)
;;          (loop :for x :below size :do (format file "~s,~s,~s~%"
;;                                               x y (random 18))))))))
;; (time
;;  (let ((size (* 64 64)))
;;    (with-open-file (file "./test-db2.txt" :direction :output
;;                                           :if-exists :supersede
;;                                           :if-does-not-exist :create)
;;      (loop :for y :below size :do
;;        (progn
;;          (print y)
;;          (format file "~W~%"
;;                  (loop :for x :below size :collect (random 18) )))))))

(defun get-splits (start end splits)
  "Given start and end, return roughly size chunk inclusive ranges"
  (declare (type integer start end splits))
  (let ((amt (floor
              (/
               (- end start)
               splits))))
    (if (<= amt 1)
        (list (list start end))
        (loop for idx below splits
              :collect (let ((newstart (+ start (* idx amt))))
                         (list newstart
                               (if (= idx (1- splits))
                                   end
                                   (+ newstart amt -1))))))))

(defun sql-batch-insert (db schema col)
  (sqlite:with-transaction db
    (sqlite:execute-non-query
     db
     (format nil
             "INSERT INTO ~S VALUES ~{(~{~a~^, ~})~^, ~}"
             schema
             col))))


(defun test-byte-vec (size &key (start 0) (name "vec-db.bin"))
  (alexandria:write-byte-vector-into-file
   (make-array size
               :initial-contents (loop for _ from start below (+ start size)
                                       collect (random 126))
               :element-type '(unsigned-byte 8))
   name
   :if-exists :supersede)
  nil)


(defun test-byte-vec-split (size split)
  (time
   (let* ((tl *standard-output*)
          (ranges (get-splits 0 size split))
          (splits (length ranges)))
     (print (utils:enumerate ranges))
     (if (< splits 2)
         (test-byte-vec size :name "vec-db.bin.1")
         (dolist (range (utils:enumerate ranges))
           (sb-thread:make-thread
            (lambda ()
              (format tl "~S~%" range)
              (test-byte-vec (- (caddr range) (cadr range))
                             :name (format nil "vec-db.bin.~d" (car range))))))))))

(sqlite:with-open-database (db "vec-db.db")
  (sqlite:execute-non-query "CREATE TABLE pos (idx INT PRIMARY KEY, val INT)")
  (dotimes (idx (length *ff-arr*))
    (sqlite:execute-non-query "INSERT INTO pos(idx,val) VALUES (?,?)" idx (aref *ff-arr* idx))))

(defun test-byte-vec-2 (size)
  (let* ((file "vec-db2.bin")
         (arr (make-array size :element-type '(unsigned-byte 8))))
    (uiop:delete-file-if-exists file)
    (loop for rows below size do
      (alexandria:write-byte-vector-into-file
       (progn
         (loop for it below size
               do (setf (aref arr it) (random 255)))
         arr)
       file
       :if-does-not-exist :create
       :if-exists :append))))


(defun test-byte-vec-2-split (size)
  (time

   (lparallel:pdotimes (it size :parts 8)
     (let* ((arr (make-array size :element-type '(unsigned-byte 8))))
       (alexandria:write-byte-vector-into-file
        (progn
          (loop for it below size
                do (setf (aref arr it) (random 255)))
          arr)
        (format nil "vec-db.bin.~D" it)
        :if-does-not-exist :create
        :if-exists :append)))))


(defun test-byte-vec-3 (size)
  (time
   (with-open-file (ff "vec-db3.bin"

                       :if-exists :supersede
                       :direction :output :element-type '(unsigned-byte 8))
     (loop for rows below size do
       (write-sequence
        ;; (make-array size :element-type '(unsigned-byte 8):initial-contents
        (loop for items below size collect (random 255))
        ff)))))


(defun test-byte-vec-4 ()
  (time
   (let ((lock (bt:make-lock))
         (size (* 25600 4)))
     (with-open-file (ff "vec-db3.bin"
                         :if-exists :supersede
                         :direction :io
                         :element-type '(unsigned-byte 8))
       (lparallel:pdotimes (idx size)
         (let ((row (loop for items below size collect (random 255))))
           (bt:with-lock-held (lock)

             (file-position ff (* idx size))
             (write-sequence row ff)
             (print idx))))))))

(defun test-byte-vec-5 ()
  (time
   (let* ((size (* 25600 4))
          (arr (make-array size :element-type '(unsigned-byte 8))))
     (lparallel:pdotimes (idx size)
       (alexandria:write-byte-vector-into-file
        (progn
          (loop for it below size
                do (setf (aref arr it) (random 255)))
          arr)
        (format nil "vec-db2_~S.bin" idx)
        :if-does-not-exist :create
        :if-exists :append)))))
