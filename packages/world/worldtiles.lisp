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

(defun test-db-ins (size)

  (sqlite:with-open-database (db "test-db.db")
    (sqlite:execute-non-query db
                              "CREATE TABLE IF NOT EXISTS world ( idx int, val int )")
    (sqlite:with-transaction db
      (loop :for y :below size :do
        (progn
          (if (eql 0 (mod y 1000))

              (print
               (format nil "hey ~2,2F%" (* 100 (/ y size)))))
          (sqlite:execute-non-query
           db
           (format nil "INSERT INTO world (idx,val) VALUES ~{(~{~a~^, ~})~^, ~}"
                   (loop :for x :below size
                         :collect (list (+ x (* y size)) (random 255))))))))))

(defun test-db-ins-prepared (size)
  (declare (type fixnum size))
  (declare  (optimize (speed 3) (safety 0)))
  (sqlite:with-open-database (db "test-db.db")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS world ( idx int, val int )")
    (let ((amt (* size size))
          (stmt (sqlite:prepare-statement db  "INSERT INTO world (idx,val) VALUES (?,?)")))
      (declare (type fixnum amt))
      (sqlite:execute-non-query db "PRAGMA synchronous = OFF")
      (sqlite:execute-non-query db "PRAGMA journal_mode = WAL")
      (sqlite:with-transaction db
        (dotimes (idx amt)
          (let ((val (random 255)))
            (declare (type fixnum val idx))
            (sqlite:bind-parameter stmt 1 idx)
            (sqlite:bind-parameter stmt 2 val)
            (sqlite:step-statement stmt)
            (sqlite:clear-statement-bindings stmt)
            (sqlite:reset-statement stmt))))
      (sqlite:finalize-statement stmt))))

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


(defun test-db-ins-prepared-2 (size)
  (declare (type fixnum size))
  (declare (optimize (speed 3) (safety 0)))
  (sqlite:with-open-database (db "test-db.db")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS world ( idx int, val int, val2 int, val3 int )")
    (sqlite:execute-non-query db "PRAGMA synchronous = OFF")
    (sqlite:execute-non-query db "PRAGMA journal_mode = WAL")
    (sqlite:execute-non-query db "PRAGMA cache_size = -2000000") ; 2GB cache
    (sqlite:execute-non-query db "PRAGMA temp_store = MEMORY")

    (let* ((amt (* size size))
           (stmt (sqlite:prepare-statement db "INSERT INTO world (idx,val,val2,val3) VALUES (?,?,?,?)"))
           (batch-size 1000))
      (declare (type fixnum amt))
      (loop for idx from 0 below amt by batch-size do
        (sqlite:with-transaction db
          (dotimes (i (min batch-size (- amt idx)))
            (let ((ii (+ idx i))
                  (val (random 255)))
              (declare (type fixnum ii))
              (sqlite:bind-parameter stmt 1 ii)
              (sqlite:bind-parameter stmt 2 val)
              (sqlite:bind-parameter stmt 3 val)
              (sqlite:bind-parameter stmt 4 val)
              (sqlite:step-statement stmt)
              (sqlite:reset-statement stmt))))))))

(defun test-db-bulk (size)
  (uiop:delete-file-if-exists "test-bulk.db")
  (sqlite:with-open-database (db "test-bulk.db")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS data ( idx int, value int )"))
  (sqlite-world:init "test-bulk.db")
  (loop for idx below (expt size 2) do
    (let ((val 3))
      (declare (type fixnum idx val))
      (sqlite-world:insert* idx val)))
  (sqlite-world:finish*))

(defun test-db-bulk-2 (size)
  (declare (type fixnum size))
  (declare (optimize (speed 3) (safety 0)))
  (sqlite:with-open-database (db "test-bulk.db")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS data ( idx int, value int )"))
  (sqlite-world:init "test-bulk.db")
  (let* ((amt (* size size))
         (batch-size 1000))
    (declare (type fixnum amt))
    (loop for idx from 0 below amt by batch-size do
      (dotimes (i (min batch-size (- amt idx)))
        (let ((ii (+ idx i))
              (val (random 255)))
          (declare (type fixnum ii))
          (sqlite-world:insert* ii val))))
    (sqlite-world:finish*)))
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

;; (sqlite:with-open-database (db "vec-db.db")
;;   (sqlite:execute-non-query "CREATE TABLE pos (idx INT PRIMARY KEY, val INT)")
;;   (dotimes (idx (length *ff-arr*))
;;     (sqlite:execute-non-query "INSERT INTO pos(idx,val) VALUES (?,?)" idx (aref *ff-arr* idx))))

(defun test-byte-vec-2 (size)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum size))
  (let* ((file "vec-db2.bin.test")
         (arr (make-array size :element-type '(unsigned-byte 8))))
    (declare (type (vector (unsigned-byte 8) *) arr))
    (uiop:delete-file-if-exists file)
    (loop for rows below size do
      (alexandria:write-byte-vector-into-file
       (progn
         (loop for it below size
               do (let ((val (random 255)))
                    (declare (type fixnum val))
                    (setf (aref arr it) val)))
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
