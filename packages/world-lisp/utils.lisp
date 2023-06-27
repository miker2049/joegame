(defpackage utils (:use :cl)
  (:export
    enumerate
    e-distance
    memoize
    miles-to-tiles
    tiles-to-miles
    tile-n))
(in-package utils)

(defun e-distance (x1 y1 x2 y2)
    (sqrt
        (+
            (expt (- x2 x1) 2)
            (expt (- y2 y1) 2))))

(defun miles-to-tiles (miles)
  (* miles 1790))

(defun tiles-to-miles (tiles)
  (floor (/ tiles 1790)))

(defun delete-n-hashtable (table n)
  (let ((keys '()))
    (with-hash-table-iterator (hiter table)
      (loop
        (multiple-value-bind (entry-p key value)
          (hiter)
          (if entry-p
            (progn
              (setf keys
                (nconc keys (list key)))
              (if (>= (length keys) n)
                (return)))
            (return)))))
    (dolist (key keys)
      (remhash key table))))

(defvar *memo-cache-limit* 1000000)
(defun memoize (fn)
  "ty pg. With a very dirty limiter in size."
  (let ((cache (make-hash-table :synchronized t :test #'eql :weakness :key-or-value :rehash-size *memo-cache-limit*)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (let ((nv (apply fn args)))
              (setf (gethash args cache) nv)
              nv))))))


(defun compose (&rest fns)
  "ty pg"
  (if fns
    (let ((fn1 (car (last fns)))
           (fns (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall fns
            :from-end t
            :initial-value (apply fn1 args))))
    #'identity))

(defun enumerate (lst)
  (let ((n -1))
    (map 'list
      #'(lambda (item)
          (cons (incf n) item))
      lst)))


(defun tile-n (tile-h tile-w width)
  (loop
    :for row
    :from 0
    :to (- width 1)
    :collect
    (loop
      :for tile
      :from 0
      :to (- width 1)
      :collect
      `( :x ,(* tile tile-w)
         :y ,(* row tile-h)
         :width ,tile-w
         :height ,tile-h))))




(defun csv-to-db (csv db table)
  (print (format nil "file=~a" csv))
  (print (format nil "table=~a" table))
  (print (format nil "db=~a" db))
  (uiop:run-program (list "make" "csv-to-db"
                      (format nil "file=~a" csv)
                      (format nil "table=~a" table)
                      (format nil "db=~a" db))))

(defun dump-csv (x y w h &key (threads 8) (dbpath "./world.db"))
  (async:promise-all (mapcar
                       (lambda (item)
                         (let ( (this-x (+ x (getf item :x)))
                                (this-y (+ y (getf item :y)))
                                (this-w (getf item :w))
                                (this-h (getf item :h)))
                           (async:await
                             (let ((file-path (format nil
                                                "~S_~S_~S_~S.csv"
                                                this-x
                                                this-y
                                                this-w
                                                this-h)))
                               (with-open-file (fs
                                                 file-path
                                                 :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
                                 ;; (format fs "terrain_id,x,y,alt~%")
                                 (iter-terrs finalconf this-x this-y this-w this-h
                                   #'(lambda (terr x y)
                                       (if  (eql terr 0) nil
                                         (format fs "~S,~S,~S~%"  terr x y))))
                                 file-path)))))
                       (split-rect w h threads))
    (lambda (l)
      (dolist (item l)
        (format *standard-output* "Adding ~a...~%" item)
        (csv-to-db item dbpath "area")
        (format *standard-output* "Done Adding ~a~%" item)
        (uiop:run-program (list "rm" item))
        (format *standard-output* "removed ~a~%" item)))))
