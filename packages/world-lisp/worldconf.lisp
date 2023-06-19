(in-package worldconf)

(defvar *land-signal*
  (*&
    (perlin~ 0.0004 108 '())
    1.25))


(defun land~~ (&rest children)
  (let ((ss  *land-signal*))
    (child-sigg ss
      (list
        nil
        (__ :sand
          (stretch&
            (child-sigg
              ss
              children)
            :n 0.5))))))

(defun border~ (sigg ter1 ter2 &rest children)
  (let ((ss sigg) (n 0.58))
    (child-sigg ss
      (list
        (child-sigg ter1
          (list
            (child-sigg ss
              (list
                (stretch&
                  (binary&
                    (child-sigg ter2 '()) n))))))))))

(defvar finalconf '()
  "final conf thats turned to json")




(defmethod terr-id ((s sig))
  (terr-id
    (car (children s))))

(defun land~ (&rest children)
  (child-sigg *land-signal*
    (list
      nil
      (__ :sand
        (router&
          (child-sigg (stretch& *land-signal*)
            (list
              (__ :sand)
              (__ :grass)
              (__ :deep-grass)
              (__ :sand)
              (__ :hard-sand)
              ))
          '(0.1 0.5 0.7 0.75 1))))))



(defun cliffs~~ ()
  (land~
    (perlin~ 0.1 108 (list (__ :sand) (__ :ocean) (__ :ocean) (__ :ocean)))
    (perlin~ 0.01 108 (list (__ :sand
                              (perlin~ 0.1 420 (list (__ :ocean)
                                                 (__ :ocean)
                                                 (__ :sand)
                                                 (__ :sand)
                                                 (__ :sand))))
                        (__ :ocean)))
    (perlin~ 0.1 69 (list (__ :grass) (__ :sand) (__ :sand) (__ :sand) (__ :hard-sand)))
    (binary& (perlin~ 0.08 69 (list (__ :grass) (__ :sand)))
      0.3)
    (perlin~ 0.08 69
      (list (stretch&
              (perlin~ 0.08 69
                (list (__ :sand) (__ :grass) (__ :ocean) (__ :deep-grass)))
              0.5)))
    (perlin~ 0.08 69 (list (__ :clay) (__ :sand) (__ :grass) (__ :grass) (__ :deep-grass)))
    (perlin~ 0.08 108 (list
                        (__ :grass)
                        (__ :clay)
                        (__ :sand)
                        (__ :grass)
                        (__ :grass)
                        (__ :deep-grass)
                        (__ :sand)))
    (perlin~ 0.05 108 (list (__ :deep-grass) (__ :sand) (__ :hard-sand) ))
    (perlin~ 0.05 108 (list (__ :deep-grass) (__ :hard-sand) (__ :hard-sand) ))
    (perlin~ 0.05 108 (list (__ :deep-grass) (__ :sand) (__ :hard-sand) ))
    (perlin~ 0.05 108 (list
                        (__ :deep-grass)
                        (__ :sand)
                        (__ :stone)
                        (__ :hard-sand)))
    (perlin~ 0.05 108 (list
                        (__ :stone)
                        (__ :sand)
                        (__ :deep-grass)
                        (__ :hard-sand)))
    (perlin~ 0.05 108 (list
                        (__ :stone)
                        (__ :sand)
                        (__ :stone)
                        (__ :ice)))
    (perlin~ 0.05 108 (list
                        (__ :stone)
                        (__ :stone)
                        (__ :ice)))))

(defmacro router&& (sigg &rest rs)
  `(router&
     (child-sigg ,sigg
       (list ,@(mapcar #'cdr rs)))
     ',(mapcar #'car rs)))

(defmacro fade% (sig terr-a terr-b &key (iters 8))
  `(list
     ,@(loop :for idx to (- iters 1)
         :collect `(router&& ,sig
                     (,(* idx (/ 1 iters)) . (__ ,terr-a))
                     (1 . (__ ,terr-b))))))




(defun create-routes (ll &key (func #'(lambda (idx) (/ idx ll))))
  (loop
    for ii to (- ll 1)
    :collect (funcall func ii)))

(defun create-compressed-routes (amt n max)
  "Compress the first n children of amt children to stop at max,
where the rest of children have even distance"
  (create-routes amt
    :func #'(lambda (idx)
              (if (< idx n)
                (map-to-range 0 1 0 0.3 (/ (+ 1 idx) amt))
                (/ (+ 1 idx) amt)))))

(setf finalconf
  (__ :ocean
    (land~~
      (router&
        (child-sigg (stretch& *land-signal* :n 0.5)
          (append
            (fade% (perlin~ 0.2 108 nil)
              :sand :ocean
              :iters 16)
            (list (__ :hard-sand))))
        (create-compressed-routes 17 16 0.3)))))


(defun csv-to-db (csv db table)
  (print (format nil "file=~a" csv))
  (print (format nil "table=~a" table))
  (print (format nil "db=~a" db))
  (uiop:run-program (list "make" "csv-to-db"
                      (format nil "file=~a" csv)
                      (format nil "table=~a" table)
                      (format nil "db=~a" db))))

(defun dump-csv (x y w h &key (threads 8))
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
        (csv-to-db item config:*db-path* "area")
        (format *standard-output* "Done Adding ~a~%" item)
        (uiop:run-program (list "rm" item))
        (format *standard-output* "removed ~a~%" item)))))
