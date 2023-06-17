(in-package worldconf)

(defvar *land-signal*
  (perlin~ 0.0004 108 '()))

(defun land~ (&rest children)
  (let ((ss *land-signal*))
    (child-sigg ss
      (list
        (sand_
          (stretch&
            (child-sigg
              ss
              children)))))))

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



(defmacro make-terrains ()
  `(progn
     (make-terrain ocean_ 0 #xB7C4CF)
     (make-terrain dirt_ 1 #x967E76)
     (make-terrain grass_ 2 #xA0D8B3)
     (make-terrain deep-grass_ 3 #xA2A378)
     (make-terrain sand_ 4 #xEEE3CB)
     (make-terrain hard-sand_ 5 #xD7C0AE)
     (make-terrain stone_ 6 #xD6E8DB)
     (make-terrain cliff_ 7 #x000000)))

(make-terrains)

(defmethod terr-id ((s sig))
  (terr-id
    (car (children s))))

(defun cliffs~ ()
  (land~
    (sand_)
    (grass_)
    (perlin~ 0.0001 108 (list (sand_) (ocean_)))
    (hard-sand_)
    (grass_)
    (deep-grass_)
    (hard-sand_)
    (sand_)
    (grass_)

    (deep-grass_)
    (hard-sand_)
    (grass_)
    (deep-grass_)
    (hard-sand_)
    ;; (perlin~ 0.00008 109 (list
    ;;                          (sand_)
    ;;                          (hard-sand_)
    ;;                          (border~
    ;;                              (perlin~ 0.00001 108 (list))
    ;;                              (ocean_)
    ;;                              (grass_))))
    ;; (perlin~ 0.00001 108 (list
    ;;                          (sand_)
    ;;                          (hard-sand_)
    ;;                          (sand_)
    ;;                          (border~
    ;;                              (perlin~ 0.00001 108 (list))
    ;;                              (grass_)
    ;;                              (ocean_))
    ;;                          (sand_)
    ;;                          (ocean_)
    ;;                          (ocean_)))
    ;; (perlin~ 0.00003 108 (list
    ;;                          (sand_)
    ;;                          (deep-grass_
    ;;                              (perlin~ 0.0003 108 (list
    ;;                                                      nil
    ;;                                                      nil
    ;;                                                      nil
    ;;                                                      nil
    ;;                                                      (ocean_))))))
    ))

(defvar finalconf)
(setf finalconf
  (ocean_
    (cliffs~)
    ;; (edge& 1
    ;;   (binary& (stretch& (land~ (cliff_)) 0.4) (/ 2 8)))
    ;; (edge& 1
    ;;   (binary& (stretch& (land~ (cliff_)) 0.4) (/ 3 8)))
    ;; (edge& 1
    ;;   (binary& (stretch& (land~ (cliff_)) 0.4) (/ 4 8)))
    ;; (edge& 1
    ;;   (binary& (stretch& (land~ (cliff_)) 0.4) (/ 5 8)))
    ;; (edge& 1
    ;;   (binary& (stretch& (land~ (cliff_)) 0.4) (/ 6 8)))
    ))



(mapcar #'name
  (resolve-terrains finalconf 2000 2000))

(defun csv-to-db (csv db table)
  (uiop:run-program (list "make" "csv-to-db"
                      (format nil "file=~a" csv)
                      (format nil "table=~a" table)
                      (format nil "db=~a" db))
    ))

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
                                 (format fs "terrain_id,x,y,alt~%")
                                 (iter-terrs finalconf this-x this-y this-w this-h
                                   #'(lambda (alt x y terr)
                                       (if  (eql terr 0) nil
                                         (format fs "~S,~S,~S,~S~%"  terr x y alt))))
                                 file-path)))))
                       (split-rect w h threads))
    (lambda (l)
      (dolist (item l)
        (format *standard-output* "Adding ~a...~%" item)
        (csv-to-db item config:*db-path* "quads")
        (format *standard-output* "Done Adding ~a~%" item)
        (uiop:run-program (list "rm" item))
        (format *standard-output* "removed ~a~%" item))
      (sb-ext:exit))))
