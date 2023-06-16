(in-package worldconf)

(defvar *land-signal*
  (perlin~ 0.000004 108 '()))

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



(make-terrain ocean_ #xB7C4CF)
(make-terrain dirt_ #x967E76)
(make-terrain grass_ #xA0D8B3)
(make-terrain deep-grass_ #xA2A378)
(make-terrain sand_ #xEEE3CB)
(make-terrain hard-sand_ #xD7C0AE)
(make-terrain stone_ #xD6E8DB)
(make-terrain cliff_ #x000000)

(defun cliffs~ ()
  (land~
    (sand_)
    (grass_)
    (edge& 1
      (binary&
        (perlin~ 0.0001 108 (list (sand_) (ocean_))) 0.9))
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
(setf finalconf
  (ocean_
    (cliffs~)
    (edge& 1
      (binary& (stretch& (land~ (cliff_)) 0.4) (/ 2 8)))
    (edge& 1
      (binary& (stretch& (land~ (cliff_)) 0.4) (/ 3 8)))
    (edge& 1
      (binary& (stretch& (land~ (cliff_)) 0.4) (/ 4 8)))
    (edge& 1
      (binary& (stretch& (land~ (cliff_)) 0.4) (/ 5 8)))
    (edge& 1
      (binary& (stretch& (land~ (cliff_)) 0.4) (/ 6 8)))))

(defun add-terrs-to-db (terrs x y)
  "Terrain come in lists from 0 to n 'alt' indexes."
  (dotimes (tidx (length terrs))
    (let ((trr
            (db:get-terr-by-name
              (string-downcase
                (remove #\_
                  (name
                    (nth tidx terrs)))))))
      (db:add-terr trr x y tidx))))

(defun sync-db (conf x y w h)
  (loop
    :for _y :from y :to (+ y h)
    :do (loop
          :for _x :from x :to (+ x w)
          :do (add-terrs-to-db (resolve-terrains conf _x _y) _x _y))))

;; (sync-db finalconf 400 1000 (* 1979 1) (* 1979 5))
(sync-db finalconf 400 1000 (* 10 1) (* 10 1))
;; (sb-thread:list-all-threads)
(sync-db finalconf 0 0 100 100)

(defun collect-tiles (conf x y w h)
  (loop
    :for _y :from y :to (+ y h)
    :collect (loop
               :for _x :from x :to (+ x w)
               :collect (resolve-terrains conf _x _y))))

(defun iter-terrs (conf x y w h cb)
  (loop
    :for _y :from y :to (+ y h)
    :do (loop
          :for _x :from x :to (+ x w)
          :do (progn
                (dolist (terrain (utils:enumerate (resolve-terrains conf _x _y)))
                  (funcall cb (car terrain) _x _y (cdr terrain)))))))

(progn
  (time (collect-tiles finalconf 0 0 10 10))
  (print *trace-output*))

(progn
  (time (sync-db finalconf 0 0 10 10))
  (print *trace-output*))

(with-open-file (fs "tiles.csv"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)
  (iter-terrs finalconf 0 0 5 5
    #'(lambda (alt x y terr)
        (format fs "~S,~S,~S,~S~%" (name terr) x y alt))))

(utils:enumerate (resolve-terrains finalconf 1 2))
