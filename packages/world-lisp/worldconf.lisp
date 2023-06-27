(in-package worldconf)

(defvar *worldconf* nil
  "The active terrain/signal graph for generating worlds (area pixels)")

(defvar *land-signal* nil
  "Main continent/land signal.")


(defmethod terr-id ((s sig))
  (terr-id
    (car (children s))))


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
                (map-to-range 0 1 0 max (/ (+ 1 idx) amt))
                (/ (+ 1 idx) amt)))))


(defmacro blend% (ss bss iters from-t to-t amt children )
  `(router&
     (child-sigg
       ,ss
       (append
         (fade% ,bss
           ,to-t ,from-t
           :iters ,iters)
         ,children))
     (create-compressed-routes (+ 1 ,iters) ,iters ,amt)))


(setf *land-signal*
  ;; (circle&
  (circle&
    (not-circle&
      (not-circle&
        (in-circle&
          (*&
            (perlin~ 0.0004 108 '())
            1.25)
          (point 10000 10000)
          5000 -0.5)
        (point 12500 7500)
        8000 1)
      (point 16000 1200)
      6000 1)
    (point 6000 5200)
    3000 1.2)
  ;; (point 3000 8000)
  ;; 5000 1.5)
  )
(setf *worldconf*
  (child-sigg *land-signal*
    (list
      (router&& (stretch& *land-signal* :n 0 :end 0.5)
        (0.8 .
          (__ :depths))
        (0.98 .
          (__ :trench))
        (1 .
          (__ :ocean)))
      (router&&
        (warp& (stretch& *land-signal* :n 0.5 :end 1.0) :amount 500)
        (0.1 . (__ :shore))
        (0.2 . (__ :late-shore))
        (0.5 .
          (router&&
            (warp&
              (stretch&
                (stretch& *land-signal* :n 0.5 :end 1.0)
                :n 0.2 :end 0.5)
              :amount 4000
              :offset-a1 (point 1000 0) :offset-b2 (point 1000 5000))
            (0.5 . (__ :coastal))
            (1.0 . (__ :desert))))
        (0.67 . (router&& (warp&
                            (perlin~ 0.001 10 '())
                            :amount 1000)
                  (0.1 . (__ :field
                           (child-sigg
                             (warp&
                               (stretch&
                                 (perlin~ 0.01 10 '())
                                 :n 0 :end 0.1)
                               :amount 1000)
                             (list
                               (__ :field)
                               (__ :grass-and-sand)))))
                  (0.5 . (__ :late-shore))
                  (0.9 . (__ :desert))
                  (1.0 . (__ :field))))
        (0.8 . (router&& (warp&
                           (perlin~ 0.004 10 '())
                           :amount 2000)
                 (0.2 . (__ :coastal))
                 (0.8 . (__ :rocky-sand))
                 (1.0 . (__ :desert))))
        (1 . (child-sigg (stretch& *land-signal* :n 0.75 :end 1.0)
               (list
                 (__ :grass-and-sand)
                 (__ :dead-forest)
                 (__ :old-pavement-desert)
                 (__ :grass-and-sand)
                 (__ :field)
                 (__ :forest
                   (child-sigg
                     (warp&
                       (perlin~ 0.01 109 nil)
                       :amount 100)
                     (list
                       (__ :forest)
                       (__ :old-pavement-field)
                       (__ :old-pavement-forest)))))))))))
